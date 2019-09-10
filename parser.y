%require "3.2"
%language "c++"
%define api.value.type variant
%define api.token.constructor
%locations
%define parse.trace
%define parse.error verbose


%code requires {
#include "context.hpp"

namespace seagol {
struct Lexer;
}
}

%{
#include <string>

#include "parser.hpp"
#include "semantic.hpp"

using namespace std::literals;
using str = std::string;

namespace yy {
parser::symbol_type yylex( seagol::Lexer & );
}

void chk_and_add_arg( seagol::CallInfo* ci, const seagol::ExprInfo &arg,
                      const yy::parser::location_type &loc,
                      yy::parser &p );

#define pt( t ) (ctx.format_type(( t )))
#define IRB (ctx.irb)
#define LOOP (ctx.loop_stack.back())
#define NOT_IMPLEMENTED(l) do { error( (l), "NOT IMPLEMENTED" ); } while(0);
#define EI_RVal seagol::ExprInfo::RVal
%}

%lex-param { seagol::Lexer &lexer }
%parse-param { seagol::Context &ctx }
%parse-param { seagol::Lexer &lexer }

%token END_OF_FILE 0 "end of file"
%token TODO "unimplemented"

%token <std::string> IDENTIFIER "identifier"
%token <llvm::Type*> TYPE_NAME "type name"
%token <seagol::ConstantInt> CONSTANT_I "integral constant"
%token <std::string> CONSTANT_S "string constant"
%token NULLPTR "NULL"

%token TYPEDEF "typedef"
%token RETURN "return"
%token IF "if"
%token ELSE "else"
%token SIZEOF "sizeof"
%token DO "do"
%token WHILE "while"
%token BREAK "break"
%token CONTINUE "continue"

%token ';' '(' ')' '{' '}' '!' '?' ':'
%token L_OR "||"
%token L_AND "&&"
%token EQ "=="
%token NEQ "!="
%token LEQ "<="
%token GEQ ">="
%token SHL "<<"
/* %token LSHR ">>" */
%token ASHR ">>"
%token ARR "->"
%token INC "++"
%token DEC "--"

%nonassoc ELSELESS
%nonassoc ELSE

%left L_OR
%left L_AND
%left '|'
%left '^'
%left '&'
%nonassoc EQ NEQ
%nonassoc '<' '>' LEQ GEQ
%left SHL LSHR ASHR
%left '+' '-'
%left '*' '/' '%'
%right UNARY_PLUS UNARY_MINUS

%type <llvm::Type*> type
%type <llvm::Type*> complete_type
%type <llvm::Type*> returnable_type
%type <llvm::Type*> _bool
%type <llvm::Type*> _i64
%type <std::vector<llvm::Type*>> type_list

%type <seagol::IdentifierInfo*> toplevel_identifier
%type <seagol::IdentifierInfo*> fresh_identifier
%type <seagol::IdentifierInfo*> identifier
%type <std::string> nodecl_fresh_identifier
%type <seagol::IdentifierInfo*> argument_decl
%type <seagol::IdentifierInfo*> declaration

%type <seagol::ArgumentList> arguments
%type <seagol::ArgumentList> argument_decl_list

%type <seagol::CallInfo_u> _fn
%type <seagol::CallInfo*> arg_list
%type <seagol::ExprInfo> _mkcall

%type <seagol::ExprInfo> arg
%type <seagol::ExprInfo> expression
%type <seagol::ExprInfo> expr
%type <seagol::ExprInfo> bexpr
%type <seagol::ExprInfo> aexpr
%type <seagol::ExprInfo> cast_expr
%type <seagol::ExprInfo> unary_expr
%type <seagol::ExprInfo> postfix_expr
%type <seagol::ExprInfo> primary_expr
%type <seagol::ExprInfo> _coerce
%type <llvm::Constant*> _const

%type <seagol::IfInfo> _if

%type <llvm::BasicBlock*> _trashify

%start toplevel

%%

/* Non-terminals starting with an underscore are purely semantic empty rules,
 * (here, e.g., for opening and closing declaration scopes). Sometimes, they
 * may reference previous semantic values ($0, $-1 etc.), so beware!
 */

_open : %empty { ctx.open_scope(); } ;
_close : %empty { ctx.close_scope(); } ;

/* usage in rules: ... <ExprInfo> <llvm::Type*> _coerce[res] ... */
_coerce : %empty {
             @$ = @-1;
             auto *to_type = $<llvm::Type*>0;
             auto &from_ex = $<seagol::ExprInfo>-1;
             auto *from_type = from_ex.type();
             if ( ! ctx.coercible( from_type, to_type ) ) {
                 error( @-1, "invalid conversion from `"s + pt( from_type ) +
                         "\' to `" + pt( to_type ) + '\'' );
             } else {
                 $$ = ctx.coerce( from_ex, to_type );
             }
         } ;
_bool : %empty { $$ = IRB.getInt1Ty(); }
_i64 : %empty { $$ = IRB.getInt64Ty(); }

/* Usage in rules: <ExprInfo> _lvalue */
_lvalue: %empty {
            if ( ! $<seagol::ExprInfo>0.assignable() )
                error( @0, "expecting an lvalue" );
        } ;

/* Usage in rules: <ExprInfo> _addressable */
_addressable: %empty {
            if ( ! $<seagol::ExprInfo>0.addressable() )
                error( @0, "cannot take expression's address" );
        } ;

/* Usage in rules: <ExprInfo> _isptr */
_isptr: %empty {
            if ( ! $<seagol::ExprInfo>0.derefable() )
                error( @0, "expression is not of pointer type" );
        } ;

/* Usage in rules: <ExprInfo> _const */
_const: %empty {
            auto *llval = $<seagol::ExprInfo>0.llval;
            if ( auto *cexpr = llvm::dyn_cast< llvm::Constant >( llval ) ) {
                $$ = cexpr;
            } else
                error( @0, "expression is not (blatantly) constant" );
        } ;

_inloop: %empty {
             if ( ctx.loop_stack.empty() )
                 error( @0, "statement can be used only within a loop" );
        } ;

toplevel
    : toplevel_entry_list END_OF_FILE
    ;

toplevel_entry_list
    : toplevel_entry
    | toplevel_entry_list toplevel_entry
    ;

toplevel_entry
    /* function declaration */
    : returnable_type[rt] toplevel_identifier[fii] arguments[args] ';' _close {
        llvm::Type *oldt;
        if ( ! ctx.decl_fun( $fii, $rt, $args, &oldt ) )
            error( @$, "`"s + $fii->name + "\' was already declared with type `"
            + pt( oldt ) + "\'" );
    }
    /* function definition */
    | returnable_type[rt] toplevel_identifier[fii] arguments[args] {
        llvm::Type *oldt;
        if ( ! ctx.decl_fun( $fii, $rt, $args, &oldt ) )
            error( @$, "`"s + $fii->name + "\' was already declared with type `"
            + pt( oldt ) + "\'" );
        if ( ! llvm::cast< llvm::Function >( $fii->llval )->isDeclaration() )
            error( @fii, "`"s + $fii->name + "\' was already defined" );
        ctx.start_fun( $fii, $args );
    }
      block_stmt _close[end] {
          if ( $rt->isVoidTy() && IRB.GetInsertBlock() != ctx.bb_trash ) {
              IRB.CreateRetVoid();
              ctx.discard();
          }
          if ( !ctx.end_fun() )
              error( @end, "control reaches end of non-void function" );
    }
    /* global variable declaration */
    | returnable_type[ty] toplevel_identifier[vii] ';' {
        /* using complete_type introduces a shift/reduce conflict */
        if ( $1->isVoidTy() )
            error( @$, "void type is not allowed here" );
        llvm::Type *oldt;
        if ( ! ctx.decl_global( $vii, $ty, &oldt ) )
            error( @$, "`"s + $vii->name + "\' was already declared with type `"
                    + pt( oldt ) + "\'" );
    }
    /* global variable definition */
    | returnable_type[ty] toplevel_identifier[vii] '=' {
        /* using complete_type introduces a shift/reduce conflict */
        if ( $1->isVoidTy() )
            error( @$, "void type is not allowed here" );
        llvm::Type *oldt;
        if ( ! ctx.decl_global( $vii, $ty, &oldt ) )
            error( @$, "`"s + $vii->name + "\' was already declared with type `"
                    + pt( oldt ) + "\'" );
        if ( llvm::cast< llvm::GlobalVariable >( $vii->llval )->hasInitializer() )
            error( @vii, "`"s + $vii->name + "\' was already initialised" );
        ctx.discard();
    }   expr <llvm::Type*>{ $$ = $ty; } _coerce _const[ini] ';' {
            llvm::cast< llvm::GlobalVariable >( $vii->llval )->setInitializer( $ini );
    }
    | TYPEDEF type nodecl_fresh_identifier ';' { ctx.decl_type( $3, $2 ); }
    ;

toplevel_identifier /* may or may not be already declared */
    : IDENTIFIER
        {   auto id_info = ctx.find_id( $1 );
            $$ = id_info ? id_info : ctx.decl_id( $1 );
        }
    ;

fresh_identifier /* must not exist, gets declared */
    : IDENTIFIER
        {   auto id_info = ctx.decl_id( $1 );
            if ( !id_info )
                error( @1, "multiple declarations of `"s + $1 + "\' within scope" );
            $$ = id_info;
        }
    ;

identifier /* must exist */
    : IDENTIFIER
        {   auto id_info = ctx.find_id( $1 );
            if ( !id_info )
                error( @1, "`"s + $1 + "\' was not declared in this scope" );
            $$ = id_info;
        }
    ;

nodecl_fresh_identifier /* must not exist, doesn't get declared */
    : IDENTIFIER
        {   if ( ctx.find_id( $1 ) || ctx.find_type( $1 ) )
                error( @1, "name `"s + $1 + "\' already in use" );
            $$ = $1;
        }


type
    : TYPE_NAME
    | type '*'
        { if ( $1->isVoidTy() )
              $$ = ctx.anyptr_ty;
          else
              $$ = $1->getPointerTo();
        }
    | returnable_type '(' '*' ')' '(' ')'
        { $$ = llvm::FunctionType::get( $1, false)->getPointerTo(); }
    | returnable_type '(' '*' ')' '(' type_list[args] ')'
        { $$ = llvm::FunctionType::get( $1, $args, false)->getPointerTo(); }
    ;

type_list
    : complete_type { $$.push_back( $1 ); }
    | type_list ',' complete_type { $$ = std::move( $1 ); $$.push_back( $3 ); }
    ;

returnable_type /* complete or void */
    : type
        { $$ = $1;
            if ( $$->isStructTy() && llvm::cast<llvm::StructType>($$)->isOpaque() )
                error( @$, "type `"s + pt( $1 ) + "\' is incomplete" );
        }
    ;

complete_type
    : returnable_type
        { $$ = $1;
            if ( $$->isVoidTy() )
                error( @$, "void type is not allowed here" );
        }
    ;

arguments
    : '(' _open ')' { $$.args = {}; }
    | '(' _open argument_decl_list ')' { $$ = std::move( $3 ); }
    ;

argument_decl_list
    : argument_decl { $$.args.push_back( $1 ); }
    | argument_decl_list ',' argument_decl {
        $$ = std::move( $1 );
        $$.args.push_back( $3 );
    }
    ;

argument_decl
    : complete_type {
        auto *ii = ctx.gen_id( "arg" );
        $$ = ctx.mk_arg( $1, ii );
    }
    | complete_type fresh_identifier { $$ = ctx.mk_arg( $1, $2 ); }
    ;

block_stmt
    : '{' '}'
    | '{' _open stmt_list '}' _close
    ;

stmt_list
    : statement
    | stmt_list statement
    ;

statement
    : block_stmt
    | if_stmt
    | RETURN ';' {
        auto *rty = IRB.getCurrentFunctionReturnType();
        if ( ! rty->isVoidTy() )
            error( @2, "expected expression of type `"s + pt( rty ) + "\'" );
        IRB.CreateRetVoid(); ctx.discard();
    }
    | RETURN expression <llvm::Type*>{ $$ = IRB.getCurrentFunctionReturnType(); }
      _coerce[res] ';' { IRB.CreateRet( $res.llval ); ctx.discard(); }
    | ';'
    | expression ';'
    | declaration ';'
    | declaration[l] '=' expression <llvm::Type*>{ $$ = $l->type; } _coerce[r] ';'
        { IRB.CreateStore( $r.llval, $l->llval ); }
    | _loop_prepare loop_stmt _loop_finish
    | CONTINUE _inloop ';' { IRB.CreateBr( LOOP.bb_cont ); ctx.discard(); }
    | BREAK _inloop ';' { IRB.CreateBr( LOOP.bb_false ); ctx.discard(); }
    ;

declaration
    : complete_type fresh_identifier {
        $$ = $2;
        $$->type = $1;
        $$->llval = IRB.CreateAlloca( $$->type, nullptr, $$->name + ".addr" );
    }
    ;

if_stmt
    : IF _if[br] '(' expression _bool _coerce ')' _then statement ELSE _else statement
    {
        IRB.CreateBr( $br.bb_cont );
        IRB.SetInsertPoint( $br.bb_cont );
    }
    | IF _if[br] '(' expression _bool _coerce ')' _then statement %prec ELSELESS
    {
        IRB.CreateBr( $br.bb_cont );
        IRB.SetInsertPoint( $br.bb_false );
        IRB.CreateBr( $br.bb_cont );
        IRB.SetInsertPoint( $br.bb_cont );
    }
    ;

_if: %empty
    {
        auto *bb_cont = ctx.mk_bb( "if.cont" );
        auto *bb_false = ctx.mk_bb( "if.false" );
        auto *bb_true = ctx.mk_bb( "if.true" );
        $$ = { bb_true, bb_false, bb_cont };
    };

_then: %empty
    {
        auto & _if = $<seagol::IfInfo>-5;
        auto & pred_expr = $<seagol::ExprInfo>-1;
        IRB.CreateCondBr( pred_expr.llval, _if.bb_true, _if.bb_false );
        IRB.SetInsertPoint( _if.bb_true );
    } ;

_else: %empty
    {
        auto & _if = $<seagol::IfInfo>-8;
        IRB.CreateBr( _if.bb_cont );
        IRB.SetInsertPoint( _if.bb_false );
    } ;

loop_stmt
    : WHILE '('
        { IRB.CreateBr( LOOP.bb_cont ); IRB.SetInsertPoint( LOOP.bb_cont ); }
        expression _bool _coerce[p] ')'
        { IRB.CreateCondBr( $p.llval, LOOP.bb_true, LOOP.bb_false );
          IRB.SetInsertPoint( LOOP.bb_true ); }
        statement
        { IRB.CreateBr( LOOP.bb_cont ); }
    | DO { IRB.CreateBr( LOOP.bb_true ); IRB.SetInsertPoint( LOOP.bb_true ); }
        statement
        WHILE'('
        { IRB.CreateBr( LOOP.bb_cont ); IRB.SetInsertPoint( LOOP.bb_cont ); }
        expression _bool _coerce[p] ')'
        { IRB.CreateCondBr( $p.llval, LOOP.bb_true, LOOP.bb_false ); }
    ;

_loop_prepare: %empty
    {
        auto *bb_false = ctx.mk_bb( "loop.after" );
        auto *bb_true = ctx.mk_bb( "loop.body" );
        auto *bb_cont = ctx.mk_bb( "loop.cond" );
        ctx.loop_stack.push_back({ bb_true, bb_false, bb_cont });
    } ;

_loop_finish: %empty
    {
        IRB.SetInsertPoint( LOOP.bb_false );
        ctx.loop_stack.pop_back();
    } ;

expression
    : expr
    | unary_expr[l] _lvalue '=' expression <llvm::Type*>{ $$ = $l.type(); } _coerce[r]
    {
        IRB.CreateStore( $r.llval, $l.llval );
        $$ = $l;
    }
    ;

expr
    : aexpr
    | bexpr[l] L_OR <seagol::IfInfo>{
        auto *bb_this = IRB.GetInsertBlock();
        auto *bb_post = ctx.mk_bb( "or.post" );
        auto *bb_right = ctx.mk_bb( "or.right" );
        IRB.CreateCondBr( $l.llval, bb_post, bb_right );
        IRB.SetInsertPoint( bb_right );
        $$ = { bb_this, bb_right, bb_post };
    }[br] bexpr[r] {
        auto *bb_right = IRB.GetInsertBlock();
        IRB.CreateBr( $br.bb_cont );
        IRB.SetInsertPoint( $br.bb_cont );
        auto *phi = IRB.CreatePHI( IRB.getInt1Ty(), 2 );
        phi->addIncoming( IRB.getTrue(), $br.bb_true );
        phi->addIncoming( $r.llval, bb_right );
        $$ = ctx.mk_bin( $l, $r, phi );
    }
    | bexpr[l] L_AND <seagol::IfInfo>{
        auto *bb_this = IRB.GetInsertBlock();
        auto *bb_post = ctx.mk_bb( "and.post" );
        auto *bb_right = ctx.mk_bb( "and.right" );
        IRB.CreateCondBr( $l.llval, bb_post, bb_right );
        IRB.SetInsertPoint( bb_right );
        $$ = { bb_right, bb_this, bb_post };
    }[br] bexpr[r] {
        auto *bb_right = IRB.GetInsertBlock();
        IRB.CreateBr( $br.bb_cont );
        IRB.SetInsertPoint( $br.bb_cont );
        auto *phi = IRB.CreatePHI( IRB.getInt1Ty(), 2 );
        phi->addIncoming( IRB.getFalse(), $br.bb_false );
        phi->addIncoming( $r.llval, bb_right );
        $$ = ctx.mk_bin( $l, $r, phi );
    }
    ;

bexpr: expr _bool _coerce[res] { $$ = $res; } ;

aexpr
    : cast_expr
    | aexpr  '+' aexpr { $$ = ctx.mk_arith( this, @$, $1, $3, llvm::Instruction::Add ); }
    | aexpr  '-' aexpr { $$ = ctx.mk_arith( this, @$, $1, $3, llvm::Instruction::Sub ); }
    | aexpr  '*' aexpr { $$ = ctx.mk_arith( this, @$, $1, $3, llvm::Instruction::Mul ); }
    | aexpr  '/' aexpr { $$ = ctx.mk_arith( this, @$, $1, $3, llvm::Instruction::SDiv); }
    | aexpr  '%' aexpr { $$ = ctx.mk_arith( this, @$, $1, $3, llvm::Instruction::SRem); }
    | aexpr  '|' aexpr { $$ = ctx.mk_arith( this, @$, $1, $3, llvm::Instruction::Or  ); }
    | aexpr  '^' aexpr { $$ = ctx.mk_arith( this, @$, $1, $3, llvm::Instruction::Xor ); }
    | aexpr  '&' aexpr { $$ = ctx.mk_arith( this, @$, $1, $3, llvm::Instruction::And ); }
    | aexpr  SHL aexpr { $$ = ctx.mk_arith( this, @$, $1, $3, llvm::Instruction::Shl ); }
    | aexpr ASHR aexpr { $$ = ctx.mk_arith( this, @$, $1, $3, llvm::Instruction::AShr); }
    | aexpr   EQ aexpr { $$ = ctx.mk_cmp( this, @$, $1, $3, llvm::CmpInst::ICMP_EQ ); }
    | aexpr  NEQ aexpr { $$ = ctx.mk_cmp( this, @$, $1, $3, llvm::CmpInst::ICMP_NE ); }
    | aexpr  GEQ aexpr { $$ = ctx.mk_cmp( this, @$, $1, $3, llvm::CmpInst::ICMP_SGE ); }
    | aexpr  LEQ aexpr { $$ = ctx.mk_cmp( this, @$, $1, $3, llvm::CmpInst::ICMP_SLE ); }
    | aexpr  '>' aexpr { $$ = ctx.mk_cmp( this, @$, $1, $3, llvm::CmpInst::ICMP_SGT ); }
    | aexpr  '<' aexpr { $$ = ctx.mk_cmp( this, @$, $1, $3, llvm::CmpInst::ICMP_SLT ); }
    ;

cast_expr
    : unary_expr
    | '(' complete_type[to_type] ')' cast_expr[from_ex] {
        auto *from_type = $from_ex.type();
        if ( ! ctx.castable( from_type, $to_type ) ) {
            error( @$, "invalid cast from `"s + pt( from_type ) +
                    "\' to `" + pt( $to_type ) + '\'' );
        } else {
            $$ = ctx.cast( $from_ex, $to_type );
        }
    }
    ;

unary_expr
    : postfix_expr
    | '-' cast_expr _i64  _coerce[res] %prec UNARY_MINUS {
        $$ = $res;
        $$.llval = IRB.CreateNeg( $res.llval );
    }
    | '+' cast_expr _i64  _coerce[res] %prec UNARY_PLUS {
        $$ = $res;
    }
    | '!' cast_expr _bool _coerce[res] {
        $$ = $res;
        $$.llval = IRB.CreateNot( $res.llval );
    }
    | '&' cast_expr _addressable {
        $$ = $2;
        $$.rvalise();
    }
    | '*' cast_expr _isptr {
        if ( llvm::isa< llvm::Function >( $2.llval ) )
            $$ = $2;
        else {
            if ( $2.addressable() ) {
                $$.llval = IRB.CreateLoad( $2.llval );
                $$.cat = seagol::Value::LVALUE;
            } else {
                $$ = $2;
                $$.cat = seagol::Value::LVALUE;
            }
        }
    }
    | SIZEOF _trashify[bb] unary_expr[e] {
        IRB.SetInsertPoint( $bb );
        if ( ! $e.type()->isSized() )
            error( @e, "expression type does not have size" );
        auto sz = ctx.lldatalayout.getTypeAllocSize( $e.type() );
        $$.llval = IRB.getInt32( sz );
        $$.cat = seagol::Value::RVALUE;
    }
    | SIZEOF _trashify[bb] '(' type[ty] ')' {
        IRB.SetInsertPoint( $bb );
        if ( ! $ty->isSized() )
            error( @ty, "type does not have size" );
        auto sz = ctx.lldatalayout.getTypeAllocSize( $ty );
        $$.llval = IRB.getInt32( sz );
        $$.cat = seagol::Value::RVALUE;
    }
    ;

_trashify : %empty
{
    $$ = IRB.GetInsertBlock();
    ctx.discard();
} ;

postfix_expr
    : primary_expr
    | postfix_expr '(' _fn <seagol::CallInfo*>{ $$ = $3.get(); } ')' _mkcall[c] { $$ = $c; }
    | postfix_expr '(' _fn arg_list ')' _mkcall[c] { $$ = $c; }
    | postfix_expr[l] '[' expr[r] ']' {
        $$ = ctx.mk_arith( this, @$, $l, $r, llvm::Instruction::Add );
        if ( ! $$.pointer() )
            error( @$, "invalid operand types `" + pt( $l.type() ) + "' and `"
                        + pt( $r.type() ) + "'" );
        assert( ! $$.addressable() );
        $$.cat = seagol::Value::LVALUE;
    }
    ;

arg_list
    : arg {
        $$ = $<seagol::CallInfo_u>0.get();
        auto err = ctx.push_param( $$, $1 );
        if ( !err.empty() )
            error( @1, err );
    }
    | arg_list ',' arg {
        $$ = $1;
        auto err = ctx.push_param( $$, $3 );
        if ( !err.empty() )
            error( @3, err );
    }
    ;

arg : expression ;

_fn : %empty {
          auto fn_expr = $<seagol::ExprInfo>-1;
          if ( fn_expr.callable() ) {
              fn_expr = ctx.coerce( fn_expr );
              $$.reset( new seagol::CallInfo{} );
              $$->fn = fn_expr.llval;
              auto ftype = llvm::dyn_cast< llvm::FunctionType >( $$->fn->getType() );
              if ( !ftype ) {
                  assert( $$->fn->getType()->isPointerTy() );
                  ftype = llvm::dyn_cast< llvm::FunctionType >(
                        $$->fn->getType()->getPointerElementType() );
                  assert( ftype );
              }
              $$->param_it = ftype->param_begin();
              $$->param_end = ftype->param_end();
              $$->ftype = ftype;
          } else {
              error( @0, "cannot call to a non-callable value" );
          }
      } ;

_mkcall : %empty {
              auto *ci = $<seagol::CallInfo*>-1;
              unsigned argc_req = ci->ftype->getNumParams();
              unsigned argc = ci->args.size();
              if ( argc < argc_req )
                  error( @0, "too few arguments for call to type `"s +
                          pt( ci->ftype ) + "\': requested " +
                          std::to_string( argc_req ) + ", but only " +
                          std::to_string( argc ) + " provided" );
              $$.cat = seagol::Value::RVALUE;
              $$.llval = IRB.CreateCall( ci->fn, ci->args );
          } ;


primary_expr
    : identifier {
        $$.llval = $1->llval;
        if ( llvm::isa< llvm::Function >( $$.llval ) ) {
            $$.cat = seagol::Value::FVALUE;
        } else {
            $$.cat = seagol::Value::LVALUE;
        }
    }
    | NULLPTR {
        $$.llval = llvm::Constant::getNullValue( ctx.anyptr_ty );
        $$.cat = seagol::Value::CVALUE;
    }
    | CONSTANT_I {
        $$.llval = IRB.getIntN( $1.width, $1.number );
        $$.cat = seagol::Value::CVALUE;
    }
    | CONSTANT_S {
        if ( ctx.in_global_scope() ) {
            error( @$, "no support for global strings yet" );
        } else {
            $$.llval = IRB.CreateGlobalStringPtr( $1 );
        }
        $$.cat = seagol::Value::CVALUE;
    }
    | '(' expression ')' { $$ = $2; }
    ;

