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

#define pt( t ) (ctx.format_type(( t )))
#define IRB (ctx.irb)
#define NOT_IMPLEMENTED(l) do { error( (l), "NOT IMPLEMENTED" ); } while(0);
#define EI_RVal seagol::ExprInfo::RVal
%}

%lex-param { seagol::Lexer &lexer }
%parse-param { seagol::Context &ctx }
%parse-param { seagol::Lexer &lexer }

%token END_OF_FILE 0 "end of file"
%token TODO "unimplemented"

%token <std::string> IDENTIFIER "identifier"
%token <seagol::TypeName> TYPE_NAME "type name"
%token <seagol::ConstantInt> CONSTANT_I "integral constant"

%token RETURN "return"

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

%type <seagol::IdentifierInfo*> toplevel_identifier
%type <seagol::IdentifierInfo*> fresh_identifier
%type <seagol::IdentifierInfo*> identifier
%type <seagol::IdentifierInfo*> argument_decl

%type <seagol::ArgumentList> arguments
%type <seagol::ArgumentList> argument_decl_list

%type <seagol::ExprInfo> expression
%type <seagol::ExprInfo> conditional_expr
%type <seagol::ExprInfo> expr
%type <seagol::ExprInfo> bexpr
%type <seagol::ExprInfo> arith_expr
%type <seagol::ExprInfo> aexpr
%type <seagol::ExprInfo> cast_expr
%type <seagol::ExprInfo> unary_expr
%type <seagol::ExprInfo> postfix_expr
%type <seagol::ExprInfo> primary_expr
%type <seagol::ExprInfo> _coerce

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
      block_stmt _close { ctx.end_fun(); }
    /* global variable declaration */
    | returnable_type[ty] toplevel_identifier[vii] ';' {
        /* using complete_type introduces a shift/reduce conflict */
        if ( $1->isVoidTy() )
            error( @$, "void type is not allowed here" );
        /* TODO check whether types match */
    }
    /* global variable definition */
    | returnable_type[ty] toplevel_identifier[vii] '=' {
        /* using complete_type introduces a shift/reduce conflict */
        if ( $1->isVoidTy() )
            error( @$, "void type is not allowed here" );
        /* TODO check whether types match */
    }   conditional_expr ';'
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


type
    : TYPE_NAME
        { if ( !($$ = ctx.get_type( $1.tid )))
            error( @1, "`"s + $1.name + "\' does not name a type" );
        }
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
    | RETURN ';' {
        auto *rty = IRB.getCurrentFunctionReturnType();
        if ( ! rty->isVoidTy() )
            error( @2, "expected expression of type `"s + pt( rty ) + "\'" );
        IRB.CreateRetVoid(); ctx.after_return();
    }
    | RETURN expression <llvm::Type*>{ $$ = IRB.getCurrentFunctionReturnType(); }
      _coerce[res] ';' { IRB.CreateRet( $res.llval ); ctx.after_return(); }
    ;

expression
    : conditional_expr
    | unary_expr '=' expression { NOT_IMPLEMENTED(@$) }
    ;

conditional_expr
    : expr
    | expr _bool _coerce[p] '?' expression[t] ':' conditional_expr[f] { NOT_IMPLEMENTED(@$) }
    ;

expr
    : arith_expr
    | bexpr L_OR  bexpr { NOT_IMPLEMENTED(@$) }
    | bexpr L_AND bexpr { NOT_IMPLEMENTED(@$) }
    ;

bexpr: expr _bool _coerce ;

arith_expr
    : cast_expr
    | aexpr  '+' aexpr { $$ = ctx.mk_arith( $1, $3, llvm::Instruction::Add ); }
    | aexpr  '-' aexpr { $$ = ctx.mk_arith( $1, $3, llvm::Instruction::Sub ); }
    | aexpr  '*' aexpr { $$ = ctx.mk_arith( $1, $3, llvm::Instruction::Mul ); }
    | aexpr  '/' aexpr { $$ = ctx.mk_arith( $1, $3, llvm::Instruction::SDiv); }
    | aexpr  '%' aexpr { $$ = ctx.mk_arith( $1, $3, llvm::Instruction::SRem); }
    | aexpr  '|' aexpr { $$ = ctx.mk_arith( $1, $3, llvm::Instruction::Or  ); }
    | aexpr  '^' aexpr { $$ = ctx.mk_arith( $1, $3, llvm::Instruction::Xor ); }
    | aexpr  '&' aexpr { $$ = ctx.mk_arith( $1, $3, llvm::Instruction::And ); }
    | aexpr  SHL aexpr { $$ = ctx.mk_arith( $1, $3, llvm::Instruction::Shl ); }
    | aexpr ASHR aexpr { $$ = ctx.mk_arith( $1, $3, llvm::Instruction::AShr); }
    | aexpr   EQ aexpr { $$ = ctx.mk_cmp( $1, $3, llvm::CmpInst::ICMP_EQ ); }
    | aexpr  NEQ aexpr { $$ = ctx.mk_cmp( $1, $3, llvm::CmpInst::ICMP_NE ); }
    | aexpr  GEQ aexpr { $$ = ctx.mk_cmp( $1, $3, llvm::CmpInst::ICMP_SGE ); }
    | aexpr  LEQ aexpr { $$ = ctx.mk_cmp( $1, $3, llvm::CmpInst::ICMP_SLE ); }
    | aexpr  '>' aexpr { $$ = ctx.mk_cmp( $1, $3, llvm::CmpInst::ICMP_SGT ); }
    | aexpr  '<' aexpr { $$ = ctx.mk_cmp( $1, $3, llvm::CmpInst::ICMP_SLT ); }
    ;

aexpr: arith_expr _i64 _coerce ; /* TODO: actual integer promotion */

cast_expr
    : unary_expr
    | '(' type ')' cast_expr { NOT_IMPLEMENTED(@$) }
    ;

unary_expr
    : postfix_expr
    | '-' cast_expr _i64  _coerce[res] %prec UNARY_MINUS {
        $$ = $res;
        $$.llval = IRB.CreateNeg( $res.llval );
    }
    | '!' cast_expr _bool _coerce[res] {
        $$ = $res;
        $$.llval = IRB.CreateNot( $res.llval );
    }
    ;

postfix_expr
    : primary_expr
    ;

primary_expr
    : identifier { NOT_IMPLEMENTED(@$) }
    | CONSTANT_I {
        $$.llval = IRB.getIntN( $1.width, $1.number );
        $$.cat = seagol::Value::CVALUE;
    }
    | '(' expression ')' { $$ = $2; }
    ;
