#include "context.hpp"
#include "parser.hpp"

#include <llvm/IR/CFG.h>

namespace seagol {

const Value::Category Value::LVALUE = { 1, 0, 1, 0 };
const Value::Category Value::RVALUE = { 0, 0, 0, 0 };
const Value::Category Value::CVALUE = { 0, 1, 0, 0 };
const Value::Category Value::FVALUE = { 1, 0, 0, 1 };

llvm::Type* Context::get_type( typeid_t id )
{
    switch ( id ) {
    case TYPEID::UNKNOWN: return nullptr;
    case TYPEID::VOID:    return llvm::Type::getVoidTy( llcontext );
    case TYPEID::LONG:    return llvm::Type::getInt64Ty( llcontext );
    case TYPEID::INT:     return llvm::Type::getInt32Ty( llcontext );
    case TYPEID::SHORT:   return llvm::Type::getInt16Ty( llcontext );
    case TYPEID::CHAR:    return llvm::Type::getInt8Ty( llcontext );
    case TYPEID::BOOL:    return llvm::Type::getInt1Ty( llcontext );
    }
    throw std::runtime_error( "requesting nonexistent type" );
}

std::string Context::format_type( llvm::Type *ty ) const
{
    if ( ty->isVoidTy() ) return "void";
    if ( ty->isIntegerTy( 32 ) ) return "int";
    if ( ty->isIntegerTy( 64 ) ) return "long";
    if ( ty->isIntegerTy( 16 ) ) return "short";
    if ( ty->isIntegerTy( 8 ) ) return "char";
    if ( ty->isIntegerTy( 1 ) ) return "bool";
    if ( auto *pty = llvm::dyn_cast< llvm::PointerType >( ty ) ) {
        return format_type( pty->getElementType() ) + " *";
    }
    if ( auto *fty = llvm::dyn_cast< llvm::FunctionType >( ty ) ) {
        std::string s = format_type( fty->getReturnType() ) + "(*)(";
        bool first = true;
        for ( auto *p : fty->params() ) {
            if ( !first )
                s += ", ";
            s += format_type( p );
            first = false;
        }
        s += ")";
        return s;
    }
    return "<TODO>";
}

bool Context::coercible( llvm::Type *from, llvm::Type *to )
{
    if ( from == to )
        return true;
    if ( from->isVoidTy() )
        return false;
    if ( from->isIntegerTy() && to->isIntegerTy() )
        return true;
    if ( from->isFunctionTy() && to->isPointerTy() &&
            to->getPointerElementType() == from )
        return true;
    return false;
}

ExprInfo Context::coerce( ExprInfo expr, llvm::Type *to, bool rvalise )
{
    if ( !to )
        to = expr.type();

    if ( rvalise && expr.loadable() ) {
        expr.rvalise();
        expr.llval = irb.CreateLoad( expr.llval );
        if ( to->isPointerTy() && to->getPointerElementType()->isFunctionTy() )
            expr.cat.callable = true;
    }

    auto *from = expr.type();
    if ( from == to )
        return expr;
    if ( from->isIntegerTy() && to->isIntegerTy() ) {
        if ( from->getIntegerBitWidth() == 1 ) {
            expr.llval = irb.CreateZExt( expr.llval, to );
        } else if ( to->getIntegerBitWidth() == 1 ) {
            expr.llval = irb.CreateICmpNE( expr.llval,
                    irb.getIntN( from->getIntegerBitWidth(), 0 ) );
        } else {
            if ( from->getIntegerBitWidth() > to->getIntegerBitWidth() ) {
                // TODO: emit a warning
            }
            expr.llval = irb.CreateSExtOrTrunc( expr.llval, to );
        }
        expr.rvalise();
    }
    if ( from->isFunctionTy() && to->isPointerTy() &&
            to->getPointerElementType() == from ) {
        expr.cat.callable = 1;
    }
    return expr;
}

bool Context::promotable( ExprInfo l, ExprInfo r )
{
    return l.type()->isIntegerTy() && r.type()->isIntegerTy();
}

std::pair< ExprInfo, ExprInfo > Context::promote( ExprInfo l, ExprInfo r )
{
    unsigned width = std::max( l.type()->getIntegerBitWidth(),
                               r.type()->getIntegerBitWidth() );
    return { coerce( l, irb.getIntNTy( width ) ),
             coerce( r, irb.getIntNTy( width ) )};
}

void Context::open_scope()
{
    scope_stack.emplace_back();
}

void Context::close_scope()
{
    scope_stack.pop_back();
}

IdentifierInfo* Context::find_id( const std::string &ident )
{
    auto scope_it = scope_stack.rbegin();
    while ( scope_it != scope_stack.rend() ) {
        auto id_it = scope_it->ids.find( ident );
        if ( id_it != scope_it->ids.end() )
            return const_cast< IdentifierInfo* >( &*id_it ); // OK, since actual key is const
        ++scope_it;
    }
    return nullptr;
}

IdentifierInfo* Context::decl_id( const std::string &ident )
{
    auto id_it = scope_stack.back().ids.find( ident );
    if ( id_it != scope_stack.back().ids.end() )
        return nullptr; /* already declared in this scope */
    return const_cast< IdentifierInfo* >(
            &*( scope_stack.back().ids.insert({ ident, nullptr }).first ) );
}

IdentifierInfo* Context::gen_id( const std::string & root )
{
    return decl_id( '.' + root + '-' + std::to_string( seed++ ) );
}

bool Context::decl_global( IdentifierInfo* var, llvm::Type* type,
                           llvm::Type** old_type )
{
    if ( var->type ) {
        if ( type != var->type ) { /* redeclaration with different type */
            if ( old_type )
                *old_type = var->type;
            return false;
        }
        /* redeclaration with the same type */
        return true;
    }

    /* new declaration */
    var->type = type;
    var->llval = llmodule->getOrInsertGlobal( var->name, type );
    return true;
}

IdentifierInfo* Context::mk_arg( llvm::Type* ty, IdentifierInfo* ii )
{
    assert( !ii->llval );
    ii->llval = nullptr;
    ii->type = ty;
    return ii;
}

bool /*TODO*/ Context::decl_fun( IdentifierInfo *fn, llvm::Type* ret_type,
                                 const ArgumentList & args, llvm::Type ** old_type )
{
    std::vector< llvm::Type* > argtys;
    argtys.reserve( args.args.size() );
    for ( auto * a : args.args )
        argtys.push_back( a->type );
    auto ftype = llvm::FunctionType::get( ret_type, argtys, false );
    if ( fn->type ) {
        if ( ftype != fn->type ) { /* redeclaration with different type */
            if ( old_type )
                *old_type = fn->type;
            return false;
        }
        /* redeclaration with the same type */
        return true;
    }

    /* new declaration */
    fn->type = ftype;
    fn->llval = llvm::Function::Create( ftype, llvm::GlobalValue::ExternalLinkage,
                                        fn->name, llmodule.get() );
    return true;
}

void Context::start_fun( IdentifierInfo *fii, const ArgumentList & args )
{
    auto *fn = llvm::cast< llvm::Function >( fii->llval );
    auto *bb_entry = llvm::BasicBlock::Create( llcontext, "entry", fn );
    irb.SetInsertPoint( bb_entry );

    auto argii_it = args.args.begin();
    for ( llvm::Argument &arg : fn->args() ) {
        assert( ! (*argii_it)->llval );
        auto name = (*argii_it)->name;
        arg.setName( name );
        if ( arg.getName().startswith( ".arg-" ) ) /* unnamed */ {
            (*argii_it)->llval = &arg;
        } else {
            auto *argaddr = irb.CreateAlloca( arg.getType(), nullptr, name + ".addr" );
            irb.CreateStore( &arg, argaddr );
            (*argii_it)->llval = argaddr;
        }
        ++argii_it;
    }

    bb_trash->insertInto( fn );
}

bool Context::end_fun()
{
    auto bb_this = irb.GetInsertBlock();
    if ( bb_this != bb_trash &&
            std::all_of( llvm::pred_begin( bb_this ), llvm::pred_end( bb_this ),
                      [this]( auto *bb ){ return bb == bb_trash; } ) )
            bb_this->removeFromParent();
    else if ( bb_this != bb_trash )
        return false;

    bb_trash->removeFromParent();
    return true;
}

void Context::after_return()
{
    irb.SetInsertPoint( bb_trash );
}

std::string Context::push_param( CallInfo* ci, const ExprInfo &arg )
{
    if ( ci->param_it == ci->param_end )
        return std::string( "too many arguments (function requires " ) +
            std::to_string( ci->ftype->getNumParams() ) + ')';
    if ( !coercible( arg.type(), *ci->param_it ) )
        return std::string( "invalid conversion from `" ) +
            format_type( arg.type() ) + "\' to `" + format_type( *ci->param_it )
            + "\' on argument " + std::to_string( ci->args.size() + 1 );
    ci->args.push_back( coerce( arg, *ci->param_it ).llval );
    ci->param_it++;
    return {};
}

ExprInfo Context::mk_bin( const ExprInfo &l, const ExprInfo &r,
                          llvm::Value *val )
{
    ExprInfo res;
    res.cat = ExprInfo::RVALUE;
    res.cat.constant = l.constant() && r.constant();
    res.llval = val;
    return res;
}
ExprInfo Context::mk_arith( yy::parser *p, const yy::location &loc,
                            ExprInfo l, ExprInfo r,
                            llvm::Instruction::BinaryOps op )
{
    /* Both are integers */
    if ( promotable( l, r ) ) {
        auto ops = promote( l, r );
        return mk_bin( ops.first, ops.second,
                       irb.CreateBinOp( op, ops.first.llval, ops.second.llval ) );
    }
    /* Both are pointers - only subtraction for equal types is defined */
    if ( l.pointer() && r.pointer()
            && op == llvm::Instruction::Sub
            && l.type() == l.type() ) {
        l = coerce( l );
        r = coerce( r );
        auto lp = irb.CreatePtrToInt( l.llval, irb.getInt64Ty() );
        auto rp = irb.CreatePtrToInt( r.llval, irb.getInt64Ty() );
        return { irb.CreateBinOp( op, lp, rp ), Value::RVALUE };
    }
    /* Left is pointer, right is integer - pointer arithmetic */
    if ( l.pointer() && ! r.pointer()
            && ( op == llvm::Instruction::Add || op == llvm::Instruction::Sub )
            && coercible( r.type(), irb.getInt64Ty() ) ) {
        auto ri = coerce( r, irb.getInt64Ty() );
        if ( op == llvm::Instruction::Sub )
        {
            ri.llval = irb.CreateNeg( ri.llval );
            ri.rvalise();
        }
        l = coerce( l );
        auto p = irb.CreateGEP( l.llval, ri.llval );
        return { p, Value::RVALUE };
    }
    /* Left is integer, right is pointer - pointer arithmetic (addition only) */
    if ( !l.pointer() && r.pointer()
            && op == llvm::Instruction::Add
            && coercible( l.type(), irb.getInt64Ty() ) ) {
        auto li = coerce( l, irb.getInt64Ty() );
        r = coerce( r );
        auto p = irb.CreateGEP( r.llval, li.llval );
        return { p, Value::RVALUE };
    }

    /* Else: error */
    return p->error( loc, "invalid operand types: `"
                          + format_type( l.type() ) + "' and `"
                          + format_type( r.type() ) + "'" ), l;
}

ExprInfo Context::mk_cmp( yy::parser *p, const yy::location &loc,
                          ExprInfo l, ExprInfo r, llvm::CmpInst::Predicate pred )
{
    if ( promotable( l, r ) ) {
        auto ops = promote( l, r );
        l = ops.first;
        r = ops.second;
    } else if ( l.pointer() && r.pointer() ) {
        l = coerce( l );
        r = coerce( r );
        l.llval = irb.CreatePtrToInt( l.llval, irb.getInt64Ty() );
        r.llval = irb.CreatePtrToInt( r.llval, irb.getInt64Ty() );
    } else {
        return p->error( loc, "invalid operand types: `"
                              + format_type( l.type() ) + "' and `"
                              + format_type( r.type() ) + "'" ), l;
    }
    ExprInfo res;
    res.cat = ExprInfo::RVALUE;
    res.cat.constant = l.constant() && r.constant();
    res.llval = irb.CreateICmp( pred, l.llval, r.llval );
    return res;
}

llvm::BasicBlock* Context::mk_bb( const llvm::Twine &name )
{
    auto *bb_this = irb.GetInsertBlock();
    auto *fn = bb_this->getParent();
    auto *bb_next = fn->getBasicBlockList().getNextNode( *bb_this );
    auto *bb = llvm::BasicBlock::Create( llcontext, name, fn, bb_next );
    return bb;
}

} /* seagol */
