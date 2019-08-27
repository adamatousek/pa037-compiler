#include "context.hpp"

namespace seagol {

llvm::Type* Context::get_type( typeid_t id )
{
    switch ( id ) {
    case TYPEID::UNKNOWN: return nullptr;
    case TYPEID::VOID:    return llvm::Type::getVoidTy( llcontext );
    case TYPEID::INT:     return llvm::Type::getInt32Ty( llcontext );
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
    if ( from->isIntegerTy() && to->isIntegerTy() )
        return true;
    return false;
}

ExprInfo Context::coerce( ExprInfo expr, llvm::Type *to )
{
    auto *from = expr.type();
    if ( from == to )
        return expr;
    if ( from->isIntegerTy() && to->isIntegerTy() ) {
        if ( from->getIntegerBitWidth() == 1 ) {
            expr.llval = irb.CreateZExt( expr.llval, to );
        } else if ( to->getIntegerBitWidth() == 1 ) {
            expr.llval = irb.CreateICmpNE( expr.llval, irb.getFalse() );
        } else {
            if ( from->getIntegerBitWidth() > to->getIntegerBitWidth() ) {
                // TODO: emit a warning
            }
            expr.llval = irb.CreateSExtOrTrunc( expr.llval, to );
        }
    }
    return expr;
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
    return decl_id( root + '$' + std::to_string( seed++ ) );
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
        if ( fn->type ) /* redeclaration with the same type */
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
        (*argii_it)->llval = &arg;
    }

    bb_trash->insertInto( fn );
}

void Context::end_fun()
{
    bb_trash->removeFromParent();
}

void Context::after_return()
{
    irb.SetInsertPoint( bb_trash );
}

ExprInfo Context::mk_arith( const ExprInfo &l, const ExprInfo &r,
                            llvm::Instruction::BinaryOps op )
{
    ExprInfo res;
    res.cat = ExprInfo::RVALUE;
    res.cat.constant = l.constant() && r.constant();
    res.llval = irb.CreateBinOp( op, l.llval, r.llval );
    return res;
}
ExprInfo Context::mk_cmp( const ExprInfo &l, const ExprInfo &r,
                          llvm::CmpInst::Predicate p )
{
    ExprInfo res;
    res.cat = ExprInfo::RVALUE;
    res.cat.constant = l.constant() && r.constant();
    res.llval = irb.CreateICmp( p, l.llval, r.llval );
    return res;
}

} /* seagol */
