#pragma once
#ifndef SEAGOL_CONTEXT_HPP
#define SEAGOL_CONTEXT_HPP

SEAGOL_RELAX_WARNINGS
#include <llvm/IR/IRBuilder.h>
SEAGOL_UNRELAX_WARNINGS

#include <set>
#include <stack>

#include "semantic.hpp"

namespace yy {
    class parser;
    class location;
}

namespace seagol {

struct ScopeInfo {
    std::set< IdentifierInfo, IdentifierInfo::Cmp > ids;
};

struct Context {
    llvm::LLVMContext llcontext;
    std::unique_ptr< llvm::Module > llmodule;
    std::vector< ScopeInfo > scope_stack;
    llvm::IRBuilder<> irb;
    llvm::BasicBlock* bb_trash;
    uint16_t seed = 0;

    llvm::Type* get_type( typeid_t );
    bool coercible( llvm::Type *, llvm::Type * );
    ExprInfo coerce( ExprInfo, llvm::Type * t = nullptr );
    bool castable( llvm::Type *, llvm::Type * );
    ExprInfo cast( ExprInfo, llvm::Type * );
    bool promotable( ExprInfo, ExprInfo );
    std::pair< ExprInfo, ExprInfo > promote( ExprInfo, ExprInfo );
    std::string format_type( llvm::Type* ) const;

    void open_scope();
    void close_scope();
    IdentifierInfo* find_id( const std::string & );
    IdentifierInfo* decl_id( const std::string & );
    IdentifierInfo* gen_id( const std::string & );

    bool decl_global( IdentifierInfo*, llvm::Type*, llvm::Type** );

    IdentifierInfo* mk_arg( llvm::Type*, IdentifierInfo* );
    bool /*TODO*/ decl_fun( IdentifierInfo*, llvm::Type*, const ArgumentList &,
                            llvm::Type** );
    void start_fun( IdentifierInfo*, const ArgumentList & );
    bool end_fun(); /* whether a return instruction is present */
    void after_return();

    std::string push_param( CallInfo*, const ExprInfo & );

    ExprInfo mk_bin( const ExprInfo &, const ExprInfo &, llvm::Value* );
    ExprInfo mk_arith( yy::parser *, const yy::location &,
                       ExprInfo, ExprInfo, llvm::Instruction::BinaryOps );
    ExprInfo mk_cmp( yy::parser *, const yy::location &,
                     ExprInfo, ExprInfo, llvm::CmpInst::Predicate );

    llvm::BasicBlock* mk_bb( const llvm::Twine &name = "" );

    Context( const std::string & name )
        : llmodule( new llvm::Module( name, llcontext ) )
        , irb( llcontext )
        , bb_trash( llvm::BasicBlock::Create( llcontext, "trash" ) )
    {
        open_scope(); // top-level (global) scope
    }
};

} /* seagol */

#endif /* end of include guard: SEAGOL_CONTEXT_HPP */
