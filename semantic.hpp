#pragma once
#ifndef SEAGOL_SEMANTIC_HPP
#define SEAGOL_SEMANTIC_HPP

#define DEFAULT_DTOR_COPY_MOVE( T ) \
    ~T() = default; \
    T( const T & ) = default; \
    T( T && ) = default; \
    T& operator=( const T & ) = default; \
    T& operator=( T && ) = default; \

namespace seagol {

/*
 * Various declarations
 */

/* IDs of primitive types */
using typeid_t = uint16_t;
struct TYPEID {
    enum : typeid_t {
        UNKNOWN = 0,
        VOID,
        LONG,
        INT,
        SHORT,
        CHAR,
        BOOL,
        ANYPTR,
        _last_
    };
};


/*
 * Semantic types for parser
 */

struct IdentifierInfo {
    const std::string name; // sort key in sets, therefore must not be modified
    llvm::Value *llval = nullptr;
    llvm::Type *type = nullptr;

    bool operator<( const IdentifierInfo & o ) const { return name < o.name; }

    struct Cmp {
        using II = IdentifierInfo;
        using is_transparent = std::true_type;
        bool operator()( const II & ii, const std::string & s ) const {
            return ii.name < s;
        }
        bool operator()( const std::string & s, const II & ii ) const {
            return s < ii.name;
        }
        bool operator()( const II & ii1, const II & ii2 ) const {
            return ii1 < ii2;
        }
    };
};

struct ArgumentList {
    std::vector<IdentifierInfo*> args;
    bool variadic = false;
};

struct ConstantInt {
    uint8_t width;
    int64_t number;
};

struct Value {
    llvm::Value *llval;

    struct Category {
        uint8_t addressable : 1; // If 1, then then llval is the address
        uint8_t constant    : 1;
        uint8_t assignable  : 1;
    } cat;
    bool addressable() const { return cat.addressable; }
    bool loadable() const { return cat.addressable && cat.assignable; }
    bool constant() const { return cat.constant; }
    bool assignable() const { return cat.assignable; }
    bool callable() const {
        return llvm::isa< llvm::Function >( llval ) ||
                ( type()->isPointerTy() &&
                  type()->getPointerElementType()->isFunctionTy() );
    }
    bool derefable() const { return pointer() || callable(); }
    bool pointer() const { return type()->isPointerTy(); }
    void setCategory( Category category ) { cat = category; }

    llvm::Type* type() const {
        if ( addressable() ) {
            assert( llvm::isa< llvm::PointerType >( llval->getType() ) );
            return llval->getType()->getPointerElementType();
        }
        return llval->getType();
    }
    void rvalise() { cat.addressable = 0; cat.assignable = 0; }

    // only for constant expressions!
    bool getBool() const {
        assert( constant() );
        return ! llvm::cast< llvm::Constant >( llval )->isZeroValue();
    }

    Value() = default;
    DEFAULT_DTOR_COPY_MOVE( Value )
    Value( llvm::Value * v, Category c ) : llval( v ), cat( c ) {}

    static const Category LVALUE;
    static const Category RVALUE;
    static const Category CVALUE;
    static const Category FVALUE;
};

using ExprInfo = Value;

struct CallInfo {
    llvm::FunctionType::param_iterator param_it;
    llvm::FunctionType::param_iterator param_end;
    llvm::Value *fn;
    llvm::FunctionType *ftype;
    std::vector<llvm::Value*> args;
};
using CallInfo_u = std::unique_ptr< CallInfo >;

struct IfInfo {
    llvm::BasicBlock *bb_true;
    llvm::BasicBlock *bb_false;
    llvm::BasicBlock *bb_cont;

    bool operator==( const IfInfo &o ) const {
        return bb_true == o.bb_true && bb_false == o.bb_false && bb_cont == o.bb_cont;
    }
    bool operator!=( const IfInfo &o ) const { return ! ( *this == o ) ; }
};

} /* seagol */

#endif /* end of include guard: SEAGOL_SEMANTIC_HPP */
