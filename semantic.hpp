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
        INT,
        CHAR,
        BOOL,
        _last_
    };
};


/*
 * Semantic types for parser
 */

struct TypeName {
    typeid_t tid = 0;
    std::string name;
};

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
};

struct ConstantInt {
    uint8_t width;
    int64_t number;
};

struct Value {
    llvm::Value *llval;

    struct Category {
        uint8_t addressable : 1;
        uint8_t constant    : 1;
        uint8_t assignable  : 1;
        uint8_t callable    : 1;
    } cat;
    bool addressable() const { return cat.addressable; }
    bool constant() const { return cat.constant; }
    bool assignable() const { return cat.assignable; }
    bool callable() const { return cat.callable; }
    void setCategory( Category category ) { cat = category; }

    llvm::Type* type() { return llval->getType(); }

    static constexpr Category LVALUE = { 1, 0, 1, 0 };
    static constexpr Category RVALUE = { 0, 0, 0, 0 };
    static constexpr Category CVALUE = { 0, 1, 0, 0 };
    static constexpr Category FVALUE = { 1, 0, 0, 1 };
};

using ExprInfo = Value;

} /* seagol */

#endif /* end of include guard: SEAGOL_SEMANTIC_HPP */
