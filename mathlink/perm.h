#pragma once

#include <assert.h>
#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <unordered_set>
#include <deque>
#include "util.h"

namespace pperm {

typedef std::uint32_t point_type;

struct Permutation {
    bool isNegative;
    Ptr<std::uint32_t> images;
    std::uint32_t len;
    void copyFrom(const Array<std::uint32_t> &pool, const Permutation &perm);
    void print(std::ostream &os, const Array<std::uint32_t> &pool);
};

struct GenSet {
    std::uint32_t len, permLen;
    Ptr<std::uint32_t> generators;
    Ptr<std::uint32_t> signs;
    Permutation getPermutation(const Array<std::uint32_t> &pool, std::size_t i) const;
    void print(std::ostream &os, const Array<std::uint32_t> &pool);
};

/**
 * Points are optional int: zero means obsent entry, non-zeros are values plus 1.
*/
struct SchreierVector {
    Ptr<std::uint32_t> generator;
    Ptr<std::uint32_t> sourcePoint;
};

struct SchreierVectorBuilder {
    const GenSet &genset;
    Array<std::uint32_t> &pool;
    std::unordered_set<std::uint32_t> orbitPoints;
    std::deque<std::uint32_t> workingPoints;
    SchreierVector vector;
    SchreierVectorBuilder(Array<std::uint32_t> &pool, const GenSet &genset);
    void appendOrbit(std::uint32_t point);
    void appendAllOrbits();
    void appendOrbitAndRest(std::uint32_t point) {
        this->appendOrbit(point);
        this->appendAllOrbits();
    };
};

struct SchreierSims {
    
};

template<typename T>
struct Printer {
    const T &val;
    const Array<std::uint32_t> &pool;
    Printer(const T &val, const Array<std::uint32_t> &pool): val(val), pool(pool) {}
};

template<typename T>
std::ostream &operator << (std::ostream &os, const Printer<T> &printer) {
    printer.val.print(os, printer.pool);
    return os;
}

void permutationProductSimp(std::uint32_t *dest, const std::uint32_t *p1, const std::uint32_t *p2, std::size_t len);
Permutation identity(Array<std::uint32_t> &pool, std::uint32_t len);
Permutation product(Array<std::uint32_t> &pool, const Permutation &perm1, const Permutation &perm2);
Permutation inverse(Array<std::uint32_t> &pool, const Permutation &perm);
Permutation traceSchreierVector(Array<std::uint32_t> &pool, std::uint32_t point, const GenSet &genset, const SchreierVector &vec);
GenSet stablizer(Array<std::uint32_t> &pool, const GenSet &genset);
bool isIdentity(const std::uint32_t *perm, std::size_t len);

}