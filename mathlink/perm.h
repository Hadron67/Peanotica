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
    Ptr<point_type> images;
    std::size_t len;
    void copyFrom(const Array<point_type> &pool, const Permutation &perm);
    void print(std::ostream &os, const Array<point_type> &pool);
    int compare(Array<point_type> &pool, const Permutation &other);
    std::size_t hash(Array<point_type> &pool);
};

struct GenSet {
    point_type len, permLen;
    Ptr<point_type> generators;
    Ptr<point_type> signs;
    HashMap<std::uint32_t, bool> generatorToId;
    Permutation getPermutation(const Array<point_type> &pool, std::size_t i) const;
    void print(std::ostream &os, const Array<point_type> &pool);
};

/**
 * Points are optional int: zero means obsent entry, non-zeros are values plus 1.
*/
struct SchreierVector {
    Ptr<point_type> generator;
    Ptr<point_type> sourcePoint;
};

struct SchreierVectorBuilder {
    const GenSet &genset;
    Array<point_type> &pool;
    std::unordered_set<point_type> orbitPoints;
    std::deque<point_type> workingPoints;
    SchreierVector vector;
    SchreierVectorBuilder(Array<point_type> &pool, const GenSet &genset);
    void appendOrbit(point_type point);
    void appendAllOrbits();
    void appendOrbitAndRest(point_type point) {
        this->appendOrbit(point);
        this->appendAllOrbits();
    };
};

struct SchreierSims {

};

template<typename T>
struct Printer {
    const T &val;
    const Array<point_type> &pool;
    Printer(const T &val, const Array<point_type> &pool): val(val), pool(pool) {}
};

template<typename T>
std::ostream &operator << (std::ostream &os, const Printer<T> &printer) {
    printer.val.print(os, printer.pool);
    return os;
}

void permutationProductSimp(point_type *dest, const point_type *p1, const point_type *p2, std::size_t len);
Permutation identity(Array<point_type> &pool, std::size_t len);
Permutation product(Array<point_type> &pool, const Permutation &perm1, const Permutation &perm2);
Permutation inverse(Array<point_type> &pool, const Permutation &perm);
Permutation traceSchreierVector(Array<point_type> &pool, point_type point, const GenSet &genset, const SchreierVector &vec);
GenSet stablizer(Array<point_type> &pool, const GenSet &genset);
bool isIdentity(const point_type *perm, std::size_t len);

}