#include <algorithm>
#include "perm.h"

using namespace pperm;

void Permutation::copyFrom(const Array<point_type> &pool, const Permutation &perm) {
    this->isNegative = perm.isNegative;
    copyArray(pool.getPtr(this->images), pool.getPtr(perm.images), perm.len);
}

Permutation GenSet::getPermutation(const Array<point_type> &pool, std::size_t i) const {
    auto ptr = this->generators.offset(i * (this->permLen + 1));
    return Permutation {
        *pool.getPtr(ptr) == 0 ? false : true,
        ptr.offset(1),
        this->permLen
    };
}
void Permutation::print(std::ostream &os, const Array<point_type> &pool) {
    if (this->isNegative) {
        os << "-";
    }
    os << "/";
    auto ptr = pool.getPtr(this->images);
    for (std::size_t i = 0; i < this->len; i++) {
        if (i > 0) os << ", ";
        os << *ptr++ + 1;
    }
    os << "/";
}

int Permutation::compare(Array<point_type> &pool, const Permutation &other) {
    if (this->isNegative != other.isNegative) {
        return this->isNegative ? -1 : 1;
    }
    if (this->len > other.len) {
        return 1;
    } else if (this->len < other.len) {
        return -1;
    }
    auto ptr1 = pool.getPtr(this->images), ptr2 = pool.getPtr(other.images);
    for (std::size_t i = 0; i < this->len; i++) {
        auto n1 = *ptr1++, n2 = *ptr2++;
        if (n1 > n2) {
            return 1;
        } else if (n1 < n2) {
            return -1;
        }
    }
    return 0;
}
std::size_t Permutation::hash(Array<point_type> &pool) {
    std::size_t ret = 0;
    if (this->isNegative) {
        ret += 1;
    }
}

void GenSet::print(std::ostream &os, const Array<point_type> &pool) {
    os << "{ ";
    for (std::size_t i = 0; i < this->len; i++) {
        if (i > 0) os << ", ";
        this->getPermutation(pool, i).print(os, pool);
    }
    os << " }";
}

SchreierVectorBuilder::SchreierVectorBuilder(Array<point_type> &pool, const GenSet &genset): genset(genset), pool(pool) {
    this->vector.generator = pool.reserve(genset.permLen);
    this->vector.sourcePoint = pool.reserve(genset.permLen);
    arraySet(pool.getPtr(this->vector.generator), genset.permLen, 0u);
    arraySet(pool.getPtr(this->vector.sourcePoint), genset.permLen, 0u);
}
void SchreierVectorBuilder::appendOrbit(point_type point) {
    auto schreierVectorP = this->pool.getPtr(this->vector.generator);
    auto backwardsVectorP = this->pool.getPtr(this->vector.sourcePoint);
    this->orbitPoints.insert(point);
    this->workingPoints.push_back(point);
    while (!workingPoints.empty()) {
        point_type point = workingPoints[0];
        workingPoints.pop_front();
        for (std::size_t j = 0; j < this->genset.len; j++) {
            auto generator = this->genset.getPermutation(pool, j);
            auto image = pool.getPtr(generator.images)[point];
            if (orbitPoints.insert(image).second) {
                schreierVectorP[image] = j + 1;
                backwardsVectorP[image] = point + 1;
                workingPoints.push_back(point);
            }
        }
    }
}
void SchreierVectorBuilder::appendAllOrbits() {
    for (std::size_t i = 0; i < this->genset.permLen; i++) {
        this->appendOrbit(i);
    }
}

void pperm::permutationProductSimp(point_type *dest, const point_type *p1, const point_type *p2, std::size_t len) {
    for (std::size_t i = 0; i < len; i++) {
        *dest++ = p2[*p1++];
    }
}

Permutation pperm::identity(Array<point_type> &pool, std::size_t len) {
    Permutation ret{false, pool.reserve(len), len};
    auto ptr = pool.getPtr(ret.images);
    for (point_type i = 0; i < len; i++) {
        *ptr++ = i;
    }
    return ret;
}

Permutation pperm::product(Array<point_type> &pool, const Permutation &perm1, const Permutation &perm2) {
    Permutation ret{perm1.isNegative != perm2.isNegative, pool.reserve(perm1.len), perm1.len};
    auto ptr = pool.getPtr(ret.images);
    auto p1 = pool.getPtr(perm1.images);
    auto p2 = pool.getPtr(perm2.images);
    for (std::size_t i = 0; i < ret.len; i++) {
        *ptr++ = p2[*p1++];
    }
    return ret;
}

Permutation pperm::inverse(Array<point_type> &pool, const Permutation &perm) {
    Permutation ret{perm.isNegative, pool.reserve(perm.len), perm.len};
    auto ptr = pool.getPtr(ret.images);
    auto p1 = pool.getPtr(perm.images);
    for (std::size_t i = 0; i < ret.len; i++) {
        ptr[*p1++] = i;
    }
    return ret;
}

Permutation pperm::traceSchreierVector(Array<point_type> &pool, point_type point, const GenSet &genset, const SchreierVector &vec) {
    Permutation ret = identity(pool, genset.permLen);
    auto cursor = pool.position();
    point_type generator;
    while ((generator = pool.getPtr(vec.generator)[point]) != 0) {
        auto cursor = pool.position();
        ret.copyFrom(pool, product(pool, ret, genset.getPermutation(pool, generator - 1)));
        pool.setPosition(cursor); // release that temporary permutation object
        point = pool.getPtr(vec.sourcePoint)[point] - 1;
    }
    return ret;
}

bool pperm::isIdentity(const point_type *perm, std::size_t len) {
    for (std::size_t i = 0; i < len; i++) {
        if (*perm++ != i) {
            return false;
        }
    }
    return true;
}