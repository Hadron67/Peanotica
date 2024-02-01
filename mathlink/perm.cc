#include <algorithm>
#include "perm.h"

using namespace pperm;

void Permutation::copyFrom(const Array<std::uint32_t> &pool, const Permutation &perm) {
    this->isNegative = perm.isNegative;
    copyArray(pool.getPtr(this->images), pool.getPtr(perm.images), perm.len);
}

Permutation GenSet::getPermutation(const Array<std::uint32_t> &pool, std::size_t i) const {
    return Permutation {
        bitSetGet(pool.getPtr(this->signs), 32, i),
        this->generators.offset(i),
        this->permLen
    };
}
void Permutation::print(std::ostream &os, const Array<std::uint32_t> &pool) {
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

void GenSet::print(std::ostream &os, const Array<std::uint32_t> &pool) {
    os << "{ ";
    for (std::size_t i = 0; i < this->len; i++) {
        if (i > 0) os << ", ";
        this->getPermutation(pool, i).print(os, pool);
    }
    os << " }";
}

SchreierVectorBuilder::SchreierVectorBuilder(Array<std::uint32_t> &pool, const GenSet &genset): genset(genset), pool(pool) {
    this->vector.generator = pool.reserve(genset.permLen);
    this->vector.sourcePoint = pool.reserve(genset.permLen);
    arraySet(pool.getPtr(this->vector.generator), genset.permLen, 0u);
    arraySet(pool.getPtr(this->vector.sourcePoint), genset.permLen, 0u);
}
void SchreierVectorBuilder::appendOrbit(std::uint32_t point) {
    auto schreierVectorP = this->pool.getPtr(this->vector.generator);
    auto backwardsVectorP = this->pool.getPtr(this->vector.sourcePoint);
    this->orbitPoints.insert(point);
    this->workingPoints.push_back(point);
    while (!workingPoints.empty()) {
        std::uint32_t point = workingPoints[0];
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

void pperm::permutationProductSimp(std::uint32_t *dest, const std::uint32_t *p1, const std::uint32_t *p2, std::size_t len) {
    for (std::size_t i = 0; i < len; i++) {
        *dest++ = p2[*p1++];
    }
}

Permutation pperm::identity(Array<std::uint32_t> &pool, std::uint32_t len) {
    Permutation ret{false, pool.reserve(len), len};
    auto ptr = pool.getPtr(ret.images);
    for (std::uint32_t i = 0; i < len; i++) {
        *ptr++ = i;
    }
    return ret;
}

Permutation pperm::product(Array<std::uint32_t> &pool, const Permutation &perm1, const Permutation &perm2) {
    Permutation ret{perm1.isNegative != perm2.isNegative, pool.reserve(perm1.len), perm1.len};
    auto ptr = pool.getPtr(ret.images);
    auto p1 = pool.getPtr(perm1.images);
    auto p2 = pool.getPtr(perm2.images);
    for (std::size_t i = 0; i < ret.len; i++) {
        *ptr++ = p2[*p1++];
    }
    return ret;
}

Permutation pperm::inverse(Array<std::uint32_t> &pool, const Permutation &perm) {
    Permutation ret{perm.isNegative, pool.reserve(perm.len), perm.len};
    auto ptr = pool.getPtr(ret.images);
    auto p1 = pool.getPtr(perm.images);
    for (std::size_t i = 0; i < ret.len; i++) {
        ptr[*p1++] = i;
    }
    return ret;
}

Permutation pperm::traceSchreierVector(Array<std::uint32_t> &pool, std::uint32_t point, const GenSet &genset, const SchreierVector &vec) {
    Permutation ret = identity(pool, genset.permLen);
    auto cursor = pool.position();
    std::uint32_t generator;
    while ((generator = pool.getPtr(vec.generator)[point]) != 0) {
        auto cursor = pool.position();
        ret.copyFrom(pool, product(pool, ret, genset.getPermutation(pool, generator - 1)));
        pool.setPosition(cursor); // release that temporary permutation object
        point = pool.getPtr(vec.sourcePoint)[point] - 1;
    }
    return ret;
}

bool pperm::isIdentity(const std::uint32_t *perm, std::size_t len) {
    for (std::size_t i = 0; i < len; i++) {
        if (*perm++ != i) {
            return false;
        }
    }
    return true;
}