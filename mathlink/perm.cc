#include <algorithm>
#include "perm.h"

using namespace pperm;

int PermutationView::compare(const PermutationView &other) const {
    if (this->isNegative() != other.isNegative()) {
        return this->isNegative() ? -1 : 1;
    }
    if (this->len > other.len) {
        return 1;
    } else if (this->len < other.len) {
        return -1;
    }
    auto ptr1 = this->images(), ptr2 = other.images();
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
bool PermutationView::isIdentity() const {
    if (this->isNegative()) {
        return false;
    }
    auto perm = this->images();
    for (std::size_t i = 0; i < this->len; i++) {
        if (*perm++ != i) {
            return false;
        }
    }
    return true;
}

upoint_type PermutationView::inverseMapPoint(upoint_type point) const {
    auto ptr = this->images();
    for (upoint_type ret = 0; ret < this->len; ret++, ptr++) {
        if (*ptr == point) {
            return ret;
        }
    }
    return 0;
}

upoint_type PermutationView::firstNonFixedPoint() const {
    auto ptr = this->images();
    upoint_type ret;
    for (ret = 0; ret < this->len - 1; ret++) {
        if (*ptr++ != ret) {
            return ret;
        }
    }
    return ret;
}

std::size_t PermutationView::hash() const {
    // TODO: use a better hasher
    std::size_t ret = 5381;
    auto ptr = this->data;
    for (std::size_t i = 0; i < this->len; i++) {
        ret = ret * 33 + *ptr++;
    }
    return ret;
}

PermutationView &PermutationView::identity() {
    this->setNegative(false);
    auto ptr = this->images();
    for (std::size_t i = 0; i < this->len; i++) {
        *ptr++ = i;
    }
    return *this;
}

PermutationView &PermutationView::assign(bool isNegative, std::initializer_list<upoint_type> list) {
    this->setNegative(isNegative);
    if (this->len == list.size()) {
        auto ptr = this->images();
        for (auto it = list.begin(); it != list.end(); ++it) {
            *ptr++ = *it;
        }
    }
    return *this;
}

PermutationView &PermutationView::multiply(const PermutationView &p1, const PermutationView &p2) {
    this->setNegative(p1.isNegative() != p2.isNegative());
    auto ptr1 = p1.images(), ptr2 = p2.images(), ptr = this->images();
    for (std::size_t i = 0; i < this->len; i++) {
        *ptr++ = ptr2[*ptr1++];
    }
    return *this;
}
PermutationView &PermutationView::inverse(const PermutationView &perm) {
    auto ptr = perm.images();
    auto images = this->images();
    for (std::size_t i = 0; i < this->len; i++) {
        images[*ptr++] = i;
    }
    this->setNegative(perm.isNegative());
    return *this;
}

std::ostream &pperm::operator << (std::ostream &os, const PermutationView &perm) {
    if (perm.isIdentity()) {
        os << "I";
        return os;
    }
    if (perm.isNegative()) {
        os << "-";
    }
    os << "/";
    auto ptr = perm.images();
    for (std::size_t i = 0; i < perm.getLength(); i++) {
        if (i > 0) os << ", ";
        os << *ptr++;
    }
    os << "/";
    return os;
}

std::ostream &pperm::operator << (std::ostream &os, PermutationSet &genset) {
    os << "{ ";
    for (std::size_t i = 0; i < genset.getSize(); i++) {
        if (i > 0) os << ", ";
        os << genset.get(i);
    }
    os << " }";
    return os;
}

void CyclesConverter::convert(PermutationView perm) {
    auto requiredSize = perm.getStorageSize() + perm.len * 2 + 1;
    if (this->size < requiredSize) {
        if (this->data) {
            delete[] this->data;
        }
        this->data = new upoint_type[requiredSize];
        this->size = requiredSize;
    }
    PermutationView tmp{this->data + perm.len * 2 + 1, perm.len};
    tmp.copy(perm);
    auto images = tmp.images();
    auto totalLen = this->data, currentCycleLen = totalLen + 1, currentCycleData = currentCycleLen + 1;
    *totalLen = 0;
    auto p = tmp.firstNonFixedPoint();
    while (p + 1 < perm.len) {
        (*totalLen)++;
        *currentCycleLen = 1;
        *currentCycleData++ = p;
        auto p2 = images[p];
        images[p] = p;
        while (p2 != p) {
            (*currentCycleLen)++;
            *currentCycleData++ = p2;
            auto p3 = p2;
            p2 = images[p2];
            images[p3] = p3;
        }
        p = tmp.firstNonFixedPoint();
        currentCycleLen = currentCycleData;
        currentCycleData = currentCycleLen + 1;
    }
}

std::ostream &pperm::operator << (std::ostream &os, const CyclesConverter &cycles) {
    auto len = cycles.data[0];
    auto ptr = cycles.data + 1;
    if (len == 0) {
        os << "I";
    }
    for (upoint_type i = 0; i < len; i++) {
        auto elemLen = *ptr;
        ptr++;
        os << "(";
        for (upoint_type j = 0; j < elemLen; j++) {
            if (j > 0) os << ", ";
            os << *ptr++;
        }
        os << ")";
    }
    return os;
}

struct PermutationHashContext {
    PermutationSet *genset;
    std::size_t hash(const PermutationView &perm) const {
        return perm.hash();
    }
    std::size_t hash(std::size_t i) const {
        return this->genset->get(i).hash();
    }
    int compare(std::size_t i, std::size_t j) const {
        return this->genset->get(i).compare(this->genset->get(j));
    }
    int compare(const PermutationView &perm, std::size_t i) const {
        return perm.compare(this->genset->get(i));
    }
};

void PermutationSet::commitPermutation() {
    auto id = this->getSize() - 1;
    this->permToId.putIfAbsent(id, PermutationHashContext{this}, id);
}

void PermutationSet::addPermutation(const PermutationView &perm) {
    this->permToId.computeIfAbsent(perm, PermutationHashContext{this}, [this](PermutationView perm){
        auto id = this->getSize();
        this->permutations.push().copy(perm);
        return id;
    });
}

PermutationView PermutationSet::appendPermutation(bool isNegative, std::initializer_list<upoint_type> list) {
    auto ret = this->reservePermutation().assign(isNegative, list);
    this->commitPermutation();
    return ret;
}

OptionalUInt<std::size_t> PermutationSet::findPermutation(const PermutationView &perm) {
    auto pt = this->permToId.find(perm, PermutationHashContext{this});
    if (pt.isNonNull()) {
        return this->permToId.getEntry(pt)->getValue();
    } else {
        return None{};
    }
}

void PermutationSet::remove(std::size_t index) {
    this->permToId.remove(this->permToId.find(index, PermutationHashContext{this}));
    for (auto it = this->permToId.begin(); it != this->permToId.end(); ++it) {
        auto &val = it.value();
        if (val > index) {
            val--;
        }
    }
    this->permutations.remove(index);
}

void SchreierOrbit::reset(std::size_t permLen) {
    this->permLen = permLen;
    this->orbitCount = 0;
    if (this->pointToOrbitId == nullptr || permLen > this->allocSize) {
        this->pointToOrbitId = std::make_unique<OptionalUInt<std::uint32_t>[]>(permLen);
        this->vector = std::make_unique<OptionalSchreierVectorEntry[]>(permLen);
        this->allocSize = permLen;
    }
    arraySet(this->pointToOrbitId.get(), permLen, None{});
    arraySet(this->vector.get(), permLen, None{});
}
void SchreierOrbit::dump(std::ostream &os, PermutationList &genset) {
    auto vec = this->vector.get();
    os << "schreier vector {" << std::endl;
    for (upoint_type point = 0; point < genset.getPermutationLength(); point++, vec++) {
        if (vec->isPresent()) {
            auto entry = vec->get();
            os << "    " << point << " <-- " << genset.get(entry.generator) << " -- " << entry.sourcePoint << std::endl;
        } else {
            os << "    " << point << std::endl;
        }
    }
    os << "}" << std::endl;
}

void SchreierVectorBuilder::reset(PermutationList &genset) {
    this->genset = &genset;
    auto permLen = genset.getPermutationLength();
    this->workingPoints.clear();
    this->orbit.reset(permLen);
}

template<typename T>
static inline OptionalUInt<std::size_t> firstNonePos(const OptionalUInt<T> *ptr, std::size_t len) {
    for (std::size_t i = 0; i < len; i++, ptr++) {
        if (!ptr->isPresent()) {
            return i;
        }
    }
    return None{};
}

void SchreierVectorBuilder::appendOrbit(upoint_type point) {
    auto orbitId1 = firstNonePos(this->orbit.pointToOrbitId.get(), this->genset->getPermutationLength());
    if (!orbitId1.isPresent()) {
        return;
    } else {
        this->orbit.orbitCount++;
    }
    auto orbitId = orbitId1.get();
    this->orbit.pointToOrbitId[point] = orbitId;
    this->workingPoints.push_back(point);
    auto gensetSize = this->genset->getSize();
    while (!workingPoints.empty()) {
        auto point = workingPoints[0];
        workingPoints.pop_front();
        for (std::size_t j = 0; j < gensetSize; j++) {
            auto generator = this->genset->get(j);
            auto image = generator.mapPoint(point);
            if (!this->orbit.pointToOrbitId[image].isPresent()) {
                this->orbit.pointToOrbitId[image] = orbitId;
                this->orbit.vector[image] = SchreierVectorEntry{std::uint32_t(j), point};
                workingPoints.push_back(image);
            }
        }
    }
}
void SchreierVectorBuilder::appendAllOrbits() {
    for (std::size_t i = 0; i < this->genset->getPermutationLength(); i++) {
        this->appendOrbit(i);
    }
}

StackedPermutation pperm::traceSchreierVector(PermutationStack &stack, upoint_type point, PermutationList &genset, const OptionalSchreierVectorEntry *vec) {
    auto permLen = genset.getPermutationLength();
    OptionalSchreierVectorEntry entry;
    StackedPermutation ret(stack, permLen), tmp(stack, permLen);
    ret.identity();
    while ((entry = vec[point]).isPresent()) {
        auto entry2 = entry.get();
        tmp.copy(ret);
        ret.multiply(genset.get(entry2.generator), tmp);
        point = entry2.sourcePoint;
    }
    return ret;
}

bool pperm::isInGroup(PermutationStack &stack, PermutationView perm, PermutationList &genset, Slice<upoint_type> base) {
    auto permLen = genset.getPermutationLength();
    PermutationList genset2(genset);
    StackedPermutation tmp(stack, permLen), tmp2(stack, permLen), tmp3(stack, permLen);
    tmp.copy(perm);
    SchreierVectorBuilder orbitBuilder(genset2);
    for (std::size_t i = 0; i < base.len; i++) {
        auto currentBase = base[i];
        auto baseImage = tmp.mapPoint(currentBase);
        orbitBuilder.reset(genset2);
        orbitBuilder.appendOrbit(currentBase);
        if (orbitBuilder.orbit.onSameOrbit(currentBase, baseImage)) {
            tmp2.inverse(traceSchreierVector(stack, baseImage, genset2, orbitBuilder.orbit.vector.get()));
            tmp3.copy(tmp);
            tmp.multiply(tmp3, tmp2);
        } else {
            return false;
        }
        stabilizerInPlace(genset2, currentBase);
    }
    return tmp.isIdentity();
}

void JerrumBranching::setPermutationLength(std::size_t permLen) {
    auto tableSize = permLen * (permLen + 1);
    this->permLen = permLen;
    if (tableSize > this->tableSize) {
        if (this->tableData) {
            delete[] this->tableData;
        }
        this->tableData = new OptionalPtr[tableSize];
        this->tableSize = tableSize;
    }
    arraySet(this->tableData, tableSize, None{});
    this->permutationStorage.setPermutationLength(permLen);
    this->freePermutations.clear();
}

JerrumBranching::Ptr JerrumBranching::allocPermutation() {
    auto freeSize = this->freePermutations.size();
    if (freeSize > 0) {
        auto ret = this->freePermutations[freeSize - 1];
        this->freePermutations.pop_back();
        return Ptr(ret);
    } else {
        auto value = this->permutationStorage.getSize();
        this->permutationStorage.push();
        return Ptr(value);
    }
}

void JerrumBranching::setEdge(upoint_type i, upoint_type j, OptionalPtr value) {
    auto &old = this->tableData[i * this->permLen + j];
    if (old.isPresent()) {
        this->retainPermutation(old.get());
    }
    old = value;
}

void JerrumBranching::recalculateVertices(std::deque<upoint_type> &workStack) {
    auto ptr = this->tableData + this->permLen*this->permLen;
    for (upoint_type p = 0; p < this->permLen; p++) {
        if (ptr[p].isPresent()) {
            this->retainPermutation(ptr[p].get());
            ptr[p] = None{};
        }
    }
    workStack.clear();

    auto root = this->findRoot();
    if (root.isPresent()) {
        auto ptr0 = this->allocPermutation();
        this->getPermutation(ptr0).identity();
        ptr[root.get()] = ptr0;
        workStack.push_back(root.get());
    }

    while (!workStack.empty()) {
        upoint_type point = workStack[0];
        workStack.pop_front();
        auto permId = ptr[point].get();
        for (upoint_type point2 = 0; point2 < this->permLen; point2++) {
            auto edge = this->getEdge(point, point2);
            if (edge.isPresent()) {
                auto ptr0 = this->allocPermutation();
                this->getPermutation(ptr0).multiply(this->getPermutation(permId), this->getPermutation(edge.get()));
                ptr[point2] = ptr0;
                workStack.push_back(point2);
            }
        }
    }
}

bool JerrumBranching::hasPath(std::deque<upoint_type> &queue, upoint_type p1, upoint_type p2) {
    queue.clear();
    queue.push_back(p1);
    while (!queue.empty()) {
        auto point = queue[0];
        queue.pop_front();
        for (upoint_type point2 = 0; point2 < this->permLen; point2++) {
            if (this->getEdge(point, point2).isPresent()) {
                if (p2 == point2) {
                    return true;
                }
                queue.push_back(point2);
            }
        }
    }
    return false;
}

OptionalUInt<upoint_type> JerrumBranching::findRoot() const {
    for (upoint_type p1 = 0; p1 < this->permLen;) {
        auto hasNext = false;
        auto hasPrev = false;
        auto nextP = p1 + 1;
        for (upoint_type p2 = 0; p2 < this->permLen; p2++) {
            if (this->getEdge(p1, p2).isPresent()) {
                hasNext = true;
            }
            if (this->getEdge(p2, p1).isPresent()) {
                hasPrev = false;
                nextP = p2;
            }
        }
        if (hasNext && !hasPrev) {
            return p1;
        }
        p1 = nextP;
    }
    return None{};
}

void JerrumBranching::siftElement(PermutationStack &stack, std::deque<upoint_type> &queue, PermutationView perm) {
    StackedPermutation tmp(stack, this->permLen), tmp2(stack, this->permLen);
    tmp.copy(perm);
    auto findEdge1 = [this](upoint_type j, upoint_type l) -> OptionalUInt<upoint_type> {
        for (upoint_type m = j + 1; m < this->permLen; m++) {
            if (this->getEdge(m, l).isPresent()) {
                return m;
            }
        }
        return None{};
    };
    while (1) {
        auto j = tmp.firstNonFixedPoint();
        auto l = tmp.mapPoint(j);
        while (j + 1 < this->permLen && this->hasPath(queue, j, l)) {
            tmp2.inverse(this->getPermutation(this->getVertex(l).get()));
            tmp.multiply(tmp, tmp2);
            tmp.multiply(tmp, this->getPermutation(this->getVertex(j).get()));
            j = tmp.firstNonFixedPoint();
            l = tmp.mapPoint(j);
        }
        if (j + 1 == this->permLen) {
            return;
        }
        auto m0 = findEdge1(j, l);
        while (m0.isPresent()) {
            auto m = m0.get();
            tmp2.inverse(this->getPermutation(this->getEdge(m, l).get()));
            tmp.multiply(tmp, tmp2);
            l = m;
            m0 = findEdge1(j, l);
        }
        m0 = findEdge1(0, l);
        if (m0.isPresent()) {
            StackedPermutation tmp3(stack, this->permLen);
            auto m = m0.get();
            tmp3.copy(this->getPermutation(this->getEdge(m, l).get()));
            this->setEdge(m, l, None{});
            this->setEdgePermutation(j, l, tmp);
            tmp2.inverse(tmp);
            tmp3.multiply(tmp3, tmp2);
            tmp.copy(tmp3);
            this->recalculateVertices(queue);
        } else {
            this->setEdgePermutation(j, l, tmp);
            this->recalculateVertices(queue);
            return;
        }
    }
}

void JerrumBranching::dump(std::ostream &os) {
    std::deque<upoint_type> queue;
    auto root = this->findRoot();
    if (root.isPresent()) {
        queue.push_back(root.get());
    }
    os << "branching {" << std::endl;
    while (!queue.empty()) {
        auto p = queue[0];
        queue.pop_front();
        for (upoint_type p2 = 0; p2 < this->permLen; p2++) {
            auto edge = this->getEdge(p, p2);
            if (edge.isPresent()) {
                os << "    " << p << " -- " << this->getPermutation(edge.get()) << " --> " << p2 << std::endl;
                queue.push_back(p2);
            }
        }
    }
    for (upoint_type p = 0; p < this->permLen; p++) {
        auto ver = this->getVertex(p);
        if (ver.isPresent()) {
            std::cout << "    v(" << p << ") = " << this->getPermutation(ver.get()) << std::endl;
        }
    }
    os << "}" << std::endl;
}

void JerrumBranchingBuilder::augment(upoint_type i) {
    auto permLen = this->currentGens.getPermutationLength();
    for (upoint_type p = 0; p < permLen; p++) if(p != i && this->orbit.orbit.pointToOrbitId[p].isPresent()) {
        for (upoint_type l = 0; l < permLen; l++) {
            this->branching.setEdge(l, p, None{});
        }
        auto u1 = traceSchreierVector(*this->permStack, p, this->currentGens, this->orbit.orbit.vector.get());
        auto permPtr = this->branching.allocPermutation();
        this->branching.getPermutation(permPtr).copy(u1);
        this->branching.setEdge(i, p, permPtr);
    }
}

void JerrumBranchingBuilder::build(PermutationStack &permStack, PermutationList &genset) {
    auto permLen = genset.getPermutationLength();
    this->branching.setPermutationLength(permLen);
    this->currentGens.setPermutationLength(permLen);
    this->schreierGens.setPermutationLength(permLen);
    this->siftingBranching.setPermutationLength(permLen);
    this->permStack = &permStack;
    this->currentGens.addAll(genset.begin(), genset.end());

    for (upoint_type p = 0; p < permLen; p++) {
        this->orbit.reset(this->currentGens);
        this->orbit.appendOrbit(p);
        this->augment(p);
        this->schreierGens.clear();
        schreierGenerators(this->schreierGens, *this->permStack, this->currentGens, this->orbit.orbit);
        this->siftingBranching.reset();
        for (auto perm : this->schreierGens) {
            this->siftingBranching.siftElement(*this->permStack, this->queue, perm);
        }
        this->currentGens.clear();
        this->siftingBranching.collectLabels([this](PermutationView perm) { this->currentGens.addPermutation(perm); });
    }
}

// Double coset representative
struct AlphaTable {
    private:
};