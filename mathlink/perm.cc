#include <algorithm>
#include <chrono>
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
    this->isNegative = tmp.isNegative();
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
    if (cycles.isNegative) {
        os << "-";
    }
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

void PermutationList::print(std::ostream &os, PermutationFormatter &formatter) {
    os << "{";
    bool first = true;
    for (auto perm : *this) {
        if (!first) {
            os << ", ";
        }
        first = false;
        formatter.print(os, perm);
    }
    os << "}";
}

namespace {
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
}

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


template<typename T>
static inline OptionalUInt<std::size_t> firstNonePos(const OptionalUInt<T> *ptr, std::size_t len) {
    for (std::size_t i = 0; i < len; i++, ptr++) {
        if (!ptr->isPresent()) {
            return i;
        }
    }
    return None{};
}

void SchreierOrbit::appendOrbit(upoint_type point, PermutationList &genset, std::deque<upoint_type> &queue) {
    if (this->pointToOrbitId[point].isPresent()) {
        return;
    }
    auto orbitId = this->orbitCount++;
    this->pointToOrbitId[point] = orbitId;
    queue.clear();
    queue.push_back(point);
    auto gensetSize = genset.getSize();
    while (!queue.empty()) {
        auto point = queue[0];
        queue.pop_front();
        for (std::size_t j = 0; j < gensetSize; j++) {
            auto generator = genset.get(j);
            auto image = generator.mapPoint(point);
            if (!this->pointToOrbitId[image].isPresent()) {
                this->pointToOrbitId[image] = orbitId;
                this->vector[image] = SchreierVectorEntry{std::uint32_t(j), point};
                queue.push_back(image);
            }
        }
    }
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
void SchreierOrbit::dump(std::ostream &os, PermutationList &genset, PermutationFormatter &formater) {
    bool empty = true;
    auto vec = this->vector.get();
    os << "schreier vector {";
    for (upoint_type point = 0; point < genset.getPermutationLength(); point++, vec++) {
        if (vec->isPresent()) {
            auto entry = vec->get();
            empty = false;
            os << std::endl << "    " << point << " <-- " << formater.formatValue(genset.get(entry.generator)) << " -- " << entry.sourcePoint;
        }
    }
    if (!empty) os << std::endl;
    os << "}";
}

StackedPermutation pperm::traceSchreierVector(PermutationStack &stack, upoint_type point, PermutationList &genset, const OptionalSchreierVectorEntry *vec) {
    auto permLen = genset.getPermutationLength();
    OptionalSchreierVectorEntry entry;
    auto ret = stack.pushStacked(permLen);
    auto tmp = stack.pushStacked(permLen);
    ret.identity();
    while ((entry = vec[point]).isPresent()) {
        auto entry2 = entry.get();
        tmp.copy(ret);
        ret.multiply(genset.get(entry2.generator), tmp);
        point = entry2.sourcePoint;
    }
    return ret;
}

StackedPermutation pperm::doubleTraceSchreierVector(PermutationStack &stack, upoint_type point1, upoint_type point2, PermutationList &genset, const OptionalSchreierVectorEntry *vec) {
    auto permLen = genset.getPermutationLength();
    StackedPermutation tmp1(stack, permLen);
    tmp1.inverse(traceSchreierVector(stack, point1, genset, vec));
    tmp1.multiply(tmp1, traceSchreierVector(stack, point2, genset, vec));
    return tmp1;
}

void pperm::computeOrbit(bool *points, upoint_type start, PermutationList &perms, std::deque<upoint_type> &workQueue) {
    auto permLen = perms.getPermutationLength();
    arraySet(points, permLen, false);
    points[start] = true;
    workQueue.clear();
    workQueue.push_back(start);
    while (!workQueue.empty()) {
        auto point = workQueue[0];
        workQueue.pop_front();
        for (auto perm : perms) {
            auto image = perm.mapPoint(point);
            if (!points[image]) {
                points[image] = true;
                workQueue.push_back(image);
            }
        }
    }
}

bool pperm::isInGroup(PermutationStack &stack, PermutationView perm, PermutationList &genset, Slice<upoint_type> base) {
    auto permLen = genset.getPermutationLength();
    PermutationList genset2(genset);
    StackedPermutation tmp(stack, permLen), tmp2(stack, permLen), tmp3(stack, permLen);
    tmp.copy(perm);
    SchreierOrbit orbit;
    std::deque<upoint_type> queue;
    for (std::size_t i = 0; i < base.len; i++) {
        auto currentBase = base[i];
        auto baseImage = tmp.mapPoint(currentBase);
        orbit.reset(permLen);
        orbit.appendOrbit(currentBase, genset2, queue);
        if (orbit.onSameOrbit(currentBase, baseImage)) {
            tmp2.inverse(traceSchreierVector(stack, baseImage, genset2, orbit.vector.get()));
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

void JerrumBranching::dump(std::ostream &os, PermutationFormatter &formatter) {
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
                os << "    " << p << " -- " << formatter.formatValue(this->getPermutation(edge.get())) << " --> " << p2 << std::endl;
                queue.push_back(p2);
            }
        }
    }
    for (upoint_type p = 0; p < this->permLen; p++) {
        auto ver = this->getVertex(p);
        if (ver.isPresent()) {
            std::cout << "    v(" << p << ") = " << formatter.formatValue(this->getPermutation(ver.get())) << std::endl;
        }
    }
    os << "}" << std::endl;
}

void JerrumBranchingBuilder::augment(upoint_type i) {
    auto permLen = this->currentGens.getPermutationLength();
    for (upoint_type p = 0; p < permLen; p++) if(p != i && this->orbit.pointToOrbitId[p].isPresent()) {
        for (upoint_type l = 0; l < permLen; l++) {
            this->branching.setEdge(l, p, None{});
        }
        auto u1 = traceSchreierVector(*this->permStack, p, this->currentGens, this->orbit.vector.get());
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
        this->orbit.reset(this->currentGens.getPermutationLength());
        this->orbit.appendOrbit(p, this->currentGens, this->queue);
        this->augment(p);
        this->schreierGens.clear();
        schreierGenerators(this->schreierGens, *this->permStack, this->currentGens, this->orbit);
        this->siftingBranching.reset();
        for (auto perm : this->schreierGens) {
            this->siftingBranching.siftElement(*this->permStack, this->queue, perm);
        }
        this->currentGens.clear();
        this->siftingBranching.collectLabels([this](PermutationView perm) { this->currentGens.addPermutation(perm); });
    }
}

static inline upoint_type findFirstPoint(const bool *points, std::size_t size) {
    for (upoint_type ret = 0; ret < size; ret++, points++) {
        if (*points) {
            return ret;
        }
    }
#ifdef PPERM_DEBUG
    throw std::runtime_error("first point not found");
#endif
}

static inline void boolListComplement(bool *dest, const bool *exclude, std::size_t len) {
    for (std::size_t i = 0; i < len; i++, dest++, exclude++) {
        if (*exclude) {
            *dest = false;
        }
    }
}

namespace {
    struct PointSet {
        const bool *data;
        std::size_t len;
    };
    std::ostream &operator << (std::ostream &os, PointSet set) {
        bool first = true;
        auto ptr = set.data;
        os << "{";
        for (upoint_type p = 0; p < set.len; p++, ptr++) if(*ptr) {
            if (!first) {
                os << ", ";
            }
            first = false;
            os << p;
        }
        os << "}";
        return os;
    }
}

void BaseChanger::setSGS(PermutationList &genset) {
    auto permLen = genset.getPermutationLength();
    this->genset.setPermutationLength(permLen);
    this->genset.copy(genset);
    this->stabilizer.setPermutationLength(permLen);
    this->stabilizer2.setPermutationLength(permLen);
    this->newGens.setPermutationLength(permLen);
}

void BaseChanger::interchange(Slice<upoint_type> base, std::size_t pos, PermutationStack &stack) {
    auto permLen = this->genset.getPermutationLength();
    this->orbitSets.ensureSize(permLen * 3);
    auto gamma = this->orbitSets.get(), deltai = gamma + permLen, tmpPointSet = deltai + permLen;
    arraySet(gamma, permLen * 3, false);

    auto bp1 = base[pos], bp2 = base[pos + 1];

    this->stabilizer.copy(this->genset);
    stabilizerPointsInPlace(this->stabilizer, base.slice(0, pos));
    this->stabilizer2.copy(this->stabilizer);
    stabilizerInPlace(this->stabilizer2, bp1);

    this->newGens.copy(this->stabilizer2);
    stabilizerInPlace(this->newGens, bp2);
    this->orbit1.reset(permLen);
    this->orbit2.reset(permLen);
    this->orbit1.appendOrbit(bp1, this->stabilizer, this->queue);
    this->orbit2.appendOrbit(bp2, this->stabilizer2, this->queue);

    computeOrbit(tmpPointSet, bp2, this->stabilizer, this->queue);
    auto barDeltaj1Size = this->orbit1.allOrbitSize() *  this->orbit2.allOrbitSize() / std::count(tmpPointSet, tmpPointSet + permLen, true);

    deltai[bp1] = true;
    this->orbit1.collectAllOrbit([gamma](upoint_type p, upoint_type orbit){ gamma[p] = true; });
    gamma[bp1] = false;
    gamma[bp2] = false;

    while (std::count(deltai, deltai + permLen, true) < barDeltaj1Size) {
        auto gamma0 = findFirstPoint(gamma, permLen);
        auto g1 = traceSchreierVector(stack, gamma0, this->stabilizer, this->orbit1.vector.get());
        auto g1InvMapBp2 = g1.inverseMapPoint(bp2);
        if (this->orbit2.pointToOrbitId[g1InvMapBp2].isPresent()) {
            auto g2 = traceSchreierVector(stack, g1InvMapBp2, this->stabilizer2, this->orbit2.vector.get());
            g2.multiply(g2, g1);
            this->newGens.addPermutation(g2);
            computeOrbit(deltai, bp1, this->newGens.permutations, this->queue);
            boolListComplement(gamma, deltai, permLen);
        } else {
            computeOrbit(tmpPointSet, gamma0, this->newGens.permutations, this->queue);
            boolListComplement(gamma, tmpPointSet, permLen);
        }
    }
    this->genset.addAll(this->newGens.begin(), this->newGens.end());
    std::swap(base[pos], base[pos + 1]);
}

void BaseChanger::completeBaseChange(Slice<upoint_type> &base, Slice<upoint_type> newBase, PermutationStack &stack) {
    auto permLen = this->genset.getPermutationLength();
    std::size_t i = 0;
    auto conjPerm = stack.pushStacked(permLen);
    conjPerm.identity();
    auto permInv = stack.pushStacked(permLen);
    permInv.inverse(conjPerm);

    this->stabilizer.copy(this->genset);
    this->orbit1.reset(permLen);
    this->orbit1.appendOrbit(base[0], this->stabilizer, this->queue);
    if (this->orbit1.pointToOrbitId[newBase[0]].isPresent()) {
        auto perm2 = stack.pushStacked(permLen);
        while (1) {
            auto t = traceSchreierVector(stack, permInv.mapPoint(newBase[i]), this->stabilizer, this->orbit1.vector.get());
            perm2.multiply(t, conjPerm);
            conjPerm.copy(perm2);
            permInv.inverse(conjPerm);
            i++;
            if (i >= base.len || i >= newBase.len) {
                break;
            }
            stabilizerInPlace(this->stabilizer, base[i - 1]);
            this->orbit1.reset(permLen);
            this->orbit1.appendOrbit(base[i], this->stabilizer, this->queue);
            if (!this->orbit1.pointToOrbitId[newBase[i]].isPresent()) {
                break;
            }
        }
    }
    if (!conjPerm.isIdentity()) {
        auto tmp = stack.pushStacked(permLen);
        for (upoint_type p = 0; p < base.len; p++) {
            base[p] = conjPerm.mapPoint(base[p]);
        }
        for (auto gen : this->genset) {
            tmp.multiply(permInv, gen);
            tmp.multiply(tmp, conjPerm);
            gen.copy(tmp);
        }
    }
    for (std::size_t j = i; j < newBase.len; j++) {
        auto pos = base.indexOf(newBase[j]);
        if (pos == base.len) {
            base[base.len++] = newBase[j];
        }
        this->moveToFirst(base, pos, stack);
    }
}

std::size_t GroupOrderCalculator::nextFactor() {
    if (this->stabilizer < this->genset.getPermutationLength()) {
        auto permLen = this->genset.getPermutationLength();
        arraySet(this->orbit.get(), permLen, false);
        computeOrbit(this->orbit.get(), this->stabilizer, this->genset, this->queue);
        auto ret = std::count(this->orbit.get(), this->orbit.get() + permLen, true);
        stabilizerInPlace(this->genset, this->stabilizer);
        this->stabilizer++;
        return ret;
    } else {
        return 1;
    }
}

std::size_t GroupOrderCalculator::order() {
    auto permLen = this->genset.getPermutationLength();
    std::size_t ret = 1;
    for (std::size_t i = 0; i < permLen; i++) {
        ret *= this->nextFactor();
    }
    return ret;
}

void BaseChangingStrongGenSetProvider::stabilizeOnePoint(PermutationStack &stack, Slice<upoint_type> base, upoint_type point) {
    auto pos = base.indexOf(point);
    if (pos > 0) {
        this->baseChanger->setSGS(this->genset);
        upoint_type newBase[]{point};
        auto newBaseSlice = makeSlice(newBase, 1);
        // this->baseChanger->moveToFirst(base, pos, stack);
        this->baseChanger->completeBaseChange(base, newBaseSlice, stack);
    }
    stabilizerInPlace(this->genset, point);
}
PermutationList &BaseChangingStrongGenSetProvider::getStrongGenSet() {
    return this->genset;
}

namespace {
    struct AlphaTableHashContext {
        AlphaTable *self;
        int compare(std::size_t i, std::size_t j) const {
            return this->self->get(i).getL().compare(this->self->get(j).getL());
        }
        int compare(Slice<upoint_type> l, std::size_t i) const {
            return l.compare(this->self->get(i).getL());
        }
        std::size_t hash(std::size_t i) const {
            return this->self->get(i).getL().hash();
        }
        std::size_t hash(Slice<upoint_type> l) const {
            return l.hash();
        }
    };
}

void AlphaTable::Entry::print(std::ostream &os, PermutationFormatter &formatter) {
    os << "[" << this->getL() << "] -> {" << formatter.formatValue(this->getS()) << ", " << formatter.formatValue(this->getD()) << "}";
}

std::ostream &pperm::operator << (std::ostream &os, AlphaTable::Entry entry) {
    os << "[" << entry.getL() << "] -> (" << entry.getS() << ", " << entry.getD() << ")";
    return os;
}

static inline void mapPointSet(bool *dest, const bool *set, PermutationView perm) {
    for (upoint_type p = 0; p < perm.len; p++) if (set[p]) {
        dest[perm.mapPoint(p)] = true;
    }
}

static inline void pointSetIntersection(bool *dest, const bool *set2, std::size_t len) {
    for (std::size_t i = 0; i < len; i++, dest++, set2++) {
        *dest = *dest && *set2;
    }
}

void DoubleCosetRepresentativeSolver::subroutineF1(bool *ret, const bool *orbitB, TableEntry entry, PermutationView perm) {
#ifdef PPERM_DEBUG
    if (this->log) {
        *this->log << "calling F1 with ret = " << PointSet{ret, this->permLen}
            << ", Delta_b = " << PointSet{orbitB, this->permLen}
            << ", entry = " << this->permFormatter.formatValue(entry)
            << std::endl;
    }
#endif
    auto sgd = this->permStack.pushStacked(this->permLen);
    sgd.multiply(entry.getS(), perm);
    sgd.multiply(sgd, entry.getD());
    auto orbitBsgd = this->boolSetPool.pushStacked(this->permLen);
    arraySet(orbitBsgd.begin(), this->permLen, false);
    mapPointSet(orbitBsgd.begin(), orbitB, sgd);
#ifdef PPERM_DEBUG
    if (this->log) {
        *this->log << "sgd = " << this->permFormatter.formatValue(sgd)
            << ", Delta_b^(sgd) = " << PointSet{orbitBsgd.begin(), this->permLen}
            << std::endl;
    }
#endif
    auto selectedOrbits = this->boolSetPool.pushStacked(this->orbitD.orbitCount);
    arraySet(selectedOrbits.begin(), this->orbitD.orbitCount, false);
    auto pointToOrbitId = this->orbitD.pointToOrbitId.get();
    for (upoint_type p = 0; p < this->permLen; p++) {
        auto orbitId0 = pointToOrbitId[p];
        if (orbitId0.isPresent() && orbitBsgd[p]) {
            selectedOrbits[orbitId0.get()] = true;
        }
    }
    for (upoint_type p = 0; p < this->permLen; p++) {
        auto orbitId0 = pointToOrbitId[p];
        if (orbitId0.isPresent() && selectedOrbits[orbitId0.get()]) {
            ret[p] = true;
        }
    }
#ifdef PPERM_DEBUG
    if (this->log) {
        *this->log << "ret = " << PointSet{ret, this->permLen} << ", end of F1" << std::endl;
    }
#endif
}

std::optional<StackedPermutation> DoubleCosetRepresentativeSolver::solve(StrongGenSetProvider &gensetSProvider, StrongGenSetProvider &gensetDProvider, PermutationView perm) {
#ifdef PPERM_DEBUG
    if (this->log) {
        *this->log << "============== Begin double coset representative algorithm on g = " << this->permFormatter.formatValue(perm)
            << ", S = " << this->permFormatter.formatRef(gensetSProvider.getStrongGenSet())
            << ", D = " << this->permFormatter.formatRef(gensetDProvider.getStrongGenSet())
            << std::endl;
    }
#endif
    auto &gensetD = gensetDProvider.getStrongGenSet();
    this->setPermutationLength(gensetD.getPermutationLength());
    this->baseChangeOfDTime = 0;
    this->baseChangeOfSTime = 0;

    // set up bases
//     auto baseProvider = [](std::size_t i) { return i; };
//     auto tmpBase = makeSlice(this->getTmpBase(), 0);
//     filterBasePoints([&](upoint_type p) { tmpBase[tmpBase.len++] = p; }, baseProvider, this->permLen, gensetSProvider.getStrongGenSet(), this->baseFilterTmp);
//     this->initialBaseSLen = tmpBase.len;
//     this->baseSStart = this->permLen - tmpBase.len;
//     this->getInitialBaseS().copy(tmpBase);
//     this->getBaseS().copy(tmpBase);
//     tmpBase.len = 0;
//     filterBasePoints([&](upoint_type p) { tmpBase[tmpBase.len++] = p; }, baseProvider, this->permLen, gensetD, this->baseFilterTmp);
//     this->baseDStart = this->permLen - tmpBase.len;
//     this->getBaseD().copy(tmpBase);
// #ifdef PPERM_DEBUG
//     if (this->log) {
//         *this->log << "initial base(S) = {" << this->getInitialBaseS() << "}" << std::endl
//             << "initial base (D) = {" << this->getBaseD() << "}" << std::endl;
//     }
// #endif
    this->initialBaseSLen = this->permLen;
    this->baseSStart = 0;
    this->baseDStart = 0;
    auto initialBaseS = this->getInitialBaseS(), baseS = this->getBaseS(), baseD = this->getBaseD();
    for (upoint_type p = 0; p < this->permLen; p++) {
        initialBaseS[p] = p;
        baseS[p] = p;
        baseD[p] = p;
    }

    if (this->useTwoStep) {
        upoint_type minNonFixedPointOfD = this->permLen;
        for (auto p : gensetD) {
            auto p2 = p.firstNonFixedPoint();
            if (p2 < minNonFixedPointOfD) {
                minNonFixedPointOfD = p2;
            }
        }
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "minNonFixedPointOfD = " << minNonFixedPointOfD << std::endl;
        }
#endif
        auto start = std::chrono::system_clock::now();
        auto perm2 = this->solveRightCosetRepresentative(perm, gensetSProvider, minNonFixedPointOfD);
        auto middle = std::chrono::system_clock::now();
        this->extendBaseS(minNonFixedPointOfD, perm2);
        auto ret = this->solveDoubleCosetRepresentative(gensetSProvider, gensetDProvider, perm2, minNonFixedPointOfD);
        auto end = std::chrono::system_clock::now();
        if (this->log) {
            *this->log << "time in right coset rep: " << std::chrono::duration_cast<std::chrono::microseconds>(middle - start).count() << std::endl
                << "time in double coset rep: " << std::chrono::duration_cast<std::chrono::microseconds>(end - middle).count() << std::endl
                << "base change time of S: " << this->baseChangeOfSTime << std::endl
                << "base change time of D: " << this->baseChangeOfDTime << std::endl;
        }
        return ret;
    } else {
        // no need to extend base of S, it's full
        auto start = std::chrono::system_clock::now();
        auto ret = this->solveDoubleCosetRepresentative(gensetSProvider, gensetDProvider, perm, 0);
        auto end = std::chrono::system_clock::now();
        if (this->log) {
            *this->log << "time in double coset rep: " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() << std::endl
                << "base change time of D: " << this->baseChangeOfDTime << std::endl;
        }
        return ret;
    }
}

static upoint_type minPointWithRespectToBase(const bool *points, std::size_t permLen, Slice<upoint_type> base) {
    for (auto b : base) {
        if (points[b]) return b;
    }
    for (upoint_type p = 0; p < permLen; p++) {
        if (points[p]) return p;
    }
    throw std::runtime_error("first point not found");
}

StackedPermutation DoubleCosetRepresentativeSolver::solveRightCosetRepresentative(PermutationView perm, StrongGenSetProvider &gensetSProvider, upoint_type minNonFixedPointOfD) {
#ifdef PPERM_DEBUG
    if (this->log) {
        *this->log << "========= begin right coset representative subroutine =======" << std::endl;
        *this->log << "g = " << this->permFormatter.formatValue(perm) << std::endl;
    }
#endif
    auto freesOwned = this->boolSetPool.pushStacked(this->permLen), freesOwned2 = this->boolSetPool.pushStacked(this->permLen);
    auto frees = freesOwned.begin(), frees2 = freesOwned2.begin();
    arraySet(frees, this->permLen, false);
    arraySet(frees2, this->permLen, false);
    for (upoint_type i = 0; i < minNonFixedPointOfD; i++) {
        frees[i] = true;
    }
    auto perm2 = this->permStack.pushStacked(this->permLen);
    perm2.copy(perm);
    auto tmpPerm1 = this->permStack.pushStacked(this->permLen);
    tmpPerm1.inverse(perm);
    mapPointSet(frees2, frees, tmpPerm1);
    std::swap(frees, frees2);
#ifdef PPERM_DEBUG
    if (this->log) {
        *this->log << "frees = " << PointSet{frees, this->permLen} << std::endl;
    }
#endif
    auto orbitB = this->boolSetPool.pushStacked(this->permLen);
    auto orbitBInFrees = this->boolSetPool.pushStacked(this->permLen);
    auto orbitBInFreesPerm = this->boolSetPool.pushStacked(this->permLen);
    std::size_t foundFrees = 0;
    auto freeCount = std::count(frees, frees + this->permLen, true);
    auto initialBaseS = this->getInitialBaseS();
    for (upoint_type i = 0; i < initialBaseS.len && foundFrees < freeCount; i++) {
        auto basePoint = initialBaseS[i];
        auto &gensetS = gensetSProvider.getStrongGenSet();
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "======= iteration i = " << i
                << ", S = " << this->permFormatter.formatRef(gensetS)
                << ", base(S) = {" << this->getBaseS() << "}"
                << ", g = " << this->permFormatter.formatValue(perm2)
                << ", frees = " << PointSet{frees, this->permLen} << std::endl;
            *this->log << "working on point " << basePoint << std::endl;
        }
#endif
        this->orbitS.reset(this->permLen);
        this->orbitS.appendOrbit(basePoint, gensetS, this->queue);
        arraySet(orbitB.begin(), this->permLen, false);
        this->orbitS.collectOneOrbit(basePoint, [&](upoint_type p){ orbitB[p] = true; });

        copyArray(orbitBInFrees.begin(), orbitB.begin(), this->permLen);
        pointSetIntersection(orbitBInFrees.begin(), frees, this->permLen);

#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "Delta_S = ";
            this->orbitS.dump(*this->log, gensetS, this->permFormatter);
            *this->log << std::endl;
            *this->log << "Delta_" << basePoint << " = " << PointSet{orbitB.begin(), this->permLen} << std::endl;
            *this->log << "Delta_" << basePoint << " int. frees = " << PointSet{orbitBInFrees.begin(), this->permLen} << std::endl;
        }
#endif
        if (std::count(orbitBInFrees.begin(), orbitBInFrees.end(), true) == 0) {
            continue;
        }
        foundFrees++;

        arraySet(orbitBInFreesPerm.begin(), this->permLen, false);
        mapPointSet(orbitBInFreesPerm.begin(), orbitBInFrees.begin(), perm2);
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "(Delta_" << basePoint << " int. frees)^g = " << PointSet{orbitBInFreesPerm.begin(), this->permLen} << std::endl;
        }
#endif
        auto pi = perm2.inverseMapPoint(findFirstPoint(orbitBInFreesPerm.begin(), this->permLen));
        auto permOmega = traceSchreierVector(this->permStack, pi, gensetS, this->orbitS.vector.get());
        tmpPerm1.multiply(permOmega, perm2);
        perm2.copy(tmpPerm1);

        tmpPerm1.inverse(permOmega);
        arraySet(frees2, this->permLen, false);
        mapPointSet(frees2, frees, tmpPerm1);
        std::swap(frees, frees2);
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "pi = " << pi << ", trace(pi) = " << this->permFormatter.formatValue(permOmega) << std::endl;
            *this->log << "new frees = " << PointSet{frees, this->permLen} << std::endl;
        }
#endif
        this->baseChangeOfSTime += measureElapsed([&]() {
            gensetSProvider.stabilizeOnePoint(this->permStack, this->getBaseS(), basePoint);
        }).count();
        this->baseSStart++;
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "S_" << basePoint << " = " << this->permFormatter.formatRef(gensetSProvider.getStrongGenSet()) << std::endl;
        }
#endif
    }
#ifdef PPERM_DEBUG
    if (this->log) {
        *this->log << "end of right coset representative, g = " << this->permFormatter.formatValue(perm2) << std::endl;
    }
#endif
    return perm2;
}

void DoubleCosetRepresentativeSolver::extendBaseS(upoint_type minNonFixedPointOfD, PermutationView perm) {
    auto permInv = this->permStack.pushStacked(this->permLen);
    permInv.inverse(perm);
    auto nonFixedPoints = this->boolSetPool.pushStacked(this->permLen);
    arraySet(nonFixedPoints.begin(), this->permLen, false);
    for (upoint_type i = minNonFixedPointOfD; i < this->permLen; i++) {
        nonFixedPoints[permInv.mapPoint(i)] = true;
    }
#ifdef PPERM_DEBUG
    if (this->log) {
        *this->log << "nonFixedPoints = " << PointSet{nonFixedPoints.begin(), this->permLen} << std::endl;
    }
#endif
    for (auto b : this->getBaseS()) {
        nonFixedPoints[b] = true;
    }
    auto newBaseSLen = std::count(nonFixedPoints.begin(), nonFixedPoints.end(), true);
    this->baseSStart = this->permLen - newBaseSLen;
    auto ptr = this->getBaseS().ptr;
    for (upoint_type p = 0; p < this->permLen; p++) if (nonFixedPoints[p]) {
        *ptr++ = p;
    }
#ifdef PPERM_DEBUG
    if (this->log) {
        *this->log << "extended base(S) = {" << this->getBaseS() << "}" << std::endl;
    }
#endif
}

std::optional<StackedPermutation> DoubleCosetRepresentativeSolver::solveDoubleCosetRepresentative(StrongGenSetProvider &gensetSProvider, StrongGenSetProvider &gensetDProvider, PermutationView perm, upoint_type minNonFixedPointOfD) {
#ifdef PPERM_DEBUG
    if (this->log) {
        *this->log << "========= begin double coset representative subroutine =======" << std::endl;
        *this->log << "S = " << this->permFormatter.formatRef(gensetSProvider.getStrongGenSet())
            << ", base(S) = {" << this->getBaseS() << "}"
            << ", D = " << this->permFormatter.formatRef(gensetDProvider.getStrongGenSet())
            << ", g = " << this->permFormatter.formatValue(perm) << std::endl;
    }
#endif
    auto ret = this->permStack.pushStacked(this->permLen);

    this->alphaPtr = 0;
    for (unsigned int i = 0; i < 2; i++) {
        this->alphas[i].setPermutationLength(this->permLen);
        this->alphas[i].clear();
    }
    {
        auto entry = this->alphas[this->alphaPtr].pushEntry();
        entry.setL(Slice<upoint_type>{nullptr, 0});
        entry.getS().identity();
        entry.getD().identity();
    }

    auto orbitB = this->boolSetPool.pushStacked(this->permLen);
    auto orbitPi = this->boolSetPool.pushStacked(this->permLen);
    auto images = this->boolSetPool.pushStacked(this->permLen);
    auto baseLen = this->permLen - this->baseSStart;
    auto baseS0 = this->getBaseS();
    for (upoint_type i = 0; i < baseLen; i++) {
        auto baseS = this->getBaseS();
        auto baseD = this->getBaseD();
        auto currentBaseSPoint = baseS[0];
        auto &gensetS = gensetSProvider.getStrongGenSet();
        auto &gensetD = gensetDProvider.getStrongGenSet();
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "============ iteration i = " << i
                << ", S = " << this->permFormatter.formatRef(gensetS)
                << ", base(S) = {" << baseS << "}"
                << ", D = " << this->permFormatter.formatRef(gensetD)
                << ", base(D) = {" << baseD << "}"
                << std::endl;
            *this->log << "working on base point " << currentBaseSPoint << std::endl;
        }
#endif
        auto &currentAlphaTab = this->alphas[this->alphaPtr];
        auto &nextAlphaTab = this->alphas[(this->alphaPtr + 1) & 1];

        this->orbitS.reset(this->permLen);
        // In the original algorithm here we should calculate all orbits of S, but it seems that just calculating the orbit of b_i sufficies
        this->orbitS.appendOrbit(currentBaseSPoint, gensetS, this->queue);
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "Delta_S = ";
            this->orbitS.dump(*this->log, gensetS, this->permFormatter);
            *this->log << std::endl;
        }
#endif
        arraySet(orbitB.begin(), this->permLen, false);
        this->orbitS.collectOneOrbit(currentBaseSPoint, [&](upoint_type p){ orbitB[p] = true; });
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "Delta_b = " << PointSet{orbitB.begin(), this->permLen} << std::endl;
        }
#endif

        this->orbitD.reset(this->permLen);
        this->orbitD.appendAllOrbits(gensetD, this->queue);

#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "Delta_D = ";
            this->orbitD.dump(*this->log, gensetD, this->permFormatter);
            *this->log << std::endl;
        }
#endif

        arraySet(images.begin(), this->permLen, false);
        for (auto entry : currentAlphaTab) {
            this->subroutineF1(images.begin(), orbitB.begin(), entry, perm);
        }
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "IMAGES = " << PointSet{images.begin(), this->permLen} << std::endl;
        }
#endif

        // Note: the ordering for determining the minimal point is just a convention
        auto pi = findFirstPoint(images.begin(), this->permLen);
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "pi = " << pi << std::endl;
        }
#endif
        this->orbitD.reset(this->permLen);
        this->orbitD.appendOrbit(pi, gensetD, this->queue);
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "Delta_D = ";
            this->orbitD.dump(*this->log, gensetD, this->permFormatter);
            *this->log << std::endl;
        }
#endif

        arraySet(orbitPi.begin(), this->permLen, false);
        computeOrbit(orbitPi.begin(), pi, gensetD, this->queue);
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "Delta_p = " << PointSet{orbitPi.begin(), this->permLen} << std::endl;
        }
#endif

        nextAlphaTab.clear();
        for (auto entry : currentAlphaTab) {
#ifdef PPERM_DEBUG
            if (this->log) {
                *this->log << "working on entry " << this->permFormatter.formatRef(entry) << std::endl;
            }
#endif
            auto s = entry.getS(), d = entry.getD();
            auto next = this->boolSetPool.pushStacked(this->permLen);
            arraySet(next.begin(), this->permLen, false);
            mapPointSet(next.begin(), orbitB.begin(), s);
#ifdef PPERM_DEBUG
            if (this->log) {
                *this->log << "Delta_b^s = " << PointSet{next.begin(), this->permLen} << std::endl;
            }
#endif
            auto gd = this->permStack.pushStacked(this->permLen);
            gd.multiply(perm, d);
            auto gdInv = this->permStack.pushStacked(this->permLen);
            gdInv.inverse(gd);
            auto orbitPigdInv = this->boolSetPool.pushStacked(this->permLen);
            arraySet(orbitPigdInv.begin(), this->permLen, false);
            mapPointSet(orbitPigdInv.begin(), orbitPi.begin(), gdInv);
            pointSetIntersection(next.begin(), orbitPigdInv.begin(), this->permLen);
#ifdef PPERM_DEBUG
            if (this->log) {
                *this->log << "gd = " << this->permFormatter.formatValue(gd) << std::endl;
                *this->log << "Delta_p^((gd)^-1) = " << PointSet{orbitPigdInv.begin(), this->permLen} << std::endl;
                *this->log << "NEXT = " << PointSet{next.begin(), this->permLen} << std::endl;
            }
#endif

            for (upoint_type j = 0; j < this->permLen; j++) if (next[j]) {
                auto s1 = traceSchreierVector(this->permStack, s.inverseMapPoint(j), gensetS, this->orbitS.vector.get());
                s1.multiply(s1, s);
                auto d2 = this->permStack.pushStacked(this->permLen);
                d2.inverse(traceSchreierVector(this->permStack, gd.mapPoint(j), gensetD, this->orbitD.vector.get()));
                auto d1 = this->permStack.pushStacked(this->permLen);
                d1.multiply(d, d2);

#ifdef PPERM_DEBUG
                if (this->log) {
                    *this->log << "for j = " << j
                        << ", s_1 = " << this->permFormatter.formatValue(s1)
                        << ", d_1 = " << this->permFormatter.formatValue(d1)
                        << std::endl;
                }
#endif
                auto newEntry = nextAlphaTab.pushEntry();
                newEntry.getS().copy(s1);
                newEntry.getD().copy(d1);
                newEntry.setL(entry.getL());
                newEntry.appendToL(j);
            }
        }
        this->alphaPtr = (this->alphaPtr + 1) & 1;

        this->sgdSet.clear();
        for (auto entry : nextAlphaTab) {
            auto sgd = this->permStack.pushStacked(this->permLen);
            sgd.multiply(entry.getS(), perm);
            sgd.multiply(sgd, entry.getD());
            this->sgdSet.addPermutation(sgd);
        }
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "sgd = " << this->permFormatter.formatRef(this->sgdSet) << std::endl;
        }
#endif
        for (auto elem : this->sgdSet) {
            auto elem2 = this->permStack.pushStacked(this->permLen);
            elem2.copy(elem);
            elem2.setNegative(!elem2.isNegative());
            if (this->sgdSet.findPermutation(elem2).isPresent()) {
#ifdef PPERM_DEBUG
                if (this->log) {
                    *this->log << "found permutations with opposite signs "
                        << this->permFormatter.formatValue(elem)
                        << " and " << this->permFormatter.formatValue(elem2)
                        << " in sgd"
                        << std::endl;
                }
#endif
                return std::nullopt;
            }
        }

        this->baseChangeOfDTime += measureElapsed([&]() {
            gensetSProvider.stabilizeOnePoint(this->permStack, baseS, currentBaseSPoint);
            gensetDProvider.stabilizeOnePoint(this->permStack, baseD, pi);
            this->baseSStart++;
            this->baseDStart++;
        }).count();

#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "TAB = {" << std::endl;
            for (auto entry : this->alphas[this->alphaPtr]) {
                *this->log << "    " << this->permFormatter.formatRef(entry) << std::endl;
            }
            *this->log << "}" << std::endl;
            *this->log << "Length[TAB] = " << this->alphas[this->alphaPtr].getSize() << std::endl;
            *this->log << "iteration i = " << i << " done" << std::endl;
        }
#endif
    }
    auto firstEntry = this->alphas[this->alphaPtr].get(0);
    ret.multiply(firstEntry.getS(), perm);
    ret.multiply(ret, firstEntry.getD());
#ifdef PPERM_DEBUG
    if (this->log) {
        *this->log << "end of double coset representative, result ret = " << this->permFormatter.formatValue(ret) << std::endl;
    }
#endif
    return ret;
}

std::ostream &pperm::operator << (std::ostream &os, SymmetricBlock &block) {
    os << "SymmetricBlock[";
    bool first = true;
    for (auto b : block) {
        if (!first) os << ", ";
        first = false;
        os << b;
    }
    os << "]";
    return os;
}

namespace {
    struct SymmetricBlockFirstColumnHashContext {
        std::vector<SymmetricBlock> *blocks;
        std::size_t hash(std::size_t i) const {
            return this->blocks->at(i).get(0).getData().hash();
        }
        std::size_t hash(Slice<upoint_type> d) const {
            return d.hash();
        }
        int compare(std::size_t i, std::size_t j) const {
            return this->blocks->at(i).get(0).getData().compare(this->blocks->at(j).get(0).getData());
        }
        int compare(Slice<upoint_type> d, std::size_t i) const {
            return d.compare(this->blocks->at(i).get(0).getData());
        }
    };
    struct SymmetricBlockLastColumnHashContext {
        std::vector<SymmetricBlock> *blocks;
        std::size_t hash(std::size_t i) const {
            return this->blocks->at(i).getLast().getData().hash();
        }
        std::size_t hash(Slice<upoint_type> d) const {
            return d.hash();
        }
        int compare(std::size_t i, std::size_t j) const {
            return this->blocks->at(i).getLast().getData().compare(this->blocks->at(j).getLast().getData());
        }
        int compare(Slice<upoint_type> d, std::size_t i) const {
            return d.compare(this->blocks->at(i).getLast().getData());
        }
    };
}

bool SymmetricBlockBuilder::tryAddOneGenerator(PermutationView perm) {
    auto images = perm.images();
    this->pairs.ensureSize(perm.len * 2);
    std::size_t pairCount = 0;
    auto pairFirst = this->pairs.get(), pairLast = pairFirst + perm.len;
    for (upoint_type p = 0; p < perm.len; p++) {
        auto pImage = images[p];
        if (pImage > p) {
            if (images[pImage] == p) {
                pairFirst[pairCount] = p;
                pairLast[pairCount] = pImage;
                pairCount++;
            } else return false;
        }
    }
    auto column1 = makeSlice(pairFirst, pairCount), column2 = makeSlice(pairLast, pairCount);
    auto lastMatching = this->byLastColumn.find(column1, SymmetricBlockLastColumnHashContext{&this->blocks});
    auto firstMatching = this->byFirstColumn.find(column2, SymmetricBlockFirstColumnHashContext{&this->blocks});
    if (firstMatching.isNonNull() && lastMatching.isNonNull()) {
        auto firstMatchingBlockId = this->byFirstColumn.getEntry(firstMatching)->value;
        auto lastMatchingBlockId = this->byLastColumn.getEntry(lastMatching)->value;
        auto &lastMatchingBlock = this->blocks[lastMatchingBlockId];
        auto &firstMatchingBlock = this->blocks[firstMatchingBlockId];
        for (auto entry : firstMatchingBlock) {
            auto newBlock = lastMatchingBlock.pushBlock();
            newBlock.setNegative(perm.isNegative() != entry.isNegative());
            newBlock.getData().copy(entry.getData());
        }
        firstMatchingBlock.clear();
        this->freeBlocks.push_back(firstMatchingBlockId);
        this->byLastColumn.remove(lastMatching);
        this->byFirstColumn.remove(firstMatching);
        auto newLast = lastMatchingBlock.getLast().getData();
        auto lastOfFirstMatchingBlock = this->byLastColumn.find(newLast, SymmetricBlockLastColumnHashContext{&this->blocks});
        this->byLastColumn.getEntry(lastOfFirstMatchingBlock)->value = lastMatchingBlockId;
    } else if (firstMatching.isNonNull()) {
        auto firstMatchingBlockId = this->byFirstColumn.getEntry(firstMatching)->value;
        auto &firstMatchingBlock = this->blocks[firstMatchingBlockId];

    }
}
