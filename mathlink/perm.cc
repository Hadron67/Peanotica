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

void BaseChanger::setSGS(PermutationList &genset) {
    auto permLen = genset.getPermutationLength();
    this->genset.setPermutationLength(permLen);
    this->genset.clear();
    this->genset.addAll(genset.begin(), genset.end());
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

    this->stabilizer.clear();
    this->stabilizer.addAll(this->genset.begin(), this->genset.end());
    stabilizerPointsInPlace(this->stabilizer, base.slice(0, pos));
    this->stabilizer2.clear();
    this->stabilizer2.addAll(this->stabilizer.begin(), this->stabilizer.end());
    stabilizerInPlace(this->stabilizer2, bp1);

    this->newGens.clear();
    this->newGens.addAll(this->stabilizer2.begin(), this->stabilizer2.end());
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
    if (this->verbose) {
        std::cout << "calling F1 with ret = " << PointSet{ret, this->permLen}
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
    if (this->verbose) {
        std::cout << "sgd = " << this->permFormatter.formatValue(sgd)
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
    if (this->verbose) {
        std::cout << "ret = " << PointSet{ret, this->permLen} << ", end of F1" << std::endl;
    }
#endif
}

void DoubleCosetRepresentativeSolver::stabilizeOnePoint(upoint_type b, upoint_type p) {
    stabilizerInPlace(this->gensetS, b);
#ifdef PPERM_DEBUG
    if (this->verbose) {
        std::cout << "S_" << b << " = " << this->permFormatter.formatRef(this->gensetS) << std::endl;
    }
#endif
    this->baseChanger.setSGS(this->gensetD);
    auto base = makeSlice(this->gensetDBase.data(), this->gensetDBase.size());
    auto pos = base.indexOf(p);
    this->baseChanger.moveToFirst(base, pos, this->permStack);
    this->gensetDBase.erase(this->gensetDBase.begin());
    this->gensetD.clear();
    this->gensetD.addAll(this->baseChanger.genset.begin(), this->baseChanger.genset.end());
#ifdef PPERM_DEBUG
    if (this->verbose) {
        std::cout << "after base change, D = " << this->permFormatter.formatRef(this->gensetD) << std::endl;
    }
#endif
    stabilizerInPlace(this->gensetD, p);
#ifdef PPERM_DEBUG
    if (this->verbose) {
        std::cout << "D_" << p << " = " << this->permFormatter.formatRef(this->gensetD) << std::endl;
    }
#endif
}

std::optional<StackedPermutation> DoubleCosetRepresentativeSolver::solve(PermutationList &gensetS, PermutationList &gensetD, PermutationView perm) {
#ifdef PPERM_DEBUG
    if (this->verbose) {
        std::cout << "Begin double coset representative algorithm on g = " << this->permFormatter.formatValue(perm)
            << ", S = " << this->permFormatter.formatRef(gensetS)
            << ", D = " << this->permFormatter.formatRef(gensetD)
            << std::endl;
    }
#endif
    this->setPermutationLength(gensetS.getPermutationLength());
    this->gensetS.addAll(gensetS.begin(), gensetS.end());
    this->gensetD.addAll(gensetD.begin(), gensetD.end());
    this->boolSetPool.blockSize = this->permLen * 16;
    this->permStack.setBlockSize(this->permLen * 16);

    auto ret = this->permStack.pushStacked(this->permLen);

    this->gensetDBase.clear();
    for (upoint_type p = 0; p < this->permLen; p++) {
        this->gensetDBase.push_back(p);
    }

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
    for (upoint_type i = 0; i < this->permLen; i++) {
#ifdef PPERM_DEBUG
        if (this->verbose) {
            std::cout << "iteration i = " << i
                << ", S = " << this->permFormatter.formatRef(this->gensetS)
                << ", D = " << this->permFormatter.formatRef(this->gensetD)
                << ", base(D) = {" << makeSlice(this->gensetDBase.data(), this->gensetDBase.size()) << "}"
                << std::endl;
        }
#endif
        auto &currentAlphaTab = this->alphas[this->alphaPtr];
        auto &nextAlphaTab = this->alphas[(this->alphaPtr + 1) & 1];

        this->orbitS.reset(this->permLen);
        for (upoint_type j = i; j < this->permLen; j++) {
            this->orbitS.appendOrbit(j, this->gensetS, this->queue);
        }
#ifdef PPERM_DEBUG
        if (this->verbose) {
            std::cout << "Delta_S = ";
            this->orbitS.dump(std::cout, this->gensetS, this->permFormatter);
            std::cout << std::endl;
        }
#endif
        arraySet(orbitB.begin(), this->permLen, false);
        this->orbitS.collectOneOrbit(i, [&](upoint_type p){ orbitB[p] = true; });
#ifdef PPERM_DEBUG
        if (this->verbose) {
            std::cout << "Delta_b = " << PointSet{orbitB.begin(), this->permLen} << std::endl;
        }
#endif

        this->orbitD.reset(this->permLen);
        this->orbitD.appendOrbits(this->gensetDBase.begin(), this->gensetDBase.end(), this->gensetD, this->queue);
#ifdef PPERM_DEBUG
        if (this->verbose) {
            std::cout << "Delta_D = ";
            this->orbitD.dump(std::cout, this->gensetD, this->permFormatter);
            std::cout << std::endl;
        }
#endif

        arraySet(images.begin(), this->permLen, false);
        for (auto entry : currentAlphaTab) {
            this->subroutineF1(images.begin(), orbitB.begin(), entry, perm);
        }
#ifdef PPERM_DEBUG
        if (this->verbose) {
            std::cout << "IMAGES = " << PointSet{images.begin(), this->permLen} << std::endl;
        }
#endif

        auto pi = findFirstPoint(images.begin(), this->permLen);
#ifdef PPERM_DEBUG
        if (this->verbose) {
            std::cout << "pi = " << pi << std::endl;
        }
#endif
        this->orbitD.reset(this->permLen);
        this->orbitD.appendOrbit(pi, this->gensetD, this->queue);
#ifdef PPERM_DEBUG
        if (this->verbose) {
            std::cout << "Delta_D = ";
            this->orbitD.dump(std::cout, this->gensetD, this->permFormatter);
            std::cout << std::endl;
        }
#endif

        arraySet(orbitPi.begin(), this->permLen, false);
        computeOrbit(orbitPi.begin(), pi, this->gensetD, this->queue);
#ifdef PPERM_DEBUG
        if (this->verbose) {
            std::cout << "Delta_p = " << PointSet{orbitPi.begin(), this->permLen} << std::endl;
        }
#endif

        nextAlphaTab.clear();
        for (auto entry : currentAlphaTab) {
#ifdef PPERM_DEBUG
            if (this->verbose) {
                std::cout << "working on entry " << this->permFormatter.formatRef(entry) << std::endl;
            }
#endif
            auto s = entry.getS(), d = entry.getD();
            auto next = this->boolSetPool.pushStacked(this->permLen);
            arraySet(next.begin(), this->permLen, false);
            mapPointSet(next.begin(), orbitB.begin(), s);
#ifdef PPERM_DEBUG
            if (this->verbose) {
                std::cout << "Delta_b^s = " << PointSet{next.begin(), this->permLen} << std::endl;
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
            if (this->verbose) {
                std::cout << "Delta_p^((gd)^-1) = " << PointSet{orbitPigdInv.begin(), this->permLen} << std::endl;
                std::cout << "NEXT = " << PointSet{next.begin(), this->permLen} << std::endl;
            }
#endif

            for (upoint_type j = 0; j < this->permLen; j++) if (next[j]) {
                auto s1 = traceSchreierVector(this->permStack, s.inverseMapPoint(j), this->gensetS, this->orbitS.vector.get());
                s1.multiply(s1, s);
                auto d2 = this->permStack.pushStacked(this->permLen);
                d2.inverse(traceSchreierVector(this->permStack, gd.mapPoint(j), this->gensetD, this->orbitD.vector.get()));
                auto d1 = this->permStack.pushStacked(this->permLen);
                d1.multiply(d, d2);

#ifdef PPERM_DEBUG
                if (this->verbose) {
                    std::cout << "for j = " << j
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
        if (this->verbose) {
            std::cout << "sgd = " << this->permFormatter.formatRef(this->sgdSet) << std::endl;
        }
#endif
        for (auto elem : this->sgdSet) {
            auto elem2 = this->permStack.pushStacked(this->permLen);
            elem2.copy(elem);
            elem2.setNegative(!elem2.isNegative());
            if (this->sgdSet.findPermutation(elem2).isPresent()) {
#ifdef PPERM_DEBUG
                if (this->verbose) {
                    std::cout << "found permutations with opposite signs "
                        << this->permFormatter.formatValue(elem)
                        << " and " << this->permFormatter.formatValue(elem2)
                        << " in sgd"
                        << std::endl;
                }
#endif
                return std::nullopt;
            }
        }

        this->stabilizeOnePoint(i, pi);
#ifdef PPERM_DEBUG
        if (this->verbose) {
            std::cout << "TAB = {" << std::endl;
            for (auto entry : this->alphas[this->alphaPtr]) {
                std::cout << "    " << this->permFormatter.formatRef(entry) << std::endl;
            }
            std::cout << "}" << std::endl;
            std::cout << "iteration i = " << i << " done" << std::endl;
        }
#endif
    }
    auto firstEntry = this->alphas[this->alphaPtr].get(0);
    ret.multiply(firstEntry.getS(), perm);
    ret.multiply(ret, firstEntry.getD());
    return ret;
}
