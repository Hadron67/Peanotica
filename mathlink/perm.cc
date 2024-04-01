#include <algorithm>
#include <chrono>
#include "perm.h"

using namespace pperm;

int PermutationView::compare(const PermutationView &other, bool ignoreSign) const {
    if (!ignoreSign && this->isNegative() != other.isNegative()) {
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

std::size_t PermutationView::hash(bool ignoreSign) const {
    // TODO: use a better hasher
    std::size_t ret = 5381;
    auto ptr = this->data, end = ptr + this->getStorageSize();
    if (ignoreSign) ptr++;
    while (ptr != end) {
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
        bool ignoreSign;
        std::size_t hash(const PermutationView &perm) const {
            return perm.hash(this->ignoreSign);
        }
        std::size_t hash(std::size_t i) const {
            return this->genset->get(i).hash(this->ignoreSign);
        }
        int compare(std::size_t i, std::size_t j) const {
            return this->genset->get(i).compare(this->genset->get(j), this->ignoreSign);
        }
        int compare(const PermutationView &perm, std::size_t i) const {
            return perm.compare(this->genset->get(i), this->ignoreSign);
        }
    };
}

bool PermutationSet::addPermutation(const PermutationView &perm) {
    return this->permToId.computeIfAbsent(perm, PermutationHashContext{this, this->ignoreSign}, [this](PermutationView perm){
        auto id = this->getSize();
        this->permutations.push().copy(perm);
        return id;
    }).second;
}

bool PermutationSet::checkOppositeSignAndAddPermutation(PermutationView perm) {
    if (!this->ignoreSign) {
        return false;
    }
    auto ret = this->permToId.computeIfAbsent(perm, PermutationHashContext{this, true}, [this](PermutationView perm) {
        auto id = this->getSize();
        this->permutations.push().copy(perm);
        return id;
    });
    if (!ret.second) {
        return this->permutations.get(this->permToId.getEntry(ret.first)->value).isNegative() != perm.isNegative();
    }
    return false;
}

OptionalUInt<std::size_t> PermutationSet::findPermutation(const PermutationView &perm) {
    auto pt = this->permToId.find(perm, PermutationHashContext{this, this->ignoreSign});
    if (pt.isNonNull()) {
        return this->permToId.getEntry(pt)->getValue();
    } else {
        return None{};
    }
}

void PermutationSet::removeAndFetchLast(std::size_t index) {
    auto size = this->getSize();
    this->permToId.remove(this->permToId.find(index, PermutationHashContext{this, this->ignoreSign}));
    if (index + 1 < size) {
        auto last = this->permToId.find(size - 1, PermutationHashContext{this, this->ignoreSign});
        this->permToId.getEntry(last)->value = index;
    }
    this->permutations.removeAndFetchLast(index);
}

void PermutationSet::updateIndex() {
    this->permToId.clear();
    for (std::size_t i = 0; i < this->permutations.getSize(); i++) {
        this->permToId.putIfAbsent(i, PermutationHashContext{this, this->ignoreSign}, i);
    }
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
    PermutationFormatter formatter;
    formatter.useCycles = true;
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
        std::cout << "j = " << j << ", l = " << l << std::endl;
        while (j + 1 < this->permLen && this->hasPath(queue, j, l)) {
            tmp2.inverse(this->getPermutation(this->getVertex(l).get()));
            tmp.multiply(tmp, tmp2);
            tmp.multiply(tmp, this->getPermutation(this->getVertex(j).get()));
            std::cout << "path exists, new g = " << formatter.formatValue(tmp) << std::endl;
            j = tmp.firstNonFixedPoint();
            l = tmp.mapPoint(j);
            std::cout << "new j = " << j << ", l = " << l << std::endl;
        }
        if (j + 1 == this->permLen) {
            std::cout << "returning" << std::endl;
            return;
        }
        auto m0 = findEdge1(j, l);
        while (m0.isPresent()) {
            auto m = m0.get();
            std::cout << "edge ml with m > j exists, m = " << m << std::endl;
            tmp2.inverse(this->getPermutation(this->getEdge(m, l).get()));
            tmp.multiply(tmp, tmp2);
            l = m;
            m0 = findEdge1(j, l);
            std::cout << "new g = " << formatter.formatValue(tmp) << std::endl;
        }
        m0 = findEdge1(0, l);
        if (m0.isPresent()) {
            std::cout << "edge ml exists" << std::endl;
            StackedPermutation tmp3(stack, this->permLen);
            auto m = m0.get();
            tmp3.copy(this->getPermutation(this->getEdge(m, l).get()));
            this->setEdge(m, l, None{});
            this->setEdgePermutation(j, l, tmp);
            tmp2.inverse(tmp);
            tmp3.multiply(tmp3, tmp2);
            tmp.copy(tmp3);
            std::cout << "new g = " << formatter.formatValue(tmp) << std::endl;
            this->recalculateVertices(queue);
        } else {
            std::cout << "edge ml does not exist" << std::endl;
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

void JerrumBranching2::recalculateVertices(std::deque<upoint_type> &queue) {
    for (upoint_type p = 0; p < this->permLen; p++) {
        auto entry = this->get(p);
        if (!entry.getParent().isPresent()) {
            queue.clear();
            queue.push_back(p);
            entry.setRoot(p);
            entry.getVertexLabel().identity();
            while (!queue.empty()) {
                auto p2 = queue[0];
                queue.pop_front();
                auto entry2 = this->get(p2);
                for (upoint_type p3 = 0; p3 < this->permLen; p3++) {
                    auto entry3 = this->get(p3);
                    if (entry3.getParent() == p2) {
                        entry3.getVertexLabel().multiply(entry2.getVertexLabel(), entry3.getEdgeLabel());
                        entry3.setRoot(entry2.getRoot());
                        queue.push_back(p3);
                    }
                }
            }
        }
    }
}

bool JerrumBranching2::hasPath(upoint_type p1, upoint_type p2) {
    auto parent = this->get(p2).getParent();
    while (parent.isPresent()) {
        auto p3 = parent.get();
        if (p3 == p1) {
            return true;
        }
        parent = this->get(p3).getParent();
    }
    return false;
}

void JerrumBranching2::dump(std::ostream &os, PermutationFormatter &formatter, std::deque<upoint_type> &queue) {
    os << "JerrumBraching {" << std::endl;
    for (upoint_type p = 0; p < this->permLen; p++) {
        auto entry = this->get(p);
        if (!entry.getParent().isPresent()) {
            queue.clear();
            queue.push_back(p);
            while (!queue.empty()) {
                auto p2 = queue[0];
                queue.pop_front();
                for (upoint_type p3 = 0; p3 < this->permLen; p3++) {
                    auto entry3 = this->get(p3);
                    if (p2 == entry3.getParent()) {
                        os << "    " << p2 << " -- " << formatter.formatValue(entry3.getEdgeLabel()) << " --> " << p3 << std::endl;
                        queue.push_back(p3);
                    }
                }
            }
        }
    }
    for (upoint_type p = 0; p < this->permLen; p++) {
        os << "    v(" << p << ") = " << formatter.formatValue(this->get(p).getVertexLabel()) << std::endl;
    }
    for (upoint_type p = 0; p < this->permLen; p++) {
        std::cout << "    root(" << p << ") = " << this->get(p).getRoot() << std::endl;
    }
    os << "}" << std::endl;
}

void JerrumBranching2::reset() {
    for (upoint_type p = 0; p < this->permLen; p++) {
        auto entry = this->get(p);
        entry.setParent(None{});
        entry.setRoot(p);
    }
}

void JerrumBranching2::siftElement(PermutationStack &stack, std::deque<upoint_type> &queue, PermutationView perm, SiftLogger log) {
    StackedPermutation tmp(stack, this->permLen), tmp2(stack, this->permLen);
    tmp.copy(perm);
    while (1) {
        auto j = tmp.firstNonFixedPoint();
        auto l = tmp.mapPoint(j);
#ifdef PPERM_DEBUG
        if (log.os) {
            *log.os << "j = " << j << ", l = " << l << std::endl;
        }
#endif
        while (j + 1 < this->permLen && this->hasPath(j, l)) {
            tmp2.inverse(this->get(l).getVertexLabel());
            tmp.multiply(tmp, tmp2);
            tmp.multiply(tmp, this->get(j).getVertexLabel());
#ifdef PPERM_DEBUG
            if (log.os) {
                *log.os << "path exists, new g = " << log.formatter->formatValue(tmp) << std::endl;
            }
#endif
            j = tmp.firstNonFixedPoint();
            l = tmp.mapPoint(j);
#ifdef PPERM_DEBUG
            if (log.os) {
                *log.os << "new j = " << j << ", l = " << l << std::endl;
            }
#endif
        }
        if (j + 1 == this->permLen) {
#ifdef PPERM_DEBUG
            if (log.os) {
                *log.os << "returning" << std::endl;
            }
#endif
            return;
        }
        auto m0 = this->get(l).getParent();
        while (m0.isPresent() && m0.get() > j) {
            auto m = m0.get();
#ifdef PPERM_DEBUG
            if (log.os) {
                *log.os << "edge ml with m > j exists, m = " << m << std::endl;
            }
#endif
            tmp2.inverse(this->get(l).getEdgeLabel());
            tmp.multiply(tmp, tmp2);
#ifdef PPERM_DEBUG
            if (log.os) {
                *log.os << "new g = " << log.formatter->formatValue(tmp) << std::endl;
            }
#endif
            l = m;
            m0 = this->get(l).getParent();
        }
        auto entry = this->get(l);
        if (entry.getParent().isPresent()) {
#ifdef PPERM_DEBUG
            if (log.os) {
                *log.os << "edge ml exists" << std::endl;
            }
#endif
            StackedPermutation tmp3(stack, this->permLen);
            tmp3.copy(entry.getEdgeLabel());
            entry.setParent(j);
            entry.getEdgeLabel().copy(tmp);
            tmp2.inverse(tmp);
            tmp3.multiply(tmp3, tmp2);
            tmp.copy(tmp3);
#ifdef PPERM_DEBUG
            if (log.os) {
                *log.os << "new g = " << log.formatter->formatValue(tmp) << std::endl;
            }
#endif
            this->recalculateVertices(queue);
        } else {
#ifdef PPERM_DEBUG
            if (log.os) {
                *log.os << "edge ml does not exist" << std::endl;
            }
#endif
            entry.setParent(j);
            entry.getEdgeLabel().copy(tmp);
            this->recalculateVertices(queue);
            return;
        }
    }
}

void JerrumBranchingBuilder::augment(upoint_type i) {
    auto permLen = this->currentGens.getPermutationLength();
    for (upoint_type p = 0; p < permLen; p++) if(p != i && this->orbit.pointToOrbitId[p].isPresent()) {
        auto entry = this->branching.get(p);
        auto u1 = traceSchreierVector(*this->permStack, p, this->currentGens, this->orbit.vector.get());
        entry.setParent(i);
        entry.getEdgeLabel().copy(u1);
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
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "========= iteration p = " << p << std::endl;
        }
#endif
        this->orbit.reset(this->currentGens.getPermutationLength());
        this->orbit.appendOrbit(p, this->currentGens, this->queue);
        this->augment(p);
        this->schreierGens.clear();
        schreierGenerators(this->schreierGens, *this->permStack, this->currentGens, this->orbit);
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "schreier generators = " << this->formatter.formatRef(this->schreierGens) << std::endl;
        }
#endif
        this->siftingBranching.reset();
        for (auto perm : this->schreierGens) {
#ifdef PPERM_DEBUG
            if (this->log) {
                *this->log << "sifting element " << this->formatter.formatValue(perm) << std::endl;
            }
#endif
            this->siftingBranching.siftElement(*this->permStack, this->queue, perm, JerrumBranching2::SiftLogger(this->log, this->formatter));
#ifdef PPERM_DEBUG
            if (this->log) {
                std::deque<upoint_type> queue;
                *this->log << "the branching is: ";
                this->siftingBranching.dump(*this->log, this->formatter, queue);
                *this->log << std::endl;
            }
#endif
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

void BaseChanger::interchange(Slice<upoint_type> partialBase, upoint_type b1, upoint_type b2, PermutationStack &stack) {
    auto permLen = this->genset.getPermutationLength();
    this->orbitSets.ensureSize(permLen * 3);
    auto gamma = this->orbitSets.get(), deltai = gamma + permLen, tmpPointSet = deltai + permLen;
    arraySet(gamma, permLen * 3, false);

    this->stabilizer.copy(this->genset);
    stabilizerPointsInPlace(this->stabilizer, partialBase);
    this->stabilizer2.copy(this->stabilizer);
    stabilizerInPlace(this->stabilizer2, b1);

    this->newGens.copy(this->stabilizer2);
    stabilizerInPlace(this->newGens, b2);
    this->orbit1.reset(permLen);
    this->orbit2.reset(permLen);
    this->orbit1.appendOrbit(b1, this->stabilizer, this->queue);
    this->orbit2.appendOrbit(b2, this->stabilizer2, this->queue);

    computeOrbit(tmpPointSet, b2, this->stabilizer, this->queue);
    auto barDeltaj1Size = this->orbit1.allOrbitSize() *  this->orbit2.allOrbitSize() / std::count(tmpPointSet, tmpPointSet + permLen, true);

    deltai[b1] = true;
    this->orbit1.collectAllOrbit([gamma](upoint_type p, upoint_type orbit){ gamma[p] = true; });
    gamma[b1] = false;
    gamma[b2] = false;

    while (std::count(deltai, deltai + permLen, true) < barDeltaj1Size) {
        auto gamma0 = findFirstPoint(gamma, permLen);
        auto g1 = traceSchreierVector(stack, gamma0, this->stabilizer, this->orbit1.vector.get());
        auto g1InvMapBp2 = g1.inverseMapPoint(b2);
        if (this->orbit2.pointToOrbitId[g1InvMapBp2].isPresent()) {
            auto g2 = traceSchreierVector(stack, g1InvMapBp2, this->stabilizer2, this->orbit2.vector.get());
            g2.multiply(g2, g1);
            this->newGens.addPermutation(g2);
            computeOrbit(deltai, b1, this->newGens.permutations, this->queue);
            boolListComplement(gamma, deltai, permLen);
        } else {
            computeOrbit(tmpPointSet, gamma0, this->newGens.permutations, this->queue);
            boolListComplement(gamma, tmpPointSet, permLen);
        }
    }
    this->genset.addAll(this->newGens.begin(), this->newGens.end());
}

void BaseChanger::completeBaseChange(MutableSlice<upoint_type> base, Slice<upoint_type> newBase, PermutationStack &stack) {
    auto permLen = this->genset.getPermutationLength();
    std::size_t i = 0;
    auto conjPerm = stack.pushStacked(permLen);
    conjPerm.identity();
    auto permInv = stack.pushStacked(permLen);
    permInv.inverse(conjPerm);

    this->stabilizer.copy(this->genset);
    this->orbit1.reset(permLen);
    base.begin();
    this->orbit1.appendOrbit(base[0], this->stabilizer, this->queue);
    if (this->orbit1.pointToOrbitId[newBase[0]].isPresent()) {
        auto perm2 = stack.pushStacked(permLen);
        while (1) {
            auto t = traceSchreierVector(stack, permInv.mapPoint(newBase[i]), this->stabilizer, this->orbit1.vector.get());
            perm2.multiply(t, conjPerm);
            conjPerm.copy(perm2);
            permInv.inverse(conjPerm);
            i++;
            if (i >= base.getLength() || i >= newBase.len) {
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
        for (upoint_type p = 0; p < base.getLength(); p++) {
            base[p] = conjPerm.mapPoint(base[p]);
        }
        for (auto gen : this->genset) {
            tmp.multiply(permInv, gen);
            tmp.multiply(tmp, conjPerm);
            gen.copy(tmp);
        }
        this->genset.updateIndex();
    }
    for (std::size_t j = i; j < newBase.len; j++) {
        auto pos = base.indexOf(newBase[j]);
        if (pos == base.getLength()) {
            base.append(newBase[j]);
        }
        this->moveToFirstDirectly(base, pos, stack);
    }
}

void BaseChanger::makeFirstPoint(MutableSlice<upoint_type> base, upoint_type point, PermutationStack &stack) {
    auto permLen = this->genset.getPermutationLength();
    std::size_t pos = 0;
    auto baseLen = base.getLength();
    this->orbit1.reset(permLen);
    this->orbit1.appendOrbit(point, this->genset.permutations, this->queue);
    while (pos < baseLen) {
        if (this->orbit1.pointToOrbitId[base[pos]].isPresent()) {
            auto gInv = traceSchreierVector(stack, base[pos], this->genset.permutations, this->orbit1.vector.get());
            auto g = stack.pushStacked(permLen);
            g.inverse(gInv);
            for (std::size_t i = 0; i < baseLen; i++) {
                base[i] = g.mapPoint(base[i]);
            }
            auto tmp = stack.pushStacked(permLen);
            for (auto gen : this->genset) {
                tmp.multiply(gInv, gen);
                tmp.multiply(tmp, g);
            }
            this->genset.updateIndex();
            break;
        }
        pos++;
    }
    if (pos == baseLen) {
        base.append(point);
    }
    this->moveToFirstDirectly(base, pos, stack);
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

void BaseChangingStrongGenSetProvider::setSGS(PermutationList &genset) {
    auto permLen = genset.getPermutationLength();
    base.ensureSize(permLen);
    this->baseLen = 0;
    this->stablePoints.ensureSize(permLen);
    arraySet(this->stablePoints.get(), permLen, false);
    MutableSlice baseSlice(this->base.get(), this->baseLen);
    filterBasePoints([&](upoint_type p) { baseSlice.append(p); }, [](std::size_t i) { return i; }, permLen, genset, this->genset);
    this->genset.copy(genset);
    this->updateStablePoints();
}

void BaseChangingStrongGenSetProvider::stabilizeOnePoint(PermutationStack &stack, upoint_type point) {
    auto stablePoints = this->stablePoints.get();
    if (!stablePoints[point]) {
        this->baseChanger->setSGS(this->genset);
        MutableSlice base(this->base.get(), this->baseLen);
        this->baseChanger->makeFirstPoint(base, point, stack);
        if (base.getLength() > 0) {
            assert(base[0] == point);
            base.shift();
            this->genset.copy(this->baseChanger->genset.permutations);
            stabilizerInPlace(this->genset, point);
            stablePoints[point] = true;
            this->updateStablePoints();
        }
    }
}
PermutationList &BaseChangingStrongGenSetProvider::getStrongGenSet() {
    return this->genset;
}

void BaseChangingStrongGenSetProvider::print(std::ostream &os, PermutationFormatter &formatter) {
    auto permLen = this->genset.getPermutationLength();
    os << "BaseChangingStrongGenSetProvider[Base -> {" << Slice(this->base.get(), this->baseLen)
        << "}, GenSet -> " << formatter.formatRef(this->genset)
        << ", StablePoints -> " << PointSet{this->stablePoints.get(), permLen}
        << "]";
}

void BaseChangingStrongGenSetProvider::updateStablePoints() {
    auto permLen = this->genset.getPermutationLength();
    auto points = this->stablePoints.get();
    for (upoint_type p = 0; p < permLen; p++) if (!points[p]) {
        bool stable = true;
        for (auto perm : this->genset) {
            if (perm.mapPoint(p) != p) {
                stable = false;
                break;
            }
        }
        points[p] = stable;
    }
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

void DoubleCosetRepresentativeSolver::subroutineF1(const bool *orbitB, PermutationView sgd) {
#ifdef PPERM_DEBUG
    if (this->log) {
        *this->log << "calling F1 with Delta_b = " << PointSet{orbitB, this->permLen}
            << ", sgd = " << this->permFormatter.formatValue(sgd)
            << std::endl;
    }
#endif
    auto orbitBsgd = this->boolSetPool.pushStacked(this->permLen);
    arraySet(orbitBsgd.begin(), this->permLen, false);
    mapPointSet(orbitBsgd.begin(), orbitB, sgd);
#ifdef PPERM_DEBUG
    if (this->log) {
        *this->log << "Delta_b^(sgd) = " << PointSet{orbitBsgd.begin(), this->permLen} << std::endl;
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
            if (p == this->minP) {
#ifdef PPERM_DEBUG
                if (this->log) {
                    *this->log << "minimal point " << p << " matches previous one" << std::endl;
                }
#endif
                this->selectedSgD.addPermutation(sgd);
            } else if (p < this->minP) {
#ifdef PPERM_DEBUG
                if (this->log) {
                    *this->log << "new minimal point " << p << std::endl;
                }
#endif
                this->minP = p;
                this->selectedSgD.clear();
                this->selectedSgD.addPermutation(sgd);
            }
#ifdef PPERM_DEBUG
            else if (this->log) {
                *this->log << "ignoring non-minimal point " << p << std::endl;
            }
#endif
            break;
        }
    }
}

std::optional<StackedPermutation> DoubleCosetRepresentativeSolver::solve(StrongGenSetProvider &gensetSProvider, StrongGenSetProvider &gensetDProvider, PermutationView perm) {
#ifdef PPERM_DEBUG
    if (this->log) {
        *this->log << "============== Begin double coset representative algorithm ==========" << std::endl
            << "g = " << perm << std::endl
            << "S = " << this->permFormatter.formatRef(gensetSProvider) << std::endl
            << "D = " << this->permFormatter.formatRef(gensetDProvider) << std::endl;
    }
#endif
    auto &gensetD = gensetDProvider.getStrongGenSet();
    this->setPermutationLength(gensetD.getPermutationLength());
    this->baseChangeOfDTime = 0;
    this->baseChangeOfSTime = 0;

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

    auto finishedPoints = this->boolSetPool.pushStacked(this->permLen);
    arraySet(finishedPoints.begin(), this->permLen, false);
    if (this->useTwoStep) {
        auto start = std::chrono::system_clock::now();
        auto perm2 = this->solveRightCosetRepresentative(perm, gensetSProvider, minNonFixedPointOfD, finishedPoints.begin());
        auto middle = std::chrono::system_clock::now();
        auto ret = this->solveDoubleCosetRepresentative(gensetSProvider, gensetDProvider, perm2, finishedPoints.begin());
        auto end = std::chrono::system_clock::now();
        if (this->log) {
            *this->log << "time in right coset rep: " << std::chrono::duration_cast<std::chrono::microseconds>(middle - start).count() << std::endl
                << "time in double coset rep: " << std::chrono::duration_cast<std::chrono::microseconds>(end - middle).count() << std::endl
                << "base change time of S: " << this->baseChangeOfSTime << std::endl
                << "base change time of D: " << this->baseChangeOfDTime << std::endl;
        }
        return ret;
    } else {
        auto start = std::chrono::system_clock::now();
        auto ret = this->solveDoubleCosetRepresentative(gensetSProvider, gensetDProvider, perm, finishedPoints.begin());
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

StackedPermutation DoubleCosetRepresentativeSolver::solveRightCosetRepresentative(PermutationView perm, StrongGenSetProvider &gensetSProvider, upoint_type minNonFixedPointOfD, bool *finishedPoints) {
#ifdef PPERM_DEBUG
    if (this->log) {
        *this->log << "========= begin right coset representative subroutine =======" << std::endl;
        *this->log << "g = " << perm << std::endl;
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
    for (upoint_type i = 0; i < this->permLen && foundFrees < freeCount; i++) {
        auto &gensetS = gensetSProvider.getStrongGenSet();
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "======= iteration i = " << i
                << ", S = " << this->permFormatter.formatRef(gensetS)
                << ", g = " << perm2
                << ", frees = " << PointSet{frees, this->permLen} << std::endl;
        }
#endif
        this->orbitS.reset(this->permLen);
        this->orbitS.appendOrbit(i, gensetS, this->queue);
        arraySet(orbitB.begin(), this->permLen, false);
        this->orbitS.collectOneOrbit(i, [&](upoint_type p){ orbitB[p] = true; });

        copyArray(orbitBInFrees.begin(), orbitB.begin(), this->permLen);
        pointSetIntersection(orbitBInFrees.begin(), frees, this->permLen);

#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "Delta_S = ";
            this->orbitS.dump(*this->log, gensetS, this->permFormatter);
            *this->log << std::endl;
            *this->log << "Delta_" << i << " = " << PointSet{orbitB.begin(), this->permLen} << std::endl;
            *this->log << "Delta_" << i << " int. frees = " << PointSet{orbitBInFrees.begin(), this->permLen} << std::endl;
        }
#endif
        if (std::count(orbitBInFrees.begin(), orbitBInFrees.end(), true) == 0) {
            continue;
        }
        foundFrees++;
        finishedPoints[i] = true;

        arraySet(orbitBInFreesPerm.begin(), this->permLen, false);
        mapPointSet(orbitBInFreesPerm.begin(), orbitBInFrees.begin(), perm2);
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "(Delta_" << i << " int. frees)^g = " << PointSet{orbitBInFreesPerm.begin(), this->permLen} << std::endl;
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
            gensetSProvider.stabilizeOnePoint(this->permStack, i);
        }).count();
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "S_" << i << " = " << this->permFormatter.formatRef(gensetSProvider) << std::endl;
        }
#endif
    }
#ifdef PPERM_DEBUG
    if (this->log) {
        *this->log << "end of right coset representative, g = " << perm2 << std::endl;
    }
#endif
    return perm2;
}

std::optional<StackedPermutation> DoubleCosetRepresentativeSolver::solveDoubleCosetRepresentative(StrongGenSetProvider &gensetSProvider, StrongGenSetProvider &gensetDProvider, PermutationView perm, const bool *finishedPoints) {
#ifdef PPERM_DEBUG
    if (this->log) {
        *this->log << "========= begin double coset representative subroutine =======" << std::endl;
        *this->log << "S = " << this->permFormatter.formatRef(gensetSProvider) << std::endl
            << "D = " << this->permFormatter.formatRef(gensetDProvider) << std::endl
            << "g = " << perm << std::endl
            << "finishedPoints = " << PointSet{finishedPoints, this->permLen} << std::endl;
    }
#endif
    auto ret = this->permStack.pushStacked(this->permLen);

    this->sgdSet.clear();
    this->sgdSet.setIgnoreSign(true);
    this->sgdSet.addPermutation(perm);

    auto orbitB = this->boolSetPool.pushStacked(this->permLen);
    auto orbitPi = this->boolSetPool.pushStacked(this->permLen);
    for (upoint_type i = 0; i < this->permLen; i++) if (!finishedPoints[i]) {
        auto &gensetS = gensetSProvider.getStrongGenSet();
        auto &gensetD = gensetDProvider.getStrongGenSet();
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "============ iteration i = " << i << std::endl
                << "S = " << this->permFormatter.formatRef(gensetSProvider) << std::endl
                << "D = " << this->permFormatter.formatRef(gensetDProvider) << std::endl
                << "sgd = " << this->permFormatter.formatRef(this->sgdSet) << std::endl
                << "Length[sgd] = " << this->sgdSet.getSize() << std::endl;
        }
#endif

        this->orbitS.reset(this->permLen);
        // In the original algorithm here we should calculate all orbits of S, but it seems that just calculating the orbit of b_i sufficies
        this->orbitS.appendOrbit(i, gensetS, this->queue);
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "vec(b_i^S) = ";
            this->orbitS.dump(*this->log, gensetS, this->permFormatter);
            *this->log << std::endl;
        }
#endif
        arraySet(orbitB.begin(), this->permLen, false);
        this->orbitS.collectOneOrbit(i, [&](upoint_type p){ orbitB[p] = true; });
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "b_i^S = " << PointSet{orbitB.begin(), this->permLen} << std::endl;
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

        this->minP = this->permLen;
        for (auto sgd : this->sgdSet) {
            this->subroutineF1(orbitB.begin(), sgd);
        }
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "selected sgd = " << this->permFormatter.formatRef(this->selectedSgD) << std::endl
                << "Length[sgd] = " << this->selectedSgD.getSize() << std::endl;
        }
#endif

        // Note: the ordering for determining the minimal point is just a convention
        auto pi = this->minP;
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "p_i = " << pi << std::endl;
        }
#endif
        this->orbitD.reset(this->permLen);
        this->orbitD.appendOrbit(pi, gensetD, this->queue);
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "vec(p_i^D) = ";
            this->orbitD.dump(*this->log, gensetD, this->permFormatter);
            *this->log << std::endl;
        }
#endif

        arraySet(orbitPi.begin(), this->permLen, false);
        computeOrbit(orbitPi.begin(), pi, gensetD, this->queue);
#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "p_i^D = " << PointSet{orbitPi.begin(), this->permLen} << std::endl;
        }
#endif

        this->sgdSet.clear();
        for (auto sgd : this->selectedSgD) {
#ifdef PPERM_DEBUG
            if (this->log) {
                *this->log << "working on sgd = " << this->permFormatter.formatValue(sgd) << std::endl;
            }
#endif
            auto sgdInv = this->permStack.pushStacked(this->permLen);
            sgdInv.inverse(sgd);
            auto next = this->boolSetPool.pushStacked(this->permLen);
            arraySet(next.begin(), this->permLen, false);
            mapPointSet(next.begin(), orbitB.begin(), sgd);
#ifdef PPERM_DEBUG
            if (this->log) {
                *this->log << "(b_i^S)^(sgd) = " << PointSet{next.begin(), this->permLen} << std::endl;
            }
#endif
            pointSetIntersection(next.begin(), orbitPi.begin(), this->permLen);
#ifdef PPERM_DEBUG
            if (this->log) {
                *this->log << "Intersection[(b_i^S)^(sgd), p_i^D] = " << PointSet{next.begin(), this->permLen} << std::endl;
            }
#endif

            for (upoint_type j = 0; j < this->permLen; j++) if (next[j]) {
                auto s1 = traceSchreierVector(this->permStack, sgdInv.mapPoint(j), gensetS, this->orbitS.vector.get());
                auto d2 = traceSchreierVector(this->permStack, j, gensetD, this->orbitD.vector.get());
                // b_i^(s1 sgd) = p_i^(d2)
                auto d1 = this->permStack.pushStacked(this->permLen);
                d1.inverse(d2);
                auto newsgd = this->permStack.pushStacked(this->permLen);
                newsgd.multiply(s1, sgd);
                newsgd.multiply(newsgd, d1);

#ifdef PPERM_DEBUG
                if (this->log) {
                    *this->log << "for j = " << j
                        << ", s = " << this->permFormatter.formatValue(s1)
                        << ", d = " << this->permFormatter.formatValue(d1)
                        << ", new sgd = " << newsgd << std::endl;
                }
#endif
                if (this->sgdSet.checkOppositeSignAndAddPermutation(newsgd)) {
#ifdef PPERM_DEBUG
                    if (this->log) {
                        *this->log << "found permutations with opposite signs of " << this->permFormatter.formatValue(newsgd) << std::endl;
                    }
#endif
                    return std::nullopt;
                }
            }
        }

        this->baseChangeOfDTime += measureElapsed([&]() {
            gensetSProvider.stabilizeOnePoint(this->permStack, i);
            gensetDProvider.stabilizeOnePoint(this->permStack, pi);
        }).count();

#ifdef PPERM_DEBUG
        if (this->log) {
            *this->log << "S_" << i << " = " << this->permFormatter.formatRef(gensetSProvider) << std::endl;
            *this->log << "D_" << i << " = " << this->permFormatter.formatRef(gensetDProvider) << std::endl;
            *this->log << "new sgd = " << this->sgdSet << std::endl;
            *this->log << "Length[sgd] = " << this->sgdSet.getSize() << std::endl;
            *this->log << "iteration i = " << i << " done" << std::endl;
        }
#endif
    }
    ret.copy(this->sgdSet.get(0));
#ifdef PPERM_DEBUG
    if (this->log) {
        *this->log << "end of double coset representative, result ret = " << ret << std::endl;
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

void GroupEnumerator::generate() {
    auto permLen = this->generators.getPermutationLength();
    this->elements.clear();
    this->elements.setPermutationLength(permLen);
    {
        auto g = this->permStack->pushStacked(permLen).identity();
        this->elements.addPermutation(g);
        auto first = this->generators.get(0);
        g.copy(first);
        while (!g.isIdentity()) {
            this->elements.addPermutation(g);
            g.multiply(g, first);
        }
    }
    auto tmp = this->permStack->pushStacked(permLen);
    auto elt = this->permStack->pushStacked(permLen);
    for (std::size_t i = 1; i < this->generators.getSize(); i++) {
        auto curGenerator = this->generators.get(i);
        if (this->elements.addPermutation(curGenerator)) {
            auto cosetSize = this->elements.getSize() - 1;
            auto cosetRepPos = cosetSize;
            for (std::size_t j = 1; j < cosetRepPos; j++) {
                tmp.multiply(this->elements.get(i), curGenerator);
                this->elements.addPermutation(tmp);
            }
            while (cosetRepPos < this->elements.getSize()) {
                for (std::size_t j = 0; j <= i; j++) {
                    elt.multiply(this->elements.get(cosetRepPos), this->generators.get(j));
                    if (this->elements.addPermutation(elt)) {
                        for (std::size_t k = 1; k < cosetSize; k++) {
                            tmp.multiply(this->elements.get(k), elt);
                            this->elements.addPermutation(tmp);
                        }
                    }
                }
                cosetRepPos += cosetSize;
            }
        }
    }
}