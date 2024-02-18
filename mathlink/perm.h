#pragma once

#include <assert.h>
#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <unordered_set>
#include <deque>
#include <functional>
#include "util.h"
#include <optional>

namespace pperm {

typedef std::uint32_t upoint_type;

struct SignedPoint {
    bool isNegative;
    upoint_type point;

    bool operator == (SignedPoint other) const {
        return this->isNegative == other.isNegative && this->point == other.point;
    }
};

struct OptionalSignedPoint {
    OptionalSignedPoint(): tag(2), point(0) {}
    OptionalSignedPoint(None n): OptionalSignedPoint() {}
    OptionalSignedPoint(SignedPoint p): tag(p.isNegative), point(p.point) {}
    bool isPresent() const {
        return this->tag != 2;
    }
    SignedPoint get() const {
        return SignedPoint{this->tag == 1, this->point};
    }
    private:
    std::uint8_t tag;
    upoint_type point;
};

struct PermutationView {
    upoint_type *data;
    std::size_t len;

    bool isNegative() const {
        return bool(this->data[0]);
    }
    void setNegative(bool isNegative) {
        this->data[0] = upoint_type(isNegative);
    }
    upoint_type *images() const {
        return this->data + 1;
    }
    std::size_t getLength() const {
        return this->len;
    }
    template<typename... T>
    PermutationView &cycle(T... args) {
        upoint_type arr[sizeof...(args)]{upoint_type(args)...};
        auto images = this->images();
        for (unsigned int i = 0; i < sizeof...(args); i++) {
            images[arr[i]] = arr[(i + 1) % sizeof...(args)];
        }
        return *this;
    }
    PermutationView &assign(bool isNegative, std::initializer_list<upoint_type> list);
    PermutationView &identity();
    PermutationView &multiply(const PermutationView &p1, const PermutationView &p2);
    PermutationView &inverse(const PermutationView &perm);
    PermutationView &copy(const PermutationView &perm) {
        copyArray(this->data, perm.data, this->getStorageSize());
        return *this;
    }
    int compare(const PermutationView &other) const;
    bool isIdentity() const;
    bool fixesPoint(upoint_type p) const {
        return (this->data + 1)[p] == p;
    };
    upoint_type mapPoint(upoint_type point) const {
        return (this->data + 1)[point];
    }
    upoint_type firstNonFixedPoint() const;
    upoint_type inverseMapPoint(upoint_type point) const;
    std::size_t hash() const;
    std::size_t getStorageSize() const { return this->len + 1; }
    bool operator == (PermutationView other) const {
        return this->compare(other) == 0;
    }
    bool operator != (PermutationView other) const {
        return this->compare(other) != 0;
    }
    static std::size_t storageSize(std::size_t len) { return len + 1; }
};

std::ostream &operator << (std::ostream &os, const PermutationView &perm);

struct Cycles {
    struct Iterator {
        private:
        std::size_t i, size;
        upoint_type *ptr;
        friend Cycles;
    };
    upoint_type *data;

    Iterator begin() const {

    }
};

struct CyclesConverter {
    ~CyclesConverter() {
        if (this->data) delete[] this->data;
    }
    void convert(PermutationView perm);
    private:
    upoint_type *data = nullptr;
    std::size_t size = 0;
    friend std::ostream &operator << (std::ostream &os, const CyclesConverter &cycles);
};

std::ostream &operator << (std::ostream &os, const CyclesConverter &cycles);

struct PermutationList {
    struct Iterator {
        Iterator(PermutationList *list, std::size_t cursor): list(list), cursor(cursor) {}
        bool operator == (Iterator other) const {
            return this->list == other.list && this->cursor == other.cursor;
        }
        PermutationView operator -> () const {
            return list->get(this->cursor);
        }
        PermutationView operator * () {
            return list->get(this->cursor);
        }
        Iterator &operator ++ () {
            this->cursor++;
            return *this;
        }
        private:
        PermutationList *list;
        std::size_t cursor;
        friend PermutationList;
    };
    PermutationList() = default;
    PermutationList(std::size_t permLen): data(PermutationView::storageSize(permLen)) {};
    PermutationList(const PermutationList &) = default;
    PermutationList(PermutationList &&) = default;
    void setPermutationLength(std::size_t permLen) {
        this->data.setElementLen(PermutationView::storageSize(permLen));
    }
    void clear() {
        this->data.clear();
    }
    std::size_t getSize() const {
        return this->data.getSize();
    }
    std::size_t getPermutationLength() const {
        return this->data.getElementSize() - 1;
    }
    PermutationView get(std::size_t i) {
        return PermutationView{this->data[i], this->getPermutationLength()};
    }
    void setNegative(std::size_t i, bool isNegative) {
        this->data[i][0] = upoint_type(isNegative);
    }
    PermutationView push() {
        return PermutationView{this->data.push(), this->getPermutationLength()};
    }
    void pop() {
        this->data.pop();
    }
    void remove(std::size_t i) {
        this->data.remove(i);
    }
    void addPermutation(PermutationView perm) {
        this->push().copy(perm);
    }
    Iterator begin() {
        return Iterator(this, 0);
    }
    Iterator end() {
        return Iterator(this, this->getSize());
    }
    template<typename Iter1, typename Iter2>
    void addAll(Iter1 begin, Iter2 end) {
        for (auto iter = begin; iter != end; ++iter) {
            this->addPermutation(*iter);
        }
    }
    private:
    ArrayVector<upoint_type> data;
};

template<typename T, typename Fn>
inline void filterPermutationsInPlace(T &set, Fn &&predicate) {
    auto size = set.getSize();
    for (std::size_t i = 0; i < size;) {
        if (!predicate(set.get(i))) {
            set.remove(i);
            size--;
        } else {
            i++;
        }
    }
}

template<typename T>
inline void stabilizerInPlace(T &set, upoint_type point) {
    filterPermutationsInPlace(set, [=](PermutationView perm){ return perm.fixesPoint(point); });
}

template<typename T>
inline void stabilizerPointsInPlace(T &set, Slice<upoint_type> point) {
    for (std::size_t i = 0; i < point.len; i++) {
        stabilizerInPlace(set, point[i]);
    }
}

struct PermutationSet {
    using Iterator = PermutationList::Iterator;
    PermutationList permutations;
    PermutationSet() = default;
    PermutationSet(std::size_t permLen): permutations(permLen) {}
    PermutationSet(PermutationSet &&other) = default;
    PermutationView get(std::size_t i) {
        return this->permutations.get(i);
    }
    void setPermutationLength(std::size_t permLen) {
        this->permutations.setPermutationLength(permLen);
        this->permToId.clear();
    }
    void clear() {
        this->permutations.clear();
        this->permToId.clear();
    }
    template<typename Iter1, typename Iter2>
    void addAll(Iter1 begin, Iter2 end) {
        for (auto iter = begin; iter != end; ++iter) {
            this->addPermutation(*iter);
        }
    }
    PermutationSet copy() {
        PermutationSet ret(this->getPermutationLength());
        ret.addAll(this->begin(), this->end());
        return ret;
    }
    PermutationView reservePermutation() {
        return this->permutations.push();
    }
    void commitPermutation();
    void addPermutation(const PermutationView &perm);
    PermutationView appendPermutation(bool isNegative, std::initializer_list<upoint_type> list);
    OptionalUInt<std::size_t> findPermutation(const PermutationView &perm);
    void remove(std::size_t index);
    void removePermutation(PermutationView perm) {
        auto p = this->findPermutation(perm);
        if (p.isPresent()) {
            this->remove(p.get());
        }
    }
    bool contains(const PermutationView &perm) { return this->findPermutation(perm).isPresent(); }
    std::size_t getSize() const { return this->permutations.getSize(); }
    std::size_t getPermutationLength() const { return this->permutations.getPermutationLength(); }
    template<typename Fn>
    void filterInPlace(Fn &&predicate) {
        auto size = this->getSize();
        for (std::size_t i = 0; i < size;) {
            if (!predicate(this->get(i))) {
                this->remove(i);
                size--;
            } else {
                i++;
            }
        }
    }
    template<typename Fn>
    PermutationSet filter(Fn &&predicate) {
        PermutationSet ret(this->getPermutationLength());
        for (std::size_t i = 0; i < this->getSize(); i++) {
            auto perm = this->get(i);
            if (predicate(perm)) {
                ret.addPermutation(perm);
            }
        }
        return ret;
    }
    PermutationSet intersection(PermutationSet &other) {
        return this->filter([&](const PermutationView &perm){ return other.contains(perm); });
    }
    PermutationSet complement(PermutationSet &other) {
        return this->filter([&](const PermutationView &perm){ return !other.contains(perm); });
    }
    Iterator begin() {
        return this->permutations.begin();
    }
    Iterator end() {
        return this->permutations.end();
    }
    private:
    HashTable<std::uint32_t> permToId;
};

std::ostream &operator << (std::ostream &os, PermutationSet &genset);

struct SchreierVectorEntry {
    std::uint32_t generator;
    upoint_type sourcePoint;
};

struct OptionalSchreierVectorEntry {
    OptionalSchreierVectorEntry() = default;
    OptionalSchreierVectorEntry(None n): OptionalSchreierVectorEntry() {}
    OptionalSchreierVectorEntry(SchreierVectorEntry entry): generator(entry.generator), sourcePoint(entry.sourcePoint) {}
    bool isPresent() const {
        return this->sourcePoint.isPresent();
    }
    SchreierVectorEntry get() const {
        return SchreierVectorEntry{this->generator, this->sourcePoint.get()};
    }
    private:
    std::uint32_t generator = 0;
    OptionalUInt<upoint_type> sourcePoint;
};

inline std::ostream &operator << (std::ostream &os, SignedPoint point) {
    if (point.isNegative) {
        os << "-";
    }
    os << point.point;
    return os;
}

struct SchreierOrbit {
    std::size_t permLen, allocSize, orbitCount = 0;
    std::unique_ptr<OptionalUInt<upoint_type>[]> pointToOrbitId;
    std::unique_ptr<OptionalSchreierVectorEntry[]> vector;
    SchreierOrbit() = default;
    SchreierOrbit(std::size_t permLen) {
        this->reset(permLen);
    }
    SchreierOrbit(SchreierOrbit &&other) = default;
    SchreierOrbit &operator = (SchreierOrbit &&other) = default;
    bool onSameOrbit(upoint_type p1, upoint_type p2) const {
        return this->pointToOrbitId[p1] == this->pointToOrbitId[p2];
    }
    void reset(std::size_t permLen);
    std::size_t allOrbitSize() const {
        auto ptr = this->pointToOrbitId.get();
        return std::count_if(ptr, ptr + this->permLen, [](OptionalUInt<upoint_type> point){ return point.isPresent(); });
    }
    template<typename Fn>
    void collectAllOrbit(Fn consumer) const {
        auto ptr = this->pointToOrbitId.get();
        for (upoint_type p = 0; p < this->permLen; p++, ptr++) {
            if (ptr->isPresent()) {
                consumer(p, ptr->get());
            }
        }
    }
    void dump(std::ostream &os, PermutationList &genset);
};

struct SchreierVectorBuilder {
    PermutationList *genset;
    SchreierOrbit orbit;
    std::deque<upoint_type> workingPoints;
    SchreierVectorBuilder() = default;
    SchreierVectorBuilder(PermutationList &genset) {
        this->reset(genset);
    };
    void reset(PermutationList &genset);
    void appendOrbit(upoint_type point);
    void appendAllOrbits();
    void appendOrbitAndRest(upoint_type point) {
        this->appendOrbit(point);
        this->appendAllOrbits();
    };
};

struct PermutationStack {
    PermutationStack(std::size_t blockSize): inner(blockSize) {};
    PermutationView push(std::size_t len) {
        return PermutationView{this->inner.push(PermutationView::storageSize(len)), len};
    }
    void pop(const PermutationView &perm) {
        this->inner.pop(perm.getStorageSize());
    }
    inline void pop(struct StackedPermutation &perm);
    std::size_t isEmpty() {
        return this->inner.isEmpty();
    }
    private:
    OBStack<upoint_type> inner;
};

struct StackedPermutation : PermutationView {
    StackedPermutation(PermutationStack &stack, std::size_t len): stack(&stack) {
        auto perm = stack.push(len);
        this->data = perm.data;
        this->len = len;
    }
    StackedPermutation(const StackedPermutation &) = delete;
    StackedPermutation(StackedPermutation &&other) {
        this->data = other.data;
        this->len = other.len;
        this->stack = other.stack;
        other.leak();
    }
    void leak() {
        this->stack = nullptr;
    }
    ~StackedPermutation() {
        if (this->stack != nullptr) {
            this->stack->pop(*this);
        }
    }
    private:
    PermutationStack *stack;
};

inline void PermutationStack::pop(StackedPermutation &perm) {
    perm.leak();
    this->inner.pop(perm.getStorageSize());
}

struct JerrumBranching {
    struct Ptr {
        Ptr() = default;
        explicit Ptr(std::uint32_t value): value(value) {}
        private:
        std::uint32_t value;
        friend JerrumBranching;
    };
    struct OptionalPtr {
        OptionalPtr() = default;
        OptionalPtr(None n): OptionalPtr() {}
        OptionalPtr(Ptr p): value(p.value) {}
        explicit OptionalPtr(OptionalUInt<std::uint32_t> value): value(value) {}
        bool isPresent() const {
            return this->value.isPresent();
        }
        Ptr get() const {
            return Ptr(this->value.get());
        };
        private:
        OptionalUInt<std::uint32_t> value;
        friend JerrumBranching;
    };
    JerrumBranching() = default;
    JerrumBranching(std::size_t permLen): JerrumBranching() {
        this->setPermutationLength(permLen);
    }
    ~JerrumBranching() {
        if (this->tableData != nullptr) {
            delete[] this->tableData;
        }
    }
    void setPermutationLength(std::size_t permLen);
    void reset() {
        this->setPermutationLength(this->permLen);
    }
    Ptr allocPermutation();
    void retainPermutation(Ptr ptr) {
        this->freePermutations.push_back(ptr);
    }
    PermutationView getPermutation(Ptr p) {
        return this->permutationStorage.get(p.value);
    }
    OptionalPtr getEdge(upoint_type i, upoint_type j) const {
        return this->tableData[i * this->permLen + j];
    }
    void setEdge(upoint_type i, upoint_type j, OptionalPtr value);
    void setEdgePermutation(upoint_type i, upoint_type j, PermutationView perm) {
        this->setEdge(i, j, None{});
        auto ptr = this->allocPermutation();
        this->getPermutation(ptr).copy(perm);
        this->setEdge(i, j, ptr);
    }
    OptionalPtr getVertex(upoint_type i) {
        return this->tableData[this->permLen * this->permLen + i];
    }
    void setVertex(upoint_type i, OptionalPtr value) {
        auto &old = this->tableData[this->permLen * this->permLen + i];
        if (old.isPresent()) {
            this->retainPermutation(old.get());
        }
        old = value;
    }
    void recalculateVertices(std::deque<upoint_type> &workStack);
    bool hasPath(std::deque<upoint_type> &queue, upoint_type p1, upoint_type p2);
    OptionalUInt<upoint_type> findRoot() const;
    void siftElement(PermutationStack &stack, std::deque<upoint_type> &queue, PermutationView perm);
    template<typename Fn>
    void collectLabels(Fn &&consumer) {
        auto ptr = this->tableData;
        for (upoint_type i = 0; i < this->permLen * this->permLen; i++, ptr++) {
            if (ptr->isPresent()) {
                consumer(this->getPermutation(ptr->get()));
            }
        }
    }
    void dump(std::ostream &os);
    private:
    std::size_t permLen = 0;
    OptionalPtr *tableData = nullptr;
    std::size_t tableSize = 0;
    PermutationList permutationStorage;
    std::vector<Ptr> freePermutations;
};

struct JerrumBranchingBuilder {
    JerrumBranching branching;
    void build(PermutationStack &permStack, PermutationList &genset);
    private:
    PermutationStack *permStack;
    SchreierVectorBuilder orbit;
    PermutationList currentGens;
    PermutationSet schreierGens; // maybe PermutationList is also ok? cause we don't know whether the Schreier generators contain dupes
    JerrumBranching siftingBranching;
    std::deque<upoint_type> queue;
    void augment(upoint_type i);
};

StackedPermutation traceSchreierVector(PermutationStack &stack, upoint_type point, PermutationList &genset, const OptionalSchreierVectorEntry *vec);
StackedPermutation doubleTraceSchreierVector(PermutationStack &stack, upoint_type point1, upoint_type point2, PermutationList &genset, const OptionalSchreierVectorEntry *vec);
void computeOrbit(bool *points, upoint_type start, PermutationList &perms, std::deque<upoint_type> &workQueue);
// has allocation
bool isInGroup(PermutationStack &stack, PermutationView perm, PermutationList &genset, Slice<upoint_type> base);

template<typename Set>
inline void schreierGenerators(Set &ret, PermutationStack &stack, PermutationList &gens, SchreierOrbit &orbit) {
    auto permLen = gens.getPermutationLength();
    StackedPermutation tmp(stack, permLen);
    for (upoint_type gamma = 0; gamma < permLen; gamma++) if (orbit.pointToOrbitId[gamma].isPresent()) {
        auto tmp1 = traceSchreierVector(stack, gamma, gens, orbit.vector.get());
        for (auto gen : gens) {
            StackedPermutation tmp2(stack, permLen);
            tmp2.inverse(traceSchreierVector(stack, gen.mapPoint(gamma), gens, orbit.vector.get()));
            tmp.multiply(tmp1, gen);
            tmp.multiply(tmp, tmp2);
            if (!tmp.isIdentity()) {
                ret.addPermutation(tmp);
            }
        }
    }
}

struct BaseChanger {
    PermutationSet genset;
    void setSGS(Slice<upoint_type> base, PermutationList &genset);
    void interchange(std::size_t pos, PermutationStack &stack);
    private:
    PermutationSet newGens;
    PermutationList stabilizer, stabilizer2;
    SchreierVectorBuilder orbit1, orbit2;
    Array<bool> orbitSets;
    Array<upoint_type> base;
    std::size_t baseLen;
    std::deque<upoint_type> queue;
};

}

template<>
struct std::hash<pperm::SignedPoint> {
    std::size_t operator()(pperm::SignedPoint point) const noexcept {
        std::size_t ret = point.point;
        if (point.isNegative) {
            ret = ~ret;
        }
        return ret;
    }
};