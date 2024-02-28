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

struct PermutationFormatter;

struct PermutationView {
    upoint_type *data;
    std::size_t len;

    bool isNegative() const {
        return bool(this->data[0]);
    }
    PermutationView &setNegative(bool isNegative) {
        this->data[0] = upoint_type(isNegative);
        return *this;
    }
    upoint_type *images() const {
        return this->data + 1;
    }
    std::size_t getLength() const {
        return this->len;
    }
    template<typename... T>
    inline PermutationView &cycle(T... args);
    PermutationView &assign(bool isNegative, std::initializer_list<upoint_type> list);
    PermutationView &identity();
    PermutationView &multiply(const PermutationView &p1, const PermutationView &p2);
    PermutationView &inverse(const PermutationView &perm);
    PermutationView &copy(const PermutationView &perm) {
        copyArray(this->data, perm.data, this->getStorageSize());
        return *this;
    }
    int compare(const PermutationView &other, bool ignoreSign) const;
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
        return this->compare(other, false) == 0;
    }
    bool operator != (PermutationView other) const {
        return this->compare(other, false) != 0;
    }
    inline void print(std::ostream &os, PermutationFormatter &formatter) const;
    static std::size_t storageSize(std::size_t len) { return len + 1; }
    static std::size_t permutationLengthFromStorageSize(std::size_t size) { return size - 1; }
};

std::ostream &operator << (std::ostream &os, const PermutationView &perm);

struct CyclesBuilder {
    CyclesBuilder(PermutationView perm): images(perm.images()) {}
    void append(upoint_type p) {
        if (this->first.isPresent()) {
            this->images[this->last] = p;
        } else {
            this->first = p;
        }
        this->last = p;
    }
    void finish() const {
        if (this->first.isPresent()) {
            this->images[this->last] = this->first.get();
        }
    }
    private:
    upoint_type *images;
    OptionalUInt<upoint_type> first;
    upoint_type last;
};

template<typename... T>
inline PermutationView &PermutationView::cycle(T... args) {
    CyclesBuilder builder(*this);
    for (auto p : {args...}) {
        builder.append(p);
    }
    builder.finish();
    return *this;
}

struct CyclesConverter {
    ~CyclesConverter() {
        if (this->data) delete[] this->data;
    }
    void convert(PermutationView perm);
    private:
    upoint_type *data = nullptr;
    std::size_t size = 0;
    bool isNegative;
    friend std::ostream &operator << (std::ostream &os, const CyclesConverter &cycles);
};

std::ostream &operator << (std::ostream &os, const CyclesConverter &cycles);

struct PermutationFormatter {
    bool useCycles;

    template<typename T>
    struct FormattedValue {
        T arg;
        PermutationFormatter &formatter;
    };
    template<typename T>
    struct FormattedRef {
        T &arg;
        PermutationFormatter &formatter;
    };
    void print(std::ostream &os, PermutationView perm) {
        if (this->useCycles) {
            this->cyclesConverter.convert(perm);
            os << this->cyclesConverter;
        } else {
            os << perm;
        }
    }
    template<typename T>
    FormattedValue<T> formatValue(T &&arg) {
        return FormattedValue<T>{arg, *this};
    }
    template<typename T>
    FormattedRef<T> formatRef(T &arg) {
        return FormattedRef<T>{arg, *this};
    }
    private:
    CyclesConverter cyclesConverter;
};

template<typename T>
inline std::ostream &operator << (std::ostream &os, const PermutationFormatter::FormattedValue<T> &perm) {
    perm.arg.print(os, perm.formatter);
    return os;
}

template<typename T>
inline std::ostream &operator << (std::ostream &os, const PermutationFormatter::FormattedRef<T> &perm) {
    perm.arg.print(os, perm.formatter);
    return os;
}

inline void PermutationView::print(std::ostream &os, PermutationFormatter &formatter) const {
    formatter.print(os, *this);
}

struct PermutationList {
    struct Iterator {
        Iterator(ArrayVector<upoint_type>::Iterator inner): inner(inner) {}
        bool operator == (Iterator other) const {
            return this->inner == other.inner;
        }
        PermutationView operator -> () const {
            return PermutationView{*this->inner, PermutationView::permutationLengthFromStorageSize(this->inner.getElementSize())};
        }
        PermutationView operator * () {
            return PermutationView{*this->inner, PermutationView::permutationLengthFromStorageSize(this->inner.getElementSize())};
        }
        Iterator &operator ++ () {
            ++this->inner;
            return *this;
        }
        private:
        ArrayVector<upoint_type>::Iterator inner;
        friend PermutationList;
    };
    PermutationList() = default;
    PermutationList(std::size_t permLen): data(PermutationView::storageSize(permLen)) {};
    PermutationList(const PermutationList &) = default;
    PermutationList(PermutationList &&) = default;
    void setPermutationLength(std::size_t permLen) {
        this->clear();
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
    void removeAndFetchLast(std::size_t i) {
        this->data.removeAndFetchLast(i);
    }
    void addPermutation(PermutationView perm) {
        this->push().copy(perm);
    }
    Iterator begin() {
        return Iterator(this->data.begin());
    }
    Iterator end() {
        return Iterator(this->data.end());
    }
    template<typename Iter1, typename Iter2>
    void addAll(Iter1 begin, Iter2 end) {
        for (auto iter = begin; iter != end; ++iter) {
            this->addPermutation(*iter);
        }
    }
    template<typename Set>
    void copy(Set &set) {
        this->clear();
        this->setPermutationLength(set.getPermutationLength());
        this->addAll(set.begin(), set.end());
    }
    void print(std::ostream &os, PermutationFormatter &formatter);
    private:
    ArrayVector<upoint_type> data;
};

template<typename T, typename Fn>
inline bool filterPermutationsInPlace(T &set, Fn predicate) {
    auto size = set.getSize();
    bool ret = false;
    for (std::size_t i = 0; i < size;) {
        if (!predicate(set.get(i))) {
            set.removeAndFetchLast(i);
            size--;
            ret = true;
        } else {
            i++;
        }
    }
    return ret;
}

template<typename T>
inline bool stabilizerInPlace(T &set, upoint_type point) {
    return filterPermutationsInPlace(set, [=](PermutationView perm){ return perm.fixesPoint(point); });
}

template<typename T>
inline bool stabilizerPointsInPlace(T &set, Slice<upoint_type> point) {
    bool ret = false;
    for (std::size_t i = 0; i < point.len; i++) {
        ret = stabilizerInPlace(set, point[i]) || ret;
    }
    return ret;
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
    void addPermutation(const PermutationView &perm);
    OptionalUInt<std::size_t> findPermutation(const PermutationView &perm);
    void removeAndFetchLast(std::size_t index);
    void removePermutation(PermutationView perm) {
        auto p = this->findPermutation(perm);
        if (p.isPresent()) {
            this->removeAndFetchLast(p.get());
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
                this->removeAndFetchLast(i);
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
    void print(std::ostream &os, PermutationFormatter &format) {
        this->permutations.print(os, format);
    }
    template<typename Set>
    void copy(Set &set) {
        this->clear();
        this->setPermutationLength(set.getPermutationLength());
        this->addAll(set.begin(), set.end());
    }
    void updateIndex();
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
    void appendOrbit(upoint_type point, PermutationList &genset, std::deque<upoint_type> &queue);
    void appendAllOrbits(PermutationList &genset, std::deque<upoint_type> &queue) {
        for (upoint_type p = 0; p < this->permLen; p++) {
            this->appendOrbit(p, genset, queue);
        }
    }

    template<typename Iter1, typename Iter2>
    void appendOrbits(Iter1 begin, Iter2 end, PermutationList &genset, std::deque<upoint_type> &queue) {
        for (auto iter = begin; iter != end; ++iter) {
            this->appendOrbit(*iter, genset, queue);
        }
    }
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
    template<typename Fn>
    void collectOneOrbit(upoint_type p, Fn consumer) {
        auto ptr = this->pointToOrbitId.get();
        auto orbit = ptr[p];
        if (orbit.isPresent()) {
            for (upoint_type p = 0; p < this->permLen; p++, ptr++) {
                if (orbit == *ptr) {
                    consumer(p);
                }
            }
        }
    }
    void dump(std::ostream &os, PermutationList &genset, PermutationFormatter &formater);
};

struct StackedPermutation;

struct PermutationStack {
    PermutationStack() = default;
    PermutationStack(std::size_t blockSize): inner(blockSize) {};
    PermutationView push(std::size_t len) {
        return PermutationView{this->inner.push(PermutationView::storageSize(len)), len};
    }
    void pop(const PermutationView &perm) {
        this->inner.pop(perm.getStorageSize());
    }
    inline void pop(struct StackedPermutation &perm);
    inline StackedPermutation pushStacked(std::size_t len);
    std::size_t isEmpty() {
        return this->inner.isEmpty();
    }
    void setBlockSize(std::size_t size) {
        this->inner.blockSize = size;
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
        other.forget();
    }
    void forget() {
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
    perm.forget();
    this->inner.pop(perm.getStorageSize());
}

inline StackedPermutation PermutationStack::pushStacked(std::size_t permLen) {
    return StackedPermutation(*this, permLen);
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
    void dump(std::ostream &os, PermutationFormatter &formatter);
    private:
    std::size_t permLen = 0;
    OptionalPtr *tableData = nullptr;
    std::size_t tableSize = 0;
    PermutationList permutationStorage;
    std::vector<Ptr> freePermutations;
};

struct JerrumBranching2 {
    struct Entry {
        OptionalUInt<upoint_type> getParent() const {
            return OptionalUInt<upoint_type>::fromRaw(this->data[0]);
        }
        void setParent(OptionalUInt<upoint_type> p) {
            this->data[0] = p.getRaw();
        }
        upoint_type getRoot() const {
            return this->data[1];
        }
        void setRoot(upoint_type p) {
            this->data[1] = p;
        }
        PermutationView getEdgeLabel() {
            return PermutationView{this->data + 2, this->permLen};
        }
        PermutationView getVertexLabel() {
            return PermutationView{this->data + 2 + PermutationView::storageSize(this->permLen), this->permLen};
        }
        static std::size_t storageSize(std::size_t permLen) {
            return 2 + 2 * PermutationView::storageSize(permLen);
        }
        private:
        upoint_type *data;
        std::size_t permLen;
        Entry(upoint_type *data, std::size_t permLen): data(data), permLen(permLen) {}
        friend JerrumBranching2;
    };
    struct SiftLogger {
        std::ostream *os = nullptr;
        PermutationFormatter *formatter = nullptr;
        SiftLogger() = default;
        SiftLogger(std::ostream *os, PermutationFormatter &formatter): os(os), formatter(&formatter) {}
    };
    JerrumBranching2() = default;
    JerrumBranching2(std::size_t permLen) {
        this->setPermutationLength(permLen);
    }
    void setPermutationLength(std::size_t permLen) {
        this->permLen = permLen;
        this->storage.ensureSize(permLen * Entry::storageSize(permLen));
        this->reset();
    }
    Entry get(upoint_type p) {
        return Entry(this->storage.get() + p * Entry::storageSize(this->permLen), this->permLen);
    }
    void recalculateVertices(std::deque<upoint_type> &queue);
    bool hasPath(upoint_type p1, upoint_type p2);
    void dump(std::ostream &os, PermutationFormatter &formatter, std::deque<upoint_type> &queue);
    void reset();
    void siftElement(PermutationStack &stack, std::deque<upoint_type> &queue, PermutationView perm, SiftLogger log);
    template<typename Fn>
    void collectLabels(Fn consumer) {
        for (upoint_type i = 0; i < this->permLen; i++) {
            auto entry = this->get(i);
            if (entry.getParent().isPresent()) {
                consumer(entry.getEdgeLabel());
            }
        }
    }
    private:
    std::size_t permLen = 0;
    Array<upoint_type> storage;
};

struct JerrumBranchingBuilder {
    std::ostream *log = nullptr;
    PermutationFormatter formatter;
    JerrumBranching2 branching;
    void build(PermutationStack &permStack, PermutationList &genset);
    private:
    PermutationStack *permStack;
    SchreierOrbit orbit;
    PermutationList currentGens;
    PermutationSet schreierGens; // maybe PermutationList is also ok? cause we don't know whether the Schreier generators contain dupes
    JerrumBranching2 siftingBranching;
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
    auto tmp = stack.pushStacked(permLen);
    for (upoint_type gamma = 0; gamma < permLen; gamma++) if (orbit.pointToOrbitId[gamma].isPresent()) {
        auto tmp1 = traceSchreierVector(stack, gamma, gens, orbit.vector.get());
        for (auto gen : gens) {
            auto tmp2 = stack.pushStacked(permLen);
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
    void setSGS(PermutationList &genset);
    void interchange(Slice<upoint_type> partialBase, upoint_type b1, upoint_type b2, PermutationStack &stack);
    void moveToFirstDirectly(Slice<upoint_type> base, std::size_t pos, PermutationStack &stack) {
        while (pos > 0) {
            auto &b1 = base[pos - 1], &b2 = base[pos];
            this->interchange(base.slice(0, pos - 1), b1, b2, stack);
            std::swap(b1, b2);
            pos--;
        }
    }
    void makeFirstPoint(MutableSlice<upoint_type> base, upoint_type point, PermutationStack &stack);
    void completeBaseChange(MutableSlice<upoint_type> base, Slice<upoint_type> newBase, PermutationStack &stack);
    private:
    PermutationSet newGens;
    PermutationList stabilizer, stabilizer2;
    SchreierOrbit orbit1, orbit2;
    Array<bool> orbitSets;
    std::deque<upoint_type> queue;
};

struct StrongGenSetProvider {
    virtual void stabilizeOnePoint(PermutationStack &stack, upoint_type point) = 0;
    virtual PermutationList &getStrongGenSet() = 0;
    virtual void print(std::ostream &os, PermutationFormatter &formatter) = 0;
};

struct BaseChangingStrongGenSetProvider : StrongGenSetProvider {
    BaseChanger *baseChanger = nullptr;
    BaseChangingStrongGenSetProvider() = default;
    BaseChangingStrongGenSetProvider(BaseChanger &baseChanger): baseChanger(&baseChanger) {}
    void setSGS(PermutationList &genset);
    virtual void stabilizeOnePoint(PermutationStack &stack, upoint_type point) override final;
    virtual PermutationList &getStrongGenSet() override final;
    virtual void print(std::ostream &os, PermutationFormatter &formatter) override final;
    private:
    PermutationList genset;
    Array<upoint_type> base;
    std::size_t baseLen;
    Array<bool> stablePoints;
    void updateStablePoints();
};

struct DoubleCosetRepresentativeSolver {
    std::ostream *log = nullptr;
    bool useTwoStep = false;
    PermutationFormatter permFormatter;
    std::optional<StackedPermutation> solve(StrongGenSetProvider &gensetS, StrongGenSetProvider &gensetD, PermutationView perm);
    private:
    std::size_t permLen;
    PermutationStack permStack;
    SchreierOrbit orbitS, orbitD;
    BaseChanger baseChanger;
    OBStack<bool> boolSetPool;
    std::deque<upoint_type> queue;

    PermutationSet sgdSet;
    PermutationList selectedSgD;
    upoint_type minP;

    std::size_t baseChangeOfDTime, baseChangeOfSTime;

    void setPermutationLength(std::size_t permLen) {
        this->permLen = permLen;
        this->boolSetPool.blockSize = this->permLen * 16;
        this->permStack.setBlockSize(this->permLen * 16);
        this->sgdSet.setPermutationLength(permLen);
        this->selectedSgD.setPermutationLength(permLen);
    }
    void subroutineF1(const bool *orbitB, PermutationView perm);
    StackedPermutation solveRightCosetRepresentative(PermutationView perm, StrongGenSetProvider &gensetSProvider, upoint_type minNonFixedPointOfD, bool *finishedPoints);
    std::optional<StackedPermutation> solveDoubleCosetRepresentative(StrongGenSetProvider &gensetSProvider, StrongGenSetProvider &gensetDProvider, PermutationView perm, const bool *finishedPoints);
};

struct GroupOrderCalculator {
    void setGroup(PermutationList &list) {
        this->stabilizer = 0;
        this->orbit.ensureSize(list.getPermutationLength());
        this->genset.copy(list);
    }
    std::size_t nextFactor();
    std::size_t order();
    private:
    PermutationList genset;
    Array<bool> orbit;
    std::deque<upoint_type> queue;
    std::size_t stabilizer;
};

struct SymmetricBlock {
    using InnerIterator = ArrayVector<upoint_type>::Iterator;
    struct Iterator;
    struct Block {
        bool isNegative() const {
            return bool(this->data[0]);
        }
        void setNegative(bool n) {
            this->data[0] = upoint_type(n);
        }
        Slice<upoint_type> getData() const {
            return makeSlice(this->data + 1, this->elemLen);
        }
        private:
        Block(upoint_type *data, std::size_t elemLen): data(data), elemLen(elemLen) {}
        upoint_type *data;
        std::size_t elemLen;
        friend SymmetricBlock;
        friend Iterator;
    };
    struct Iterator {
        bool operator == (Iterator other) const {
            return this->inner == other.inner;
        }
        Iterator &operator ++ () {
            ++this->inner;
            return *this;
        }
        Block operator * () const {
            return Block(*this->inner, this->inner.getElementSize() - 1);
        }
        Block operator -> () const {
            return this->operator*();
        }
        private:
        Iterator(InnerIterator inner): inner(inner) {}
        InnerIterator inner;
        friend SymmetricBlock;
    };
    SymmetricBlock() = default;
    SymmetricBlock(std::size_t elemLen) {
        this->setElementLength(elemLen);
    }
    SymmetricBlock(const SymmetricBlock &) = delete;
    SymmetricBlock(SymmetricBlock &&) = default;
    SymmetricBlock &operator = (SymmetricBlock &&) = default;
    void setElementLength(std::size_t elemLen) {
        this->blocks.setElementLen(elemLen + 1);
    }
    std::size_t getElementLength() const {
        return this->blocks.getElementSize() - 1;
    }
    Iterator begin() {
        return Iterator(this->blocks.begin());
    }
    Iterator end() {
        return Iterator(this->blocks.end());
    }
    void clear() {
        this->blocks.clear();
    }
    Block pushBlock() {
        return Block(this->blocks.push(), this->getElementLength());
    }
    Block get(std::size_t i) {
        return Block(this->blocks.get(i), this->getElementLength());
    }
    Block getLast() {
        return Block(this->blocks.get(this->blocks.getSize() - 1), this->getElementLength());
    }
    std::size_t getSize() const {
        return this->blocks.getSize();
    }
    private:
    ArrayVector<upoint_type> blocks;
};

inline std::ostream &operator << (std::ostream &os, SymmetricBlock::Block block) {
    if (block.isNegative()) {
        os << "-";
    }
    os << "{" << block.getData() << "}";
    return os;
}

std::ostream &operator << (std::ostream &os, SymmetricBlock &block);

struct GroupEnumerator {
    PermutationStack *permStack = nullptr;
    PermutationSet elements;
    private:
    std::size_t prevCosetRep = 0;
    PermutationList generators;
    void addGenerator(PermutationView generator);
};

namespace meta {
    template<unsigned int ...N> struct List {};
    template<typename ...T> struct SCycles {
        private:
        template<typename T2>
        struct OneCycle {};
        template<unsigned int ...N>
        struct OneCycle<List<N...>> {
            static void oneCycle(PermutationView perm) {
                perm.cycle(N...);
            }
        };
        public:
        static void assignPermutation(PermutationView perm) {
            int perms[]{0, (OneCycle<T>::oneCycle(perm), 0)...};
            (void)perms;
        }
    };
    template<unsigned int ...N> struct Images {
        static void assignPermutation(PermutationView perm) {
            perm.assign(false, {N...});
        }
    };

    template<typename T> struct Neg {
        static void assignPermutation(PermutationView perm) {
            T::assignPermutation(perm);
            perm.setNegative(true);
        }
    };

    template<typename ...T>
    struct GenSet {
        static PermutationList build(unsigned int n) {
            PermutationList ret(n);
            GenSet<T...>::buildInPlace(ret);
            return ret;
        }
        static void buildInPlace(PermutationList &list) {
            int perms[]{0, (GenSet<T...>::addOnePermutation<T>(list), 0)...};
            (void)perms;
        }
        private:
        template<typename T2>
        static void addOnePermutation(PermutationList &list) {
            T2::assignPermutation(list.push().identity());
        }
    };

    template<typename ...T> struct JoinType {};
    template<template<typename...> typename H, typename ...T>
    struct JoinType<H<T...>> {
        using Type = H<T...>;
    };
    template<template<typename...> typename H, typename ...T1, typename ...T2, typename ...T3>
    struct JoinType<H<T1...>, H<T2...>, T3...> {
        using Type = typename JoinType<H<T1..., T2...>, T3...>::Type;
    };

    template<typename ...T>
    using Join = typename JoinType<T...>::Type;

    template<unsigned int ...N> struct SymmetricGenSetType {};
    template<unsigned int N1, unsigned int N2, unsigned int ...N>
    struct SymmetricGenSetType<N1, N2, N...> {
        using Type = Join<GenSet<SCycles<List<N1, N2>>>, typename SymmetricGenSetType<N2, N...>::Type>;
    };
    template<unsigned int N>
    struct SymmetricGenSetType<N> {
        using Type = GenSet<>;
    };

    template<unsigned int ...N>
    using Symmetric = typename SymmetricGenSetType<N...>::Type;

    template<template<typename> typename Fn, typename T> struct MapType {};
    template<template<typename> typename Fn, template<typename...> typename H>
    struct MapType<Fn, H<>> {
        using Type = H<>;
    };
    template<template<typename> typename Fn, template<typename...> typename H, typename T, typename ...T2>
    struct MapType<Fn, H<T, T2...>> {
        using Type = Join<H<Fn<T>>, typename MapType<Fn, H<T2...>>::Type>;
    };
    template<template<typename> typename Fn, typename T>
    using Map = typename MapType<Fn, T>::Type;

    template<unsigned int ...N>
    using Antisymmetric = Map<Neg, Symmetric<N...>>;

    template<unsigned int N1, unsigned int N2, unsigned int N3, unsigned int N4>
    using RiemannSymmetric = GenSet<Neg<SCycles<List<N1, N2>>>, Neg<SCycles<List<N3, N4>>>, SCycles<List<N1, N3>, List<N2, N4>>>;
};

template<typename Provider, typename Acceptor>
inline void filterBasePoints(Acceptor acceptor, Provider provider, std::size_t baseLen, PermutationList &genset, PermutationList &tmp) {
    tmp.copy(genset);
    for (std::size_t i = 0; i < baseLen; i++) {
        upoint_type p = provider(i);
        if (stabilizerInPlace(tmp, p)) {
            acceptor(p);
        }
    }
}

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