#pragma once
#include <cstdlib> // for size_t
#include <iostream>
#include <utility>
#include <vector>
#include <functional>
#include <exception>
#include <chrono>

namespace pperm {

template<typename T>
inline bool bitSetGet(const T *data, std::size_t wordBits, std::size_t i) {
    return data[i / wordBits] & (1 << (i % wordBits));
}

template<typename T>
void copyArray(T *dest, const T *src, std::size_t len) {
    for (std::size_t i = 0; i < len; i++) {
        *dest++ = *src++;
    }
}

template<typename T, typename T2>
void arraySet(T *dest, std::size_t len, T2 &&val) {
    for (std::size_t i = 0; i < len; i++) {
        *dest++ = val;
    }
}

struct SlicePtr {
    std::size_t ptr, len;
};

template<typename T>
struct InstantArray {
    T *arr;
    InstantArray(std::size_t len): arr(new T[len]) {}
    ~InstantArray() {
        delete [] this->arr;
    }
};

struct Unit {};
struct None {};

template<typename T>
struct OptionalUInt {
    OptionalUInt(): val(0) {}
    OptionalUInt(None n): val(0) {}
    OptionalUInt(T val): val(val + 1) {}
    OptionalUInt(T val, Unit u): val(val) {}
    static OptionalUInt<T> fromRaw(T val) { return OptionalUInt<T>(val, Unit{}); }
    bool isPresent() const {
        return this->val != 0;
    }
    T get() const {
        if (this->val == 0) {
            throw std::runtime_error("attempting to get() a None OptionalUInt");
        }
        return this->val - 1;
    }
    T getRaw() const { return this->val; }
    bool operator == (OptionalUInt<T> other) const {
        return this->val == other.val;
    }
    bool operator == (T other) const {
        return this->val == other + 1;
    }
    template<typename Fn>
    T orElse(Fn &&fn) const {
        return this->val == 0 ? fn() : this->val - 1;
    }
    template<typename Fn>
    OptionalUInt<T> map(Fn &&fn) const {
        if (this->val == 0) {
            return *this;
        } else return OptionalUInt<T>(fn(this->val - 1));
    }
    private:
    T val;
};

template<typename T>
std::ostream &operator << (std::ostream &os, OptionalUInt<T> val) {
    if (val.isPresent()) {
        os << val.get();
    } else {
        os << "None";
    }
    return os;
}

template<typename T>
struct OptionalInt {
    OptionalInt() = default;
    OptionalInt(None n): val(0) {}
    OptionalInt(T v): val(v > 0 ? v + 1 : v) {}
    bool isPresent() const {
        return this->val != 0;
    }
    T get() const {
        return this->val < 0 ? this->val : this->val - 1;
    }
    bool operator == (OptionalInt<T> other) const {
        return this->val == other.val;
    }
    bool operator == (T other) const {
        return this->isPresent() && this->get() == other;
    }
    template<typename Fn>
    OptionalInt<T> map(Fn &&fn) const {
        if (this->val == 0) {
            return *this;
        } else {
            return OptionalInt<T>(fn(this->get()));
        }
    }
    private:
    T val = 0;
};

template<typename T>
std::ostream &operator << (std::ostream &os, OptionalInt<T> val) {
    if (val.isPresent()) {
        os << val.get();
    } else {
        os << "None";
    }
    return os;
}

template<typename T>
struct Ptr {
    std::size_t value;
    Ptr(): value(0) {}
    explicit Ptr(std::size_t value): value(value) {}
    Ptr offset(std::int32_t offset) const {
        return Ptr<T>(this->value + offset);
    }
    T *offsetFrom(T *ptr) const {
        return ptr + this->value;
    }
};

template<typename T>
struct Slice {
    T *ptr;
    std::size_t len;

    Slice<T> slice(std::size_t begin, std::size_t len) const {
        return Slice<T>{this->ptr + begin, len};
    }
    Slice<T> slice(std::size_t begin) const {
        return Slice<T>{this->ptr + begin, this->len - begin};
    }
    T *begin() const {
        return this->ptr;
    }
    T *end() const {
        return this->ptr + this->len;
    }
    void copy(Slice<T> l) const {
        auto ptr = this->ptr, ptr2 = l.ptr;
        for (std::size_t i = 0; i < this->len && i < l.len; i++, ptr++, ptr2++) {
            *ptr = *ptr2;
        }
    }

    const T &operator [] (std::size_t i) const {
#ifdef PPERM_DEBUG
        if (i >= this->len) {
            throw std::runtime_error("index out of bounds");
        }
#endif
        return this->ptr[i];
    }
    T &operator [] (std::size_t i) {
#ifdef PPERM_DEBUG
        if (i >= this->len) {
            throw std::runtime_error("index out of bounds");
        }
#endif
        return this->ptr[i];
    }
    std::size_t indexOf(const T &val) {
        auto ptr = this->ptr;
        for (std::size_t i = 0; i < this->len; i++, ptr++) {
            if (*ptr == val) {
                return i;
            }
        }
        return this->len;
    }

    int compare(Slice<T> other) const {
        if (this->len != other.len) {
            return this->len > other.len ? 1 : -1;
        }
        auto ptr1 = this->ptr, ptr2 = other.ptr;
        for (std::size_t i = 0; i < this->len; i++, ptr1++, ptr2++) {
            auto c1 = *ptr1, c2 = *ptr2;
            if (c1 > c2) {
                return 1;
            }
            if (c1 < c2) {
                return -1;
            }
        }
        return 0;
    }
    std::size_t hash() const {
        // TODO: use a better hasher
        std::size_t ret = 5381;
        auto ptr = this->ptr;
        for (std::size_t i = 0; i < this->len; i++) {
            ret = ret * 33 + *ptr++;
        }
        return ret;
    }
};

template<typename T>
inline Slice<T> makeSlice(T *ptr, std::size_t len) {
    return Slice<T>{ptr, len};
}

template<typename T>
inline std::ostream &operator << (std::ostream &os, Slice<T> slice) {
    bool first = true;
    for (auto t : slice) {
        if (!first) {
            os << ", ";
        }
        first = false;
        os << t;
    }
    return os;
}

template<typename T>
struct Array {
    Array() = default;
    Array(const Array<T> &) = delete;
    Array(Array &&other) {
        this->ptr = other.ptr;
        this->size = other.size;
        other.ptr = nullptr;
        other.size = 0;
    }
    ~Array() { if (this->ptr) delete[] this->ptr; }
    T *get() const {
        return this->ptr;
    }
    void ensureSize(std::size_t size) {
        if (this->ptr == nullptr || this->size < size) {
            if (this->ptr) {
                delete[] this->ptr;
            }
            this->ptr = new T[size];
            this->size = size;
        }
    }

    private:
    T *ptr = nullptr;
    std::size_t size = 0;
};

struct Trees {
    template<typename PtrType>
    struct InsertionPoint {
        OptionalUInt<PtrType> node;
        int dir;
        static constexpr int SELF_DIR = 2;
        static InsertionPoint<PtrType> nil() {
            return InsertionPoint<PtrType>{OptionalUInt<PtrType>(), 2};
        }
        bool isNodePresent() const {
            return this->dir == SELF_DIR && this->node.isPresent();
        }
    };
    template<typename Tree, typename PtrType>
    static void removeLeaf(Tree &tree, PtrType node) {
        auto &self = tree.getNode(node - 1);
        PtrType parent = self.getParent();
        if (parent != 0) {
            auto &parentData = tree.getNode(parent - 1);
            parentData.child[node == parentData.child[0] ? 0 : 1] = 0;
        } else {
            tree.setRoot(0);
        }
    }
    template<typename Tree, typename PtrType>
    static void rotate(Tree &tree, PtrType selfPtr, int dir) {
        auto &self = tree.getNode(selfPtr);
        OptionalUInt<PtrType> parentPtr = self.getParent();
        OptionalUInt<PtrType> rightPtr = self.child[1 - dir];
        auto &right = tree.getNode(rightPtr.get());
        OptionalUInt<PtrType> rightLeftPtr = right.child[dir];
        self.child[1 - dir] = rightLeftPtr;
        if (rightLeftPtr.isPresent()) {
            tree.getNode(rightLeftPtr.get()).setParent(OptionalUInt<PtrType>(selfPtr));
        }
        right.child[dir] = OptionalUInt<PtrType>(selfPtr);
        self.setParent(rightPtr);
        right.setParent(parentPtr);
        if (parentPtr.isPresent()) {
            auto &parent = tree.getNode(parentPtr.get());
            parent.child[OptionalUInt<PtrType>(selfPtr) == parent.child[0] ? 0 : 1] = rightPtr;
        } else {
            tree.setRoot(rightPtr);
        }
    }
    template<typename Tree, typename PtrType>
    static void swap(Tree &tree, PtrType pn1, PtrType pn2) {
        auto &node1 = tree.getNode(pn1), &node2 = tree.getNode(pn2);
        OptionalUInt<PtrType> n1 = OptionalUInt<PtrType>(pn1), n2 = OptionalUInt<PtrType>(pn2);
        OptionalUInt<PtrType> p1 = node1.getParent(), p2 = node2.getParent();
        if (p1.isPresent()) {
            if (p1 != n2) {
                auto &p1Node = tree.getNode(p1.get());
                p1Node.child[p1Node.child[0] == n1 ? 0 : 1] = n2;
            }
        } else {
            tree.setRoot(n2);
        }
        if (p2.isPresent()) {
            if (p2 != n1) {
                auto &p2Node = tree.getNode(p2.get());
                p2Node.child[p2Node.child[0] == n2 ? 0 : 1] = n1;
            }
        } else {
            tree.setRoot(n1);
        }
        if (node1.child[0].isPresent() && node1.child[0] != n2) {
            tree.getNode(node1.child[0].get()).setParent(n2);
        }
        if (node1.child[1].isPresent() && node1.child[1] != n2) {
            tree.getNode(node1.child[1].get()).setParent(n2);
        }
        if (node2.child[0].isPresent() && node2.child[0] != n1) {
            tree.getNode(node2.child[0].get()).setParent(n1);
        }
        if (node2.child[1].isPresent() && node2.child[1] != n1) {
            tree.getNode(node2.child[1].get()).setParent(n1);
        }
        node1.setParent(p2 != n1 ? p2 : n2);
        node2.setParent(p1 != n2 ? p1 : n1);
        OptionalUInt<PtrType> tmp = node1.child[0];
        node1.child[0] = node2.child[0] != n1 ? node2.child[0] : n2;
        node2.child[0] = tmp != n2 ? tmp : n1;
        tmp = node1.child[1];
        node1.child[1] = node2.child[1] != n1 ? node2.child[1] : n2;
        node2.child[1] = tmp != n2 ? tmp : n1;
    }
    template<typename Tree, typename K, typename PtrType>
    static InsertionPoint<PtrType> find(Tree &tree, const K &key, OptionalUInt<PtrType> node) {
        if (!node.isPresent()) {
            return InsertionPoint<PtrType>::nil();
        }
        int dir = 0;
        PtrType node2 = node.get();
        while (1) {
            int cmp = key.compare(tree, node2);
            if (cmp == 0) {
                return InsertionPoint<PtrType>{OptionalUInt<PtrType>(node2), InsertionPoint<PtrType>::SELF_DIR};
            }
            dir = cmp < 0 ? 0 : 1;
            OptionalUInt<PtrType> nextNode = tree.getNode(node2).child[dir];
            if (!nextNode.isPresent()) {
                return InsertionPoint<PtrType>{OptionalUInt<PtrType>(node2), dir};
            } else {
                node2 = nextNode.get();
            }
        }
    }
    template<typename Tree, typename PtrType>
    static PtrType leftmost(Tree &tree, PtrType node) {
        OptionalUInt<PtrType> next;
        while ((next = tree.getNode(node).child[0]).isPresent()) {
            node = next.get();
        }
        return node;
    }
    template<typename Tree, typename PtrType>
    static OptionalUInt<PtrType> successor(Tree &tree, PtrType node) {
        auto &nodeData = tree.getNode(node);
        if (nodeData.child[1].isPresent()) {
            return leftmost(tree, nodeData.child[1].get());
        } else while (1) {
            auto &nodeData2 = tree.getNode(node);
            OptionalUInt<PtrType> parent = nodeData2.getParent();
            if (parent.isPresent()) {
                auto &parentData = tree.getNode(parent.get());
                if (parentData.child[0] == node) {
                    return parent;
                } else {
                    node = parent.get();
                }
            } else return OptionalUInt<PtrType>();
        }
    }
};

template<typename PtrType>
struct AVLNode {
    using InsertionPoint = typename Trees::InsertionPoint<PtrType>;
    struct CheckHeightResult {
        bool ok;
        int height;
    };
    void clear() {
        this->child[0] = OptionalUInt<PtrType>();
        this->child[1] = OptionalUInt<PtrType>();
        this->parentAndBalancing = 1;
    }
    int balancingFactor() const {
        return int(this->parentAndBalancing & 3) - 1;
    }
    void setBalancingFactor(int factor) {
        this->parentAndBalancing = (this->parentAndBalancing & ~static_cast<PtrType>(3)) | (PtrType(factor + 1) & 3);
    }
    OptionalUInt<PtrType> getParent() const {
        return OptionalUInt<PtrType>::fromRaw(this->parentAndBalancing >> 2);
    }
    void setParent(OptionalUInt<PtrType> parent) {
        this->parentAndBalancing = (this->parentAndBalancing & 3) | (parent.getRaw() << 2);
    }
    template<typename Tree>
    static void rebalance(Tree &tree, OptionalUInt<PtrType> node, int dir, int deltaHeight) {
        while (node.isPresent() && deltaHeight != 0) {
            AVLNode<PtrType> &nodeData = tree.getNode(node.get());
            OptionalUInt<PtrType> parent = nodeData.getParent();
            int nextDir;
            if (parent.isPresent()) {
                AVLNode<PtrType> &parentData = tree.getNode(parent.get());
                nextDir = node == parentData.child[0] ? 0 : 1;
            }
            int bf = nodeData.balancingFactor();
            int newBf = bf + (dir == 0 ? deltaHeight : -deltaHeight);
            switch (bf * (1 - 2 * dir)) {
                default: std::abort();
                case -1: deltaHeight = 0; break;
                case 0: if (deltaHeight == -1) deltaHeight = 0; break;
                case 1: break; // No action
            }
            if (newBf == 2 || newBf == -2) {
                int ubDir = newBf == 2 ? 0 : 1;
                PtrType ubChild = nodeData.child[ubDir].get();
                AVLNode<PtrType> &ubChildData = tree.getNode(ubChild);
                int ubChildBf = ubChildData.balancingFactor();
                if ((ubDir == 1 ? 1 : -1) == ubChildBf) {
                    PtrType ubChildChild = ubChildData.child[1 - ubDir].get();
                    AVLNode<PtrType> &ubChildChildData = tree.getNode(ubChildChild);
                    Trees::rotate(tree, ubChild, ubDir);
                    Trees::rotate(tree, node.get(), 1 - ubDir);
                    int ubChildChildBf = ubChildChildData.balancingFactor();
                    int ubChildChildUbDir = ubChildChildBf == 1 ? 0 : 1;
                    if (ubChildChildBf == 0) {
                        nodeData.setBalancingFactor(0);
                        ubChildData.setBalancingFactor(0);
                    } else if (ubChildChildUbDir == ubDir) {
                        nodeData.setBalancingFactor(ubDir == 1 ? 1 : -1);
                        ubChildData.setBalancingFactor(0);
                    } else {
                        nodeData.setBalancingFactor(0);
                        ubChildData.setBalancingFactor(ubDir == 1 ? -1 : 1);
                    }
                    ubChildChildData.setBalancingFactor(0);
                    deltaHeight--;
                } else {
                    Trees::rotate(tree, node.get(), 1 - ubDir);
                    if (ubChildBf == 0) {
                        nodeData.setBalancingFactor(1 - 2 * ubDir);
                        ubChildData.setBalancingFactor(2 * ubDir - 1);
                    } else {
                        nodeData.setBalancingFactor(0);
                        ubChildData.setBalancingFactor(0);
                        deltaHeight--;
                    }
                }
            } else {
                nodeData.setBalancingFactor(newBf);
            }
            node = parent;
            dir = nextDir;
        }
    }
    template<typename Tree>
    static void insert(Tree &tree, InsertionPoint point, PtrType node) {
        AVLNode<PtrType> &nodeData = tree.getNode(node);
        if (!point.node.isPresent()) {
            tree.setRoot(OptionalUInt<PtrType>(node));
        } else {
            AVLNode<PtrType> &parentData = tree.getNode(point.node.get());
            nodeData.setParent(point.node);
            parentData.child[point.dir] = OptionalUInt<PtrType>(node);
        }
        rebalance(tree, point.node, point.dir, 1);
    }
    template<typename Tree>
    static PtrType remove(Tree &tree, PtrType node) {
        AVLNode<PtrType> *nodeData = &tree.getNode(node);
        if (nodeData->child[0].isPresent() && nodeData->child[1].isPresent()) {
            PtrType successor = nodeData->child[1].get();
            OptionalUInt<PtrType> next;
            while ((next = tree.getNode(successor).child[0]).isPresent()) {
                successor = next.get();
            }
            Trees::swap(tree, node, successor);
            AVLNode<PtrType> &successorData = tree.getNode(successor);
            int bf = nodeData->balancingFactor();
            nodeData->setBalancingFactor(successorData.balancingFactor());
            successorData.setBalancingFactor(bf);
        }
        OptionalUInt<PtrType> parent = nodeData->getParent();
        if (nodeData->child[0].isPresent() || nodeData->child[1].isPresent()) {
            PtrType child = nodeData->child[nodeData->child[0].isPresent() ? 0 : 1].get();
            AVLNode<PtrType> &childData = tree.getNode(child);
            if (!parent.isPresent()) {
                childData.setParent(OptionalUInt<PtrType>());
                tree.setRoot(OptionalUInt<PtrType>(child));
            } else {
                AVLNode<PtrType> &parentData = tree.getNode(parent.get());
                int dir = parentData.child[0] == OptionalUInt<PtrType>(node) ? 0 : 1;
                parentData.child[dir] = OptionalUInt<PtrType>(child);
                childData.setParent(parent);
                rebalance(tree, parent, dir, -1);
            }
            return node;
        }
        if (!parent.isPresent()) {
            nodeData->setParent(OptionalUInt<PtrType>());
            tree.setRoot(OptionalUInt<PtrType>());
        } else {
            AVLNode<PtrType> &parentData = tree.getNode(parent.get());
            int dir = parentData.child[0] == OptionalUInt<PtrType>(node) ? 0 : 1;
            parentData.child[dir] = OptionalUInt<PtrType>();
            rebalance(tree, parent, dir, -1);
        }
        return node;
    }
    template<typename Tree, typename Fn>
    static void dump(Tree &tree, std::ostream &os, OptionalUInt<PtrType> root, Fn &&elementVisitor, int indents) {
        for (int i = 0; i < indents; i++) os << "    ";
        if (root.isPresent()) {
            AVLNode<PtrType> &node = tree.getNode(root.get());
            os << "<node id=" << root.get() << ", bf=" << node.balancingFactor() << ", ";
            elementVisitor(os, tree, root.get());
            os << ">" << std::endl;
            dump(tree, os, node.child[0], elementVisitor, indents + 1);
            dump(tree, os, node.child[1], elementVisitor, indents + 1);
            for (int i = 0; i < indents; i++) os << "    ";
            os << "</node>" << std::endl;
        } else {
            os << "<nil/>" << std::endl;
        }
    }
    enum class CheckHeightErrorMessage {
        WRONG_PARENT_LINK1,
        WRONG_PARENT_LINK2,
        WRONG_BF,
        IMBALANCED,
    };
    struct CheckHeightError {
        CheckHeightErrorMessage msg;
        PtrType node;

        void print(std::ostream &os) const {
            switch (this->msg) {
                case CheckHeightErrorMessage::WRONG_PARENT_LINK1:
                    os << "wrong parent link on 1st child of node " << this->node << std::endl;
                    break;
                case CheckHeightErrorMessage::WRONG_PARENT_LINK2:
                    os << "wrong parent link on 2nd child of node " << this->node << std::endl;
                    break;
                case CheckHeightErrorMessage::WRONG_BF:
                    os << "wrong bf on node " << this->node << std::endl;
                    break;
                case CheckHeightErrorMessage::IMBALANCED:
                    os << "imbalanced node " << this->node << std::endl;
            }
        }
    };
    template<typename Tree>
    static int checkHeightWithErrors(Tree &tree, OptionalUInt<PtrType> root, std::vector<CheckHeightError> &errors) {
        if (root.isPresent()) {
            AVLNode<PtrType> &node = tree.getNode(root.get());
            if (node.child[0].isPresent() && tree.getNode(node.child[0].get()).getParent() != root) {
                errors.push_back(CheckHeightError{CheckHeightErrorMessage::WRONG_PARENT_LINK1, root.get()});
            }
            if (node.child[1].isPresent() && tree.getNode(node.child[1].get()).getParent() != root) {
                errors.push_back(CheckHeightError{CheckHeightErrorMessage::WRONG_PARENT_LINK2, root.get()});
            }
            int h1 = checkHeightWithErrors(tree, node.child[0], errors);
            int h2 = checkHeightWithErrors(tree, node.child[1], errors);
            int bf = h1 - h2;
            int actualBf = node.balancingFactor();
            if (bf != actualBf) {
                errors.push_back(CheckHeightError{CheckHeightErrorMessage::WRONG_BF, root.get()});
                if (bf >= 2 || bf <= -2) {
                    errors.push_back(CheckHeightError{CheckHeightErrorMessage::IMBALANCED, root.get()});
                }
            }
            return (h1 > h2 ? h1 : h2) + 1;
        } else {
            return 0;
        }
    }
    template<typename Tree>
    static bool checkHeight(Tree &tree, OptionalUInt<PtrType> root, std::ostream &os) {
        std::vector<CheckHeightError> errors;
        checkHeightWithErrors(tree, root, errors);
        if (errors.size() > 0) {
            for (auto it = errors.begin(); it != errors.end(); ++it) {
                it->print(os);
            }
            return false;
        } else return true;
    }
    PtrType parentAndBalancing = 1;
    OptionalUInt<PtrType> child[2]{OptionalUInt<PtrType>(), OptionalUInt<PtrType>()};
};

template<typename PtrType>
struct RBNode {
    using InsertionPoint = typename Trees::InsertionPoint<PtrType>;
    RBNode(): parentAndColor(0), child{0, 0} {}
    bool isRed() const {
        return static_cast<bool>(this->parentAndColor & 1);
    }
    void setRed(bool red) {
        if (red) {
            this->parentAndColor |= 1;
        } else {
            this->parentAndColor &= ~static_cast<decltype(this->parentAndColor)>(1);
        }
    }
    PtrType getParent() const {
        return this->parentAndColor >> 1;
    }
    void setParent(PtrType parent) {
        this->parentAndColor &= 1;
        this->parentAndColor |= parent << 1;
    }

    template<typename Tree>
    static void rotate(Tree &tree, PtrType selfPtr, int dir) {
        RBNode &self = tree.getNode(selfPtr - 1);
        PtrType parentPtr = self.getParent();
        PtrType rightPtr = self.child[1 - dir];
        RBNode &right = tree.getNode(rightPtr - 1);
        PtrType rightLeftPtr = right.child[dir];
        self.child[1 - dir] = rightLeftPtr;
        if (rightLeftPtr != 0) {
            tree.getNode(rightLeftPtr - 1).setParent(selfPtr);
        }
        right.child[dir] = selfPtr;
        self.setParent(rightPtr);
        right.setParent(parentPtr);
        if (parentPtr != 0) {
            RBNode &parent = tree.getNode(parentPtr - 1);
            parent.child[selfPtr == parent.child[0] ? 0 : 1] = rightPtr;
        } else {
            tree.setRoot(rightPtr);
        }
    }
    template<typename Tree>
    static void insert(Tree &tree, InsertionPoint point, PtrType node) {
        RBNode &self = tree.getNode(node - 1);
        self.setRed(true);
        self.setParent(point.node);
        PtrType parentPtr = point.node;
        if (parentPtr == 0) {
            tree.setRoot(node);
            return;
        }
        tree.getNode(parentPtr - 1).child[point.dir] = node;
        RBNode *gp;
        do {
            RBNode &parent = tree.getNode(parentPtr - 1);
            if (!parent.isRed()) {
                // 1
                return;
            }
            PtrType gpPtr = parent.getParent();
            if (gpPtr == 0) {
                // 4
                parent.setRed(false);
                return;
            }
            gp = &tree.getNode(gpPtr - 1);
            int dir = gp->child[0] == parentPtr ? 0 : 1;
            PtrType unclePtr = gp->child[1 - dir];
            if (unclePtr == 0 || !tree.getNode(unclePtr - 1).isRed()) {
                // 5, 6
                if (node == parent.child[1 - dir]) {
                    rotate(tree, parentPtr, dir);
                    node = parentPtr;
                    parentPtr = gp->child[dir];
                }
                // 6
                rotate(tree, gpPtr, 1 - dir);
                RBNode &parent2 = tree.getNode(parentPtr - 1);
                parent2.setRed(false);
                gp->setRed(true);
                return;
            }
            // 2
            RBNode &uncle = tree.getNode(unclePtr - 1);
            parent.setRed(false);
            uncle.setRed(false);
            gp->setRed(true);
            node = gpPtr;
        } while ((parentPtr = gp->getParent()) != 0);
        // 3
    }

    template<typename Tree>
    static void remove(Tree &tree, PtrType node) {
        RBNode<PtrType> *self = &tree.getNode(node - 1);
        // simple cases
        if (self->child[0] != 0 && self->child[1] != 0) {
            PtrType successor = self->child[1];
            PtrType next;
            while ((next = tree.getNode(successor - 1).child[0]) != 0) {
                successor = next;
            }
            Trees::swap(tree, successor, node);
            node = successor;
            self = &tree.getNode(node - 1);
        }
        PtrType parent = self->getParent();
        if (self->child[0] != 0 || self->child[1] != 0) {
            PtrType child = self->child[self->child[0] != 0 ? 0 : 1];
            RBNode<PtrType> &childData = tree.getNode(child - 1);
            if (childData.isRed()) {
                childData.setRed(false);
            }
            if (parent == 0) {
                tree.setRoot(child);
                childData.setParent(0);
            } else {
                RBNode<PtrType> &parentData = tree.getNode(parent - 1);
                parentData.child[parentData.child[0] == node ? 0 : 1] = child;
                childData.setParent(parent);
            }
            return;
        }
        if (parent == 0) {
            tree.setRoot(0);
            return;
        }
        // complex cases
        RBNode<PtrType> *parentData = &tree.getNode(parent - 1);
        int dir = node == parentData->child[0] ? 0 : 1;
        parentData->child[dir] = 0;
        if (self->isRed()) {
            return;
        }
        do {
            parentData = &tree.getNode(parent - 1);
            PtrType sibling = parentData->child[1 - dir];
            RBNode<PtrType> &siblingData = tree.getNode(sibling - 1);
            PtrType distantNephew = siblingData.child[1 - dir];
            PtrType closeNephew = siblingData.child[dir];
            if (siblingData.isRed()) {
                // 3
                // TODO
            }
            if (distantNephew != 0 && tree.getNode(distantNephew - 1).isRed()) {
                // 6
                // TODO
            }
            if (closeNephew != 0 && tree.getNode(closeNephew - 1).isRed()) {
                // 5
                // TODO
            }
            if (parentData->isRed()) {
                // 4
                // TODO
            }
            // 2
            parentData->setRed(true);
            node = parent;
        } while ((parent = tree.getNode(node - 1)) != 0);
    }

    template<typename Tree, typename Fn>
    static void dump(Tree &tree, std::ostream &os, PtrType root, Fn &&elementVisitor, int indent) {
        for (int i = 0; i < indent; i++) os << "    ";
        if (root == 0) {
            os << "<null />" << std::endl;
        } else {
            RBNode &node = tree.getNode(root - 1);
            if (node.isRed()) {
                os << "\x1b[31m<node ";
                elementVisitor(os, tree, root);
                os << ">\033[m" << std::endl;
            } else {
                os << "<node ";
                elementVisitor(os, tree, root);
                os << ">" << std::endl;
            }
            dump(tree, os, node.child[0], elementVisitor, indent + 1);
            dump(tree, os, node.child[1], elementVisitor, indent + 1);
            for (int i = 0; i < indent; i++) os << "    ";
            if (node.isRed()) {
                os << "\x1b[31m</node>\033[m" << std::endl;
            } else {
                os << "</node>" << std::endl;
            }
        }
    }
    private:
    PtrType parentAndColor;
    PtrType child[2];
    friend struct Trees;
};

// very much for testing
template<typename K, typename V>
struct RBTreeMap {
    using ptr_type = std::uint32_t;
    using RBNodeType = RBNode<ptr_type>;
    using InsertionPoint = typename RBNodeType::InsertionPoint;
    struct Node {
        RBNodeType rbNode;
        K key;
        V value;
        Node(K &&key, V &&value): key(key), value(value) {}
    };
    template<typename K2, typename Ctx>
    InsertionPoint insert(K2 &&key, const Ctx &ctx, V &&value) {
        InsertionPoint point = Trees::find(*this, WrappedKey<K2, Ctx>{key, ctx}, this->root);
        if (point.dir != 2) {
            ptr_type node = this->nodes.size() + 1;
            this->nodes.emplace_back(std::move(key), std::move(value));
            RBNodeType::insert(*this, point, node);
            return point;
        } else {
            return point;
        }
    }
    ptr_type getRoot() const { return this->root; }
    void dump(std::ostream &os) {
        RBNodeType::dump(*this, os, this->root, [](std::ostream &os2, RBTreeMap<K, V> &map, ptr_type node) {
            Node &n = map.nodes.at(node - 1);
            os2 << n.key << " : " << n.value;
        }, 0);
    }
    private:

    template<typename K2, typename Ctx>
    struct WrappedKey {
        const K2 &key;
        const Ctx &ctx;
        int compare(const RBTreeMap<K, V> &map, ptr_type node) const {
            return this->ctx.compare(this->key, map.nodes.at(node).key);
        }
    };
    std::vector<Node> nodes;
    ptr_type root = 0;

    RBNodeType &getNode(ptr_type node) {
        return this->nodes.at(node).rbNode;
    }
    void setRoot(ptr_type node) {
        this->root = node;
    }

    friend struct RBNode<ptr_type>;
    friend struct Trees;
};

template<typename K, typename V>
struct AVLMap {
    using ptr_type = std::uint32_t;
    using AVLNodeType = AVLNode<ptr_type>;
    using InsertionPoint = AVLNodeType::InsertionPoint;
    struct Node {
        K key;
        V value;
        Node(K &&key, V &&value): key(key), value(value) {}
        private:
        AVLNodeType node;
        bool occupied = true;
        friend struct AVLMap<K, V>;
    };
    template<typename K2, typename Ctx>
    InsertionPoint insert(K2 &&key, const Ctx &ctx, V &&value) {
        InsertionPoint point = Trees::find(*this, WrappedKey<K2, Ctx>{key, ctx}, this->root);
        if (!point.isNodePresent()) {
            AVLNodeType::insert(*this, point, this->allocNode(std::move(key), std::move(value)));
            return point;
        } else {
            return point;
        }
    }
    template<typename K2, typename Ctx>
    InsertionPoint find(const K2 &key, const Ctx &ctx) {
        return Trees::find(*this, WrappedKey<K2, Ctx>{key, ctx}, this->root);
    }
    InsertionPoint randomElement(std::size_t seed) const {
        seed %= this->nodes.size();
        ptr_type i = (seed + 1) % this->nodes.size();
        while (i != seed) {
            if (this->nodes.at(i).occupied) {
                return InsertionPoint{OptionalUInt<ptr_type>(i), InsertionPoint::SELF_DIR};
            }
            i = (i + 1) % this->nodes.size();
        }
        return InsertionPoint::nil();
    }
    void remove(InsertionPoint point) {
        if (point.isNodePresent()) {
            ptr_type removedNode = AVLNodeType::remove(*this, point.node.get());
            Node &node = this->nodes.at(removedNode);
            node.node.child[0] = this->recycle;
            node.occupied = false;
            this->recycle = OptionalUInt<ptr_type>(removedNode);
        }
    }
    K &getKey(InsertionPoint point) {
        return this->nodes.at(point.node.get()).key;
    }
    V &getValue(InsertionPoint point) {
        return this->nodes.at(point.node.get()).value;
    }
    void dump(std::ostream &os) {
        AVLNodeType::dump(*this, os, this->root, [](std::ostream &os2, AVLMap<K, V> &map, ptr_type node) {
            Node &n = map.nodes.at(node);
            os2 << n.key << " : " << n.value;
        }, 0);
    }
    void clear() {
        this->root = OptionalUInt<ptr_type>();
        this->recycle = OptionalUInt<ptr_type>();
        this->nodes.clear();
    }
    OptionalUInt<ptr_type> getRoot() const { return this->root; }

    private:
    template<typename K2, typename Ctx>
    struct WrappedKey {
        const K2 &key;
        const Ctx &ctx;
        int compare(const AVLMap<K, V> &map, ptr_type node) const {
            return this->ctx.compare(this->key, map.nodes.at(node).key);
        }
    };
    std::vector<Node> nodes;
    OptionalUInt<ptr_type> root;
    OptionalUInt<ptr_type> recycle;
    AVLNodeType &getNode(ptr_type node) {
        return this->nodes.at(node).node;
    }
    void setRoot(OptionalUInt<ptr_type> node) {
        this->root = node;
    }
    ptr_type allocNode(K &&key, V &&value) {
        if (this->recycle.isPresent()) {
            ptr_type ret = this->recycle.get();
            Node &node = this->nodes.at(ret);
            this->recycle = node.node.child[0];
            node.node.clear();
            node.key = key;
            node.value = value;
            node.occupied = true;
            return ret;
        } else {
            ptr_type ret = this->nodes.size();
            this->nodes.emplace_back(std::move(key), std::move(value));
            return ret;
        }
    }
    friend struct Trees;
    friend struct AVLNode<ptr_type>;
};

template<typename K, typename V>
struct MapEntry {
    K key;
    V value;
};

struct SimpleHashContext {
    template<typename T2, typename T3>
    int compare(const T2 &v2, const T3 &v3) const {
        if (v2 == v3) return 0;
        if (v2 > v3) return 1;
        return -1;
    }
    template<typename T, typename K, typename V>
    int compare(const T &v1, const MapEntry<K, V> &v2) const {
        return this->compare(v1, v2.key);
    }
    template<typename K1, typename V1, typename K2, typename V2>
    int compare(const MapEntry<K1, V1> &v1, const MapEntry<K2, V2> &v2) const {
        return this->compare(v1.key, v2.key);
    }
    template<typename T2>
    std::size_t hash(const T2 &v) const {
        return std::hash<T2>{}(v);
    }
    template<typename K, typename V>
    std::size_t hash(const MapEntry<K, V> &entry) const {
        return std::hash<K>{}(entry.key);
    }
};

template<typename T, typename PtrType = std::uint32_t, unsigned int loadFactor = 60>
struct HashTable {
    using AVLNodeType = AVLNode<PtrType>;
    struct Entry {
        const T &getValue() const { return this->value; }
        T value;
        private:
        AVLNodeType avlNode;
        bool occupied;
        Entry(): occupied(false) {}

        friend struct HashTable<T, PtrType, loadFactor>;
    };
    struct Pointer {
        std::size_t bucketId;
        OptionalUInt<PtrType> node;
        bool isNonNull() const {
            return this->node.isPresent();
        }
        static Pointer nil() {
            return Pointer{0, OptionalUInt<PtrType>()};
        }
    };

    struct Iterator {
        Iterator(const HashTable<T, PtrType, loadFactor> &map, std::size_t cursor): map(map), cursor(cursor) {
            this->moveToOccupied();
        };
        void moveToOccupied() {
            while (this->cursor < this->map.entriesSize && !this->map.entries[this->cursor].occupied) {
                this->cursor++;
            }
        }
        bool operator == (const Iterator &other) const {
            return this->cursor == other.cursor;
        }
        Iterator &operator ++ () {
            this->cursor++;
            this->moveToOccupied();
            return *this;
        }
        T &value() const {
            return this->map.entries[this->cursor].value;
        }
        T *operator -> () const {
            return &this->value();
        }
        private:
        const HashTable<T, PtrType, loadFactor> &map;
        std::size_t cursor;
    };

    HashTable() = default;
    HashTable(const HashTable<T, PtrType, loadFactor> &) = delete;
    HashTable(HashTable<T, PtrType, loadFactor> &&other) {
        this->buckets = other.buckets;
        this->bucketSize = other.bucketSize;
        this->entries = other.entries;
        this->entriesSize = other.entriesSize;
        this->entriesLen = other.entriesLen;
        this->size = other.size;
        other.buckets = nullptr;
        other.bucketSize = 0;
        other.entries = nullptr;
        other.entriesSize = 0;
        other.entriesLen = 0;
        other.size = 0;
    }
    ~HashTable() {
        this->clearAndFree();
    }
    std::size_t getSize() const { return this->size; }
    void clearAndFree() {
        if (this->entries != nullptr) {
            delete[] this->entries;
            this->entries = nullptr;
        }
        if (this->buckets != nullptr) {
            delete[] this->buckets;
            this->buckets = nullptr;
        }
        this->recycle = None{};
        this->bucketSize = 0;
        this->entriesSize = 0;
        this->entriesLen = 0;
        this->size = 0;
    }
    void clear() {
        this->size = 0;
        this->entriesLen = 0;
        arraySet(this->buckets, this->bucketSize, None{});
        this->recycle = None{};
        for (std::size_t i = 0; i < this->entriesSize; i++) {
            this->entries[i].occupied = false;
        }
    }

    template<typename Ctx>
    void resize(std::size_t size, const Ctx &ctx) {
        delete[] this->buckets;
        this->buckets = new OptionalUInt<PtrType>[size];
        this->bucketSize = size;
        Entry *oldEntry = this->entries;
        if (this->entriesSize > 0) {
            this->entries = new Entry[this->entriesSize];
            this->entriesLen = 0;
        }
        this->size = 0;
        for (std::size_t i = 0; i < this->entriesSize; i++) {
            Entry *entry = oldEntry + i;
            if (entry->occupied) {
                this->computeIfAbsent(std::move(entry->value), ctx, [](T &&value){ return std::move(value); });
            }
        }
        if (oldEntry != nullptr) {
            delete[] oldEntry;
        }
    }

    template<typename K2, typename Ctx>
    Pointer find(const K2 &key, const Ctx &ctx) const {
        std::size_t hash = ctx.hash(key) % this->bucketSize;
        OptionalUInt<PtrType> entryPtr = this->buckets[hash];
        if (!entryPtr.isPresent()) {
            return Pointer::nil();
        } else {
            TreeWrapper wrapper(*this, hash);
            InsertionPoint point = Trees::find(wrapper, WrappedKey<K2, Ctx>{key, ctx}, entryPtr);
            if (point.isNodePresent()) {
                return Pointer{hash, point.node};
            } else {
                return Pointer::nil();
            }
        }
    }
    template<typename K2, typename Ctx>
    Entry *findEntry(const K2 &key, const Ctx &ctx) const {
        Pointer p = this->find(key, ctx);
        if (p.isNonNull()) {

        }
    }
    Entry *getEntry(Pointer p) {
        if (p.isNonNull()) {
            return this->entries + p.node.get();
        } else return nullptr;
    }
    template<typename K2, typename Ctx, typename Fn>
    std::pair<Pointer, bool> computeIfAbsent(K2 &&key, const Ctx &ctx, Fn &&fn) {
        if (this->bucketSize == 0 || this->bucketSize < this->size * loadFactor / 100) {
            this->resize(this->bucketSize == 0 ? 16 : 2 * this->bucketSize, ctx);
        }
        std::size_t hash = ctx.hash(key) % this->bucketSize;
        auto entryPtr = this->buckets[hash];
        if (!entryPtr.isPresent()) {
            PtrType node = this->allocEntry(fn(std::forward<K2>(key)));
            this->buckets[hash] = node;
            return std::make_pair(Pointer{hash, node}, true);
        } else {
            TreeWrapper treeWrapper(*this, hash);
            InsertionPoint point = Trees::find(treeWrapper, WrappedKey<K2, Ctx>{key, ctx}, entryPtr);
            if (!point.isNodePresent()) {
                PtrType node = this->allocEntry(fn(std::forward<K2>(key)));
                // treeWrapper invalidated here
                TreeWrapper treeWrapper2(*this, hash);
                AVLNodeType::insert(treeWrapper2, point, node);
                return std::make_pair(Pointer{hash, node}, true);
            } else {
                return std::make_pair(Pointer{hash, point.node}, false);
            }
        }
    }
    template<typename K2, typename Ctx>
    std::pair<Pointer, bool> putIfAbsent(K2 &&key, const Ctx &ctx, T &&value) {
        return this->computeIfAbsent(key, ctx, [&](K2 &&key2){ return std::move(value); });
    }
    template<typename K2, typename Fn>
    std::pair<Pointer, bool> computeIfAbsentSimple(K2 &&key, Fn &&fn) {
        return this->computeIfAbsent(key, SimpleHashContext{}, fn);
    }

    void remove(Pointer pointer) {
        if (pointer.isNonNull()) {
            TreeWrapper wrapper(*this, pointer.bucketId);
            PtrType node = AVLNodeType::remove(wrapper, pointer.node.get());
            Entry &entry = this->entries[node];
            entry.avlNode.child[0] = this->recycle;
            entry.occupied = false;
            this->recycle = node;
            this->size--;
        }
    }

    Iterator begin() const {
        return Iterator(*this, 0);
    }
    Iterator end() const {
        return Iterator(*this, this->entriesSize);
    }
    void dump(std::ostream &os) const {
        for (std::size_t i = 0; i < this->bucketSize; i++) {
            os << i << " {";
            TreeWrapper wrapper(*this, i);
            OptionalUInt<PtrType> node = this->buckets[i].map([&](PtrType n) {
                return Trees::leftmost(wrapper, n);
            });
            bool first = true;
            while (node.isPresent()) {
                if (!first) {
                    os << ", ";
                }
                first = false;
                PtrType n = node.get();
                Entry &entry = this->entries[n];
                os << entry.key << " -> " << entry.value;
                node = Trees::successor(wrapper, n);
            }
            os << "}" << std::endl;
        }
    }
    const T *randomKey(std::size_t seed) {
        seed %= this->entriesLen;
        PtrType ret = (seed + 1) % this->entriesLen;
        while (ret != seed) {
            Entry &entry = this->entries[ret];
            if (entry.occupied) {
                return &entry.value;
            }
            ret = (ret + 1) % this->entriesLen;
        }
        return nullptr;
    }
    template<typename Ctx>
    bool checkHash(const Ctx &ctx) const {
        for (std::size_t i = 0; i < this->bucketSize; i++) {
            std::size_t entryPtr = this->buckets[i];
            while (entryPtr > 0) {
                Entry *entry = this->entries + entryPtr - 1;
                if (i != ctx.hash(entry->key) % this->bucketSize) {
                    return false;
                }
                entryPtr = entry->next;
            }
        }
        return true;
    }

    private:
    using InsertionPoint = Trees::InsertionPoint<PtrType>;
    struct TreeWrapper {
        OptionalUInt<PtrType> *root;
        Entry *entries;

        TreeWrapper(const HashTable<T, PtrType, loadFactor> &map, std::size_t bucketId) {
            this->root = map.buckets + bucketId;
            this->entries = map.entries;
        }
        void setRoot(OptionalUInt<PtrType> root) {
            *this->root = root;
        }
        OptionalUInt<PtrType> getRoot() {
            return *this->root;
        }
        AVLNodeType &getNode(PtrType node) {
            return this->entries[node].avlNode;
        }
    };
    template<typename K2, typename Ctx>
    struct WrappedKey {
        const K2 &key;
        const Ctx &ctx;
        int compare(const TreeWrapper &tree, PtrType node) const {
            return this->ctx.compare(this->key, tree.entries[node].value);
        }
    };
    OptionalUInt<PtrType> *buckets = nullptr;
    std::size_t bucketSize = 0;
    Entry *entries = nullptr;
    std::size_t entriesSize = 0;
    std::size_t entriesLen = 0;
    std::size_t size = 0;
    OptionalUInt<PtrType> recycle{};

    PtrType allocEntry(T &&value) {
        PtrType ret;
        if (this->recycle.isPresent()) {
            ret = this->recycle.get();
            this->recycle = this->entries[ret].avlNode.child[0];
        } else {
            if (this->entriesLen >= this->entriesSize) {
                std::size_t newSize = this->entriesSize == 0 ? 16 : this->entriesSize << 1;
                Entry *newEntry = new Entry[newSize];
                for (std::size_t i = 0; i < this->entriesSize; i++) {
                    newEntry[i] = std::move(this->entries[i]);
                }
                if (this->entriesSize > 0) {
                    delete[] this->entries;
                }
                this->entries = newEntry;
                this->entriesSize = newSize;
            }
            ret = this->entriesLen++;
        }
        Entry &entry = this->entries[ret];
        entry.avlNode.clear();
        entry.occupied = true;
        entry.value = value;
        this->size++;
        return ret;
    }
    TreeWrapper wrapTree(std::size_t hash) const {
        return TreeWrapper(*this, hash);
    }
};

template<typename T> struct StackedArray;

template<typename T>
struct OBStack {
    std::size_t blockSize = 16;
    OBStack() = default;
    OBStack(std::size_t blockSize): blockSize(blockSize) {}
    OBStack(const OBStack<T> &) = delete;
    OBStack(OBStack<T> &&) = default;
    ~OBStack() {
        for (auto it = this->blocks.begin(); it != this->blocks.end(); ++it) {
            delete[] it->ptr;
        }
    }
    bool isEmpty() const {
        if (this->allocPtr == 0) {
            return this->blocks.size() == 0 || this->blocks[this->allocPtr].len == 0;
        } else return false;
    }
    T *push(std::size_t count) {
        auto newBlockSize = count > this->blockSize ? count : this->blockSize;
        if (this->allocPtr >= this->blocks.size()) {
            Block newBlock{new T[newBlockSize], count, newBlockSize};
            auto ret = newBlock.ptr;
            this->blocks.push_back(newBlock);
            return ret;
        }
        Block &top = this->blocks[this->allocPtr];
        if (top.len + count <= top.size) {
            auto ret = top.ptr + top.len;
            top.len += count;
            return ret;
        } else {
            this->allocPtr++;
            if (this->allocPtr >= this->blocks.size()) {
                Block newBlock{new T[newBlockSize], count, newBlockSize};
                auto ret = newBlock.ptr;
                this->blocks.push_back(newBlock);
                return ret;
            } else {
                Block &block = this->blocks[this->allocPtr];
                if (block.size < count) {
                    delete[] block.ptr;
                    block.ptr = new T[newBlockSize];
                    block.size = count;
                }
                block.len = count;
                return block.ptr;
            }
        }
    }
    void pop(std::size_t count) {
        while (count > 0) {
            Block &blk = this->blocks[this->allocPtr];
            auto releasedCount = count > blk.len ? blk.len : count;
            blk.len -= releasedCount;
            count -= releasedCount;
            if (this->allocPtr == 0) {
                break;
            }
            if (blk.len == 0) {
                this->allocPtr--;
            }
        }
    }
    inline StackedArray<T> pushStacked(std::size_t count);
    private:
    struct Block {
        T *ptr;
        std::size_t len, size;
    };
    std::size_t allocPtr = 0;
    std::vector<Block> blocks;
};

template<typename T>
struct StackedArray {
    StackedArray(OBStack<T> &stack, std::size_t len): stack(&stack), ptr(stack.push(len)), len(len) {}
    StackedArray(const StackedArray<T> &) = delete;
    StackedArray(StackedArray<T> &&other): stack(other.stack), ptr(other.ptr), len(other.len) {
        other.forget();
    }
    ~StackedArray() {
        if (this->stack) {
            this->stack->pop(this->len);
        }
    }
    void forget() {
        this->stack = nullptr;
    }
    T *begin() const { return this->ptr; }
    T *end() const { return this->ptr + this->len; }
    const T &operator [] (std::size_t i) const {
        return this->ptr[i];
    }
    T &operator [] (std::size_t i) {
        return this->ptr[i];
    }
    private:
    OBStack<T> *stack;
    T *ptr;
    std::size_t len;
};

template<typename T>
inline StackedArray<T> OBStack<T>::pushStacked(std::size_t count) {
    return StackedArray<T>(*this, count);
}

template<typename T>
struct Array2d {
    std::size_t size1, size2;
    T *ptr;
};

template<typename T>
struct ArrayVector {
    struct Iterator {
        Iterator(const Iterator &other) = default;
        T *operator * () const {
            return this->ptr;
        }
        Iterator &operator ++ () {
            this->ptr += this->step;
            return *this;
        }
        bool operator == (Iterator other) const {
            return this->ptr == other.ptr;
        }
        std::size_t getElementSize() const {
            return this->step;
        }
        private:
        Iterator(T *ptr, std::size_t step): ptr(ptr), step(step) {}
        T *ptr;
        std::size_t step;
        friend ArrayVector<T>;
    };
    ArrayVector() = default;
    ArrayVector(std::size_t elementLen): elementLen(elementLen) {}
    ArrayVector(const ArrayVector &) = default;
    ArrayVector(ArrayVector &&) = default;
    ArrayVector<T> &operator = (ArrayVector<T> &&) = default;
    void setElementLen(std::size_t elementLen) {
        auto size = this->getSize();
        this->elementLen = elementLen;
        this->data.resize(elementLen * size);
    }
    void clear() {
        this->data.clear();
    }
    bool isEmpty() const {
        return this->data.empty();
    }
    std::size_t getSize() const {
        return this->data.size() / this->elementLen;
    }
    std::size_t getElementSize() const {
        return this->elementLen;
    }
    const T *get(std::size_t i) const {
        return this->data.data() + this->elementLen * i;
    }
    T *get(std::size_t i) {
        return this->data.data() + this->elementLen * i;
    }
    const T *operator[] (std::size_t i) const {
        return this->get(i);
    }
    T *operator[] (std::size_t i) {
        return this->get(i);
    }
    void pop() {
        this->data.resize(this->data.size() - this->elementLen);
    }
    T *push() {
        auto size = this->data.size();
        this->data.resize(size + this->elementLen);
        return this->data.data() + size;
    }
    T *top() {
        return this->data.data() + (this->data.size() - this->elementLen);
    }
    const T *top() const {
        return this->data.data() + (this->data.size() - this->elementLen);
    }
    void remove(std::size_t index) {
        auto dataPtr = this->data.data() + index*this->elementLen, dataPtr2 = dataPtr + this->elementLen;
        auto size = this->getSize();
        for (std::size_t i = 0; i < this->elementLen * (size - 1 - index); i++) {
            *dataPtr++ = *dataPtr2++;
        }
        this->data.resize(this->data.size() - this->elementLen);
    }
    Iterator begin() {
        return Iterator(this->data.data(), this->elementLen);
    }
    Iterator end() {
        return Iterator(this->data.data() + this->data.size(), this->elementLen);
    }
    private:
    std::size_t elementLen = 0;
    std::vector<T> data;
};

template<typename Self, typename Arg>
struct BindLeftShift {
    private:
    Arg arg;
    Self &self;
    friend std::ostream &operator << (std::ostream &os, BindLeftShift<Self, Arg> &b);
};

template<typename Self, typename Arg>
inline std::ostream &operator << (std::ostream &os, BindLeftShift<Self, Arg> &b) {
    b.self.applyLeftShift(b.arg);
}

template<typename T>
struct SwappingPair {
    SwappingPair() = default;
    template<typename T2>
    SwappingPair(T2 &&val): first(std::forward<T2>(val)), second{} {}
    private:
    T first, second;
};

template<typename Fn, typename Unit = std::chrono::microseconds>
inline Unit measureElapsed(Fn &&fn) {
    auto start = std::chrono::steady_clock::now();
    fn();
    auto end = std::chrono::steady_clock::now();
    return std::chrono::duration_cast<Unit>(end - start);
}

struct Logger {

};

}