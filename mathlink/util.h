#pragma once
#include <cstdlib> // for size_t
#include <iostream>
#include <utility>
#include <vector>

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

template<typename T>
void arraySet(T *dest, std::size_t len, T &&val) {
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

template<typename T>
struct OptionalInt {
    OptionalInt(): val(0) {}
    explicit OptionalInt(T val): val(val + 1) {}
    bool isPresent() const {
        return this->val != 0;
    }
    T get() const {
        return this->val - 1;
    }
    private:
    T val;
};

template<typename T>
struct Ptr {
    std::size_t value;
    Ptr(): value(0) {}
    explicit Ptr(std::size_t value): value(value) {}
    Ptr offset(std::int32_t offset) const {
        return Ptr<T>(this->value + offset);
    }
};

template<typename T>
struct Slice {
    Ptr<T> ptr;
    std::size_t len;

    Slice<T> slice(std::size_t begin, std::size_t len) const {
        return Slice<T>{this->ptr.offset(begin), len};
    }

    const T &operator [] (std::size_t i) const {
        return this->ptr[i];
    }
    T &operator [] (std::size_t i) {
        return this->ptr[i];
    }
};

template<typename T>
struct Array {
    T *ptr;
    std::size_t size, len;
    Array(): Array(64) {}
    Array(std::uint32_t size): ptr(new T[size]), size(size), len(0) {};
    ~Array() { delete[] this->ptr; }
    void resize(std::size_t size) {
        T *ptr = new T[size];
        for (std::size_t i = 0; i < this->len; i++) {
            ptr[i] = this->ptr[i];
        }
        this->size = size;
        delete [] this->ptr;
        this->ptr = ptr;
    }
    Ptr<T> position() const { return Ptr<T>(this->len); }
    void setPosition(Ptr<T> ptr) { this->len = ptr.value; }
    Ptr<T> reserve(std::size_t len) {
        std::size_t ret = this->len;
        if (this->size < len + this->len) {
            this->resize(2*(this->len + len));
        }
        this->len += len;
        return Ptr<T>(ret);
    }
    void set(Ptr<T> ptr, T &&value) {
        this->ptr[ptr.value] = value;
    }

    T *getPtr(Ptr<T> ptr) const { return this->ptr + ptr.value; }
};

struct Trees {
    template<typename PtrType>
    struct InsertionPoint {
        PtrType node;
        int dir;
        static constexpr int SELF_DIR = 2;
        static InsertionPoint<PtrType> nil() {
            return InsertionPoint<PtrType>{0, 2};
        }
        bool isNodePresent() const {
            return this->dir == SELF_DIR && this->node != 0;
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
        auto &self = tree.getNode(selfPtr - 1);
        PtrType parentPtr = self.getParent();
        PtrType rightPtr = self.child[1 - dir];
        auto &right = tree.getNode(rightPtr - 1);
        PtrType rightLeftPtr = right.child[dir];
        self.child[1 - dir] = rightLeftPtr;
        if (rightLeftPtr != 0) {
            tree.getNode(rightLeftPtr - 1).setParent(selfPtr);
        }
        right.child[dir] = selfPtr;
        self.setParent(rightPtr);
        right.setParent(parentPtr);
        if (parentPtr != 0) {
            auto &parent = tree.getNode(parentPtr - 1);
            parent.child[selfPtr == parent.child[0] ? 0 : 1] = rightPtr;
        } else {
            tree.setRoot(rightPtr);
        }
    }
    template<typename Tree, typename PtrType>
    static void swap(Tree &tree, PtrType n1, PtrType n2) {
        auto &node1 = tree.getNode(n1 - 1), &node2 = tree.getNode(n2 - 1);
        PtrType p1 = node1.getParent(), p2 = node2.getParent();
        if (p1 != 0) {
            if (p1 != n2) {
                auto &p1Node = tree.getNode(p1 - 1);
                p1Node.child[p1Node.child[0] == n1 ? 0 : 1] = n2;
            }
        } else {
            tree.setRoot(n2);
        }
        if (p2 != 0) {
            if (p2 != n1) {
                auto &p2Node = tree.getNode(p2 - 1);
                p2Node.child[p2Node.child[0] == n2 ? 0 : 1] = n1;
            }
        } else {
            tree.setRoot(n1);
        }
        if (node1.child[0] != 0 && node1.child[0] != n2) {
            tree.getNode(node1.child[0] - 1).setParent(n2);
        }
        if (node1.child[1] != 0 && node1.child[1] != n2) {
            tree.getNode(node1.child[1] - 1).setParent(n2);
        }
        if (node2.child[0] != 0 && node2.child[0] != n1) {
            tree.getNode(node2.child[0] - 1).setParent(n1);
        }
        if (node2.child[1] != 0 && node2.child[1] != n1) {
            tree.getNode(node2.child[1] - 1).setParent(n1);
        }
        node1.setParent(p2 != n1 ? p2 : n2);
        node2.setParent(p1 != n2 ? p1 : n1);
        PtrType tmp = node1.child[0];
        node1.child[0] = node2.child[0] != n1 ? node2.child[0] : n2;
        node2.child[0] = tmp != n2 ? tmp : n1;
        tmp = node1.child[1];
        node1.child[1] = node2.child[1] != n1 ? node2.child[1] : n2;
        node2.child[1] = tmp != n2 ? tmp : n1;
    }
    template<typename Tree, typename K, typename PtrType>
    static InsertionPoint<PtrType> find(Tree &tree, const K &key, PtrType node) {
        if (node == 0) {
            return InsertionPoint<PtrType>::nil();
        }
        int dir = 0;
        while (1) {
            int cmp = key.compare(tree, node - 1);
            if (cmp == 0) {
                return InsertionPoint<PtrType>{node, InsertionPoint<PtrType>::SELF_DIR};
            }
            dir = cmp < 0 ? 0 : 1;
            PtrType nextNode = tree.getNode(node - 1).child[dir];
            if (nextNode == 0) {
                return InsertionPoint<PtrType>{node, dir};
            }
            node = nextNode;
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
        this->child[0] = 0;
        this->child[1] = 0;
        this->parentAndBalancing = 1;
    }
    int balancingFactor() const {
        return int(this->parentAndBalancing & 3) - 1;
    }
    void setBalancingFactor(int factor) {
        this->parentAndBalancing = (this->parentAndBalancing & ~static_cast<PtrType>(3)) | (PtrType(factor + 1) & 3);
    }
    PtrType getParent() const {
        return this->parentAndBalancing >> 2;
    }
    void setParent(PtrType parent) {
        this->parentAndBalancing = (this->parentAndBalancing & 3) | (parent << 2);
    }
    template<typename Tree>
    static void rebalance(Tree &tree, PtrType node, int dir, int deltaHeight) {
        while (node != 0 && deltaHeight != 0) {
            AVLNode<PtrType> &nodeData = tree.getNode(node - 1);
            PtrType parent = nodeData.getParent();
            int nextDir;
            if (parent != 0) {
                AVLNode<PtrType> &parentData = tree.getNode(parent - 1);
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
                PtrType ubChild = nodeData.child[ubDir];
                AVLNode<PtrType> &ubChildData = tree.getNode(ubChild - 1);
                int ubChildBf = ubChildData.balancingFactor();
                if ((ubDir == 1 ? 1 : -1) == ubChildBf) {
                    PtrType ubChildChild = ubChildData.child[1 - ubDir];
                    AVLNode<PtrType> &ubChildChildData = tree.getNode(ubChildChild - 1);
                    Trees::rotate(tree, ubChild, ubDir);
                    Trees::rotate(tree, node, 1 - ubDir);
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
                    Trees::rotate(tree, node, 1 - ubDir);
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
        AVLNode<PtrType> &nodeData = tree.getNode(node - 1);
        if (point.node == 0) {
            tree.setRoot(node);
        } else {
            AVLNode<PtrType> &parentData = tree.getNode(point.node - 1);
            nodeData.setParent(point.node);
            parentData.child[point.dir] = node;
        }
        rebalance(tree, point.node, point.dir, 1);
    }
    template<typename Tree>
    static PtrType remove(Tree &tree, PtrType node) {
        AVLNode<PtrType> *nodeData = &tree.getNode(node - 1);
        if (nodeData->child[0] != 0 && nodeData->child[1] != 0) {
            PtrType successor = nodeData->child[1];
            PtrType next;
            while ((next = tree.getNode(successor - 1).child[0]) != 0) {
                successor = next;
            }
            Trees::swap(tree, node, successor);
            AVLNode<PtrType> &successorData = tree.getNode(successor - 1);
            int bf = nodeData->balancingFactor();
            nodeData->setBalancingFactor(successorData.balancingFactor());
            successorData.setBalancingFactor(bf);
        }
        PtrType parent = nodeData->getParent();
        if (nodeData->child[0] != 0 || nodeData->child[1] != 0) {
            PtrType child = nodeData->child[nodeData->child[0] != 0 ? 0 : 1];
            AVLNode<PtrType> &childData = tree.getNode(child - 1);
            if (parent == 0) {
                childData.setParent(0);
                tree.setRoot(child);
            } else {
                AVLNode<PtrType> &parentData = tree.getNode(parent - 1);
                int dir = parentData.child[0] == node ? 0 : 1;
                parentData.child[dir] = child;
                childData.setParent(parent);
                rebalance(tree, parent, dir, -1);
            }
            return node;
        }
        if (parent == 0) {
            nodeData->setParent(0);
            tree.setRoot(0);
        } else {
            AVLNode<PtrType> &parentData = tree.getNode(parent - 1);
            int dir = parentData.child[0] == node ? 0 : 1;
            parentData.child[dir] = 0;
            rebalance(tree, parent, dir, -1);
        }
        return node;
    }
    template<typename Tree, typename Fn>
    static void dump(Tree &tree, std::ostream &os, PtrType root, Fn &&elementVisitor, int indents) {
        for (int i = 0; i < indents; i++) os << "    ";
        if (root != 0) {
            AVLNode<PtrType> &node = tree.getNode(root - 1);
            os << "<node id=" << root - 1 << ", bf=" << node.balancingFactor() << ", ";
            elementVisitor(os, tree, root - 1);
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
    static int checkHeightWithErrors(Tree &tree, PtrType root, std::vector<CheckHeightError> &errors) {
        if (root != 0) {
            AVLNode<PtrType> &node = tree.getNode(root - 1);
            if (node.child[0] != 0 && tree.getNode(node.child[0] - 1).getParent() != root) {
                errors.push_back(CheckHeightError{CheckHeightErrorMessage::WRONG_PARENT_LINK1, root - 1});
            }
            if (node.child[1] != 0 && tree.getNode(node.child[1] - 1).getParent() != root) {
                errors.push_back(CheckHeightError{CheckHeightErrorMessage::WRONG_PARENT_LINK2, root - 1});
            }
            int h1 = checkHeightWithErrors(tree, node.child[0], errors);
            int h2 = checkHeightWithErrors(tree, node.child[1], errors);
            int bf = h1 - h2;
            int actualBf = node.balancingFactor();
            if (bf != actualBf) {
                errors.push_back(CheckHeightError{CheckHeightErrorMessage::WRONG_BF, root - 1});
                if (bf >= 2 || bf <= -2) {
                    errors.push_back(CheckHeightError{CheckHeightErrorMessage::IMBALANCED, root - 1});
                }
            }
            return (h1 > h2 ? h1 : h2) + 1;
        } else {
            return 0;
        }
    }
    template<typename Tree>
    static bool checkHeight(Tree &tree, PtrType root, std::ostream &os) {
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
    PtrType child[2]{0, 0};
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
        friend class AVLMap<K, V>;
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
        ptr_type i = (seed + 1) % this->nodes.size();
        while (i != seed) {
            if (this->nodes.at(i).occupied) {
                return InsertionPoint{i + 1, InsertionPoint::SELF_DIR};
            }
            i = (i + 1) % this->nodes.size();
        }
        return InsertionPoint::nil();
    }
    void remove(InsertionPoint point) {
        if (point.isNodePresent()) {
            ptr_type removedNode = AVLNodeType::remove(*this, point.node);
            Node &node = this->nodes.at(removedNode - 1);
            V *ret = &node.value;
            node.node.child[0] = this->recycle;
            node.occupied = false;
            this->recycle = removedNode;
        }
    }
    V &getValue(InsertionPoint point) {
        return this->nodes.at(point.node - 1).value;
    }
    void dump(std::ostream &os) {
        AVLNodeType::dump(*this, os, this->root, [](std::ostream &os2, AVLMap<K, V> &map, ptr_type node) {
            Node &n = map.nodes.at(node);
            os2 << n.key << " : " << n.value;
        }, 0);
    }
    void clear() {
        this->root = 0;
        this->recycle = 0;
        this->nodes.clear();
    }
    ptr_type getRoot() const { return this->root; }

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
    ptr_type root = 0;
    ptr_type recycle = 0;
    AVLNodeType &getNode(ptr_type node) {
        return this->nodes.at(node).node;
    }
    void setRoot(ptr_type node) {
        this->root = node;
    }
    ptr_type allocNode(K &&key, V &&value) {
        if (this->recycle != 0) {
            ptr_type ret = this->recycle;
            Node &node = this->nodes.at(this->recycle - 1);
            this->recycle = node.node.child[0];
            node.node.clear();
            node.key = key;
            node.value = value;
            node.occupied = true;
            return this->recycle;
        } else {
            ptr_type ret = this->nodes.size() + 1;
            this->nodes.emplace_back(std::move(key), std::move(value));
            return ret;
        }
    }
    friend struct Trees;
    friend struct AVLNode<ptr_type>;
};

template<typename T>
struct SimpleHashContext {
    template<typename T2, typename T3>
    bool equals(const T2 &v2, const T3 &v3) const {
        return v2 == v3;
    }
    template<typename T2, typename T3>
    int compare(const T2 &v2, const T3 &v3) const {
        if (v2 == v3) return 0;
        if (v2 > v3) return 1;
        return -1;
    }
    template<typename T2>
    std::size_t hash(const T2 &v) const {
        return std::hash<T2>{}(v);
    }
    template<typename T2>
    T adaptKey(T2 &&v) const {
        return T(v);
    }
};

template<typename K, typename V, unsigned int loadFactor = 60>
struct HashMap {
    struct Entry {
        const K &getKey() const { return this->key; }
        const V &getValue() const { return this->value; }
        private:
        bool occupied;
        std::size_t next;
        K key;
        V value;
        Entry(): occupied(false), next(0) {}

        friend class HashMap<K, V, loadFactor>;
    };

    struct Iterator {
        Iterator(const HashMap<K, V, loadFactor> &map, std::size_t cursor): map(map), cursor(cursor) {
            this->moveToOccupied();
        };
        void moveToOccupied() {
            while (this->cursor < this->map.entriesSize && !this->map.entries[this->cursor].occupied) {
                this->cursor++;
            }
        }
        bool operator == (const Iterator &other) const {
            return &this->map == &other->map && this->cursor == other->cursor;
        }
        Iterator operator ++ () {
            this->cursor++;
            this->moveToOccupied();
        }
        const K &key() const {
            return this->map.entries[this->cursor].key;
        }
        const V &value() const {
            return this->map.entries[this->cursor].value;
        }
        private:
        const HashMap<K, V, loadFactor> &map;
        std::size_t cursor;
    };

    HashMap(): buckets(nullptr), bucketSize(0), entries(nullptr), entriesSize(0), size(0) {}
    HashMap(const HashMap<K, V, loadFactor> &) = delete;
    HashMap(HashMap<K, V, loadFactor> &&other) {
        this->buckets = other.buckets;
        this->bucketSize = other.bucketSize;
        this->entries = other.entries;
        this->entriesSize = other.entriesSize;
        this->size = other.size;
        other.buckets = nullptr;
        other.bucketSize = 0;
        other.entries = nullptr;
        other.entriesSize = 0;
        other.size = 0;
    }
    ~HashMap() {
        if (this->entries != nullptr) {
            delete[] this->entries;
        }
        if (this->buckets) {
            delete[] this->buckets;
        }
    }
    std::size_t getSize() const { return this->size; }

    template<typename Ctx>
    void resize(std::size_t size, const Ctx &ctx) {
        delete[] this->buckets;
        this->buckets = new std::size_t[size];
        arraySet<std::size_t>(this->buckets, size, 0);
        this->bucketSize = size;
        Entry *oldEntry = this->entries;
        auto oldEntrySize = this->entriesSize;
        this->entries = new Entry[this->entriesSize];
        this->size = 0;
        for (std::size_t i = 0; i < oldEntrySize; i++) {
            Entry *entry = oldEntry + i;
            if (entry->occupied) {
                this->computeIfAbsent(std::move(entry->key), ctx, [=]{ return std::move(entry->value); });
            }
        }
    }

    template<typename K2, typename Ctx>
    Entry *getEntry(const K2 &key, const Ctx &ctx) const {
        std::size_t entryPtr = this->buckets[ctx.hash(key) % this->bucketSize];
        if (entryPtr == 0) {
            return nullptr;
        }
        entryPtr--;
        Entry *entry = this->entries + entryPtr;
        while (1) {
            if (ctx.equals(key, entry->key)) {
                return entry;
            }
            if (entry->next != 0) {
                entry = this->entries + entry->next - 1;
            } else return nullptr;
        }
        return nullptr;
    }
    template<typename K2>
    Entry *getEntrySimple(const K2 &key) const {
        return this->getEntry(key, SimpleHashContext<K>{});
    }
    template<typename K2, typename Ctx, typename Fn>
    std::pair<Entry *, bool> computeIfAbsent(K2 &&key, const Ctx &ctx, Fn &&fn) {
        if (this->bucketSize == 0 || this->bucketSize < this->size * loadFactor / 100) {
            this->resize(this->bucketSize == 0 ? 16 : 2 * this->bucketSize, ctx);
        }
        std::size_t hash = ctx.hash(key) % this->bucketSize;
        std::size_t entryPtr = this->buckets[hash];
        if (entryPtr == 0) {
            entryPtr = findFreeEntry(0);
            this->buckets[hash] = entryPtr + 1;
            Entry *entry = this->entries + entryPtr;
            entry->occupied = true;
            entry->key = ctx.adaptKey(key);
            entry->value = fn();
            entry->next = 0;
            this->size++;
            return std::make_pair(entry, true);
        }
        entryPtr--;
        while (1) {
            Entry *entry = this->entries + entryPtr;
            if (ctx.equals(key, entry->key)) {
                return std::make_pair(entry, false);
            }
            if (entry->next != 0) {
                entryPtr = entry->next - 1;
            } else {
                entryPtr = findFreeEntry(entryPtr);
                entry->next = entryPtr + 1;
                entry = this->entries + entryPtr;
                entry->occupied = true;
                entry->key = ctx.adaptKey(key);
                entry->value = fn();
                entry->next = 0;
                this->size++;
                return std::make_pair(entry, true);
            }
        }
    }
    template<typename K2, typename Fn>
    std::pair<Entry *, bool> computeIfAbsentSimple(K2 &&key, Fn &&fn) {
        return this->computeIfAbsent(key, SimpleHashContext<K>{}, fn);
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
            std::size_t entryPtr = this->buckets[i];
            bool first = true;
            while (entryPtr > 0) {
                Entry *entry = this->entries + entryPtr - 1;
                if (!first) {
                    os << ", ";
                }
                first = false;
                os << entry->key << " -> " << entry->value;
                entryPtr = entry->next;
            }
            os << "}" << std::endl;
        }
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
    std::size_t *buckets;
    std::size_t bucketSize;
    Entry *entries;
    std::size_t entriesSize;
    std::size_t size;

    std::size_t findFreeEntry(std::size_t start) {
        if (this->size == this->entriesSize) {
            std::size_t newSize = this->entriesSize == 0 ? 16 : this->entriesSize << 1;
            Entry *newEntry = new Entry[newSize];
            for (std::size_t i = 0; i < this->entriesSize; i++) {
                newEntry[i] = std::move(this->entries[i]);
            }
            this->entries = newEntry;
            this->entriesSize = newSize;
        }
        while (this->entries[start].occupied) {
            start = (start + 1) % this->entriesSize;
        }
        return start;
    }
};

}