#include "perm.h"
#include <iostream>
#include <random>
#include <cstring>

using namespace pperm;

#define TEST(test) { std::cout << "Running test " #test << std::endl; if (!test) { std::cout << "TEST FAILED" << std::endl; passed = false; }  }
#define EXPECT(expected, actual) {auto a1 = (expected); auto a2 = (actual); if (a1 != a2) {std::cout << "unexpected value on " << __FILE__ << ":" << __LINE__ << ", expected " << a1 << ", actual " << a2 << std::endl; return false;}}

static bool testOrderedMap(int seed) {
    AVLMap<int, int> map;
    std::default_random_engine randEngine(seed);
    std::uniform_int_distribution<int> distribe(0, 10000);
    for (int i = 0; i < 1000; i++) {
        int val = distribe(randEngine);
        map.insert(val, SimpleHashContext{}, val*val);
        if (!AVLNode<std::uint32_t>::checkHeight(map, map.getRoot(), std::cout)) return false;
    }
    auto ip = map.randomElement(distribe(randEngine));
    while (ip.isNodePresent()) {
        map.remove(ip);
        if (!AVLNode<std::uint32_t>::checkHeight(map, map.getRoot(), std::cout)) return false;
        ip = map.randomElement(distribe(randEngine));
    }

    map.clear();
    for (int i = 0; i < 100; i++) {
        map.insert(i*i, SimpleHashContext{}, i*i*i);
    }
    for (int i = 0; i < 100; i++) {
        auto ip2 = map.find(i*i, SimpleHashContext{});
        EXPECT(i*i*i, map.getValue(ip2));
    }
    return true;
}

static bool testHashMap(std::size_t seed) {
    HashTable<MapEntry<int, int>> map;
    std::default_random_engine randEngine(seed);
    std::uniform_int_distribution<int> distribe(0, 10000);
    for (std::size_t i = 0; i < 1000; i++) {
        int key = distribe(randEngine);
        map.putIfAbsent(key, SimpleHashContext{}, MapEntry<int, int>{key, key*key});
    }
    for (auto it = map.begin(); it != map.end(); ++it) {
        auto key = it->key;
        EXPECT(key*key, it->value);
    }
    const MapEntry<int, int> *keyPtr;
    while ((keyPtr = map.randomKey(distribe(randEngine))) != nullptr) {
        int key = keyPtr->key;
        auto pt = map.find(key, SimpleHashContext{});
        int value = map.getEntry(pt)->getValue().value;
        if (value != key*key) {
            std::cout << "wrong value on key " << key << std::endl;
            return false;
        }
        map.remove(pt);
    }
    for (int i = 0; i < 100; i++) {
        map.computeIfAbsent(i, SimpleHashContext{}, [](int i){ return MapEntry<int, int>{i, i*i}; });
    }
    for (int i = 0; i < 100; i++) {
        auto value = map.getEntry(map.find(i, SimpleHashContext{}))->getValue().value;
        if (value != i*i) {
            std::cout << "wrong value on key " << i << std::endl;
            return false;
        }
    }
    map.clear();
    if (map.find(1, SimpleHashContext{}).isNonNull()) {
        std::cout << "unexpected key" << std::endl;
        return false;
    }
    map.computeIfAbsent(1, SimpleHashContext{}, [](int i){ return MapEntry<int, int>{i, 236}; });
    if (!map.find(1, SimpleHashContext{}).isNonNull()) {
        std::cout << "unexpected key" << std::endl;
        return false;
    }
    return true;
}

static bool testSchreierVector() {
    PermutationSet genset(4);
    upoint_type base[] = {0, 2};
    genset.appendPermutation(true, {1, 0, 2, 3});
    genset.appendPermutation(true, {0, 1, 3, 2});
    genset.appendPermutation(false, {2, 3, 0, 1});

    PermutationStack stack(64);
    {
        StackedPermutation perm(stack, 4);
        perm.assign(false, {2, 1, 0, 3});
        EXPECT(false, isInGroup(stack, perm, genset.permutations, makeSlice(base, 2)));
        perm.assign(false, {1, 0, 3, 2});
        EXPECT(true, isInGroup(stack, perm, genset.permutations, makeSlice(base, 2)));
        perm.assign(true, {3, 2, 0, 1});
        EXPECT(true, isInGroup(stack, perm, genset.permutations, makeSlice(base, 2)));
        perm.assign(true, {3, 2, 1, 0});
        EXPECT(false, isInGroup(stack, perm, genset.permutations, makeSlice(base, 2)));
    }
    EXPECT(true, stack.isEmpty());
    return true;
}

static bool testJerrumFilter() {
    PermutationStack stack(7 * 16);
    PermutationList list(7);
    StackedPermutation tmp(stack, 7);
    list.push().identity().cycle(1, 3).cycle(2, 4);
    list.push().identity().cycle(1, 5, 2, 6).cycle(3, 4);
    list.push().identity().cycle(1, 4, 5).cycle(2, 3, 6);
    list.push().identity().cycle(1, 5, 4).cycle(2, 6, 3);
    list.push().identity().cycle(3, 4).cycle(5, 6);
    list.push().identity().cycle(1, 5).cycle(2, 6);
    JerrumBranching branching(7);
    std::deque<upoint_type> queue;
    for (auto perm : list) {
        branching.siftElement(stack, queue, perm);
    }
    list.clear();
    branching.collectLabels([&](PermutationView perm){ list.addPermutation(perm); });
    EXPECT(4, list.getSize());
    EXPECT(tmp.identity().cycle(1, 3).cycle(2, 4), list.get(0));
    EXPECT(tmp.identity().cycle(3, 4).cycle(5, 6), list.get(1));
    EXPECT(tmp.identity().cycle(3, 5).cycle(4, 6), list.get(2));
    EXPECT(tmp.identity().cycle(3, 6).cycle(4, 5), list.get(3));
    return true;
}

static PermutationSet projectivePlaneGenSet(PermutationStack &stack) {
    PermutationSet genset(7);
    StackedPermutation tmp(stack, 7);
    genset.addPermutation(tmp.identity().cycle(0, 1, 3, 4, 6, 2, 5));
    genset.addPermutation(tmp.identity().cycle(1, 3).cycle(2, 4));
    return genset;
}

static bool testSchreierSims() {
    PermutationStack stack(7 * 16);
    auto genset = projectivePlaneGenSet(stack);

    JerrumBranchingBuilder builder;
    builder.build(stack, genset.permutations);
    PermutationList sgs(7);
    builder.branching.collectLabels([&](PermutationView perm){ sgs.addPermutation(perm); });
    StackedPermutation tmp(stack, 7);
    EXPECT(6, sgs.getSize());
    EXPECT(tmp.identity().cycle(0, 1, 3, 4, 6, 2, 5), sgs.get(0));
    EXPECT(tmp.identity().cycle(1, 2).cycle(5, 6), sgs.get(1));
    EXPECT(tmp.identity().cycle(1, 3).cycle(2, 4), sgs.get(2));
    EXPECT(tmp.identity().cycle(3, 4).cycle(5, 6), sgs.get(3));
    EXPECT(tmp.identity().cycle(3, 5).cycle(4, 6), sgs.get(4));
    EXPECT(tmp.identity().cycle(3, 6).cycle(4, 5), sgs.get(5));
    return true;
}

static bool testCyclesConvert() {
    PermutationStack stack(32);
    StackedPermutation perm{stack, 7};
    perm.identity().cycle(0, 5, 6).cycle(1, 3, 4, 2);
    CyclesConverter converter;
    converter.convert(perm);
    std::cout << converter << std::endl;
    return true;
}

static bool testBaseChange1() {
    constexpr int DEG = 4;
    PermutationStack stack(DEG * 16);
    StackedPermutation tmp{stack, DEG};
    PermutationList genset(4);
    genset.push().identity().cycle(0, 1);
    genset.push().identity().cycle(1, 2);
    genset.push().identity().cycle(2, 3);
    upoint_type base[]{0, 1, 2, 3};

    BaseChanger changer;
    changer.setSGS(makeSlice(base, 4), genset);
    changer.interchange(1, stack);
    EXPECT(true, changer.genset.findPermutation(tmp.identity().cycle(1, 3)).isPresent());

    return true;
}

static bool testBaseChange2() {
    constexpr int DEG = 21;
    PermutationStack stack(DEG * 16);
    StackedPermutation tmp(stack, DEG);
    PermutationList genset(DEG);

    genset.push().identity().cycle(0, 7, 8).cycle(1, 10, 14).cycle(2, 9, 11).cycle(3, 13, 18).cycle(4, 15, 16).cycle(5, 20, 19).cycle(6, 12, 17);
    genset.push().identity().cycle(8, 17, 19).cycle(11, 18, 16);
    genset.push().identity().cycle(9, 20, 10).cycle(12, 15, 13);
    genset.push().identity().cycle(7, 12, 20).cycle(9, 13, 15);
    genset.push().identity().cycle(1, 5, 2).cycle(3, 4, 6);
    genset.push().identity().cycle(11, 19, 14).cycle(16, 18, 17);
    upoint_type base[]{0, 8, 7, 9, 1, 11};

    BaseChanger changer;
    changer.setSGS(makeSlice(base, 6), genset);
    changer.interchange(0, stack);
    EXPECT(true, changer.genset.findPermutation(tmp.identity().cycle(0, 1, 2, 3, 5, 4, 6)).isPresent());

    return true;
}

int main(int argc, const char *args[]) {
    bool passed = true;
    TEST(testHashMap(9));
    TEST(testOrderedMap(9));
    TEST(testSchreierVector());
    TEST(testSchreierSims());
    TEST(testJerrumFilter());
    TEST(testCyclesConvert());
    TEST(testBaseChange1());
    TEST(testBaseChange2());
    if (passed) {
        std::cout << "All tests passed" << std::endl;
        return 0;
    } else {
        std::cout << "Some tests failed" << std::endl;
        return -1;
    }
}