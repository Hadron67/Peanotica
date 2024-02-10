#include "perm.h"
#include <iostream>
#include <random>
#include <cstring>

using namespace pperm;

#define TEST(test) { std::cout << "Running test " #test << std::endl; if (!test) { std::cout << "TEST FAILED" << std::endl; passed = false; }  }
#define EXPECT(expected, actual) {auto a1 = (expected); auto a2 = (actual); if (a1 != a2) {std::cout << "unexpected value on " << __LINE__ << ", expected " << a1 << ", actual " << a2 << std::endl; return false;}}

static bool testOrderedMap(int seed) {
    AVLMap<int, int> map;
    std::default_random_engine randEngine(seed);
    std::uniform_int_distribution<int> distribe(0, 10000);
    for (int i = 0; i < 1000; i++) {
        int val = distribe(randEngine);
        map.insert(val, SimpleHashContext<int>{}, val*val);
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
        map.insert(i*i, SimpleHashContext<int>{}, i*i*i);
    }
    for (int i = 0; i < 100; i++) {
        auto ip2 = map.find(i*i, SimpleHashContext<int>{});
        EXPECT(i*i*i, map.getValue(ip2));
    }
    return true;
}

static bool testHashMap(std::size_t seed) {
    HashMap<int, int> map;
    // HashTable<MapEntry<int, int>> map;
    std::default_random_engine randEngine(seed);
    std::uniform_int_distribution<int> distribe(0, 10000);
    for (std::size_t i = 0; i < 1000; i++) {
        int key = distribe(randEngine);
        map.putIfAbsent(key, SimpleHashContext<int>{}, key*key);
    }
    for (auto it = map.begin(); it != map.end(); ++it) {
        auto key = it.key();
        EXPECT(key*key, it.value());
    }
    const int *keyPtr;
    while ((keyPtr = map.randomKey(distribe(randEngine))) != nullptr) {
        int key = *keyPtr;
        auto pt = map.find(key, SimpleHashContext<int>{});
        int value = map.getEntry(pt)->getValue();
        if (value != key*key) {
            std::cout << "wrong value on key " << key << std::endl;
            return false;
        }
        map.remove(pt);
    }
    for (int i = 0; i < 100; i++) {
        map.computeIfAbsent(i, SimpleHashContext<int>{}, [=]{ return i*i; });
    }
    for (int i = 0; i < 100; i++) {
        auto value = map.getEntry(map.find(i, SimpleHashContext<int>{}))->getValue();
        if (value != i*i) {
            std::cout << "wrong value on key " << i << std::endl;
            return false;
        }
    }
    map.clear();
    if (map.find(1, SimpleHashContext<int>{}).isNonNull()) {
        std::cout << "unexpected key" << std::endl;
        return false;
    }
    map.computeIfAbsent(1, SimpleHashContext<int>{}, []{ return 236; });
    if (!map.find(1, SimpleHashContext<int>{}).isNonNull()) {
        std::cout << "unexpected key" << std::endl;
        return false;
    }
    return true;
}

static void testSchreierVector() {
    Array<std::uint32_t> array;

}

int main(int argc, const char *args[]) {
    bool passed = true;
    TEST(testHashMap(9));
    TEST(testOrderedMap(9));
    if (passed) {
        std::cout << "All tests passed" << std::endl;
        return 0;
    } else {
        std::cout << "Some tests failed" << std::endl;
        return -1;
    }
}