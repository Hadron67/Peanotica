#include "perm.h"
#include <iostream>
#include <random>
#include <cstring>
#include <optional>

using namespace pperm;
using namespace pperm::meta;

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
    PermutationList genset(4);
    upoint_type base[] = {0, 2};
    genset.push().identity().assign(true, {1, 0, 2, 3});
    genset.push().identity().assign(true, {1, 0, 2, 3});
    genset.push().identity().assign(true, {0, 1, 3, 2});
    genset.push().identity().assign(false, {2, 3, 0, 1});

    PermutationStack stack(64);
    {
        StackedPermutation perm(stack, 4);
        perm.assign(false, {2, 1, 0, 3});
        EXPECT(false, isInGroup(stack, perm, genset, makeSlice(base, 2)));
        perm.assign(false, {1, 0, 3, 2});
        EXPECT(true, isInGroup(stack, perm, genset, makeSlice(base, 2)));
        perm.assign(true, {3, 2, 0, 1});
        EXPECT(true, isInGroup(stack, perm, genset, makeSlice(base, 2)));
        perm.assign(true, {3, 2, 1, 0});
        EXPECT(false, isInGroup(stack, perm, genset, makeSlice(base, 2)));
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

static bool testSchreierSims2() {
    constexpr int DEG = 5;
    auto genset = GenSet<SCycles<List<0, 1, 3, 4, 2>>, SCycles<List<1, 4, 3, 2>>, SCycles<List<0, 1, 3>, List<2, 4>>>::build(DEG);
    PermutationStack stack(DEG * 16);

    JerrumBranchingBuilder builder;
    builder.formatter.useCycles = true;
    builder.log = &std::cout;
    builder.build(stack, genset);
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
    changer.setSGS(genset);
    changer.interchange(Slice(base, 1), 1, 2, stack);
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
    // upoint_type base[]{0, 8, 7, 9, 1, 11};

    BaseChanger changer;
    changer.setSGS(genset);
    changer.interchange(Slice<upoint_type>{}, 0, 8, stack);
    EXPECT(true, changer.genset.findPermutation(tmp.identity().cycle(0, 1, 2, 3, 5, 4, 6)).isPresent());

    return true;
}

static bool testBaseChange3() {
    constexpr int DEG = 9;
    auto genset = GenSet<SCycles<List<0, 4>, List<1, 5>, List<2, 3>>, SCycles<List<3, 7>, List<4, 6>, List<5, 8>>>::build(DEG);
    PermutationStack stack(DEG * 8);
    auto tmp = stack.pushStacked(DEG);
    upoint_type base[]{0, 1, 2, 3, 4, 5, 6, 7, 8};

    BaseChanger changer;
    changer.setSGS(genset);
    changer.moveToFirstDirectly(makeSlice(base, DEG), 3, stack);
    EXPECT(true, changer.genset.findPermutation(tmp.identity().cycle(0, 6).cycle(1, 8).cycle(2, 7)).isPresent());

    upoint_type base2[]{0, 1, 2, 3, 4, 5, 6, 7, 8};
    changer.setSGS(genset);
    changer.moveToFirstDirectly(makeSlice(base2, DEG), 4, stack);
    EXPECT(true, changer.genset.findPermutation(tmp.identity().cycle(0, 6).cycle(1, 8).cycle(2, 7)).isPresent());
    return true;
}

static bool testCompleteBaseChange() {
    constexpr int DEG = 4;
    PermutationStack stack(DEG * 8);
    auto genset = Symmetric<0, 1, 2, 3>::build(DEG);
    upoint_type base[]{0, 1, 2, 3};
    std::size_t baseLen = 4;
    upoint_type newBase[]{3};
    BaseChanger changer;
    changer.setSGS(genset);
    changer.completeBaseChange(MutableSlice(base, baseLen), Slice(newBase, 1), stack);
    EXPECT(3, base[0]);
    return true;
}

static bool testGroupOrder() {
    auto genset = GenSet<Neg<SCycles<List<0, 1>>>, Neg<SCycles<List<2, 3>>>, SCycles<List<0, 2>, List<1, 3>>>::build(4);
    GroupOrderCalculator calc;
    calc.setGroup(genset);
    EXPECT(8, calc.order());
    return true;
}

namespace {
    struct Zero {};
    template<typename T> struct OptPermutationBuilder {
        static std::optional<StackedPermutation> build(PermutationStack &stack, std::size_t permLen) {
            auto ret = stack.pushStacked(permLen);
            T::assignPermutation(ret);
            return ret;
        }
    };
    template<>
    struct OptPermutationBuilder<Zero> {
        static std::optional<StackedPermutation> build(PermutationStack &stack, std::size_t permLen) {
            return std::nullopt;
        }
    };
    struct OptPermutationView {
        const std::optional<StackedPermutation> &perm;
        void print(std::ostream &os, PermutationFormatter &formatter) const {
            if (this->perm.has_value()) {
                this->perm.value().print(os, formatter);
            } else {
                os << "0";
            }
        }
    };

    inline std::ostream &operator << (std::ostream &os, const OptPermutationView &perm) {
        if (perm.perm.has_value()) {
            os << perm.perm.value();
        } else {
            os << "0";
        }
        return os;
    }

    struct DoubleCosetRepTester {
        bool verbose;
        PermutationStack stack;
        PermutationList gensetS, gensetD;
        DoubleCosetRepTester() = default;
        DoubleCosetRepTester(std::size_t permLen) {
            this->setPermutationLength(permLen);
        }
        void setPermutationLength(std::size_t len) {
            this->stack.setBlockSize(len * 16);
            this->gensetS.setPermutationLength(len);
            this->gensetD.setPermutationLength(len);
        }
        bool run(PermutationView input, const std::optional<StackedPermutation> &expected) {
            this->gensetSProvider.baseChanger = &this->baseChanger;
            this->gensetDProvider.baseChanger = &this->baseChanger;
            this->gensetSProvider.setSGS(this->gensetS);
            this->gensetDProvider.setSGS(this->gensetD);
            if (this->verbose) {
                this->solver.log = &std::cout;
            } else {
                this->solver.log = nullptr;
            }
            this->solver.permFormatter.useCycles = true;
            auto actual = this->solver.solve(this->gensetSProvider, this->gensetDProvider, input);
            if (expected != actual) {
                auto &formatter = this->solver.permFormatter;
                std::cout << "unexpected results on input g = " << input
                    << ", S = " << formatter.formatRef(gensetS)
                    << ", D = " << formatter.formatRef(gensetD)
                    << ", " << std::endl
                    << "    expected: " << OptPermutationView{expected}
                    << ", actual " << OptPermutationView{actual}
                    << std::endl;
                return false;
            }
            return true;
        }
        private:
        BaseChanger baseChanger;
        BaseChangingStrongGenSetProvider gensetSProvider, gensetDProvider;
        DoubleCosetRepresentativeSolver solver;
    };
};

template<unsigned int N, typename InputPerm, typename S, typename D, typename ExpectedPerm, bool verbose = false>
static inline bool doubleCosetRepCase() {
    DoubleCosetRepTester tester(N);
    tester.verbose = verbose;
    S::buildInPlace(tester.gensetS);
    D::buildInPlace(tester.gensetD);
    auto input = tester.stack.pushStacked(N);
    InputPerm::assignPermutation(input);
    auto expected = OptPermutationBuilder<ExpectedPerm>::build(tester.stack, N);

    return tester.run(input, expected);
}

static bool doubleCosetRepRiemannMonomialCaseGeneral(DoubleCosetRepTester &tester, std::size_t n, std::size_t freen, PermutationView input, const std::optional<StackedPermutation> &expected) {
    std::size_t permLen = n * 4;
    auto &gensetS = tester.gensetS, &gensetD = tester.gensetD;
    for (std::size_t b = 0; b < permLen; b += 4) {
        gensetS.push().identity().setNegative(true).cycle(b, b + 1);
        gensetS.push().identity().setNegative(true).cycle(b + 2, b + 3);
        gensetS.push().identity().cycle(b, b + 2).cycle(b + 1, b + 3);
        if (b + 4 < permLen) {
            gensetS.push().identity().cycle(b, b + 4).cycle(b + 1, b + 5).cycle(b + 2, b + 6).cycle(b + 3, b + 7);
        }
    }
    for (std::size_t i = freen; i < permLen; i += 2) {
        gensetD.push().identity().cycle(i, i + 1);
        if (i + 2 < permLen) {
            gensetD.push().identity().cycle(i, i + 2).cycle(i + 1, i + 3);
        }
    }
    return tester.run(input, expected);
}

template<std::size_t n, std::size_t freen, typename Input, typename Expected, bool verbose = false>
static bool doubleCosetRepRiemannMonomialCase() {
    DoubleCosetRepTester tester(n * 4);
    tester.verbose = verbose;
    auto input = tester.stack.pushStacked(n * 4);
    Input::assignPermutation(input);
    auto expected = OptPermutationBuilder<Expected>::build(tester.stack, n * 4);

    return doubleCosetRepRiemannMonomialCaseGeneral(tester, n, freen, input, expected);
}

static bool testDoubleCosetRep() {
    if(!doubleCosetRepCase<
        4,
        Images<1, 3, 2, 0>,
        RiemannSymmetric<0, 1, 2, 3>,
        Symmetric<1, 2>,
        Neg<Images<0, 1, 2, 3>>
    >()) return false;
    if (!doubleCosetRepCase<
        4,
        Images<1, 3, 2, 0>,
        RiemannSymmetric<0, 1, 2, 3>,
        Symmetric<1, 3>,
        Zero
    >()) return false;
    if (!doubleCosetRepCase<
        8,
        Images<0, 2, 4, 6, 1, 3, 7, 5>,
        Join<RiemannSymmetric<0, 1, 2, 3>, RiemannSymmetric<4, 5, 6, 7>, GenSet<SCycles<List<0, 4>, List<1, 5>, List<2, 6>, List<3, 7>>>>,
        Join<Symmetric<0, 1>, Symmetric<2, 3>, Symmetric<4, 5>, Symmetric<6, 7>>,
        Neg<Images<0, 2, 4, 6, 1, 3, 5, 7>>
    >()) return false;
    if (!doubleCosetRepCase<
        4,
        Images<2, 3, 1, 0>,
        RiemannSymmetric<0, 1, 2, 3>,
        GenSet<>,
        Neg<Images<0, 1, 2, 3>>
    >()) return false;
    if (!doubleCosetRepRiemannMonomialCase<1, 2, Images<2, 1, 0, 3>, Neg<Images<0, 2, 1, 3>>>()) return false;
    if (!doubleCosetRepRiemannMonomialCase<2, 0, Images<5, 6, 1, 3, 2, 4, 7, 0>, Images<0, 2, 4, 6, 1, 5, 3, 7>>()) return false;
    if (!doubleCosetRepRiemannMonomialCase<2, 4, Images<7, 2, 5, 0, 4, 6, 3, 1>, Neg<Images<0, 4, 2, 6, 1, 3, 5, 7>>>()) return false;
    if (!doubleCosetRepRiemannMonomialCase<
        5,
        0,
        Images<19, 7, 2, 17, 11, 4, 3, 13, 18, 5, 14, 6, 0, 16, 1, 12, 9, 15, 10, 8>,
        Neg<Images<0, 2, 1, 4, 3, 6, 8, 10, 5, 7, 12, 14, 9, 13, 11, 16, 15, 18, 17, 19>>
    >()) return false;
    if (!doubleCosetRepRiemannMonomialCase<
        5,
        10,
        Images<8, 15, 11, 5, 3, 7, 19, 14, 1, 18, 16, 12, 13, 17, 9, 10, 4, 0, 2, 6>,
        Neg<Images<0, 4, 2, 6, 1, 10, 12, 14, 3, 7, 11, 16, 5, 18, 8, 17, 9, 19, 13, 15>>
    >()) return false;
    return true;
}

int main(int argc, const char *args[]) {
    bool passed = true;
    TEST(testHashMap(9));
    TEST(testOrderedMap(9));
    TEST(testSchreierVector());
    TEST(testSchreierSims());
    TEST(testSchreierSims2());
    TEST(testJerrumFilter());
    TEST(testCyclesConvert());
    TEST(testBaseChange1());
    TEST(testBaseChange2());
    TEST(testBaseChange3());
    TEST(testCompleteBaseChange());
    TEST(testGroupOrder());
    TEST(testDoubleCosetRep());
    if (passed) {
        std::cout << "All tests passed" << std::endl;
        return 0;
    } else {
        std::cout << "Some tests failed" << std::endl;
        return -1;
    }
}