#include <numeric>
#include "perm.h"

using namespace pperm;

static void enumerateRiemannScalar(Slice<unsigned int> ders, unsigned int frees) {
    const upoint_type permLen = std::accumulate(ders.begin(), ders.end(), 0);
}