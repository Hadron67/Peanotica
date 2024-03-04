<< Peanotica`Core`

IndicesCandidateOfSlotType@TangentMf ^= {{a, b, c, d}, DefaultIndex};

FindIndicesSlots[Riemann1[_, _, _, _]] ^= {{1} -> TangentMf, {2} -> TangentMf, {3} -> TangentMf, {4} -> TangentMf};
SymmetryOfExpression@Riemann1[_, _, _, _] ^= RiemannSymmetricGenSet[1];

FindIndicesSlots[g1[inds__]] ^:= Array[{#} -> TangentMf &, Length@Hold@inds];
SymmetryOfExpression@g1[inds__] ^:= SymmetricGenSet @@ Range[Length@Hold@inds];

FindIndicesSlots[t1[inds__]] ^:= Array[{#} -> TangentMf &, Length@Hold@inds];

VerificationTest[
    ITensorReduceOneTerm[g1[b, a]],
    g1[a, b]
];

VerificationTest[
    ITensorReduceOneTerm[Riemann1[a, b, DI@c, DI@d] g1[c, d]],
    0
];

VerificationTest[
    ITensorReduceOneTerm[Riemann1[a, b, c, d] Riemann1[-b, -d, -c, -a], RenameDummies -> False],
    -Riemann1[a, b, c, d] Riemann1[DI[a], DI[c], DI[b], DI[d]]
];

VerificationTest[
    ITensorReduceOneTerm[t1[b, a, c], FreeIndexNames -> {}],
    t1[a, b, c]
];

VerificationTest[
    ITensorReduceOneTerm[Riemann1[#1, #2, #3, #4] Riemann1[-#2, -#4, -#3, -#1]],
    -Riemann1[a, b, c, d] Riemann1[DI[a], DI[c], DI[b], DI[d]]
];

VerificationTest[
    ContractMetric[Riemann1[a, b, c, d] MetricOfSlotType[TangentMf][DI@b, DI@d]],
    Riemann1[a, DI@d, c, d]
];

VerificationTest[
    ContractMetric[Riemann1[a, b, c, d] g1[DI@b, DI@d], HoldPattern@g1[_, _]],
    Riemann1[a, DI@d, c, d]
];

VerificationTest[
    ETensor[g1[a, b, c, DI@c], {{a, b}}] + ETensor[t1[a, b, d, DI@d], {{b, a}}] // ITensorReduce,
    ETensor[g1[a, b, c, DI[c]] + t1[b, a, c, DI[c]], {{a, b}}]
];

VerificationTest[
    ITensorTranspose[ETensor[Riemann1[a, b, c, d], {{a, b}, {c, d}}], {1, 1}],
    ETensor[Riemann1[a, b, a, b], {{a, b}}]
];

VerificationTest[
    ITensorTranspose[ETensor[Riemann1[a, b, c, d], {a, b, c, d}], {1, 1, 2, 2}],
    ETensor[Riemann1[a, a, c, c], {a, c}]
];