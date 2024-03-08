<< Peanotica`Core`

IndicesCandidateOfSlotType@TangentMf ^= {{a, b, c, d, e, f, g, h, i, j, k, l}, DefaultIndex};

FindIndicesSlots[Riemann1[_, _, _, _]] ^= {{1} -> TangentMf, {2} -> TangentMf, {3} -> TangentMf, {4} -> TangentMf};
SymmetryOfExpression@Riemann1[_, _, _, _] ^= RiemannSymmetricGenSet[1];

FindIndicesSlots[g1[inds__]] ^:= Array[{#} -> TangentMf &, Length@Hold@inds];
SymmetryOfExpression@g1[inds__] ^:= SymmetricGenSet @@ Range[Length@Hold@inds];

FindIndicesSlots[t1[inds__]] ^:= Array[{#} -> TangentMf &, Length@Hold@inds];

DefSimpleTensor[chris, {TangentMf, TangentMf, TangentMf}, {SCycles@{2, 3}}];

VerificationTest[
    ITensorReduceOneTerm[g1[b, a]],
    g1[a, b]
];

VerificationTest[
    ITensorReduceOneTerm[Riemann1[a, b, DI@c, DI@d] g1[c, d]],
    0
];

VerificationTest[
    ITensorReduceOneTerm[Riemann1[a, b, c, -d] Riemann1[-b, d, -c, -a], RenameDummies -> False],
    -Riemann1[a, b, c, d] Riemann1[DI[a], DI[c], DI[b], DI[d]]
];

VerificationTest[
    ITensorReduceOneTerm[t1[b, a, c], FreeIndexNames -> {}],
    t1[a, b, c]
];

VerificationTest[
    ITensorReduceOneTerm[Riemann1[TempIndex[1], TempIndex[2], TempIndex[3], TempIndex[4]] Riemann1[-TempIndex[2], -TempIndex[4], -TempIndex[3], -TempIndex[1]]],
    -Riemann1[a, b, c, d] Riemann1[DI[a], DI[c], DI[b], DI[d]]
];

VerificationTest[
    ITensorReduce[chris[d, DI[b], DI[TempIndex[1]]] chris[TempIndex[1], DI[a], DI[c]], UseMetricOnSlots -> {}],
    chris[d, DI[b], DI[e]]chris[e, DI[a], DI[c]]
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
    ETensor[g1[a], {a, None}] + ETensor[t1[a], {None, a}],
    ETensor[g1[a] + t1[b], {a, b}]
];

VerificationTest[
    ITensorTranspose[ETensor[Riemann1[a, b, c, d], {{a, b}, {c, d}}], {1, 1}],
    ETensor[Riemann1[a, b, a, b], {{a, b}}]
];

VerificationTest[
    ITensorTranspose[ETensor[Riemann1[a, b, c, d], {a, b, c, d}], {1, 1, 2, 2}],
    ETensor[Riemann1[a, a, c, c], {a, c}]
];

VerificationTest[
    ITensorOuter[Times, ETensor[Riemann1[a, b, c, d], {a, b, c, d}], ETensor[g1[a, b, c, d], {a, b, c, d}], {{2, 3}}],
    ETensor[g1[e, f, b, h]Riemann1[a, b, c, d], {a, b, c, d, e, f, h}]
];

VerificationTest[
    ITensorOuter[Times, ETensor[Riemann1[a, b, c, d], {a, b, c, d}], ETensor[g1[a, b, c, d], {a, b, c, d}], {{2, 3}, {1, 4}}],
    ETensor[g1[e, f, b, a]Riemann1[a, b, c, d], {a, b, c, d, e, f}]
];

VerificationTest[
    NITensorReduce[NITensor[Riemann1, {a, b, a, b}], {}],
    NITensor[ITensorSum[ITensorTranspose[Riemann1, {1, 2, 1, 2}], {1, 2}], {}]
];

VerificationTest[
    NITensorReduce[NITensor[Riemann1, {a, b, c, d}] + NITensor[t2, {b, a, a, d}], {a, b}],
    NITensor[ITensorSum[Riemann1, {3, 4}] + ITensorTranspose[ITensorSum[ITensorTranspose[t2, {1, 2, 2, 3}], {3}], {2, 1}], {a, b}]
];

VerificationTest[
    NITensorReduce[NITensor[t1, {a, b, c, d}] NITensor[t1, {e, f, g, h}] NITensor[t2, {a, b, c, d}] NITensor[t2, {e, f, g, h}], {}],
    NITensor[ITensorSum[ITensorOuter[Times, t1, t2, {{1, 1}, {2, 2}, {3, 3}, {4, 4}}], {1, 2, 3, 4}]^2, {}]
];

VerificationTest[
    MetricOfSlotType[TangentMf][a, DI@b] (Riemann1[e, b, c, d] + t1[e, b, d]),
    t1[e, a, d] + Riemann1[e, a, c, d]
];
