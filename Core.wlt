<< Peanotica`Core`

FindIndicesSlots[Riemann1[_, _, _, _]] ^= {{TangentMf, 1}, {TangentMf, 2}, {TangentMf, 3}, {TangentMf, 4}};
SymmetryOfExpression@Riemann1[_, _, _, _] ^= RiemannSymmetricGenSet[1];

FindIndicesSlots[g1[inds__]] ^:= Array[{TangentMf, #} &, Length@Hold@inds];
SymmetryOfExpression@g1[inds__] ^:= SymmetricGenSet @@ Range[Length@Hold@inds];

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
    ContractMetric[Riemann1[a, b, c, d] MetricOfSlotType[TangentMf][DI@b, DI@d]],
    Riemann1[a, DI@d, c, d]
];

VerificationTest[
    ContractMetric[Riemann1[a, b, c, d] g1[DI@b, DI@d], HoldPattern@g1[_, _]],
    Riemann1[a, DI@d, c, d]
];
