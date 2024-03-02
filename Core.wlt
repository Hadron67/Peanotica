<< Peanotica`Core`

FindIndicesSlots[Riemann1[_, _, _, _]] ^= {{None, 1}, {None, 2}, {None, 3}, {None, 4}};
SymmetryOfExpression@Riemann1[_, _, _, _] ^= RiemannSymmetricGenSet[1];

FindIndicesSlots[g1[inds__]] ^:= Array[{None, #} &, Length@Hold@inds];
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
