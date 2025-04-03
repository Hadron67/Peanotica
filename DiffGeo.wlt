<< Peanotica`DiffGeo`

ClearAll["Global`*"];

DerConstantQ[dimf] ^= True;
DerConstantQ[dimx] ^= True;

DefSimpleSlotType[mf, dimf, {Symbol /@ CharacterRange["a", "z"], DefaultIndex}];
DefSimpleSlotType[mx, dimx, {Symbol /@ CharacterRange["a", "z"], DefaultIndex}];
MetricOfSlotType[mf] ^= g1mf;
MetricOfSlotType[mx] ^= g1mx;

DefSimpleMetric[g1mf, mf, 1, DisplayName -> "g"];
DefSimpleMetric[g1mx, mx, 1, DisplayName -> "g"];

DefTensorDerivativeOperator[cdmf, {mf}, {}, DisplayName -> "D"];
DefTensorDerivativeOperator[cdmx, {mx}, {}, DisplayName -> "\[Del]"];
cdmf[g1mf[_, _], _] = 0;
cdmx[g1mx[_, _], _] = 0;

DefSimpleTensor[riemannMf, {mf, mf, mf, mf}, RiemannSymmetricGenSet[1], DisplayName -> "R"];
DefSimpleTensor[ricciMf, {mf, mf}, {SCycles@{1, 2}}, DisplayName -> "R"];
DefRiemannToRicciRules[riemannMf, ricciMf];
RicciOf[riemannMf] ^= ricciMf;
RicciScalarOf[ricciMf] ^= ricciScalarMf;
ricciScalarMf /: MakeBoxes[ricciScalarMf, StandardForm] = InterpretationBox["R", ricciScalarMf];
SetDelayed @@@ RiemannRicciRules[riemannMf, ricciMf, ricciScalarMf];

DerFunctionQ[h] ^= True;
DerFunctionQ[f] ^= True;

cdmx[r, _] ^= 0;
cdmx[t, _] ^= 0;

DefParametreDerivativeOperator[paramd];
paramd[r, v_] := Boole[r === v];
paramd[t, v_] := Boole[t === v];
paramd[g1mx[_, _], _] = 0;

gssMetric = SparseArray[{
    {1, 1} -> ETensor[-h[r], {Null, Null}],
    {2, 2} -> ETensor[1/f[r], {Null, Null}],
    {3, 3} -> ETensor[r^2 g1mx[DI@a, DI@b], {a, b}]
}];
gssMetricInv = SparseArray[{
    {1, 1} -> ETensor[-1/h[r], {Null, Null}],
    {2, 2} -> ETensor[f[r], {Null, Null}],
    {3, 3} -> ETensor[r^-2 g1mx[a, b], {a, b}]
}];
gsspd = {ETensor[paramd[Null, t], {Null}], ETensor[paramd[Null, r], {Null}], ETensor[cdmx[Null, DI@a], {a}]};

gssChris = LeviCivitaChristoffelValue[mf, {gsspd, 0}, gssMetric, gssMetricInv] // ITensorReduce // Map[Simplify, #, {3}] &;

VerificationTest[gssChris == SparseArray[{
    {1, 1, 2} -> ETensor[h'[r]/(2 h[r]), {Null, Null, Null}],
    {1, 2, 1} -> ETensor[h'[r]/(2 h[r]), {Null, Null, Null}],
    {2, 1, 1} -> ETensor[1/2 f[r] h'[r], {Null, Null, Null}],
    {2, 2, 2} -> ETensor[-(f'[r]/(2 f[r])), {Null, Null, Null}],
    {2, 3, 3} -> ETensor[-r f[r] g1mx[DI[a], DI[b]], {Null, a, b}],
    {3, 2, 3} -> ETensor[g1mx[a, DI[b]]/r, {a, Null, b}],
    {3, 3, 2} -> ETensor[g1mx[a, DI[b]]/r, {a, b, Null}]
}]];

gssRiemann = RiemannDifferenceValue[mf, {gsspd, 0}, gssChris] + SparseArray[{
    {3, 3, 3, 3} -> ETensor[k*SymmetricRiemann[g1mx, {DI@a, DI@b, DI@c, d}], {a, b, c, d}]
}] // ITensorReduce // Map[Simplify, #, {4}] & // SparseArray;

gssRicci = ITensorSum[ITensorTranspose[gssRiemann, {1, 3, 2, 3}], {3}] // ITensorReduce // Map[Simplify, #, {2}] & // SparseArray;
gssRicciScalar = ITensorSum[ITensorTranspose[ITensorFixedContract[Times, gssMetricInv, gssRicci, 1, 1], {1, 1}], {1}] // Simplify;

VerificationTest[
    Simplify[gssRicciScalar == (
        dimx ((-1 + dimx) k - (-1 + dimx) f[r] - 
        r f'[r])
    )/r^2 + (f[r] h'[r]^2)/(
        2 h[r]^2
    ) - (
        r f'[r] h'[r] + 
        2 f[r] (dimx h'[r] + r h''[r]))/(
        2 r h[r]
    )]
];

VerificationTest[
    ITensorOuter[Times, ETensor[ricciMf[LabelI[1], LabelI[2]], {Null, Null}], ETensor[ricciMf[LabelI[1], LabelI[2]], {Null, Null}], {}],
    ETensor[ricciMf[LabelI[1], LabelI[2]] ^ 2, {Null, Null, Null, Null}]
];

DefSimpleTensor[p, {mf}, {}];

VerificationTest[
    g1mf[DI@a, DI@b] /. MakeIndexRule[g1mf[a, b] -> p[a]p[b]p[c]p[DI@c], {a, b}] // ITensorReduce,
    p[DI@a]p[DI@b]p[c]p[DI@c]
];

VerificationTest[
    MakeIndexFunction[p[a]p[b]p[c]p[DI@c], {a, b}][DI@a, a] // ITensorReduce,
    p[a]p[DI@a]p[b]p[DI@b]
];
