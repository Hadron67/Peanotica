BeginPackage["Peanotica`DiffGeo`", {"Peanotica`Perm`", "Peanotica`Core`"}];

Scan[Unprotect@#; ClearAll@#; &, Names@{$Context <> "*"}];

LeviCivitaChristoffelDer::usage = "LeviCivitaChristoffel[cd, der, metric]";
LeviCivitaChristoffel::usage = "LeviCivitaChristoffel[cd, metric] represents the Christoffel tensor relating the possibly non-metric compatible covariant derivative cd and the Levi-Civita connection. LeviCivitaChristoffel[...][a, DI@b, DI@c] gives the expression.";
RiemannDifferencePart1;
RiemannDifferencePart2;
RiemannDifference::usage = "RiemannDifference[cd, christoffel] represents the difference of the Riemann tensors of cd and cd1, where christoffel is the Christoffel tensor relation cd and cd1.";
CovDDifference::usage = "CovDDifference[expr, chris, a] represents the difference ";
CovDCommutatorNoTorsion::usage = "CovDCommutatorNoTorsion[expr, a, b, riemann]";
CovDCommutatorOfRiemann::usage = "CovDCommutator[riemann]";
SortCovD::usage = "SortCovD[expr, cd, commutator]";

Cocurvature::usage = "";
Cotorsion::usage = "";

DerConstantQ::usage = "DerConstantQ[expr] gives true if expr the derivative operator acting on expr should give 0.";
DerFunctionQ::usage = "DerFunctionQ[fn] gives true if fn should be treated as a scalar function by the derivative operators. Derivative[...][fn] is always treated so.";
DerConstants;
DerFunctions;
DerivativeExpandableQ;
ExpandDerivative;
ExpandDerivativeWithRest;
ExpandDerivativeRules;

SymmetriedDer;
AllowPassThrough;
DefParametreDerivativeOperator::usage = "";
DefTensorDerivativeOperator::usage = "DefTensorDerivativeOperator[op, slotType].";

CBTensor::usage = "CBTensor[expr, slots]";
ICovD::usage = "ICovD[expr, a, value]";
NIChristoffel::usage = "NIChristoffel[value, slot1, slot2]";
NITensorCovD::usage = "NITensorCovD[expr, value, inds, ind]";
EnsureUpDownIndices::usage = "EnsureUpDownIndices[tensor, slots, newSlotSigns, slotToMetric]";
AdaptNITensor::usage = "AdaptNITensor[value, slots, inds, metricProvider]";
AdaptNITensorCovD::usage = "AdaptCovD";

LeviCivitaChristoffelValue::usage = "LeviCivitaChristoffelValue[slot, cd, metric, metricInv]";
RiemannDifferenceValue::usage = "RiemannDifferenceValue[slot, cd, chris]";
SymmetricRiemann::usage = "SymmetricRiemann[metric, a, b, c, d]";

RiemannToRicciRules::usage = "RiemannToRicciRules[riemann, ricci]";
RicciToRicciScalarRules::usage = "RicciToRicciScalarRules[ricci, ricciScalar]";
RiemannRicciRules::usage = "RiemannRicciRules[riemann, ricci, ricciScalar]";
RiemannOf;
RicciOf;
RicciScalarOf;
DefCurvatureTensors;

RiemannScalars;

DefPerturbationOperator::usage = "DefPerturbationOperator[symbol]";
VarInverseMatrix::usage = "VarInverseMatrix[invMat[a, b], varmat, order]";
PredefinedSlotType;
PredefinedCovD;
PredefinedMetric;
ShiftedMetric;
ShiftedInverseMetric;
PerturbShiftedMetric::usage = "PerturbShiftedMetric[expr, n]";
PerturbationLeviCivitaChristoffel::usage = "PerturbationLeviCivitaChristoffel[cd, metric, n]";
PerturbationLeviCivitaRiemann::usage = "";
ExpandMetricPerturbation::usage = "ExpandMetricPerturbation[expr, {pert, metric, metricPert}]";
ExpandRiemannPerturbation::usage = "ExpandRiemannPerturbation[expr, {pert, metric, metricPert}, {cd, riemann, ricci, ricciScalar}]";

Begin["`Private`"];

LeviCivitaChristoffelDer[cd_, der_, metric_] := LeviCivitaChristoffelDer[cd, der, metric, metric];
LeviCivitaChristoffelDer[cd_, der_, metric_, metricInv_][a_, b_, c_] := With[{
    d = GetUniqueIndexOfSlotType@Null
}, 1/2 metricInv[a, d](cd[der@metric[DI@d, c], b] + cd[der@metric[b, DI@d], c] - cd[der@metric[b, c], DI@d])];
SyntaxInformation@LeviCivitaChristoffelDer = {"ArgumentsPattern" -> {_, _, _, _.}};

LeviCivitaChristoffel[cd_, metric_] := LeviCivitaChristoffelDer[cd, Identity, metric];
LeviCivitaChristoffel[cd_, metric_, metricInv_] := LeviCivitaChristoffelDer[cd, Identity, metric, metricInv];
SyntaxInformation@LeviCivitaChristoffel = {"ArgumentsPattern" -> {_, _, _.}};

RiemannDifferencePart1[cd_, chris_][a_, b_, c_, d_] := -cd[chris[d, b, c], a] + cd[chris[d, a, c], b];
SyntaxInformation@RiemannDifferencePart1 = {"ArgumentsPattern" -> {_, _}};

RiemannDifferencePart2[chris_][a_, b_, c_, d_] := Function[{e},
    chris[e, a, c]chris[d, b, DI@e] - chris[e, b, c]chris[d, a, DI@e]
]@GetUniqueIndexOfSlotType@Null;
SyntaxInformation@RiemannDifferencePart2 = {"ArgumentsPattern" -> {_}};

RiemannDifference[cd_, chris_][a_, b_, c_, d_] := RiemannDifferencePart1[cd, chris][a, b, c, d] + RiemannDifferencePart2[chris][a, b, c, d];
SyntaxInformation@RiemannDifference = {"ArgumentsPattern" -> {_, _}};

CovDDifferenceTerm[tensor_, inds_, chrisProvider_, a_ -> DI@slot0_, d_][ind_ -> DI@slot_, pos_] := -LookupListOrApply[chrisProvider, {slot0, slot}][d, DI@a, DI@ind] * tensor @@ FromNITensorIndex@ReplacePart[inds, pos -> (d -> DI@slot)];
CovDDifferenceTerm[tensor_, inds_, chrisProvider_, a_ -> DI@slot0_, d_][ind_ -> slot_?NonDIQ, pos_] := LookupListOrApply[chrisProvider, {slot0, slot}][ind, DI@a, DI@d] * tensor @@ FromNITensorIndex@ReplacePart[inds, pos -> (d -> slot)];
CovDDifference[tensor_, inds_, chrisProvider_, a_] := Total[MapIndexed[CovDDifferenceTerm[tensor, inds, chrisProvider, a, GetUniqueIndexOfSlotType@Null], inds]];
SyntaxInformation@CovDDifference = {"ArgumentsPattern" -> {_, _, _, _}};

CovDCommutatorNoTorsionTerm[expr_, a_, b_, d_, riemann_][pos_ -> type_] := With[{
    ind = Extract[expr, {pos}][[1]]
}, If[Head@ind === DI,
    riemann[a, b, ind, d] * ReplacePart[expr, pos -> DI@d],
    -riemann[a, b, DI@d, ind] * ReplacePart[expr, pos -> d]
]];
CovDCommutatorNoTorsion[expr_, a_, b_, riemann_] := Total[CovDCommutatorNoTorsionTerm[expr, a, b, GetUniqueIndexOfSlotType@Null, riemann] /@ FindIndicesSlots@expr];
SyntaxInformation@CovDCommutatorNoTorsion = {"ArgumentsPattern" -> {_, _, _, _}};

CovDCommutatorOfRiemann[riemann_][expr_, a_, b_] := CovDCommutatorNoTorsion[expr, a, b, riemann];
SyntaxInformation@CovDCommutatorOfRiemann = {"ArgumentsPattern" -> {_}};

SortCovD[expr_, cd_, commutator_] := SortCovD[expr, cd, commutator, False];
SortCovD[expr_, cd_, commutator_, reverse_] := expr //. cd[cd[expr2_, a_], b_] :> cd[cd[expr2, b], a] + commutator[expr2, b, a] /; Xor[!OrderedQ@{a, b}, reverse];
SyntaxInformation@SortCovD = {"ArgumentsPattern" -> {_, _, _, _.}};

DerConstantQ[_?NumberQ] = True;
SyntaxInformation@DerConstantQ = {"ArgumentsPattern" -> {_}};

DerFunctionQ[Times] = True;
DerFunctionQ[Power] = True;
SyntaxInformation@DerFunctionQ = {"ArgumentsPattern" -> {_}};

MapDerivativeOnFnDerivative[der_, ders_, fn_, {args__}] := With[{
    mat = IdentityMatrix@Length@{args}
},
    Total@MapIndexed[der[#1, (Derivative @@ (mat[[#2[[1]]]] + ders))[fn][args]] &, {args}]
];

Options[ExpandDerivativeRules] = {
    DerConstants -> {_?DerConstantQ},
    DerFunctions -> {_?DerFunctionQ}
};
ExpandDerivativeRules[lhs_ :> rhs_, opt : OptionsPattern[]] := With[{
    exprPatName = Cases[lhs, Verbatim[PatternTest][Verbatim[Pattern][p_, Blank[]], DerivativeExpandableQ] :> p, {0, Infinity}][[1]]
}, With[{
    lhsFn = Function @@ {{expr}, lhs /. Verbatim[PatternTest[Pattern[exprPatName, Blank[]], DerivativeExpandableQ]] -> expr},
    patPlus = expr_Plus,
    patList = expr_List,
    patProd = prod_?ProductQ[args__],
    patDer = Derivative[ders__][fn_][args__],
    indexScopePat = HoldPattern@IndexScope[expr_],
    zeroDerValue = rhs /. {ExpandDerivative[_, exprPatName] -> 0, ExpandDerivativeWithRest[_, exprPatName] -> 0}
}, Join[{
    (lhsFn[patPlus] :> rhs) //. {
        ExpandDerivative[fn2_, exprPatName] :> (fn2 /@ expr),
        ExpandDerivativeWithRest[fn2_, exprPatName] :> (fn2[#, 1] & /@ expr)
    },
    (lhsFn[patList] :> rhs) //. {
        ExpandDerivative[fn2_, exprPatName] :> (fn2 /@ expr),
        ExpandDerivativeWithRest[fn2_, exprPatName] :> (fn2[#, 1] & /@ expr)
    },
    (lhsFn[patDer] :> rhs) /. {
        ExpandDerivative[fn2_, exprPatName] :> MapDerivativeOnFnDerivative[#2 * fn2@#1 &, {ders}, fn, {args}],
        ExpandDerivativeWithRest[fn2_, exprPatName] :> MapDerivativeOnFnDerivative[fn2, {ders}, fn, {args}]
    },
    (lhsFn[indexScopePat] :> rhs) /. {
        ExpandDerivative[fn2_, exprPatName] :> fn2[ReplaceDummiesToUnique@expr],
        ExpandDerivativeWithRest[fn2_, exprPatName] :> fn2[ReplaceDummiesToUnique@expr, 1]
    },
    (lhsFn[_?NumberQ] -> zeroDerValue)
},
    lhsFn[#] -> zeroDerValue & /@ OptionValue@DerConstants,
    With[{fnPat = (fn : #)[args__]},
        lhsFn[fnPat] :> rhs /. {
            ExpandDerivative[fn2_, exprPatName] :> MapDerivativeOnFnDerivative[#2 * fn2@#1 &, 0, fn, {args}],
            ExpandDerivativeWithRest[fn2_, exprPatName] :> MapDerivativeOnFnDerivative[fn2, 0, fn, {args}]
        }
    ] & /@ OptionValue@DerFunctions
]]];
SyntaxInformation@ExpandDerivativeRules = {"ArgumentsPattern" -> {_, OptionsPattern[]}};
SyntaxInformation@ExpandDerivative = {"ArgumentsPattern" -> {_, _}};
SyntaxInformation@ExpandDerivativeWithRest = {"ArgumentsPattern" -> {_, _}};

Options[DefTensorDerivativeOperator] = Join[Options[ExpandDerivativeRules], {
    DisplayName -> "\[PartialD]",
    SymmetriedDer -> False,
    AllowPassThrough -> True (* any usecase for setting this to False ? *)
}];
SymmetryOfSymmetrizedDer[sym_[expr_, inds__], singleSym_] := With[{
    subExprAndInd = NestWhile[{
        Extract[#[[1]], {1, 1}, Hold],
        #[[2]] + 1
    } &, {Hold@expr, 1}, MatchQ[#[[1]], Hold@sym[_, __]] &],
    exprSlotCount = Length@FindIndicesSlots@expr
},
    Join[
        Extract[subExprAndInd[[1]], 1, SymmetryOfExpression],
        Join @@ Array[ShiftPermutation[singleSym, (# - 1) * Length@{inds}] &, subExprAndInd[[2]]],
        ShiftPermutation[BlockSymmetricGenSet @@ Partition[Range[Length@{inds} * subExprAndInd[[2]]], Length@{inds}], exprSlotCount]
    ]
];
SetAttributes[SymmetryOfSymmetrizedDer, HoldAll];
DefTensorDerivativeOperator[sym_Symbol, slots_List, symmetry_List, opt : OptionsPattern[]] := (
    SetDelayed @@@ ExpandDerivativeRules[sym[expr_?DerivativeExpandableQ, inds__] :> ExpandDerivative[sym[#, inds] &, expr], FilterRules[{opt}, Options@ExpandDerivativeRules]];
    With[{
        len = Length@slots
    }, sym[Null, a__][expr_] := sym[expr, a] /; Length@{a} === len];
    With[{
        slotsAndPos = MapIndexed[#2 + 1 -> #1 &, slots]
    }, sym /: FindIndicesSlots[sym[expr_, ##]] := Join[FindIndicesSlots[expr, {1}], slotsAndPos]] & @@ ConstantArray[_, Length@slots];
    If[OptionValue@SymmetriedDer,
        sym /: SymmetryOfExpression[s_sym] := SymmetryOfSymmetrizedDer[s, symmetry]
    , If[Length@symmetry > 0,
        sym /: SymmetryOfExpression[sym[expr_, __]] := Join[SymmetryOfExpression@expr, ShiftPermutation[symmetry, Length@FindIndicesSlots@expr]]
    ,
        sym /: SymmetryOfExpression[sym[expr_, __]] := SymmetryOfExpression@expr
    ]];
    sym /: SumPassThroughQ[_sym, _] = True;
    If[OptionValue@AllowPassThrough,
        sym /: ExpressionPassThroughQ[sym[expr_, inds__], tensor_, pos_] := Switch[pos[[1]],
            1, sym[tensor, inds] === 0 && ExpressionPassThroughQ[expr, tensor, Delete[pos, 1]],
            2, True,
            _, False
        ]
    ,
        sym /: ExpressionPassThroughQ[_sym, _, pos_] := pos[[1]] === 2
    ];
    With[{
        name = OptionValue@DisplayName
    },
        sym /: MakeBoxes[expr : sym[expr2_, inds__], StandardForm] := With[{
            box = RowBox@{
                TensorGridBox[name, SeparateIndexName /@ {inds}],
                If[expr2 =!= Null, MakeBoxes@expr2, Nothing]
            }
        }, InterpretationBox[box, expr, Editable -> False]]
    ];
    sym /: NITensorReduce[sym[expr_, inds__], frees_] := ReduceNITensorContractions[ReverseApplied@Construct, {expr, NITensor[sym, IndexName /@ {inds}]}, frees];
);
SyntaxInformation@DefTensorDerivativeOperator = {"ArgumentsPattern" -> {_, _, OptionsPattern[]}};

Options[DefParametreDerivativeOperator] = Join[Options@ExpandDerivativeRules, {
    DisplayName -> "\[PartialD]",
    AllowPassThrough -> True
}];
DefParametreDerivativeOperator[sym_, opt : OptionsPattern[]] := (
    SetDelayed @@@ ExpandDerivativeRules[sym[expr_?DerivativeExpandableQ, v__] :> ExpandDerivative[sym[#, v] &, expr], FilterRules[{opt}, Options@ExpandDerivativeRules]];
    sym[Null, vs__]@expr_ := sym[expr, vs];
    sym[expr_] := expr;
    sym[r_, r_] := 1;
    sym[sym[expr_, v1__], v2__] := sym[expr, v1, v2];
    sym[ETensor[expr_, inds_], v__] := ETensor[sym[expr, v], inds];
    sym[NITensor[expr_, inds_], v__] := NITensor[sym[expr, v], inds];
    sym /: FindIndicesSlots[sym[expr_, __]] := FindIndicesSlots[expr, {1}];
    sym /: SymmetryOfExpression[sym[expr_, __]] := SymmetryOfExpression@expr;
    sym /: SumPassThroughQ[_sym, _] = True;
    If[OptionValue@AllowPassThrough,
        sym /: ExpressionPassThroughQ[sym[expr_, v__], tensor_, pos_] := Switch[pos[[1]],
            1, sym[tensor, v] === 0 && ExpressionPassThroughQ[expr, tensor, Delete[pos, 1]],
            2, True,
            _, False
        ]
    ,
        sym /: ExpressionPassThroughQ[_sym, _, pos_] := pos[[1]] === 2
    ];
    With[{
        name = OptionValue@DisplayName
    },
        sym /: MakeBoxes[expr : sym[expr2_, vs__], StandardForm] := With[{
            box = RowBox@Join[
                SubscriptBox[name, MakeBoxes@#] & /@ {vs},
                If[expr2 =!= Null, {MakeBoxes@expr2}, {}]
            ]
        }, InterpretationBox[box, expr, Editable -> False]]
    ];
    sym /: NITensorReduce[sym[expr_, vs__], frees_] := sym[NITensorReduce[expr, frees], vs];
);

RiemannToRicciRules[riem_, ricci_] := {
    riem[a_, c_, b_, DI@c_] :> ricci[a, b],
    riem[a_, DI@c_, b_, c_] :> ricci[a, b],
    riem[a_, c_, DI@c_, b_] :> -ricci[a, b],
    riem[a_, DI@c_, c_, b_] :> -ricci[a, b],
    riem[c_, a_, b_, DI@c_] :> -ricci[a, b],
    riem[DI@c_, a_, b_, c_] :> -ricci[a, b],
    riem[c_, a_, DI@c_, b_] :> ricci[a, b],
    riem[DI@c_, a_, c_, b_] :> ricci[a, b]
};
SyntaxInformation@RiemannToRicciRules = {"ArgumentsPattern" -> {_, _}};

RicciToRicciScalarRules[ricci_, ricciScalar_] := {
    ricci[DI@a_, a_] :> ricciScalar,
    ricci[a_, DI@a_] :> ricciScalar
};
SyntaxInformation@RicciToRicciScalarRules = {"ArgumentsPattern" -> {_, _}};

RiemannRicciRules[riemann_, ricci_, ricciScalar_] := Join[
    RiemannToRicciRules[riemann, ricci],
    RicciToRicciScalarRules[ricci, ricciScalar]
];
SyntaxInformation@RiemannRicciRules = {"ArgumentsPattern" -> {_, _, _}};

RiemannScalars::undef = "Riemann scalars of order `1` is not yet defined.";
RiemannScalars[riem_, slot_, n_] := RiemannScalars[riem, slot, n, RicciOf@riem];
RiemannScalars[riem_, slot_, n_, ricci_] := RiemannScalars[riem, slot, n, ricci, RicciScalarOf@ricci];
RiemannScalars[riem_, slot_, 1, ricci_, ricciScalar_] := {ricciScalar};
RiemannScalars[riem_, slot_, 2, ricci_, ricciScalar_] := Function[{a, b, c, d},
    {ricciScalar^2, ricci[a, b] ricci[DI@a, DI@b], riem[a, b, c, d] riem[DI@a, DI@b, DI@c, DI@d]}
] @@ GetIndicesOfSlotType[ConstantArray[slot, 4], {}];
RiemannScalars[riem_, slot_, 3, ricci_, ricciScalar_] := Function[{a, b, c, d, e, f}, {
    ricciScalar^3,
    ricciScalar * ricci[a, b] * ricci[DI@a, DI@b],
    ricci[a, DI@b] * ricci[b, DI@c] * ricci[c, DI@a],
    riem[DI@a, DI@b, DI@c, DI@d] ricci[a, c] ricci[b, d],
    ricciScalar * riem[a, b, c, d] riem[DI@a, DI@b, DI@c, DI@d],
    ricci[a, b] riem[DI@a, c, d, e] riem[DI@b, DI@c, DI@d, DI@e],
    riem[a, b, DI@c, DI@d] riem[c, d, DI@e, DI@f] riem[e, f, DI@a, DI@b],
    riem[a, DI@c, b, DI@d] riem[c, DI@e, d, DI@f] riem[e, DI@a, f, DI@b]
}] @@ GetIndicesOfSlotType[ConstantArray[slot, 6], {}];
RiemannScalars[riem_, slot_, 4, ricci_, ricciScalar_] := Function[{a, b, c, d, e, f, g, h}, {
    ricciScalar^4,
    ricciScalar^2 ricci[a, b] ricci[DI[a],DI[b]],
    ricciScalar ricci[a, b] ricci[DI[a], c] ricci[DI[b], DI[c]],
    IndexScope[ricci[a, b] ricci[DI[a], DI[b]]]^2,
    ricci[a, b] ricci[DI[a], c] ricci[DI[b], d] ricci[DI[c],DI[d]],
    ricciScalar ricci[a, b] ricci[c, d] riem[DI[a], DI[c], DI[b], DI[d]],
    ricci[a, b] ricci[c, d] ricci[DI[c], e] riem[DI[a], DI[d], DI[b], DI[e]],
    ricciScalar^2 riem[a, b, c, d] riem[DI[a], DI[b], DI[c], DI[d]],
    ricciScalar ricci[a, b] riem[c, d, e, DI[a]] riem[DI[c], DI[d], DI[e], DI[b]],
    ricci[a, b] ricci[DI[a], DI[b]] riem[c, d, e, f] riem[DI[c], DI[d], DI[e], DI[f]],
    ricci[a, b] ricci[DI[a], c] riem[d, e, f, DI[b]] riem[DI[d], DI[e], DI[f], DI[c]],
    ricci[a, b] ricci[c, d] riem[e, f, DI[a], DI[c]] riem[DI[e], DI[f], DI[b], DI[d]],
    ricci[a, b] ricci[c, d] riem[e, DI[a], f, DI[b]] riem[DI[e], DI[c], DI[f], DI[d]],
    ricci[a, b] ricci[c, d] riem[e, DI[a], f, DI[c]] riem[DI[e], DI[b], DI[f], DI[d]],
    ricciScalar riem[a, b, c, d] riem[DI[a], DI[b], e, f] riem[DI[c], DI[d], DI[e], DI[f]],
    ricciScalar riem[a, b, c, d] riem[DI[a], e, DI[c], f] riem[DI[b], DI[e], DI[d], DI[f]],
    ricci[a, b] riem[e, f, g, DI[c]] riem[DI[a], c, DI[b], d] riem[DI[e], DI[f], DI[g], DI[d]],
    ricci[a, b] riem[c, d, e, f] riem[DI[c], DI[d], g, DI[a]] riem[DI[e], DI[f], DI[g], DI[b]],
    ricci[a, b] riem[c, d, e, f] riem[DI[c], g, DI[e], DI[a]] riem[DI[d], DI[g], DI[f], DI[b]],
    IndexScope[riem[a, b, c, d] riem[DI[a], DI[b], DI[c], DI[d]]]^2,
    riem[a, b, c, d] riem[f, g, h, DI[d]] riem[DI[a], DI[b], DI[c], e] riem[DI[f], DI[g], DI[h], DI[e]],
    riem[a, b, c, d] riem[DI[a], DI[b], e, f] riem[DI[c], DI[d], DI[g], DI[h]] riem[DI[e], DI[f], g, h],
    riem[a, b, c, d] riem[DI[a], DI[b], e, f] riem[DI[c], DI[e], g, h] riem[DI[d], DI[f], DI[g], DI[h]],
    riem[a, b, c, d] riem[DI[a], DI[b], e, f] riem[DI[c], g, DI[e], h] riem[DI[d], DI[g], DI[f], DI[h]],
    riem[a, b, c, d] riem[DI[a], e, DI[c], f] riem[DI[b], DI[g], DI[d], DI[h]] riem[DI[e], g, DI[f], h],
    riem[a, b, c, d] riem[DI[a], e, DI[c], f] riem[DI[e], g, DI[b], h] riem[DI[f], DI[g], DI[d], DI[h]]
}] @@ GetIndicesOfSlotType[ConstantArray[slot, 8], {}];
RiemannScalars[riem_, slot_, n_Integer, ricci_, ricciScalar_] := Null /; (Message[RiemannScalars::undef, n]; False);
SyntaxInformation@RiemannScalars = {"ArgumentsPattern" -> {_, _, _, _., _.}};

ICovD[NITensor[t_, inds_], a_ -> type_, value_] := NITensor[NITensorCovD[t, value, inds[[All, 2]], type], Append[inds, a -> type]];
SyntaxInformation@ICovD = {_, _, _};

NIChristoffel[chris_, slot1_, slot2_][a_, DI@b_, DI@c_] := NITensor[chris, {a -> slot2, b -> DI@slot1, c -> DI@slot2}];
SyntaxInformation@NIChristoffel = {"ArgumentsPattern" -> {_, _, _}};

ConvertChrisProviderOne[{slot1_, slot2_}, chris_] := {slot1, slot2} -> NIChristoffel[chris, slot1, slot2];
ConvertChrisProvider[provider_List] := ConvertChrisProviderOne @@@ provider;
ConvertChrisProvider[provider_?AssociationQ] := KeyValueMap[ConvertChrisProviderOne, provider];

NITensorCovD[expr_, {ders_, 0}, slots_, DI@newSlot_] := ITensorOuter[ReverseApplied@Construct, expr, ders, {}];
NITensorCovD[expr_, {ders_, chrisProvider_}, slots_, DI@newSlot_] := NITensorCovD[expr, {ders, 0}, slots, DI@newSlot] + With[{
    inds = Array[DefaultIndex, Length@slots + 1]
},
    Block[{$TempIndexNumber = 1},
        CovDDifference[NITensor[expr, Thread[(IndexName /@ {##}) -> slots]] &, Thread[Delete[inds, -1] -> slots], ConvertChrisProvider@chrisProvider, inds[[-1]] -> DI@newSlot]
    ] // NITensorReduce[#, inds] & // ExtractNITensor[inds]
];
SyntaxInformation@NITensorCovD = {"ArgumentsPattern" -> {_, _, _, _}};

LookupListOrApply[list_List, elem_] := If[Length@list === 1 && Head@list[[1]] =!= Rule, list[[1]], elem /. list];
LookupListOrApply[list_, elem_] := list@elem;
EnsureUpDownIndices[tensor_, slots_, newSlotSigns_, slotToMetric_] := Fold[RaiseLowerOneSlot[slotToMetric], tensor, Thread@{Range@Length@slots, slots, newSlotSigns}];
RaiseLowerOneSlot[slotToMetric_][t_, {n_, DI@slot_, 1}] := ITensorFixedContract[
    Times, LookupListOrApply[slotToMetric, slot][[2]], t, ContractionSlotOfMetric@MetricOfSlotType[slot][a, b], n
];
RaiseLowerOneSlot[slotToMetric_][t_, {n_, slot_?NonDIQ, -1}] := ITensorFixedContract[
    Times, LookupListOrApply[slotToMetric, slot][[1]], t, ContractionSlotOfMetric@MetricOfSlotType[slot][DI@a, DI@b], n
];
RaiseLowerOneSlot[_][t_, _] := t;
SyntaxInformation@EnsureUpDownIndices = {"ArgumentsPattern" -> {_, _, _, _}};

AdaptNITensor[value_, slots_, inds_, metricProvider_] := NITensor[EnsureUpDownIndices[value, slots, SignOfUpSlot /@ inds, metricProvider], ToNITensorIndex[inds, slots]];
SyntaxInformation@AdaptNITensor = {"ArgumentsPattern" -> {_, _, _, _}};

AdaptNITensorCovD[NITensor[tensor_, inds_], covdInd_, slot_, covdValue_, metricProvider_] := NITensor[
    EnsureUpDownIndices[
        NITensorCovD[tensor, covdValue, inds[[All, 2]], DI@slot],
        Append[ConstantArray[0, Length@inds], SignOfUpSlot@covdInd],
        metricProvider
    ],
    Append[inds, ToNITensorIndex[covdInd, slot]]
];
SyntaxInformation@AdaptNITensorCovD = {"ArgumentsPattern" -> {_, _, _, _, _}};

LeviCivitaChristoffelValue[slot_, cd_, metric_, metricInv_] := LeviCivitaChristoffel[
    ICovD[#1, ToNITensorIndex[#2, slot], cd] &,
    NITensor[metric, ToNITensorIndex[{##}, {slot, slot}]] &,
    NITensor[metricInv, ToNITensorIndex[{##}, {slot, slot}]] &
][a, DI@b, DI@c] // NITensorReduce[#, {a, b, c}] & // ExtractNITensor@{a, b, c};
SyntaxInformation@LeviCivitaChristoffelValue = {"ArgumentsPattern" -> {_, _, _, _}};

RiemannDifferenceValue[slot_, cd_, chris_] := RiemannDifference[
    ICovD[#1, ToNITensorIndex[#2, slot], cd] &,
    NITensor[chris, ToNITensorIndex[{##}, {slot, slot, slot}]] &
][DI@a, DI@b, DI@c, d] // NITensorReduce[#, {a, b, c, d}] & // ExtractNITensor@{a, b, c, d};
SyntaxInformation@RiemannDifferenceValue = {"ArgumentsPattern" -> {_, _, _}};

SymmetricRiemann[metric_, a_, b_, c_, d_] := metric[a, c]metric[b, d] - metric[a, d]metric[b, c];
SyntaxInformation@SymmetricRiemann = {"ArgumentsPattern" -> {_, _, _, _, _}};

CBTensor::unmatchedSlots = "";

Options@DefPerturbationOperator = Options@ExpandDerivativeRules;
DefPerturbationOperator[symbol_, opt : OptionsPattern[]] := (
    SetDelayed @@@ ExpandDerivativeRules[symbol[expr_?DerivativeExpandableQ, n_] :> symbol[ExpandDerivative[symbol[#, 1] &, expr], n - 1] /; n >= 1, FilterRules[{opt}, Options@ExpandDerivativeRules]];
    symbol[expr_] := symbol[expr, 1];
    symbol[expr_, 0] := expr;
    symbol[symbol[expr_, n1_], n2_] := symbol[expr, n1 + n2];
    FindIndicesSlots[symbol[expr_, n_]] ^:= FindIndicesSlots[expr, {1}];
    SymmetryOfExpression[symbol[expr_, n_]] ^:= SymmetryOfExpression@expr;
    symbol /: MakeBoxes[expr : symbol[expr2_, n_], StandardForm] := With[{
        sub = MakeBoxes@expr2,
        del = If[n === 1, "\[Delta]", SuperscriptBox["\[Delta]", MakeBoxes@n]]
    }, InterpretationBox[RowBox@{del, "(", sub, ")"}, expr]];
);
SyntaxInformation@DefPerturbationOperator = {"ArgumentsPattern" -> {_, OptionsPattern[]}};

SortedPartitions[n_] := Union[Join @@ (With[{list = #}, Append[list + # & /@ IdentityMatrix@Length@list, Append[list, 1]]] & /@ SortedPartitions[n - 1])];
SortedPartitions[1] = {{1}};
VarInverseMatrix[invMat_[a_, b_], var_, n_] := Total[
    If[EvenQ[Length@#], 1, -1] *
    (Multinomial @@ #) *
    (Times @@ ContractList[
        Join[{invMat}, Riffle[({ind1, ind2} |-> var[#, DI@ind1, DI@ind2]) & /@ #, invMat], {invMat}],
        a,
        b,
        Null
    ]) & /@ SortedPartitions[n]
];
VarInverseMatrix[{a_, b_}, var_, n_] := Total[
    If[EvenQ[Length@#], 1, -1] *
    (Multinomial @@ #) *
    (Times @@ ContractList[
        ({ind1, ind2} |-> var[#, DI@ind1, ind2]) & /@ #,
        DI@a,
        b,
        Null
    ]) & /@ SortedPartitions[n]
];
SyntaxInformation@VarInverseMatrix = {"ArgumentsPattern" -> {_, _, _}};

MetricOfSlotType[PredefinedSlotType] ^= PredefinedMetric;
DefSimpleMetric[PredefinedMetric, PredefinedSlotType, 1, DisplayName -> "g"];
DefTensorDerivativeOperator[PredefinedCovD, {PredefinedSlotType}, {}, DisplayName -> "\[Del]"];
PredefinedCovD[_PredefinedMetric, _] = 0;
FindIndicesSlots[ShiftedMetric[_, _]] ^= {{1} -> PredefinedSlotType, {2} -> PredefinedSlotType};
FindIndicesSlots[ShiftedMetric[_, _, _]] ^= {{2} -> PredefinedSlotType, {3} -> PredefinedSlotType};
SymmetryOfExpression[_ShiftedMetric] ^= {SCycles@{1, 2}};
ShiftedMetric /: MakeBoxes[expr : ShiftedMetric[a_, b_], StandardForm] := TensorInterpretationBox[expr, TensorGridBox["G", SeparateIndexName /@ {a, b}]];
ShiftedMetric /: MakeBoxes[expr : ShiftedMetric[n_, a_, b_], StandardForm] := TensorInterpretationBox[expr, TensorGridBox[If[n === 1, "\[Delta]G", RowBox@{SuperscriptBox["\[Delta]", MakeBoxes@n], "G"}], SeparateIndexName /@ {a, b}]];
DefSimpleTensor[ShiftedInverseMetric, {PredefinedSlotType, PredefinedSlotType}, {SCycles@{1, 2}}, DisplayName -> "\!\(\*SuperscriptBox[\(G\), \(-1\)]\)"];

DefPerturbationOperator[PerturbShiftedMetric];
PerturbShiftedMetric[PredefinedCovD[expr_, a_], n_] := PredefinedCovD[PerturbShiftedMetric[expr, n], a];
SyntaxInformation@PerturbShiftedMetric = {"ArgumentsPattern" -> {_, _.}};

PerturbationLeviCivitaChristoffel[cd_, metric_, metricPert_, n_][a_, b_, c_] := PerturbShiftedMetric[
    LeviCivitaChristoffel[cd, ShiftedMetric, ShiftedInverseMetric][a, b, c],
    metricPert,
    n
] /. {ShiftedMetric -> metric, ShiftedInverseMetric -> metric};
SyntaxInformation@PerturbationLeviCivitaChristoffel = {"ArgumentsPattern" -> {_, _, _}};

WithUnprotected[symbol_, body_] := WithUnprotected[{symbol}, body];
WithUnprotected[{symbols___}, body_] := (
    Unprotect[symbols];
    With[{ret = body}, Protect[symbols]; ret]
);
SetAttributes[WithUnprotected, HoldAll];
PerturbationLeviCivitaRiemann[n_] := WithUnprotected[
    PerturbationLeviCivitaRiemann
,
    PerturbationLeviCivitaRiemann[n] = Block[{$TempIndexNumber = 1},
        ToIndexedExprFunction[
            PerturbShiftedMetric[
                RiemannDifference[PredefinedCovD, LeviCivitaChristoffel[PredefinedCovD, ShiftedMetric, ShiftedInverseMetric]][DI@a, DI@b, DI@c, d],
                n
            ] /. {
                PerturbShiftedMetric[ShiftedInverseMetric[a_, b_], n2_] :> VarInverseMatrix[ShiftedInverseMetric[a, b], ShiftedMetric, n2],
                PerturbShiftedMetric[ShiftedMetric[a_, b_], n2_] :> ShiftedMetric[n2, a, b]
            } /. {
                ShiftedMetric[a_, b_] :> PredefinedMetric[a, b],
                ShiftedInverseMetric -> PredefinedMetric
            } // ContractMetric // ITensorReduce // RaiseFrees[#, {a, b, c, d}] &,
            {a, b, c, d}
        ]
    ]
];
PerturbationLeviCivitaRiemann[cd_, metric_, metricPert_, n_] := PerturbationLeviCivitaRiemann[n] /. {
    PredefinedCovD -> cd,
    ShiftedMetric -> metricPert,
    PredefinedMetric -> metric
};
SyntaxInformation@PerturbationLeviCivitaRiemann = {"ArgumentsPattern" -> {_, _., _., _.}};

ExpandMetricPerturbation[expr_, {pert_, metric_, metricPert_}] := expr /. {
    HoldPattern@pert[metric[DI@a_, DI@b_], n_] :> metricPert[n, DI@a, DI@b],
    HoldPattern@pert[metric[a_?NonDIQ, b_?NonDIQ], n_] :> VarInverseMatrix[{a, b}, metricPert, n]
};
ExpandMetricPerturbation[a_][expr_] := ExpandMetricPerturbation[expr, a];
SyntaxInformation@ExpandMetricPerturbation = {"ArgumentsPattern" -> {_, _.}};

ExpandRiemannPerturbation[expr_, {pert_, metric_, metricPert_}, {cd_, riemann_, ricci_, ricciScalar_}] := expr /. {
    HoldPattern@pert[ricciScalar, n_] :> With[{
        a = GetUniqueIndexOfSlotType@Null, b = GetUniqueIndexOfSlotType@Null
    }, pert[metric[a, b] * ricci[DI@a, DI@b], n]],
    HoldPattern@pert[e_riemann, n_] :> pert[SeparateMetricOne[e, {-1, -1, -1, 1}], n],
    HoldPattern@pert[e_ricci, n_] :> pert[SeparateMetricOne[e, {-1, -1}], n]
} /. {
    HoldPattern@pert[riemann[inds__], n_] :> PerturbationLeviCivitaRiemann[cd, metric, metricPert, n][inds],
    HoldPattern@pert[ricci[DI@a_, DI@b_], n_] :> With[{
        c = GetUniqueIndexOfSlotType@Null
    }, PerturbationLeviCivitaRiemann[cd, metric, metricPert, n][DI@a, DI@c, DI@b, c]]
};
ExpandRiemannPerturbation[a1_, a2_][expr_] := ExpandRiemannPerturbation[expr, a1, a2];
SyntaxInformation@ExpandRiemannPerturbation = {"ArgumentsPattern" -> {_, _, _.}};

End[];

Protect @@ Select[Names["`*"], !StringMatchQ[#, "$" ~~ __] &];

EndPackage[];
