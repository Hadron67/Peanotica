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
ExpandDerivativeRules;

SymmetriedDer;
AllowPassThrough;
DefParametreDerivativeOperator::usage = "";
DefTensorDerivativeOperator::usage = "DefTensorDerivativeOperator[op, slotType].";

CBTensor::usage = "CBTensor[expr, slots]";
ICovD::usage = "ICovD[expr, a, value]";
NITensorCovD::usage = "NITensorCovD[expr, value, inds, ind]";
NITensorRaiseLowerIndices::usage = "NITensorChangeSlot[tensor, slots]";

LeviCivitaChristoffelValue::usage = "LeviCivitaChristoffelValue[slot, cd, metric, metricInv]";
RiemannDifferenceValue::usage = "RiemannDifferenceValue[slot, cd, chris]";
SymmetricRiemann::usage = "SymmetricRiemann[metric, a, b, c, d]";

DefRiemannToRicciRules::usage = "DefRiemannToRicciRules[riemann, ricci]";
RiemannOf;
RicciOf;
RicciScalarOf;
DefCurvatureTensors;

RiemannScalars;

DefPerturbationOperator::usage = "DefPerturbationOperator[symbol]";
DefMetricPerturbationRules::usage = "DefMetricPerturbationRules[pert, metric, metricDet, metricPert]";
DefLeviCivitaCovDPerturbationRules::usage = "DefLeviCivitaCovDPerturbationRules[pert, cd, metric, metricPert]";
DefLeviCivitaCurvaturePerturbationRules::usage = "DefLeviCivitaCurvaturePerturbationRules[pert, cd, metric, metricPert, riem, ricci, ricciScalar]";
ShiftedMetric;
ShiftedInverseMetric;
PerturbShiftedMetric::usage = "PerturbShiftedMetric[expr, n]";
PerturbationLeviCivitaChristoffel::usage = "PerturbationLeviCivitaChristoffel[cd, metric, n]";

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

CovDDifferenceTerm[expr_, chris_, a_, d_][pos_ -> type_] := With[{
    ind = Extract[expr, {pos}][[1]]
}, If[Head@ind === DI,
    -chris[d, a, ind] * ReplacePart[expr, pos -> DI@d],
    chris[ind, a, DI@d] * ReplacePart[expr, pos -> d]
]];
CovDDifference[expr_, chris_, a_] := Total[CovDDifferenceTerm[expr, chris, a, GetUniqueIndexOfSlotType@Null] /@ FindIndicesSlots@expr];
SyntaxInformation@CovDDifference = {"ArgumentsPattern" -> {_, _, _}};

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

SortCovD[expr_, cd_, commutator_] := expr //. cd[cd[expr2_, a_], b_] :> cd[cd[expr2, b], a] + commutator[expr2, b, a] /; !OrderedQ@{a, b};
SyntaxInformation@SortCovD = {"ArgumentsPattern" -> {_, _, _}};

DerConstantQ[_?NumberQ] = True;
SyntaxInformation@DerConstantQ = {"ArgumentsPattern" -> {_}};

DerFunctionQ[Times] = True;
DerFunctionQ[Power] = True;
SyntaxInformation@DerFunctionQ = {"ArgumentsPattern" -> {_}};

MapDerivativeOnFnDerivative[der_, ders_, fn_, {args__}] := With[{
    mat = IdentityMatrix@Length@{args}
},
    Total@MapIndexed[(Derivative @@ (mat[[#2[[1]]]] + ders))[fn][args] der[#1] &, {args}]
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
    patDer = Derivative[ders__][fn_][args__],
    indexScopePat = HoldPattern@IndexScope[expr_]
}, Join[{
    (lhsFn[patPlus] :> rhs) /. ExpandDerivative[fn_, exprPatName] :> (fn /@ exprPatName) /. exprPatName -> expr,
    (lhsFn[patList] :> rhs) /. ExpandDerivative[fn_, exprPatName] :> (fn /@ exprPatName) /. exprPatName -> expr,
    (lhsFn[patDer] :> rhs) /. ExpandDerivative[fn_, exprPatName] :> MapDerivativeOnFnDerivative[fn, {ders}, fn, {args}] /. exprPatName -> expr,
    (lhsFn[indexScopePat] :> rhs) /. ExpandDerivative[fn_, exprPatName] :> fn[ReplaceDummiesToUnique@expr],
    (lhsFn[_?NumberQ] -> 0)
},
    lhsFn[#] -> 0 & /@ OptionValue@DerConstants,
    With[{fnPat = (fn2 : #)[args__]},
        lhsFn[fnPat] :> rhs /. ExpandDerivative[fn_, exprPatName] :> MapDerivativeOnFnDerivative[fn, 0, fn2, {args}] /. exprPatName -> expr
    ] & /@ OptionValue@DerFunctions
]]];
SyntaxInformation@ExpandDerivativeRules = {"ArgumentsPattern" -> {_, OptionsPattern[]}};
SyntaxInformation@ExpandDerivative = {"ArgumentsPattern" -> {_, _}};

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

DefRiemannToRicciRules[riem_, ricci_] := (
    HoldPattern@riem[a_, c_, b_, DI@c_] := ricci[a, b];
    HoldPattern@riem[a_, DI@c_, b_, c_] := ricci[a, b];
    HoldPattern@riem[a_, c_, DI@c_, b_] := -ricci[a, b];
    HoldPattern@riem[a_, DI@c_, c_, b_] := -ricci[a, b];
    HoldPattern@riem[c_, a_, b_, DI@c_] := -ricci[a, b];
    HoldPattern@riem[DI@c_, a_, b_, c_] := -ricci[a, b];
    HoldPattern@riem[c_, a_, DI@c_, b_] := ricci[a, b];
    HoldPattern@riem[DI@c_, a_, c_, b_] := ricci[a, b];
);
DefRiemannToRicciRules[riem_] := DefRiemannToRicciRules[riem, RicciOf@riem];
SyntaxInformation@DefRiemannToRicciRules = {"ArgumentsPattern" -> {_, _.}};

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
ICovD /: FindIndicesSlots@ICovD[expr_, a_, _] := Join[FindIndicesSlots[expr, {1}], {{2} -> Null}];
ICovD /: FindIndicesSlots@ICovD[expr_, a_ -> type_, _] := Join[FindIndicesSlots[expr, {1}], {{2} -> type}];
ICovD /: SymmetryOfExpression@ICovD[expr_, _, _] := SymmetryOfExpression@expr;
SyntaxInformation@ICovD = {_, _, _};

NITensorCovD[expr_, {ders_, 0}, slots_, DI@newSlot_] := ITensorOuter[ReverseApplied@Construct, expr, ders, {}];
SyntaxInformation@NITensorCovD = {"ArgumentsPattern" -> {_, _, _, _}};

NITensorRaiseLowerIndices[expr_, slots_, metric_, metricInv_] := NITensorRaiseLowerIndices[expr, If[# === -1, {-1, metric, ContractionSlotOfMetric@metric[DI@a, DI@b]}, {1, metricInv, ContractionSlotOfMetric@metricInv[a, b]}] & /@ slots];
ChangeOneSign[name_ -> slot_, -1] := name -> DI@slot;
ChangeOneSign[name_ -> DI@slot_, 1] := name -> slot;
ChangeOneSign[e_, _] := e;
NITensorRaiseLowerIndices[NITensor[t_, slots_], slots2_] := NITensor[Fold[RaiseLowerOneSlot, t, Thread@{Range@Length@slots, slots, slots2}], MapThread[ChangeOneSign, {slots, slots2[[All, 1]]}]];
SyntaxInformation@NITensorRaiseLowerIndices = {"ArgumentsPattern" -> {_, _, _., _.}};
RaiseLowerOneSlot[t_, {n_, indName_ -> slot_, {sign_, metric_, cslot_}}] := If[SignOfUpSlot@slot =!= sign, ITensorFixedContract[Times, metric, t, cslot, n], t];

LeviCivitaChristoffelValue[slot_, cd_, metric_, metricInv_] := LeviCivitaChristoffel[
    ICovD[#1, IndexName@#2 -> DI@slot, cd] &,
    NITensor[metric, {IndexName@#1 -> DI@slot, IndexName@#2 -> DI@slot}] &,
    NITensor[metricInv, {IndexName@#1 -> slot, IndexName@#2 -> slot}] &
][a, DI@b, DI@c] // NITensorReduce[#, {a, b, c}] & // ExtractNITensor@{a, b, c};
SyntaxInformation@LeviCivitaChristoffelValue = {"ArgumentsPattern" -> {_, _, _, _}};

RiemannDifferenceValue[slot_, cd_, chris_] := RiemannDifference[
    ICovD[#1, IndexName@#2 -> DI@slot, cd] &,
    NITensor[chris, {IndexName@#1 -> slot, IndexName@#2 -> DI@slot, IndexName@#3 -> DI@slot}] &
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
    FindIndicesSlots[symbol[t_, n_][inds__]] ^:= FindIndicesSlots[t[inds]];
    SymmetryOfExpression[symbol[t_, n_][inds__]] ^:= SymmetryOfExpression@t@inds;
    symbol /: MakeBoxes[expr : symbol[t_, n_][inds__], StandardForm] := With[{
        sub = MakeBoxes@t[inds],
        del = If[n === 1, "\[Delta]", SuperscriptBox["\[Delta]", MakeBoxes@n]]
    }, InterpretationBox[RowBox@{del, sub}, expr]];
);
SyntaxInformation@DefPerturbationOperator = {"ArgumentsPattern" -> {_, OptionsPattern[]}};

DefMetricPerturbationRules[pert_, metric_, metricDet_, metricPert_] := (
    pert[metric[a_, b_]] := Switch[{Head@a, Head@b},
        {DI, DI}, metricPert[1, a, b],
        {DI, _}, 0,
        {_, DI}, 0,
        _, -metricPert[1, a, b]
    ];
    pert[metricPert[n_, a_, b_]] := If[Head@a === DI && Head@b === DI,
        metricPert[n + 1, a, b],
        pert@SeparateMetricOne[metricPert[n, a, b], {-1, -1}]
    ];
    pert[metricDet] := metricDet * IndexScope@metricPert[1, DefaultIndex[1], DI@DefaultIndex[1]];
);
SyntaxInformation@DefMetricPerturbationRules = {"ArgumentsPattern" -> {_, _, _, _}};

LeviCivitaPert[cd_, metricPert_][a_, b_, c_] := 1/2 (cd[metricPert[1, a, c], b] + cd[metricPert[1, b, a], c] - cd[metricPert[1, b, c], a]);

DefLeviCivitaCovDPerturbationRules[pert_, cd_, metric_, metricPert_] := (
    pert[cd[expr_, DI@a_]] := cd[pert@expr, DI@a] + CovDDifference[expr, LeviCivitaPert[cd, metricPert], DI@a];
    pert[cd[expr_, a_?NonDIQ]] := With[{d = GetUniqueIndexOfSlotType@Null}, pert[metric[a, d]cd[expr, DI@d]]];
);
SyntaxInformation@DefLeviCivitaCovDPerturbationRules = {"ArgumentsPattern" -> {_, _, _, _}};

DefLeviCivitaCurvaturePerturbationRules[pert_, cd_, metric_, metricPert_, riem_, ricci_, ricciScalar_] := (
    pert[riem[a_, b_, c_, d_]] := If[SignOfUpSlot /@ {a, b, c, d} === {-1, -1, -1, 1},
        RiemannDifferencePart1[cd, LeviCivitaPert[cd, metricPert]][a, b, c, d],
        pert@SeparateMetricOne[riem[a, b, c, d], {-1, -1, -1, 1}]
    ];
    pert[ricci[a_, b_]] := If[Head@a === DI && Head@b === DI,
        With[{d = GetUniqueIndexOfSlotType@Null}, pert[Unevaluated@riem[a, DI@d, b, d]]],
        pert@SeparateMetricOne[ricci[a, b], {-1, -1}]
    ];
    pert[ricciScalar] := With[{
        a = GetUniqueIndexOfSlotType@Null,
        b = GetUniqueIndexOfSlotType@Null
    }, pert[ricci[DI@a, DI@b]]metric[a, b] + ricci[DI@a, DI@b] pert[metric[a, b]]];
);
SyntaxInformation@DefLeviCivitaCurvaturePerturbationRules = {"ArgumentsPattern" -> {_, _, _, _, _, _, _}};

DefSimpleTensor[ShiftedMetric, {Null, Null}, {SCycles@{1, 2}}];
DefSimpleTensor[ShiftedInverseMetric, {Null, Null}, {SCycles@{1, 2}}];

SetDelayed @@@ ExpandDerivativeRules[
    PerturbShiftedMetric[expr_?DerivativeExpandableQ, n_] :> PerturbShiftedMetric[ExpandDerivative[PerturbShiftedMetric[#, 1] &, expr], n - 1] /; n >= 1
];
PerturbShiftedMetric[expr_] := PerturbShiftedMetric[expr, 1];
PerturbShiftedMetric[expr_, 0] := expr;
PerturbShiftedMetric[cd_[ShiftedMetric[a_, b_], c_], metricPert_, n_] := cd[metricPert[n, a, b], c];
(* PerturbShiftedMetric[ShiftedInverseMetric[a_, b_], metricPert_, n_] := ; *)

PerturbationLeviCivitaChristoffel[cd_, metric_, metricPert_, n_][a_, b_, c_] := PerturbShiftedMetric[
    LeviCivitaChristoffel[cd, ShiftedMetric, ShiftedInverseMetric][a, b, c],
    metricPert,
    n
] /. {ShiftedMetric -> metric, ShiftedInverseMetric -> metric};
SyntaxInformation@PerturbationLeviCivitaChristoffel = {"ArgumentsPattern" -> {_, _, _}};

End[];

EndPackage[];
