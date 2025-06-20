BeginPackage["Peanotica`DiffGeo`", {"Peanotica`Perm`", "Peanotica`Core`"}];

Scan[Unprotect@#; ClearAll@#; &, Names@{$Context <> "*"}];

LeviCivitaChristoffelDer::usage = "LeviCivitaChristoffel[cd, der, metric]";
LeviCivitaChristoffel::usage = "LeviCivitaChristoffel[cd, metric] represents the Christoffel tensor relating the possibly non-metric compatible covariant derivative cd and the Levi-Civita connection. LeviCivitaChristoffel[...][a, DI@b, DI@c] gives the expression.";
RiemannDifferencePart1;
RiemannDifferencePart2;
RiemannDifference::usage = "RiemannDifference[cd, christoffel] represents the difference of the Riemann tensors of cd and cd1, where christoffel is the Christoffel tensor relation cd and cd1.";
CovDDifference::usage = "CovDDifference[expr, inds, chrisProvider, a] gives the difference ";
CovDDifferenceDirect::usage = "CovDDifferenceDirect[expr, chrisProvider, a]";
CovDCommutatorNoTorsion::usage = "CovDCommutatorNoTorsion[expr, a, b, riemann]";
CovDCommutatorOfRiemann::usage = "CovDCommutator[riemann]";
ParamDLCCovDCommutator::usage = "ParamDLCCovDCommutator[expr, paramd, cd, metric]";
SortParamDLCCovDRule::usage = "SortParamDLCCovDRule[paramd, covd, metric]";
CovDDefaultIndexOrderedQ::usage = "CovDDefaultIndexOrderedQ[a, b]";
SortCovDRules::usage = "SortCovDRules[cd, commutator, orderedQ]";
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
ExpandDerivativeRules::usage = "ExpandDerivativeRules[lhs :> rhs]";

SymmetriedDer;
AllowPassThrough;
DefParametreDerivativeOperator::usage = "DefParametreDerivativeOperator[optr]";
DerParameters::usage = "DerParameters is an option for DefParametreDerivativeOperator";
DefTensorDerivativeOperator::usage = "DefTensorDerivativeOperator[op, slotType, symmetry].";

NICovD::usage = "NICovD[expr, a, value]";
NIChristoffel::usage = "NIChristoffel[value, slot1, slot2]";
ITensorCovD::usage = "ITensorCovD[expr, value, inds, ind]";
EnsureUpDownIndices::usage = "EnsureUpDownIndices[tensor, slots, newSlotSigns, slotToMetric]";
AdaptIndices::usage = "AdaptIndices[tensor, slots, indPats, adapter]";
AdaptOneIndex::usage = "AdaptOneIndex[adapter, tensor, i, slot, indPat]";
MetricAdapter::usage = "MetricAdapter[{slot1 -> {metric, invMetric}, ...}]";
AdaptNITensor::usage = "AdaptNITensor[value, slots, inds, metricProvider]";
AdaptNITensorCovD::usage = "AdaptNITensorCovD[NITensor[...], ind, slot, covdValue, metricProvider]";

LeviCivitaChristoffelValue::usage = "LeviCivitaChristoffelValue[slot, cd, metric, metricInv]";
RiemannDifferenceValue::usage = "RiemannDifferenceValue[slot, cd, chris]";
CoRiemannValue::usage = "CoRiemannValue[]";
SymmetricRiemann::usage = "SymmetricRiemann[metric, a, b, c, d]";
SymmetricRiemannETensor::usage = "SymmetricRiemannETensor[metric]";

RiemannToRicciRules::usage = "RiemannToRicciRules[riemann, ricci]";
RicciToRicciScalarRules::usage = "RicciToRicciScalarRules[ricci, ricciScalar]";
RiemannRicciRules::usage = "RiemannRicciRules[riemann, ricci, ricciScalar]";
DefRiemannCurvature::usage = "DefRiemannCurvature[slot, riemann, ricci, ricciScalar]";

RiemannScalars::usage = "RiemannScalars[riemann, slot, n]";
RiemannScalarIndexPermutations::usage = "RiemannScalarIndexPermutations[n]";

LovelockDensity::usage = "LovelockDensity[riemann, n]";
WeylToRiemann::usage = "WeylToRiemann[riemann, metric, {a, b, c, d}]";
WeylToRiemannSchouten::usage = "WeylToRiemann[riemann, metric, schouten, {a, b, c, d}]";
RiemannBianchiRelations::usage = "RiemannBianchiRelations[expr, covd, riemann]";
CovDCommutatorRelations::usage = "CovDCommutatorRelations[expr, covd, commu]";

(* perturbation *)
DefPerturbationOperator::usage = "DefPerturbationOperator[symbol]";
DefTensorPerturbation::usage = "DefTensorPerturbation[pert, name, slots]";
VarInverseMatrix::usage = "VarInverseMatrix[invMat[a, b], varmat, order]";
PredefinedSlotType;
PredefinedCovD;
PredefinedMetric;
ShiftedMetric;
ShiftedInverseMetric;
PerturbShiftedMetric::usage = "PerturbShiftedMetric[expr, n]";
PerturbationLeviCivitaChristoffel::usage = "PerturbationLeviCivitaChristoffel[cd, metric, n]";
PerturbationLeviCivitaRiemann::usage = "PerturbationLeviCivitaRiemann[covd, metric, metricPert, n]";
PerturbationCovD::usage = "PerturbationCovD[covd[expr, a], covdPert, metric, n]";
ExpandMetricPerturbation::usage = "ExpandMetricPerturbation[expr, {pert, metric, metricPert}]";
ExpandTensorDetPerturbation::usage = "ExpandTensorDetPerturbation[expr, {pert, tensor, det}]";
ExpandRiemannPerturbation::usage = "ExpandRiemannPerturbation[expr, {pert, metric, metricPert}, {cd, riemann, ricci, ricciScalar}]";
ExpandCovDPerturbation::usage = "ExpandCovDPerturbation[expr, {cd, cdPert, metric, metricPert}]";
VectorBasisCommutator::usage = "VectorBasisCommutator[mat, ders]";

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

DropDummiesFromIndPosList[list_] := With[{
    dummies = With[{d = list[[All, 2, 1]]}, Intersection[d, DI /@ d]]
},
    Select[list, !MemberQ[dummies, #[[2, 1]]] &]
];
CovDDifferenceDirect[expr_, chrisProvider_, a_] := With[{
    inds = DropDummiesFromIndPosList@FindIndicesSlotsAndNames@expr
},
    CovDDifference[
        Function@@{ReplacePart[expr, MapIndexed[#1 -> Slot @@ #2 &, inds[[All, 1]]]]},
        ToNITensorIndex @@@ inds[[All, 2]],
        chrisProvider,
        a
    ]
];
SyntaxInformation@CovDDifferenceDirect = {"ArgumentsPattern" -> {_, _, _}};

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

SortParamDLCCovDRule[paramd_, covd_, metric_] := {
    paramd[covd[expr_, DI@a_], p_, ders___] :> paramd[covd[paramd[expr, p], DI@a] + ParamDLCCovDCommutator[expr, DI@a, paramd[#, p] &, covd, metric], ders]
};
SyntaxInformation@SortParamDLCCovDRule = {"ArgumentsPattern" -> {_, _, _}};

ParamDLCCovDCommutator[expr_, ind_, paramd_, cd_, metric_] := ParamDLCCovDCommutator[expr, ind, paramd, cd, metric, metric];
ParamDLCCovDCommutator[expr_, DI@ind_, paramd_, cd_, metric_, metricInv_] := CovDCommutatorOfRiemann[-LeviCivitaChristoffelDer[cd, paramd, metric, metricInv][#4, #2, #3] &][expr, Null, DI@ind];
SyntaxInformation@ParamDLCCovDCommutator = {"ArgumentsPattern" -> {_, _, _, _, _.}};

CovDDefaultIndexOrderedQ[a_, b_, expr_] := With[{
    inds = Keys@FindAllIndicesNames@expr,
    aInd = SeparateIndexName@a,
    bInd = SeparateIndexName@b
}, OrderedQ@{Prepend[aInd, !MemberQ[inds, aInd[[1]]]], Prepend[bInd, !MemberQ[inds, bInd[[1]]]]}];
SyntaxInformation@CovDDefaultIndexOrderedQ = {"ArgumentsPattern" -> {_, _, _}};

SortCovDRules[cd_, commutator_] := SortCovDRules[cd, commutator, CovDDefaultIndexOrderedQ];
SortCovDRules[cd_, commutator_, orderedQ_] := {cd[cd[expr2_, a_], b_] :> cd[cd[expr2, b], a] + commutator[expr2, b, a] /; !orderedQ[a, b, expr2]};
SyntaxInformation@SortCovDRules = {"ArgumentsPattern" -> {_, _, _.}};

SortCovD[expr_, cd_, commutator_] := SortCovD[expr, cd, commutator, CovDDefaultIndexOrderedQ];
SortCovD[expr_, cd_, commutator_, orderedQ_] := expr //. SortCovDRules[cd, commutator, orderedQ];
SyntaxInformation@SortCovD = {"ArgumentsPattern" -> {_, _, _, _.}};

DerConstantQ[_?NumericQ] = True;
DerConstantQ[symbol_Symbol] := MemberQ[Attributes@symbol, Constant];
DerConstantQ[_?DerFunctionQ[args___]] := AllTrue[{args}, DerConstantQ];
SyntaxInformation@DerConstantQ = {"ArgumentsPattern" -> {_}};

DerFunctionQ[Times] = True;
DerFunctionQ[Power] = True;
DerFunctionQ[symbol_Symbol] := MemberQ[Attributes@symbol, NumericFunction];
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
    indexedSumPat = expr_IndexedSum
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
    (lhsFn[indexedSumPat] :> rhs) /. {
        ExpandDerivative[fn2_, exprPatName] :> ReplaceIndexedSumIndsToUnique[expr, fn2],
        ExpandDerivativeWithRest[fn2_, exprPatName] :> ReplaceIndexedSumIndsToUnique[expr, fn2[#, 1] &]
    },
    (lhsFn[_?NumberQ] :> rhs) /. {
        ExpandDerivative[_, exprPatName] -> 0,
        ExpandDerivativeWithRest[_, exprPatName] -> 0
    }
},
    (lhsFn[#] :> rhs) /. {
        ExpandDerivative[_, exprPatName] -> 0,
        ExpandDerivativeWithRest[_, exprPatName] -> 0
    } & /@ OptionValue@DerConstants,
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
    } &, {Hold@expr, 1}, MatchQ[#[[1]], Hold@sym[_, __]] &]
},
    Join[
        Extract[subExprAndInd[[1]], 1, SymmetryOfExpression],
        Join[
            Join @@ Array[ShiftPermutation[singleSym, (# - 1) * Length@{inds}] &, subExprAndInd[[2]]],
            ShiftPermutation[BlockSymmetricGenSet @@ Partition[Range[Length@{inds} * subExprAndInd[[2]]], Length@{inds}], With[{sub = subExprAndInd[[1]]}, Length@FindIndicesSlots@sub]]
        ]
    ]
];
SetAttributes[SymmetryOfSymmetrizedDer, HoldAll];
DefTensorDerivativeOperator[sym_Symbol, slots_List, symmetry_List, opt : OptionsPattern[]] := (
    SetDelayed @@@ ExpandDerivativeRules[sym[expr_?DerivativeExpandableQ, inds___] :> ExpandDerivative[sym[#, inds] &, expr], FilterRules[{opt}, Options@ExpandDerivativeRules]];
    With[{
        len = Length@slots
    }, sym[Null, a__][expr_] := sym[expr, a] /; Length@{a} === len];
    With[{
        slotsAndPos = MapIndexed[#2 + 1 -> #1 &, slots]
    }, sym /: FindIndicesSlots[sym[expr_, ##]] := Join[FindIndicesSlots[expr, {1}], slotsAndPos]] & @@ ConstantArray[_, Length@slots];
    If[OptionValue@SymmetriedDer,
        sym /: SymmetryOfExpression[s_sym] := SymmetryOfSymmetrizedDer[s, symmetry];
    ,
        If[Length@symmetry > 0,
            SymmetryOfExpression[sym[expr_, ___]] ^:= Join[SymmetryOfExpression@expr, ShiftPermutation[symmetry, Length@FindIndicesSlots@expr]]
        ,
            SymmetryOfExpression[sym[expr_, ___]] ^:= SymmetryOfExpression@expr
        ]
    ];
    sym /: SumPassThroughQ[sym[expr_, inds___], pos_] := If[pos[[1]] >= 2, True, SumPassThroughQ[expr, Delete[pos, 1]]];
    If[OptionValue@AllowPassThrough,
        sym /: ExpressionPassThroughQ[sym[expr_, inds___], tensor_, pos_] := Switch[pos[[1]],
            1, sym[tensor, inds] === 0 && ExpressionPassThroughQ[expr, tensor, Delete[pos, 1]],
            2, True,
            _, False
        ]
    ,
        sym /: ExpressionPassThroughQ[_sym, _, pos_] := pos[[1]] === 2
    ];
    With[{
        name = OptionValue@DisplayName
    }, If[name =!= None,
        sym /: MakeBoxes[expr : sym[expr2_, inds___], StandardForm] := With[{
            box = RowBox@{
                TensorGridBox[DisplayNameOf@sym, IndexBoxForm /@ {inds}],
                If[expr2 =!= Null, MakeBoxes@expr2, Nothing]
            }
        }, InterpretationBox[box, expr, Editable -> False]];
        If[name =!= Automatic, DisplayNameOf@sym ^= name];
    ]];
    sym /: NITensorReduce[sym[expr_, inds__], frees_] := ReduceNITensorContractions[ReverseApplied@Construct, {expr, NITensor[sym, IndexName /@ {inds}]}, frees];
);
SyntaxInformation@DefTensorDerivativeOperator = {"ArgumentsPattern" -> {_, _, OptionsPattern[]}};

Options[DefParametreDerivativeOperator] = Join[Options@ExpandDerivativeRules, {
    DisplayName -> "\[PartialD]",
    DerParameters -> {},
    AllowPassThrough -> True
}];
DefParametreDerivativeOperator[sym_, opt : OptionsPattern[]] := (
    SetDelayed @@@ ExpandDerivativeRules[sym[expr_?DerivativeExpandableQ, v_, rest___] :> sym[ExpandDerivative[sym[#, v] &, expr], rest], FilterRules[{opt}, Options@ExpandDerivativeRules]];
    sym[Null, vs__]@expr_ := sym[expr, vs];
    sym[expr_] := expr;
    sym[r_, r_] = 1;
    With[{
        params = OptionValue@DerParameters
    },
        Outer[If[#1 =!= #2, sym[#1, #2] = 0] &, params, params];
    ];
    sym[sym[expr_, v1__], v2__] := sym[expr, v1, v2];
    sym[ETensor[expr_, inds_], v__] := ETensor[sym[expr, v], inds];
    sym[NITensor[expr_, inds_], v__] := NITensor[sym[expr, v], inds];
    sym /: FindIndicesSlots[sym[expr_, __]] := FindIndicesSlots[expr, {1}];
    sym /: SymmetryOfExpression[sym[expr_, __]] := SymmetryOfExpression@expr;
    sym /: SumPassThroughQ[sym[expr_, __], pos_] := If[pos[[1]] === 1, SumPassThroughQ[expr, Delete[pos, 1]], True];
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
SyntaxInformation@DefParametreDerivativeOperator = {"ArgumentsPattern" -> {_, OptionsPattern[]}};

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

Options@DefRiemannCurvature = Options@DefTensorFormatings;
DefRiemannCurvature[slot_, riemann_, ricci_, ricciScalar_, opt : OptionsPattern[]] := (
    DefSimpleTensor[riemann, ConstantArray[slot, 4], RiemannSymmetricGenSet[1], opt];
    DefSimpleTensor[ricci, {slot, slot}, {SCycles@{1, 2}}, opt];
    ricciScalar /: MakeBoxes[ricciScalar, StandardForm] = InterpretationBox["R", ricciScalar];
    SetDelayed @@@ RiemannRicciRules[riemann, ricci, ricciScalar];
);
SyntaxInformation@DefRiemannCurvature = {"ArgumentsPattern" -> {_, _, _, _, OptionsPattern[]}};

RiemannScalarIndexPermutations::undef = "Riemann scalar permutations of order `1` is not yet defined.";
RiemannScalarIndexPermutations[1] = {{1, 3, 2, 4}};
RiemannScalarIndexPermutations[2] = {{1, 3, 2, 4, 5, 7, 6, 8}, {1, 3, 5, 4, 2, 7, 6, 8}, {1, 3, 5, 7, 2, 4, 6, 8}};
RiemannScalarIndexPermutations[3] = {
    {1, 3, 2, 4, 5, 7, 6, 8, 9, 11, 10, 12},
    {1, 3, 2, 4, 5, 7, 9, 8, 6, 11, 10, 12},
    {1, 3, 5, 4, 2, 7, 9, 8, 6, 11, 10, 12},
    {1, 3, 5, 7, 2, 9, 6, 10, 4, 11, 8, 12},
    {1, 3, 5, 7, 2, 4, 6, 8, 9, 11, 10, 12},
    {1, 3, 5, 7, 2, 4, 6, 9, 8, 11, 10, 12},
    {1, 3, 5, 7, 2, 4, 9, 11, 6, 8, 10, 12},
    {1, 3, 5, 7, 2, 9, 6, 11, 4, 10, 8, 12}
};
RiemannScalarIndexPermutations[4] = {
    {1, 3, 2, 4, 5, 7, 6, 8, 9, 11, 10, 12, 13, 15, 14, 16},
    {1, 3, 2, 4, 5, 7, 6, 8, 9, 11, 13, 12, 10, 15, 14, 16},
    {1, 3, 2, 4, 5, 7, 9, 8, 6, 11, 13, 12, 10, 15, 14, 16},
    {1, 3, 5, 4, 9, 11, 13, 12, 2, 7, 6, 8, 10, 15, 14, 16},
    {1, 3, 5, 4, 2, 7, 9, 8, 6, 11, 13, 12, 10, 15, 14, 16},
    {1, 3, 5, 7, 13, 15, 14, 16, 2, 9, 6, 10, 4, 11, 8, 12},
    {1, 3, 5, 7, 2, 9, 6, 10, 4, 11, 13, 12, 8, 15, 14, 16},
    {1, 3, 5, 7, 2, 4, 6, 8, 9, 11, 10, 12, 13, 15, 14, 16},
    {1, 3, 5, 7, 2, 4, 6, 9, 13, 15, 14, 16, 8, 11, 10, 12},
    {1, 3, 5, 7, 2, 4, 6, 8, 9, 11, 13, 12, 10, 15, 14, 16},
    {1, 3, 5, 7, 2, 4, 6, 9, 11, 13, 8, 14, 12, 15, 10, 16},
    {1, 3, 5, 7, 2, 4, 9, 11, 6, 13, 10, 14, 8, 15, 12, 16},
    {1, 3, 5, 7, 2, 9, 6, 11, 4, 13, 8, 14, 10, 15, 12, 16},
    {1, 3, 5, 7, 2, 9, 6, 11, 4, 13, 10, 14, 8, 15, 12, 16},
    {1, 3, 5, 7, 2, 4, 9, 11, 6, 8, 10, 12, 13, 15, 14, 16},
    {1, 3, 5, 7, 2, 9, 6, 11, 4, 10, 8, 12, 13, 15, 14, 16},
    {1, 3, 5, 7, 2, 4, 6, 9, 8, 11, 10, 13, 12, 15, 14, 16},
    {1, 3, 5, 7, 2, 4, 9, 11, 6, 8, 10, 13, 12, 15, 14, 16},
    {1, 3, 5, 7, 2, 9, 6, 11, 4, 10, 8, 13, 12, 15, 14, 16},
    {1, 3, 5, 7, 9, 11, 13, 15, 2, 4, 6, 8, 10, 12, 14, 16},
    {1, 3, 5, 7, 2, 4, 6, 9, 8, 11, 13, 15, 10, 12, 14, 16},
    {1, 3, 5, 7, 2, 4, 9, 11, 6, 8, 13, 15, 10, 12, 14, 16},
    {1, 3, 5, 7, 2, 4, 9, 11, 6, 10, 13, 15, 8, 12, 14, 16},
    {1, 3, 5, 7, 2, 4, 9, 11, 6, 13, 10, 15, 8, 14, 12, 16},
    {1, 3, 5, 7, 2, 9, 6, 11, 4, 13, 8, 15, 10, 14, 12, 16},
    {1, 3, 5, 7, 2, 9, 6, 11, 4, 13, 10, 15, 8, 14, 12, 16}
};
RiemannScalarIndexPermutations[n_] := Null /; (Message[RiemannScalarIndexPermutations::undef, n]; False);
SyntaxInformation@RiemannScalarIndexPermutations = {"ArgumentsPattern" -> {_}};

RiemannScalars[riemann_, n_Integer] := RiemannScalars[riemann, RiemannScalarIndexPermutations[n]];
RiemannScalars[riemann_, perms_List?MatrixQ] := With[{
    n = Length@perms[[1]] / 4
}, With[{
    inds = Join @@ ({DI@#, #} & /@ Array[TempIndex, 2n])
},
    Times @@ (riemann @@@ Partition[Permute[inds, InversePermutation@#], 4]) & /@ perms
]];
SyntaxInformation@RiemannScalars = {"ArgumentsPattern" -> {_, _}};

NICovD[NITensor[t_, inds_], a_ -> type_, value_] := NITensor[ITensorCovD[t, value, inds[[All, 2]], type], Append[inds, a -> type]];
SyntaxInformation@NICovD = {_, _, _};

NIChristoffel[chris_, slot1_, slot2_][a_, DI@b_, DI@c_] := NITensor[chris, {a -> slot2, b -> DI@slot1, c -> DI@slot2}];
SyntaxInformation@NIChristoffel = {"ArgumentsPattern" -> {_, _, _}};

ConvertChrisProviderOne[{slot1_, slot2_}, chris_] := {slot1, slot2} -> NIChristoffel[chris, slot1, slot2];
ConvertChrisProvider[provider_List] := ConvertChrisProviderOne @@@ provider;
ConvertChrisProvider[provider_?AssociationQ] := KeyValueMap[ConvertChrisProviderOne, provider];

ITensorCovD[expr_, {ders_, _}, {}, newSlot_] := ITensorScalarMultiply[Construct, ders, expr];
ITensorCovD[expr_, {ders_, 0}, slots_, newSlot_] := ITensorOuter[ReverseApplied@Construct, expr, ders, {}];
ITensorCovD[expr_, {ders_, chrisProvider_}, slots_, newSlot_] := ITensorCovD[expr, {ders, 0}, slots, newSlot] + With[{
    inds = Array[DefaultIndex, Length@slots + 1]
},
    Block[{$TempIndexNumber = 1},
        CovDDifference[NITensor[expr, Thread[(IndexName /@ {##}) -> slots]] &, Thread[Delete[inds, -1] -> slots], ConvertChrisProvider@chrisProvider, inds[[-1]] -> DI@newSlot]
    ] // NITensorReduce[#, inds] & // ExtractNITensor[inds]
];
SyntaxInformation@ITensorCovD = {"ArgumentsPattern" -> {_, _, _, _}};

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

AdaptIndices[tensor_, slots_, indPats_, adapter_] := Fold[AdaptIndicesHelper@adapter, {tensor, {}}, Thread@{Range@Length@slots, slots, indPats}];
AdaptIndicesHelper[adapter_][{tensor_, curSlots_}, {i_, slot_, Verbatim[_]}] := {tensor, curSlots};
AdaptIndicesHelper[adapter_][{tensor_, curSlots_}, {i_, slot_, indPat_}] := With[{
    ret = AdaptOneIndex[adapter, tensor, i, slot, indPat]
}, If[Length@ret === 2,
    {ret[[1]], Append[curSlots, ret[[2]]]},
    {ret[[1]], curSlots}
]];
SyntaxInformation@AdaptIndices = {"ArgumentsPattern" -> {_, _, _, _}};

AdaptOneIndex[_, tensor_, _, DI@slot_, DI@IndexNameSlot] := {tensor, DI@slot};
AdaptOneIndex[_, tensor_, _, slot_?NonDIQ, IndexNameSlot] := {tensor, slot};
SyntaxInformation@AdaptOneIndex = {"ArgumentsPattern" -> {_, _, _, _, _}};

MetricAdapter /: AdaptOneIndex[MetricAdapter[slotToMetric_], tensor_, i_, DI@slot_, IndexNameSlot] := {
    ITensorFixedContract[
        Times, LookupListOrApply[slotToMetric, slot][[2]], tensor, ContractionSlotOfMetric@MetricOfSlotType[slot][a, b], i
    ],
    slot
};
MetricAdapter /: AdaptOneIndex[MetricAdapter[slotToMetric_], tensor_, i_, slot_?NonDIQ, DI@IndexNameSlot] := {
    ITensorFixedContract[
        Times, LookupListOrApply[slotToMetric, slot][[1]], tensor, ContractionSlotOfMetric@MetricOfSlotType[slot][DI@a, DI@b], i
    ],
    DI@slot
};
SyntaxInformation@MetricAdapter = {"ArgumentsPattern" -> {_}};

AdaptNITensor[value_, slots_, inds_, adapter_] := NITensor[EnsureUpDownIndices[value, slots, SignOfUpSlot /@ inds, metricProvider], ToNITensorIndex[inds, slots]];
AdaptNITensor[value_, slots_, inds_, adapter_] := With[{
    inds2 = SeparateIndexName /@ inds
}, With[{
    adaptedTensorAndSlots = AdaptIndices[value, slots, inds2[[All, 2]], adapter]
}, NITensor[
    adaptedTensorAndSlots[[1]],
    Thread[inds2[[All, 1]] -> adaptedTensorAndSlots[[2]]]
]]];
SyntaxInformation@AdaptNITensor = {"ArgumentsPattern" -> {_, _, _, _}};

AdaptNITensorCovD[e_NITensor, covdInd_, slot_, covdValue_, metricProvider_] := AdaptNITensorCovD2[
    NITensorReduce@e, covdInd, slot, covdValue, metricProvider
];
AdaptNITensorCovD2[NITensor[tensor_, inds_], covdInd_, slot_, covdValue_, metricProvider_] := With[{
    ret = AdaptOneIndex[
        metricProvider,
        ITensorCovD[tensor, covdValue, inds[[All, 2]], slot],
        Length@inds + 1,
        DI@slot,
        SeparateIndexName[covdInd][[2]]
    ]
}, NITensor[
    ret[[1]],
    Append[inds, IndexName@covdInd -> ret[[2]]]
]];
SyntaxInformation@AdaptNITensorCovD = {"ArgumentsPattern" -> {_, _, _, _, _}};

LeviCivitaChristoffelValue[slot_, cd_, metric_, metricInv_] := LeviCivitaChristoffel[
    NICovD[#1, ToNITensorIndex[#2, slot], cd] &,
    NITensor[metric, ToNITensorIndex[{##}, {slot, slot}]] &,
    NITensor[metricInv, ToNITensorIndex[{##}, {slot, slot}]] &
][a, DI@b, DI@c] // NITensorReduce[#, {a, b, c}] & // ExtractNITensor@{a, b, c};
SyntaxInformation@LeviCivitaChristoffelValue = {"ArgumentsPattern" -> {_, _, _, _}};

RiemannDifferenceValue[slot_, cd_, chris_] := RiemannDifference[
    NICovD[#1, ToNITensorIndex[#2, slot], cd] &,
    NITensor[chris, ToNITensorIndex[{##}, {slot, slot, slot}]] &
][DI@a, DI@b, DI@c, d] // NITensorReduce[#, {a, b, c, d}] & // ExtractNITensor@{a, b, c, d};
SyntaxInformation@RiemannDifferenceValue = {"ArgumentsPattern" -> {_, _, _}};

SymmetricRiemann[metric_, {a_, b_, c_, d_}] := metric[a, c]metric[b, d] - metric[a, d]metric[b, c];
SymmetricRiemann[metric][inds__] := SymmetricRiemann[metric, {inds}];
SyntaxInformation@SymmetricRiemann = {"ArgumentsPattern" -> {_, _}};

SymmetricRiemannETensor[metric_] := ETensor[SymmetricRiemann[metric, {DI@a, DI@b, DI@c, d}], {a, b, c, d}];
SyntaxInformation@SymmetricRiemannETensor = {"ArgumentsPattern" -> {_}};

Options@DefPerturbationOperator = Join[Options@ExpandDerivativeRules, {
    DisplayName -> "\[Delta]"
}];
DefPerturbationOperator[symbol_, opt : OptionsPattern[]] := (
    SetDelayed @@@ ExpandDerivativeRules[symbol[expr_?DerivativeExpandableQ, n_] :> symbol[ExpandDerivative[symbol[#, 1] &, expr], n - 1] /; n >= 1, FilterRules[{opt}, Options@ExpandDerivativeRules]];
    symbol[expr_, 0] := expr;
    symbol[symbol[expr_, n1_], n2_] := symbol[expr, n1 + n2];
    FindIndicesSlots[symbol[expr_, n_]] ^:= FindIndicesSlots[expr, {1}];
    SymmetryOfExpression[symbol[expr_, n_]] ^:= SymmetryOfExpression@expr;
    FindIndicesSlots[symbol[head_][_, args___]] ^:= MapIndicesSlots[MapAt[# + 1 &, #1, 1] -> #2 &, FindIndicesSlots[head@args]];
    SymmetryOfExpression[symbol[head_][_, args___]] ^:= SymmetryOfExpression[head@args];
    With[{
        displayName = OptionValue@DisplayName
    },
        symbol /: MakeBoxes[expr : symbol[expr2_][n_, args___], StandardForm] := With[{
            sub = MakeBoxes[expr2[args]],
            del = If[n === 1, displayName, SuperscriptBox[displayName, MakeBoxes@n]]
        }, InterpretationBox[RowBox@{del, sub}, expr]];
        symbol /: MakeBoxes[expr : symbol[expr2_, n_], StandardForm] := With[{
            sub = MakeBoxes@expr2,
            del = If[n === 1, displayName, SuperscriptBox[displayName, MakeBoxes@n]]
        }, InterpretationBox[RowBox@{del, "(", sub, ")"}, expr]];
    ];
);
SyntaxInformation@DefPerturbationOperator = {"ArgumentsPattern" -> {_, OptionsPattern[]}};

DefCovdPerturbationRules[pert_, cd_, metric_] := With[{
    covdPert = pert@cd,
    metricPert = pert@metric
},
    pert[cd[expr_, DI@a_], n2_] := pert[
        cd[pert[expr, 1], DI@a] + covdPert[expr, DI@a, 1]
    , n2 - 1] /; n2 > 0;
    pert[cd[expr_, a_?NonDIQ], n2_] := pert[With[{i = GetUniqueIndexOfSlotType@Null}, cd[expr, DI@i]metric[a, i]], n2];
    pert[covdPert[expr_, DI@a_, n_], n2_] := pert[
        covdPert[pert[expr, 1], DI@a, n] + covdPert[expr, DI@a, n + 1]
    , n2 - 1] /; n2 > 0;
    pert[covdPert[expr_, a_?NonDIQ, n_], n2_] := pert[With[{i = GetUniqueIndexOfSlotType@Null}, covdPert[expr, DI@i, n]metric[a, i]], n2];
];

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
ShiftedMetric /: MakeBoxes[expr : ShiftedMetric[a_, b_], StandardForm] := TensorInterpretationBox[expr, TensorGridBox["G", IndexBoxForm /@ {a, b}]];
ShiftedMetric /: MakeBoxes[expr : ShiftedMetric[n_, a_, b_], StandardForm] := TensorInterpretationBox[expr, TensorGridBox[If[n === 1, "\[Delta]G", RowBox@{SuperscriptBox["\[Delta]", MakeBoxes@n], "G"}], IndexBoxForm /@ {a, b}]];
DefSimpleTensor[ShiftedInverseMetric, {PredefinedSlotType, PredefinedSlotType}, {SCycles@{1, 2}}, DisplayName -> "\!\(\*SuperscriptBox[\(G\), \(-1\)]\)"];

DefPerturbationOperator[PerturbShiftedMetric];
PerturbShiftedMetric[PredefinedCovD[expr_, a_], n_] := PredefinedCovD[PerturbShiftedMetric[expr, n], a];
SyntaxInformation@PerturbShiftedMetric = {"ArgumentsPattern" -> {_, _.}};

PerturbationLeviCivitaChristoffel[cd_, metric_, metricPert_, n_][a_, b_, c_] := PerturbShiftedMetric[
    LeviCivitaChristoffel[PredefinedCovD, ShiftedMetric, ShiftedInverseMetric][a, b, c],
    n
] // ExpandPerturbShiftedMetric // ReplaceAll@{PredefinedCovD -> cd, ShiftedMetric -> metricPert, PredefinedMetric -> metric};
SyntaxInformation@PerturbationLeviCivitaChristoffel = {"ArgumentsPattern" -> {_, _, _, _}};

WithUnprotected[symbol_, body_] := WithUnprotected[{symbol}, body];
WithUnprotected[{symbols___}, body_] := (
    Unprotect[symbols];
    With[{ret = body}, Protect[symbols]; ret]
);
SetAttributes[WithUnprotected, HoldAll];
ExpandPerturbShiftedMetric[expr_] := expr /. {
    PerturbShiftedMetric[ShiftedInverseMetric[a_, b_], n2_] :> VarInverseMatrix[ShiftedInverseMetric[a, b], ShiftedMetric, n2],
    PerturbShiftedMetric[ShiftedMetric[a_, b_], n2_] :> ShiftedMetric[n2, a, b]
} /. {
    ShiftedMetric[a_, b_] :> PredefinedMetric[a, b],
    ShiftedInverseMetric -> PredefinedMetric
};

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
    HoldPattern@pert[mp_metricPert, n_] :> pert[SeparateMetricOne[mp, {-1, -1}], n]
} /. {
    HoldPattern@pert[metric[DI@a_, DI@b_], n_] :> metricPert[n, DI@a, DI@b],
    HoldPattern@pert[metricPert[n_, DI@a_, DI@b_], n2_] :> metricPert[n + n2, DI@a, DI@b],
    HoldPattern@pert[metric[a_?NonDIQ, b_?NonDIQ], n_] :> VarInverseMatrix[{a, b}, metricPert, n],
    HoldPattern@pert[metric[_DI, _?NonDIQ], _] -> 0,
    HoldPattern@pert[metric[_?NonDIQ, _DI], _] -> 0
};
ExpandMetricPerturbation[a_][expr_] := ExpandMetricPerturbation[expr, a];
SyntaxInformation@ExpandMetricPerturbation = {"ArgumentsPattern" -> {_, _.}};

ExpandTensorDetPerturbation[expr_, {pert_, tensor_, det_}] := expr //. {
    HoldPattern@pert[det, n_] :> pert[det IndexScope[tensor[a, b]pert[tensor[DI@a, DI@b], 1]], n - 1] /; n > 0
};
ExpandTensorDetPerturbation[cd_][expr_] := ExpandTensorDetPerturbation[expr, cd];
SyntaxInformation@ExpandTensorDetPerturbation = {"ArgumentsPattern" -> {_, _.}};

PertCovD[];

ExpandCovDPerturbation[expr_, {pert_, cd_, metric_, metricPert_}] := expr //. {
    pert[cd[expr2_, DI@a_], n2_] :> pert[
        cd[pert[expr2, 1], DI@a] + PertCovD[cd, expr2, DI@a, 1]
    , n2 - 1] /; n2 > 0,
    pert[cd[expr2_, a_?NonDIQ], n2_] :> pert[With[{i = GetUniqueIndexOfSlotType@Null}, cd[expr2, DI@i]metric[a, i]], n2],
    pert[PertCovD[cd, expr2_, DI@a_, n_], n2_] :> pert[
        PertCovD[cd, pert[expr2, 1], DI@a, n] + PertCovD[cd, expr2, DI@a, n + 1]
    , n2 - 1] /; n2 > 0,
    pert[PertCovD[cd, expr2_, a_?NonDIQ, n_], n2_] :> pert[With[{i = GetUniqueIndexOfSlotType@Null}, PertCovD[cd, expr2, DI@i, n]metric[a, i]], n2]
} //. {
    HoldPattern[PertCovD[cd, expr2_, DI@a_, n_]] :> CovDDifferenceDirect[expr2, {PerturbationLeviCivitaChristoffel[cd, metric, metricPert, n]}, a -> DI@Null]
};
ExpandCovDPerturbation[cds_][expr_] := ExpandCovDPerturbation[expr, cds];
SyntaxInformation@ExpandCovDPerturbation = {"ArgumentsPattern" -> {_, _.}};

ExpandRiemannPerturbation[expr_, metricPertArgs : {pert_, metric_, metricPert_}, {cd_, riemann_, ricci_, ricciScalar_}] := expr /. {
    HoldPattern@pert[ricciScalar, n_] :> With[{
        a = UInd[], b = UInd[]
    }, IndexScope[pert[metric[a, b] * ricci[DI@a, DI@b], n]]],
    HoldPattern@pert[e_riemann, n_] :> pert[SeparateMetricOne[e, {-1, -1, -1, 1}], n],
    HoldPattern@pert[e_ricci, n_] :> pert[SeparateMetricOne[e, {-1, -1}], n]
} /. {
    HoldPattern@pert[riemann[inds__], n_] :> PerturbationLeviCivitaRiemann[cd, metric, metricPert, n][inds],
    HoldPattern@pert[ricci[DI@a_, DI@b_], n_] :> With[{
        c = GetUniqueIndexOfSlotType@Null
    }, PerturbationLeviCivitaRiemann[cd, metric, metricPert, n][DI@a, DI@c, DI@b, c]]
} // ExpandMetricPerturbation[metricPertArgs];
ExpandRiemannPerturbation[a1_, a2_][expr_] := ExpandRiemannPerturbation[expr, a1, a2];
SyntaxInformation@ExpandRiemannPerturbation = {"ArgumentsPattern" -> {_, _, _.}};

ApplyIfNotMissing[fn_, arg_, expr_] := If[Head@arg === Missing, expr, fn[arg, expr]];
ApplyIfNotMissing[fn_, arg_][expr_] := ApplyIfNotMissing[fn, arg, expr];

LovelockDensity[riemann_, n_] := With[{
    inds = TempIndex /@ Range[2n]
}, Total[
    (Signature[List @@ #] Times @@ (riemann @@@ MapThread[Join, {Partition[inds, 2], Partition[DI /@ Permute[inds, InversePermutation[List @@ #]], 2]}])) & /@ DoubleTransversalInSymmetricGroup[SCycles /@ Partition[Range[2n], 2], {}]
]];
SyntaxInformation@LovelockDensity = {"ArgumentsPattern" -> {_, _}};

WeylToRiemann[riemann_, metric_, {a_, b_, c_, d_}] := With[{
    dim = IndexScope@metric[a, DI@a],
    e = GetUniqueIndexOfSlotType@Null
},
    riemann[a, b, c, d] -
        1 / (dim - 2) (metric[a, c]riemann[d, DI@e, b, e] - metric[a, d]riemann[c, DI@e, b, e] - metric[b, c]riemann[d, DI@e, a, e] + metric[b, d]riemann[c, DI@e, a, e]) +
        1 / (dim - 1) / (dim - 2) IndexScope[riemann[DI@a, DI@b, a, b]] (metric[a, c]metric[d, b] - metric[a, d]metric[c, b])
];
SyntaxInformation@WeylToRiemann = {"ArgumentsPattern" -> {_, _, _}};

WeylToRiemannSchouten[riemann_, metric_, schouten_, {a_, b_, c_, d_}] := With[{
    dim = IndexScope@metric[a, DI@a]
},
    riemann[a, b, c, d] - metric[a, c] schouten[b, d] + metric[b, c] schouten[a, d] + metric[a, d] schouten[b, c] - metric[b, d] schouten[a, c]
];
SyntaxInformation@WeylToRiemannSchouten = {"ArgumentsPattern" -> {_, _, _, _}};

RiemannBianchiRelations[expr_List, covd_, riemann_] := Join @@ (RiemannBianchiRelations[#, covd, riemann] & /@ expr);
RiemannBianchiRelations[expr_Times, covd_, riemann_] := Join @@ MapIndexed[ReplacePart[expr, #2 -> RiemannBianchiRelations[#1, covd, riemann]] &, List @@ expr];
RiemannBianchiRelations[covd_[inner : riemann_[a_, b_, c_, d_], e_], covd_, riemann_] := Join[{
    covd[riemann[c, d, a, b], e] + covd[riemann[d, e, a, b], c] + covd[riemann[e, c, a, b], d],
    (* covd[riemann[a, b, c, d], e] + covd[riemann[a, b, d, e], c] + covd[riemann[a, b, e, c], d] *)
    covd[riemann[a, b, c, d], e] + covd[riemann[e, a, c, d], b] + covd[riemann[b, e, c, d], a]
}, covd[RiemannBianchiRelations[inner, covd, riemann], e]];
RiemannBianchiRelations[riemann_[a_, b_, c_, d_], _, riemann_] := {
    riemann[a, b, c, d] + riemann[a, c, d, b] + riemann[a, d, b, c]
};
RiemannBianchiRelations[covd_[expr_, a_], covd_, riemann_] := covd[RiemannBianchiRelations[expr, covd, riemann], a];
RiemannBianchiRelations[_, _, _] = {};
SyntaxInformation@RiemannBianchiRelations = {"ArgumentsPattern" -> {_, _, _}};

CovDCommutatorRelations[expr_List, covd_, commu_] := Join @@ (CovDCommutatorRelations[#, covd, commu] & /@ expr);
CovDCommutatorRelations[expr_Times, covd_, commu_] := Join @@ MapIndexed[ReplacePart[expr, #2 -> CovDCommutatorRelations[#1, covd, commu]] &, List @@ expr];
CovDCommutatorRelations[expr0 : covd_[inner : covd_[expr_, a_], b_], covd_, commu_] := Prepend[
    covd[CovDCommutatorRelations[inner, covd, commu], b],
    expr0 - covd[covd[expr, b], a] - commu[expr, b, a]
];
CovDCommutatorRelations[_, _, _] = {};
SyntaxInformation@CovDCommutatorRelations = {"ArgumentsPattern" -> {_, _, _}};

VectorBasisCommutator[mat_, ders_] := With[{
    mat2 = ITensorContractTwo[Times, mat, ITensorOuter[Construct, ders, mat, {}], {{2, 1}}]
}, mat2 - ITensorTranspose[mat2, {2, 1, 3}]];
SyntaxInformation@VectorBasisCommutator = {"ArgumentsPattern" -> {_, _}};

End[];

Protect @@ Select[Names["`*"], !StringMatchQ[#, "$" ~~ __] &];

EndPackage[];
