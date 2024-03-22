BeginPackage["Peanotica`DiffGeo`", {"Peanotica`Perm`", "Peanotica`Core`"}];

Scan[Unprotect@#; ClearAll@#; &, Names@{$Context <> "*"}];

LeviCivitaChristoffelDer::usage = "LeviCivitaChristoffel[cd, der, metric]";
LeviCivitaChristoffel::usage = "LeviCivitaChristoffel[cd, metric] represents the Christoffel tensor relating the possibly non-metric compatible covariant derivative cd and the Levi-Civita connection. LeviCivitaChristoffel[...][a, DI@b, DI@c] gives the expression.";
RiemannDifferencePart1;
RiemannDifferencePart2;
RiemannDifference::usage = "RiemannDifference[cd, christoffel] represents the difference of the Riemann tensors of cd and cd1, where christoffel is the Christoffel tensor relation cd and cd1.";
CovDDifference::usage = "CovDDifference[christoffelProvider] represents the difference ";

Cocurvature::usage = "";
Cotorsion::usage = "";

DerConstantQ::usage = "DerConstantQ[expr] gives true if expr the derivative operator acting on expr should give 0.";
DerFunctionQ::usage = "DerFunctionQ[fn] gives true if fn should be treated as a scalar function by the derivative operators. Derivative[...][fn] is always treated so.";
DerConstants;
DerFunctions;
DefGeneralDerivativeOperator;

SymmetriedDer;
AllowPassThrough;
DefParametreDerivativeOperator::usage = "";
DefTensorDerivativeOperator::usage = "DefTensorDerivativeOperator[op, slotType].";

CBTensor::usage = "CBTensor[expr, slots]";
ICovD::usage = "ICovD[expr, a, value]";
NITensorCovD::usage = "NITensorCovD[expr, value, inds, ind]";
NITensorRaiseLowerIndices::usage = "NITensorChangeSlot[tensor, slots]";

DefCurvatureTensors;
DefRiemannToRicciRules::usage = "DefRiemannToRicciRules[riemann, ricci]";
RicciOf;
RicciScalarOf;

RiemannScalars;

DefPerturbationOperator::usage = "DefPerturbationOperator[symbol]";

Begin["`Private`"];

LeviCivitaChristoffelDer[cd_, der_, metric_] := LeviCivitaChristoffelDer[cd, der, metric, metric];
LeviCivitaChristoffelDer[cd_, der_, metric_, metricInv_][a_, b_, c_] := With[{
    d = GetUniqueIndexOfSlotType[None]
}, 1/2 metricInv[a, d](cd[der@metric[DI@d, c], b] + cd[der@metric[b, DI@d], c] - cd[der@metric[b, c], DI@d])];
SyntaxInformation@LeviCivitaChristoffelDer = {"ArgumentsPattern" -> {_, _, _, _.}};

LeviCivitaChristoffel[cd_, metric_] := LeviCivitaChristoffelDer[cd, Identity, metric];
LeviCivitaChristoffel[cd_, metric_, metricInv_] := LeviCivitaChristoffelDer[cd, Identity, metric, metricInv];
SyntaxInformation@LeviCivitaChristoffel = {"ArgumentsPattern" -> {_, _, _.}};

RiemannDifferencePart1[cd_, chris_][a_, b_, c_, d_] := -cd[chris[d, b, c], a] + cd[chris[d, a, c], b];
SyntaxInformation@RiemannDifferencePart1 = {"ArgumentsPattern" -> {_, _}};

RiemannDifferencePart2[chris_][a_, b_, c_, d_] := Function[{e},
    chris[e, a, c]chris[d, b, DI@e] - chris[e, b, c]chris[d, a, DI@e]
]@GetUniqueIndexOfSlotType[None];
SyntaxInformation@RiemannDifferencePart2 = {"ArgumentsPattern" -> {_}};

RiemannDifference[cd_, chris_][a_, b_, c_, d_] := RiemannDifferencePart1[cd, chris][a, b, c, d] + RiemannDifferencePart2[chris][a, b, c, d];
SyntaxInformation@RiemannDifference = {"ArgumentsPattern" -> {_, _}};

CovDDifference[chris_, v_, a_][];

DerConstantQ[_?NumberQ] = True;
SyntaxInformation@DerConstantQ = {"ArgumentsPattern" -> {_}};

DerFunctionQ[Times] = True;
DerFunctionQ[Power] = True;
SyntaxInformation@DerFunctionQ = {"ArgumentsPattern" -> {_}};

Options[DefGeneralDerivativeOperator] = {
    DerConstants -> {_?DerConstantQ},
    DerFunctions -> {_?DerFunctionQ}
};
MapDerivativeOnFunction[der_, fn_[args__]] := With[{
    mat = IdentityMatrix@Length@{args}
},
    Total@MapIndexed[(Derivative @@ mat[[#2[[1]]]])[fn][args] der[#1] &, {args}]
];
DefGeneralDerivativeOperator[sym_, opt : OptionsPattern[]] := (
    sym[expr_Plus, rest___] := sym[#, rest] & /@ expr;
    sym[expr_List, rest__] := sym[#, rest] & /@ expr;
    sym[Derivative[ders__][fn_][args__], rest___] := With[{
        mat = IdentityMatrix@Length@{args}
    },
        Total@MapIndexed[(Derivative @@ (mat[[#2[[1]]]] + {ders}))[fn][args] sym[#1, rest] &, {args}]
    ];
    sym[IndexScope[expr_], rest___] := sym[ReplaceDummiesToUnique@expr, rest];
    Scan[sym[#, ___] = 0; &, OptionValue@DerConstants];
    Scan[sym[fn : #[args__], rest___] := MapDerivativeOnFunction[sym[#, rest] &, fn]; &, OptionValue@DerFunctions];
    sym[_?NumberQ] = 0;
);
SyntaxInformation@DefGeneralDerivativeOperator = {"ArgumentsPattern" -> {_, OptionsPattern[]}};

Options[DefTensorDerivativeOperator] = Join[Options[DefGeneralDerivativeOperator], {
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
    DefGeneralDerivativeOperator[sym, FilterRules[{opt}, Options@DefGeneralDerivativeOperator]];
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

Options[DefParametreDerivativeOperator] = Join[Options@DefGeneralDerivativeOperator, {
    DisplayName -> "\[PartialD]",
    AllowPassThrough -> True
}];
DefParametreDerivativeOperator[sym_, opt : OptionsPattern[]] := (
    DefGeneralDerivativeOperator[sym, FilterRules[{opt}, Options@DefGeneralDerivativeOperator]];
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
ICovD /: FindIndicesSlots@ICovD[expr_, a_, _] := Join[FindIndicesSlots[expr, {1}], {{2} -> None}];
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

CBTensor::unmatchedSlots = "";

Options@DefPerturbationOperator = Options@DefGeneralDerivativeOperator;
DefPerturbationOperator[symbol_, opt : OptionsPattern[]] := (
    DefGeneralDerivativeOperator[symbol, FilterRules[{opt}, Options@DefGeneralDerivativeOperator]];
    FindIndicesSlots[symbol[t_, n_][inds__]] ^:= FindIndicesSlots[t[inds]];
    SymmetryOfExpression[symbol[t_, n_][inds__]] ^:= SymmetryOfExpression@t@inds;
    symbol /: MakeBoxes[expr : symbol[t_, n_][inds__], StandardForm] := With[{
        sub = MakeBoxes@t[inds],
        del = If[n === 1, "\[Delta]", SuperscriptBox["\[Delta]", MakeBoxes@n]]
    }, InterpretationBox[RowBox@{del, sub}, expr]];
);

End[];

EndPackage[];
