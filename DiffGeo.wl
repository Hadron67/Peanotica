BeginPackage["Peanotica`DiffGeo`", {"Peanotica`Perm`", "Peanotica`Core`"}];

Scan[Unprotect@#; ClearAll@#; &, Names@{$Context <> "*"}];

LeviCivitaChristoffel::usage = "LeviCivitaChristoffel[cd, metric] represents the Christoffel tensor relating the possibly non-metric compatible covariant derivative cd and the Levi-Civita connection. LeviCivitaChristoffel[...][a, DI@b, DI@c] gives the expression.";
RiemannDifference::usage = "RiemannDifference[cd, christoffel] represents the difference of the Riemann tensors of cd and cd1, where christoffel is the Christoffel tensor relation cd and cd1.";
CovDDifference::usage = "CovDDifference[christoffel] represents the difference ";

DerConstantQ::usage = "DerConstantQ[expr] gives true if expr the derivative operator acting on expr should give 0.";
DerFunctionQ::usage = "DerFunctionQ[fn] gives true if fn should be treated as a scalar function by the derivative operators. Derivative[...][fn] is always treated so.";
DerConstants;
DerFunctions;
DefGeneralDerivativeOperator;

SymmetriedDer;
AllowPassThrough;
DefParametreDerivativeOperator::usage = "";
DefTensorDerivativeOperator::usage = "DefTensorDerivativeOperator[op, slotType].";

CovDValue::usage = "CovDValue[expr, a, {pdvalue, christoffel, torsion}]";

Begin["`Private`"];

LeviCivitaChristoffel[cd_, metric_] := LeviCivitaChristoffel[cd, metric, metric];
LeviCivitaChristoffel[cd_, metric_, metricInv_][a_, DI@b_, DI@c_] := With[{
    d = GetUniqueIndexOfSlotType[None]
}, 1/2 metricInv[a, d](cd[metric[DI@d, DI@c], DI@b] + cd[metric[DI@b, DI@d], DI@c] - cd[metric[DI@b, DI@c], DI@d])];
SyntaxInformation@LeviCivitaChristoffel = {"ArgumentsPattern" -> {_, _, _.}};

RiemannDifference[cd_, chris_][DI@a_, DI@b_, DI@c_, d_] := Function[{e},
    -cd[chris[d, DI@b, DI@c], DI@a] + cd[chris[d, DI@a, DI@c], DI@b] + chris[e, DI@a, DI@c]chris[d, DI@b, DI@e] - chris[e, DI@b, DI@c]chris[d, DI@a, DI@e]
]@GetUniqueIndexOfSlotType[None];

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
    (
        sym /: FindIndicesSlots[sym[expr_, ##]] := Append[FindIndicesSlots[expr, {1}], {2} -> slot];
        sym /: FindIndicesSlots[sym[##]] := {{1} -> slot};
    ) & @@@ ConstantArray[_, Length@slots];
    If[OptionValue@SymmetriedDer,
        sym /: SymmetryOfExpression[s_sym] := SymmetryOfSymmetrizedDer[s, symmetry]
    , If[Length@symmetry > 0,
        sym /: SymmetryOfExpression[sym[expr_, __]] := Join[SymmetryOfExpression@expr, ShiftPermutation[symmetry, Length@FindIndicesSlots@expr]]
    ,
        sym /: SymmetryOfExpression[sym[expr_, __]] := SymmetryOfExpression@expr
    ]];
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
        sym /: MakeBoxes[expr : sym[expr2_, inds__], StandardForm] := InterpretationBox[RowBox@#, expr, Editable -> False] &@{
            TensorGridBox[name, SeparateIndexName /@ {inds}],
            If[expr2 =!= Null, MakeBoxes@expr2, Nothing]
        }
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
        sym /: MakeBoxes[expr : sym[expr2_, vs__], StandardForm] := InterpretationBox[RowBox@#, expr, Editable -> False] &@Join[
            SubscriptBox[name, MakeBoxes@#] & /@ {vs},
            If[expr2 =!= Null, {MakeBoxes@expr2}, {}]
        ]
    ];
    sym /: NITensorReduce[sym[expr_, vs__], frees_] := sym[NITensorReduce[expr, frees], vs];
);

End[];

EndPackage[];
