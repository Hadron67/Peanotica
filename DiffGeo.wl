BeginPackage["Peanotica`DiffGeo`", {"Peanotica`Perm`", "Peanotica`Core`"}];

Scan[Unprotect@#; ClearAll@#; &, Names@{$Context <> "*"}];

LeviCivitaChristoffel;
RiemannDifference;

DerConstantQ::usage = "DerConstantQ[expr] gives true if expr the derivative operator acting on expr should give 0.";
DerFunctionQ::usage = "DerFunctionQ[fn] gives true if fn should be treated as a scalar function by the derivative operators. Derivative[...][fn] is always treated so.";
DerConstants;
DerFunctions;
DefGeneralDerivativeOperator;

SymmetriedDer;
AllowPassThrough;
DefScalarDerivativeOperator::usage = "";
DefVectorDerivativeOperator::usage = "DefVectorDerivativeOperator[op, slotType].";

CovDValue::usage = "CovDValue[pdvalue, christoffel]";

Begin["`Private`"];

LeviCivitaChristoffel[cd_, metric_][a_, DI@b_, DI@c_] := With[{
    d = GetUniqueIndexOfSlotType[None]
}, 1/2 metric[a, d](cd[DI@b]@metric[DI@d, DI@c] + cd[DI@c]@metric[DI@b, DI@d] - cd[DI@d]@metric[DI@b, DI@c])];
SyntaxInformation@LeviCivitaChristoffel = {"ArgumentsPattern" -> {_, _}};

RiemannDifference[cd_, chris_][DI@a_, DI@b_, DI@c_, d_] := Function[{e},
    -cd[DI@a]@chris[d, DI@b, DI@c] + cd[DI@b]@chris[d, DI@a, DI@c] + chris[e, DI@a, DI@c]chris[d, DI@b, DI@e] - chris[e, DI@b, DI@c]chris[d, DI@a, DI@e]
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

Options[DefVectorDerivativeOperator] = Join[Options[DefGeneralDerivativeOperator], {
    DisplayName -> "\[PartialD]",
    SymmetriedDer -> False,
    AllowPassThrough -> True (* any usecase for setting this to False ? *)
}];
SymmetryOfSymmetrizedDer[sym_[expr_, ind_]] := With[{
    subExprAndInd = NestWhile[{
        Extract[#[[1]], {1, 1}, Hold], Append[#[[2]],
        Extract[#[[1]], {1, 2}]]
    } &, {Hold@expr, {ind}}, MatchQ[#[[1]], Hold@sym[_, _]] &]
},
    Join[
        Extract[subExprAndInd[[1]], 1, SymmetryOfExpression],
        With[{inds = subExprAndInd[[2]]}, If[Length@inds >= 2,
            ShiftPermutation[SymmetricGenSet @@ Range@Length@inds, Length@Extract[subExprAndInd[[1]], 1, FindIndicesSlots]],
            {}
        ]]
    ]
];
SetAttributes[SymmetryOfSymmetrizedDer, HoldAll];
DefVectorDerivativeOperator[sym_, slot_, opt : OptionsPattern[]] := (
    DefGeneralDerivativeOperator[sym, FilterRules[{opt}, Options@DefGeneralDerivativeOperator]];
    sym[a_][expr_] := sym[expr, a];
    sym /: FindIndicesSlots[sym[expr_, _]] := Append[FindIndicesSlots[expr, {1}], {2} -> slot];
    sym /: FindIndicesSlots[sym[_]] := {{1} -> slot};
    If[OptionValue@SymmetriedDer,
        sym /: SymmetryOfExpression[s_sym] := SymmetryOfSymmetrizedDer[s]
    ,
        sym /: SymmetryOfExpression[sym[expr_, _]] := SymmetryOfExpression@expr
    ];
    If[OptionValue@AllowPassThrough,
        sym /: ExpressionPassThroughQ[sym[expr_, ind_], tensor_, pos_] := Switch[pos[[1]],
            1, sym[tensor, ind] === 0 && ExpressionPassThroughQ[expr, tensor, Delete[pos, 1]],
            2, True,
            _, False
        ]
    ,
        sym /: ExpressionPassThroughQ[_sym, _, pos_] := pos[[1]] === 2
    ];
    With[{
        name = OptionValue@DisplayName
    },
        sym /: MakeBoxes[expr : sym[expr2_, ind_], StandardForm] := InterpretationBox[RowBox@#, expr, Editable -> False] &@{
            TensorGridBox[name, {SeparateIndexName@ind}],
            MakeBoxes@expr2
        }
    ];
    sym /: NITensorReduce[sym[expr_, ind_], frees_] := ReduceNITensorContractions[ReverseApplied@Construct, {expr, NITensor[sym, {IndexName@ind}]}, frees];
);
SyntaxInformation@DefVectorDerivativeOperator = {"ArgumentsPattern" -> {_, _, OptionsPattern[]}};

Options[DefScalarDerivativeOperator] = Join[Options@DefGeneralDerivativeOperator, {
    DisplayName -> "\[PartialD]",
    AllowPassThrough -> True
}];
DefScalarDerivativeOperator[sym_, opt : OptionsPattern[]] := (
    DefGeneralDerivativeOperator[sym, FilterRules[{opt}, Options@DefGeneralDerivativeOperator]];
    sym[t_, t_] = 1;
    sym[v_]@expr_ := sym[expr, v];
    sym[sym[expr_, v1_], v2_] := sym[sym[expr, v2], v1] /; !OrderedQ[v1, v2];
    sym[ETensor[expr_, inds_], v_] := ETensor[sym[expr, v], inds];
    sym[NITensor[expr_, inds_], v_] := NITensor[sym[expr, v], inds];
    sym /: FindIndicesSlots[sym[expr_, _]] := FindIndicesSlots[expr, {1}];
    sym /: FindIndicesSlots[sym[_]] = {};
    sym /: SymmetryOfExpression[sym[expr_, _]] := SymmetryOfExpression@expr;
    If[OptionValue@AllowPassThrough,
        sym /: ExpressionPassThroughQ[sym[expr_, v_], tensor_, pos_] := Switch[pos[[1]],
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
        sym /: MakeBoxes[expr : sym[expr2_, v_], StandardForm] := InterpretationBox[RowBox@#, expr, Editable -> False] &@{
            SubscriptBox[name, MakeBoxes@v],
            MakeBoxes@expr2
        }
    ];
    sym /: NITensorReduce[sym[expr_, v_], frees_] := sym[NITensorReduce[expr, frees], v];
);

End[];

EndPackage[];
