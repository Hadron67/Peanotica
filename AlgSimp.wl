BeginPackage["Peanotica`AlgSimp`", {"Peanotica`Perm`", "Peanotica`Core`"}];

Scan[Unprotect@#; ClearAll@#; &, Names@{$Context <> "*"}];

SlotsOfAlgTensor::usage = "SlotsOfAlgTensor[tensor]";
SymmetryOfAlgTensor::usage = "SymmetryOfAlgTensor[tensor]";

AlgTensorOrder::usage = "AlgTensorOrder[t1, t2]";
AlgTensorTerm::usage = "AlgTensorTerm[type, frees, perm]";
AlgTensorTermReduce::usage = "AlgTensorTermReduce[expr]";
AlgRelations::usage = "AlgRelations[tensor, frees, perm]";
EnumerateAlgTensorTerms::usage = "EnumerateAlgTensorTerms[tensor, frees]";
AlgTensorReduceRules::usage = "AlgTensorReduceRules[allTerms, relations]";
AlgTensorSimplifyData::usage = "AlgTensorSimplifyData[tensor, frees]";

AlgDRiemann::usage = "AlgDRiemann[n]";
AlgDRiemannList::usage = "AlgDRiemann[list]";
AlgDRicci::usage = "AlgDRicci[n]";
AlgEpsilon::usage = "AlgEpsilon[n]";

ToAlgTensor::usage = "ToAlgTensor[expr, frees]";
RiemannToAlgTensor::usage = "RiemannToAlgTensor[expr, frees, info]";
AlgTensorToRiemann::usage = "AlgTensorToRiemann[tensor, frees, covd, riemann]";
AlgDRiemannETensor::usage = "AlgDRiemannETensor[n, covd, riemann]";
IndexedAlgTensor::usage = "IndexedAlgTensor[expr, inds]";
IndexedAlgTensorCovD::usage = "IndexedAlgTensorCovD[expr, ind]";
ExtractAlgTensor::usage = "ExtractAlgTensor[expr, frees]";
ExtractAllAlgTensors::usage = "ExtractAllAlgTensors[expr, frees]";
RiemannTensorOrder::usage = "RiemannTensorOrder[{cd, riemann, ricci, ricciScalar}, a, b]";

Begin["`Private`"];

SlotsOfAlgTensor[expr_TensorProduct] := Join @@ Map[SlotsOfAlgTensor, List @@ expr];
SyntaxInformation@SlotsOfAlgTensor = {"ArgumentsPattern" -> {_}};

SymmetryOfAlgTensor[expr_TensorProduct] := With[{
    slotCounts = Length@SlotsOfAlgTensor@# & /@ List @@ expr
}, Join[
    ShiftAndJoinGenSets[SymmetryOfAlgTensor /@ List @@ expr, slotCounts],
    ShiftAndJoinGenSets[
        MapShiftedIndexed[If[#1 === #2, With[{len = Length@SlotsOfAlgTensor@#1}, BlockSymmetricGenSet[Range@len, len + Range@len]], {}] &, List @@ expr],
        Drop[slotCounts, -1]
    ]
]];
SyntaxInformation@SymmetryOfAlgTensor = {"ArgumentsPattern" -> {_}};

AlgTensorTerm[type_, frees_, a_. * perm_Images] := a * AlgTensorTerm[type, frees, List @@ perm];
AlgTensorTerm[_, _, 0] = 0;
AlgTensorTerm[expr_ETensor, frees_, perm_] := With[{
    freeInds = Table[GetUniqueIndexOfSlotType@Null, frees],
    dummies = Table[GetUniqueIndexOfSlotType@Null, (Length@perm - frees) / 2]
}, ETensor[expr @@ Permute[Join[freeInds, Join @@ ({#, DI@#} & /@ dummies)], InversePermutation@perm], freeInds]];
AlgTensorTerm /: (head_ /; head === TensorProduct)[l___, AlgTensorTerm[type1_, frees_, perm1_], AlgTensorTerm[type2_, frees_, perm2_], r___] := TensorProduct[l, AlgTensorTerm[TensorProduct[type1, type2], frees, Join[perm1, perm2]], r];
SyntaxInformation@AlgTensorTerm = {"ArgumentsPattern" -> {_, _, _}};

SplitAlgTensor[expr_List, frees_, perm_] := MapThread[AlgTensorTerm[#1, frees, #2] &, {expr, TakeList[perm, Length@SlotsOfAlgTensor@# & /@ expr]}];

SortAlgTensor[AlgTensorTerm[tensor_, frees_, perm_]] := SortAlgTensor[tensor, frees, perm];
SortAlgTensor[expr_TensorProduct] := TensorProduct @@ Sort[SortAlgTensor /@ List @@ expr, AlgTensorOrder];
SortAlgTensor[expr_] := expr;
SortAlgTensor[expr_TensorProduct, frees_, perm_] := TensorProduct @@ SortBy[SortAlgTensor /@ SplitAlgTensor[List @@ expr, frees, perm], First, AlgTensorOrder];
SortAlgTensor[expr_, frees_, perm_] := AlgTensorTerm[expr, frees, perm];
SyntaxInformation@SortAlgTensor = {"ArgumentsPattern" -> {_, _., _.}};

AlgTensorOrder[a_, a_] = 0;
AlgTensorOrder[a_, b_] := Order[a, b];
AlgTensorOrder[e1_TensorProduct, e2_TensorProduct] := LexicographicOrder[AlgTensorOrder];
SyntaxInformation@AlgTensorOrder = {"ArgumentsPattern" -> {_, _}};

AlgTensorTermReduce[expr_Plus] := AlgTensorTermReduce /@ expr;
AlgTensorTermReduce[expr_List] := AlgTensorTermReduce /@ expr;
AlgTensorTermReduce[expr_AlgTensorTerm] := AlgTensorTermReduceSorted@SortAlgTensor@expr;
SyntaxInformation@AlgTensorTermReduce = {"ArgumentsPattern" -> {_}};

AlgTensorTermReduceSorted[AlgTensorTerm[type_, frees_, perm_]] := With[{
    slots = SlotsOfAlgTensor@type
}, AlgTensorTerm[
    type,
    frees,
    DoubleCosetRepresentative[
        SymmetryOfAlgTensor@type,
        Images @@ perm,
        Join[DummiesGenSet[1, Partition[Range[frees + 1, Length@perm], 2]]]
    ]
]];

SlotsOfAlgTensor@AlgEpsilon[n_] ^:= ConstantArray[1, n];
SymmetryOfAlgTensor@AlgEpsilon[n_] ^:= -SymmetricGenSet @@ Range@n;
AlgEpsilon /: MakeBoxes[expr : AlgEpsilon[n_], StandardForm] := With[{
    box = If[Hold@n === Hold@1, "\[Epsilon]", SubscriptBox["\[Epsilon]", MakeBoxes@n]]
}, InterpretationBox[box, expr]];
SyntaxInformation@AlgEpsilon = {"ArgumentsPattern" -> {_}};

MapShiftedIndexed[f_, expr_] := MapThread[f, {Drop[expr, -1], Drop[expr, 1], Range[Length@expr - 1]}];

AlgRelations[expr_List] := Join @@ (AlgRelations /@ expr);
AlgRelations[AlgTensorTerm[terms_, frees_, perm_]] := AlgRelations[terms, frees, perm];
AlgRelations[terms_, frees_, perm_List] := AlgRelations[terms, frees, perm, Max @@ perm];
AlgRelations[terms_TensorProduct, frees_, perm_, maxInd_] := With[{
    subTerms = SplitAlgTensor[List @@ terms, frees, perm]
}, Join @@ MapIndexed[Thread[TensorProduct @@ ReplacePart[subTerms, #2 -> #1]] &, AlgRelations[##, maxInd] & @@@ subTerms]];
AlgRelations[_, _, _, _] = {};
SyntaxInformation@AlgRelations = {"ArgumentsPattern" -> {_, _, _, _.}};

SlotsOfAlgTensor@AlgDRiemann[n_] ^:= ConstantArray[1, n + 4];
SymmetryOfAlgTensor@AlgDRiemann[n_] ^= RiemannSymmetricGenSet[n + 1];
AlgTensorOrder[AlgDRiemann[n1_], AlgDRiemann[n2_]] ^:= Order[n1, n2];
AlgDRiemann /: MakeBoxes[expr : AlgDRiemann[n_], StandardForm] := With[{
    box = Switch[Hold@n, Hold@0, "R", Hold@1, "\[Del]R", _, RowBox@{SuperscriptBox["\[Del]", MakeBoxes@n], "R"}]
}, InterpretationBox[box, expr]];

AlgDRiemann /: AlgRelations[AlgDRiemann[n_], frees_, inds : {l___, a_, b_, c_, d_}, maxInd_] := {
    AlgTensorTerm[AlgDRiemann[n], frees, #] & /@ {{l, a, b, c, d}, {l, a, c, d, b}, {l, a, d, b, c}} // Total,
    AlgRelationBianchi2[n, frees, inds]
};

AlgRelationBianchi2[n_, frees_, {l___, e_, a_, b_, c_, d_}] := Total[
    AlgTensorTerm[AlgDRiemann[n], frees, #] & /@ {{l, e, a, b, c, d}, {l, a, b, e, c, d}, {l, b, e, a, c, d}}
];
AlgRelationBianchi2[_, _, _] = Nothing;

SyntaxInformation@AlgDRiemann = {"ArgumentsPattern" -> {_}};

AlgDRiemannList[list_] := TensorProduct @@ (AlgDRiemann /@ list);
SyntaxInformation@AlgDRiemannList = {"ArgumentsPattern" -> {_}};

SlotsOfAlgTensor@AlgDRicci[n_] ^:= ConstantArray[1, n + 2];
SymmetryOfAlgTensor@AlgDRicci[n_] ^:= {SCycles@{n + 1, n + 2}};
AlgDRicci /: AlgTensorOrder[_AlgDRicci, _AlgDRiemann] = 1;
AlgDRicci /: AlgTensorOrder[_AlgDRiemann, _AlgDRicci] = -1;
AlgTensorOrder[AlgDRicci[n1_], AlgDRicci[n2_]] ^:= Order[n1, n2];
AlgDRicci /: MakeBoxes[expr : AlgDRicci[n_], StandardForm] := With[{
    box = Switch[Hold@n, Hold@0, "Ric", Hold@1, "\[Del]Ric", _, RowBox@{SuperscriptBox["\[Del]", MakeBoxes@n], "Ric"}]
}, InterpretationBox[box, expr]];
SyntaxInformation@AlgDRicci = {"ArgumentsPattern" -> {_}};

EnumerateAlgTensorTerms[tensors_List, frees_] := Union @@ (EnumerateAlgTensorTerms[#, frees] & /@ tensors);
EnumerateAlgTensorTerms[tensor_, frees_] := With[{
    tensor2 = SortAlgTensor@tensor
},
    AlgTensorTerm[tensor2, frees, #] & /@ Sort@DoubleTransversalInSymmetricGroup[SymmetryOfAlgTensor@tensor2, DummiesGenSet[1, Partition[Range[frees + 1, Length@SlotsOfAlgTensor@tensor2], 2]]]
];
SyntaxInformation@EnumerateAlgTensorTerms = {"ArgumentsPattern" -> {_, _}};

AlgTensorReduceOneRowRule[allTerms_, row_] := With[{
    pos0 = FirstPosition[Reverse@row, 1, Null, {1}]
}, If[pos0 =!= Null,
    With[{
        pos = Length@row - pos0[[1]] + 1
    },
        allTerms[[pos]] -> -ReplacePart[row, pos -> 0] . allTerms
    ]
,
    Nothing
]];
AlgTensorReduceRules[allTerms_, relations_] := AlgTensorReduceOneRowRule[allTerms, #] & /@ Simplify[
    Reverse /@ RowReduce[Reverse /@ (DeleteCases[relations, 0] /. Thread[allTerms -> IdentityMatrix@Length@allTerms])]
];
SyntaxInformation@AlgTensorReduceRules = {"ArgumentsPattern" -> {_, _}};

AlgTensorSimplifyData[tensor_, frees_] := With[{
    allTerms = EnumerateAlgTensorTerms[tensor, frees]
}, {allTerms, AlgTensorReduceRules[allTerms, AlgRelations@allTerms // TensorExpand // AlgTensorTermReduce]}];
SyntaxInformation@AlgTensorSimplifyData = {"ArgumentsPattern" -> {_, _}};

AlgDRiemannETensor[n_, covd_, riemann_] := With[{inds = Array[DefaultIndex, n + 4]}, ETensor[
    Fold[covd, riemann @@ Take[inds, -4], Reverse@Drop[inds, -4]],
    inds
]];
SyntaxInformation@AlgDRiemannETensor = {"ArgumentsPattern" -> {_, _, _}};

IndexedAlgTensor /: (head_ /; head === Times)[l___, IndexedAlgTensor[expr1_, inds1_], IndexedAlgTensor[expr2_, inds2_], r___] := Times[
    l,
    IndexedAlgTensor[TensorProduct[expr1, expr2], Join[inds1, inds2]],
    r
];
SyntaxInformation@IndexedAlgTensor = {"ArgumentsPattern" -> {_, _}};

IndexedAlgTensorCovD[IndexedAlgTensor[tensor_, inds_], ind_] := IndexedAlgTensorCovD[tensor, inds, ind];
IndexedAlgTensorCovD[AlgDRiemann[n_], inds_, ind_] := IndexedAlgTensor[AlgDRiemann[n + 1], Prepend[inds, ind]];
IndexedAlgTensorCovD[AlgDRicci[n_], inds_, ind_] := IndexedAlgTensor[AlgDRicci[n + 1], Prepend[inds, ind]];
SyntaxInformation@IndexedAlgTensorCovD = {"ArgumentsPattern" -> {_, _}};

ExtractAlgTensor[IndexedAlgTensor[tensor_, inds_], frees_] := AlgTensorTerm[tensor, Length@frees,
    InversePermutation[(Join @@ (CollectGroupedIndices /@ FindAndDropFrees[GroupIndexList@inds, frees]))[[All, 1]]]
];
ExtractAlgTensor[frees_][expr_] := ExtractAlgTensor[expr, frees];
SyntaxInformation@ExtractAlgTensor = {"ArgumentsPattern" -> {_, _.}};

ExtractAllAlgTensors[expr_, frees_] := expr /. expr2_IndexedAlgTensor :> ExtractAlgTensor[expr2, frees];
ExtractAllAlgTensors[frees_][expr_] := ExtractAllAlgTensors[expr, frees];
SyntaxInformation@ExtractAllAlgTensors = {"ArgumentsPattern" -> {_, _.}};

RiemannTensorOrder[cds_, e1_HeldTimes, e2_HeldTimes] := OrderOr[
    LexicographicOrder[Reverse[List @@ e1], Reverse[List @@ e2], RiemannTensorOrder[cds]],
    LexicographicOrder[FindIndicesSlotsAndNames[e1][[All, 2, 1]],
    FindIndicesSlotsAndNames[e2][[All, 2, 1]], IndexOrder]
];
RiemannTensorOrder[{cd_, riemann_, ricci_, ricciScalar_}, ricciScalar, ricciScalar] = 0;
RiemannTensorOrder[{cd_, riemann_, ricci_, ricciScalar_}, _ricci, _ricci] = 0;
RiemannTensorOrder[{cd_, riemann_, ricci_, ricciScalar_}, _riemann, _riemann] = 0;
RiemannTensorOrder[cds : {cd_, riemann_, ricci_, ricciScalar_}, cd[expr1_, _], cd[expr2_, _]] := RiemannTensorOrder[cds, expr1, expr2];
RiemannTensorOrder[{cd_, riemann_, ricci_, ricciScalar_}, _, _cd] = 1;
RiemannTensorOrder[{cd_, riemann_, ricci_, ricciScalar_}, _cd, _] = -1;
RiemannTensorOrder[{cd_, riemann_, ricci_, ricciScalar_}, ricciScalar, _] = 1;
RiemannTensorOrder[{cd_, riemann_, ricci_, ricciScalar_}, _, ricciScalar] = -1;
RiemannTensorOrder[{cd_, riemann_, ricci_, ricciScalar_}, _ricci, _] = 1;
RiemannTensorOrder[{cd_, riemann_, ricci_, ricciScalar_}, _, _ricci] = -1;
RiemannTensorOrder[cds_][expr1_, expr2_] := RiemannTensorOrder[cds, expr1, expr2];
SyntaxInformation@RiemannTensorOrder = {"ArgumentsPattern" -> {_, _, _}};



End[];

Protect @@ Names["`*"];

EndPackage[];
