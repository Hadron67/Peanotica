BeginPackage["Peanotica`Core`", {"Peanotica`Internal`", "Peanotica`Perm`"}];

Scan[Unprotect@#; ClearAll@#; &, Names@{"Peanotica`Core`*"}];

PeanoticaGeneral;
(* interface *)
FindIndicesSlots;
SymmetryOfExpression;
UnorderedProductQ;
TensorFactorQ;

(* indices *)
DI::usage = "DI[a] represents a down index, similar to -a or Times[-1, a].";
LabelI::usage = "LabelI[a] represents a label index, with no name.";
TempIndex::usage = "TempIndex[n] is a temporary index generated during internal ";
$TempIndexNumber::usage = "$TempIndexNumber is a global variable giving the next id of TempIndex to be used.";
IndexNameSlot;
SeparateIndexName;
SortIndices;
RenamableIndexQ::usage = "RenamableIndexQ[a] gives true if the index name a can be renamed to other.";
InterchangableIndexPairQ::usage = "InterchangableIndexPairQ[type, a] gives true if the index name a and DI[a] can be interchanged.";
SymmetricIndexPairQ::usage = "SymmetricIndexPairQ[T[..., i, ...], pos, type, index] gives True if T[..., i, ...]F[-i] = \[Epsilon]T[..., -i, ...]F[i].";
SignOfSymmetricPair::usage = "SignOfSymmetricPair[type1, type2, ind] returns 1 or -1.";
ValidateIndex;

(* utility functions *)
NoIndicesQ;

(* canonicalization *)
ISort;
ISortedProduct;
ISortedGeneral;
CanonicalizationUnitQ;
ICanonicalizeOne;
SymmetryOfIndicesList;
AllowInterchangeIndices;

(* formatting *)
TensorGridBox;
DefTensorFormatings;

Begin["`Private`"];

PeanoticaGeneral::todo = "TODO: `1`";

ValidateIndex::wrongtype = "Contractions of slots of different types: `1` and `2`, assuming the second one to be the same as the first.";

ToUpIndex[-a_] := a;
ToUpIndex[a_] := a;

ApplyHold[fn_, Hold[args__], args2___] := fn[args, args2];

IndexNameSlot /: MakeBoxes[IndexNameSlot, StandardForm] = InterpretationBox["\[FilledCircle]", IndexNameSlot];

TempIndex /: MakeBoxes[expr : TempIndex[n__], StandardForm] := InterpretationBox[SubscriptBox["\[FilledVerySmallSquare]", #], expr] &@If[Length@Hold@n == 1, MakeBoxes@n, RowBox@Riffle[MakeBoxes /@ {n}, ","]]

DI[DI[a_]] := a;
DI[-a_] := a;

SeparateIndexName[-a_] := MapAt[DI, SeparateIndexName@a, 2];
SeparateIndexName[DI@a_] := MapAt[DI, SeparateIndexName@a, 2];
SeparateIndexName[a_] := {a, IndexNameSlot};
SyntaxInformation@SeparateIndexName = {"ArgumentsPattern" -> {_}};

SortIndices[inds_] := SortBy[inds, SeparateIndexName];
SyntaxInformation@SortIndices = {"ArgumentsPattern" -> {_}};

RenamableIndexQ[_Integer] = False;
RenamableIndexQ[_LabelI] = False;
RenamableIndexQ[_] = True;
SyntaxInformation@RenamableIndexQ = {"ArgumentsPattern" -> {_}};

InterchangableIndexPairQ[_, _Integer] = False;
InterchangableIndexPairQ[_, _LabelI] = False;
InterchangableIndexPairQ[_, _] = True;
SyntaxInformation@InterchangableIndexPairQ = {"ArgumentsPattern" -> {_, _}};

SymmetricIndexPairQ[_, {_}, _, _] = True;
SymmetricIndexPairQ[expr_Times, pos_, type_, ind_] := Extract[Hold@expr, {1, pos[[1]]}, Function[{a}, SymmetricIndexPairQ[a, Drop[pos, 1], type, ind], {HoldAll}]];
SymmetricIndexPairQ[ISortedProduct[_, args_, _], {2, pos_, restPos___}, type_, ind_] := Extract[Hold@args, {1, pos}, Function[{a}, SymmetricIndexPairQ[a, {restPos}, type, ind], {HoldAll}]];
SymmetricIndexPairQ[ISortedGeneral[arg_], {1, pos__}, type_, ind_] := SymmetricIndexPairQ[arg, {pos}, type, ind];
SetAttributes[SymmetricIndexPairQ, HoldFirst];

SignOfSymmetricPair[None, None, _] = 1;
SyntaxInformation@SignOfSymmetricPair = {"ArgumentsPattern" -> {_, _, _}};

PrependPosToSlotSpec[{l___}][{vb_, pos__}] := {vb, l, pos};

FindIndicesSlots[fn_[args___]] := Join[
    PrependPosToSlotSpec[{0}] /@ FindIndicesSlots@fn
,
    Join @@ MapIndexed[Function[{elem, pos},
        PrependPosToSlotSpec[pos] /@ FindIndicesSlots@elem
    , {HoldAll}], Hold@args]
];
FindIndicesSlots[Subscript[_, inds__]] := {None, #} & /@ Range@Length@Hold@inds;
FindIndicesSlots[_] = {};
SetAttributes[FindIndicesSlots, HoldFirst];

SymmetryOfExpression[_] = {};
SymmetryOfExpression@ISortedGeneral[expr_] := SymmetryOfExpression@expr;
SymmetryOfExpression@ISortedProduct[head_, args_, symList_] := With[{
    argSlotCounts = Length@FindIndicesSlots@# & /@ args
}, Join[
    ShiftAndJoinGenSets[SymmetryOfExpression /@ args, argSlotCounts],
    Join @@ With[{blocks = PartitionedRange@argSlotCounts}, BlockSymmetricGenSet[blocks[[#]], blocks[[# + 1]]] & /@ symList]
]];
SetAttributes[SymmetryOfExpression, HoldFirst];

PartitionedRange[lens_] := FoldPairList[{Range[#1, #1 + #2 - 1], #1 + #2} &, 1, lens];

UnorderedProductQ[Times] = True;
UnorderedProductQ[Wedge] = True;
UnorderedProductQ[Inactive[head_]] := UnorderedProductQ@head;
SyntaxInformation@UnorderedProductQ = {"ArgumentsPattern" -> {_}};

CanonicalizationUnitQ[_Plus] = False;
CanonicalizationUnitQ[_List] = False;
CanonicalizationUnitQ[_] = True;
SetAttributes[CanonicalizationUnitQ, HoldAll];

NoIndicesQ[expr_] := Length@FindIndicesSlots === 0;
NoIndicesQ[expr_List] := AllTrue[expr, NoIndicesQ];
NoIndicesQ[expr_Plus] := AllTrue[List @@ expr, NoIndicesQ];
SetAttributes[NoIndicesQ, HoldAll];

MapShiftedIndexed[f_, expr_] := MapThread[f, {Drop[expr, -1], Drop[expr, 1], Range[Length@expr - 1]}];

ISortArgToSortTag[arg_] := With[{
    sym = SymmetryOfExpression@arg,
    indPos = FindIndicesSlots@arg
}, {arg, -GroupOrderFromStrongGenSet@sym, -Length@indPos, ReplacePart[arg, Thread[FindIndicesSlots[arg][[All, 2 ;;]] -> IndexSlot[1]]]}];

ISort[fn_?UnorderedProductQ[args___]] := With[{
    sortedArgs = SortBy[ISortArgToSortTag /@ (List @@ (ISort /@ Hold@args)), Delete[1]]
}, With[{
    symList = DeleteCases[MapShiftedIndexed[If[#1 === #2, #3, None] &, sortedArgs[[All, 4]]], None]
},
    ISortedProduct[fn, #, symList] &@sortedArgs[[All, 1]]
]];
ISort[expr_] := ISortedGeneral@expr;
SetAttributes[ISort, {HoldAll}];

SetAttributes[{ISortedGeneral, ISortedProduct}, {HoldAll}];

TensorGridBox[t_, inds_] := GridBox[{{
    t, StyleBox[GridBox[Transpose@Map[Replace@{
        {1, a_} :> {MakeBoxes@a, ""},
        {-1, a_} :> {"", MakeBoxes@a}
    }, inds], RowSpacings -> 0, ColumnSpacings -> 0], FontSize -> 39/4]}},
    ColumnSpacings -> 0.05, RowAlignments -> Center
];
SyntaxInformation@TensorGridBox = {"ArgumentsPattern" -> {_, _}};

SymbolToIndex[-a_] := {-1, a};
SymbolToIndex[DI[a_]] := {-1, a};
SymbolToIndex[a_] := {1, a};

DefTensorFormatings[sym_] := DefTensorFormatings[sym, MakeBoxes@sym];
DefTensorFormatings[sym_, name_] := sym /: MakeBoxes[expr : sym[inds___], StandardForm] := InterpretationBox[StyleBox[#, ShowAutoStyles -> False], expr, Editable -> False] &@TensorGridBox[name, SymbolToIndex /@ {inds}];
SyntaxInformation@DefTensorFormatings = {"ArgumentsPattern" -> {_, _}};

Options[SymmetryOfIndsBlock] = {
    AllowInterchangeIndices -> All
};

GroupIndsByType[list_] := KeyValueMap[List, SortBy[{RenamableIndexQ@First@#, First@#} &] /@ GroupBy[
    KeyValueMap[
        Join[{#2[[1]], #1}, Drop[#2, 1]] &,
        Transpose@Sort@# & /@ GroupBy[list, First -> Delete[1]]
    ],
    First -> Delete[1]
]];

MapSkewedIndexed[fn_, list_] := MapThread[fn, {Delete[list, 1], Delete[list, -1], Range@Length@list}];

RenamingSymmetryOfInds[inds_] := BlockSymmetricGenSet @@ (With[{len = Length@#3}, len * (#1 - 1) + Range@len] & @@@ Select[MapIndexed[Join[{#2[[1]]}, #1] &, inds], RenamableIndexQ@#[[2]] &]);

SymmetryOfOneIndsBlock[expr_, allIndPos_, symDummyPairSelector_][{{IndexNameSlot, DI@IndexNameSlot}, indsAndPos_}] := Join[Join @@ MapIndexed[
    With[{
        pos1 = allIndPos[[ #[[2, 1]] ]],
        pos2 = allIndPos[[ #[[2, 2]] ]],
        indName = #[[1]]
    }, If[
        FilterExprList[symDummyPairSelector, pos1[[1]]] && FilterExprList[symDummyPairSelector, pos2[[1]]] &&
        InterchangableIndexPairQ[pos1[[1]], indName] && InterchangableIndexPairQ[pos2[[1]], indName] &&
        SymmetricIndexPairQ[expr, Drop[pos1, 1], pos1[[1]], indName] && SymmetricIndexPairQ[expr, Drop[pos2, 1], pos2[[1]], indName]
    ,
        {SignOfSymmetricPair[pos1[[1]], pos2[[1]], indName] * SCycles@{2 #2[[1]] - 1, 2 #2[[1]]}}
    ,
        {}
    ]] &
,
    indsAndPos
], RenamingSymmetryOfInds@indsAndPos];
SymmetryOfOneIndsBlock[expr_, allIndPos_, _][{indPat_, indsAndPos_}] := With[{
    baseGenSet = Join @@ MapSkewedIndexed[If[#1 === #2, {SCycles@{#3, #3 + 1}}, {}] &, indPat],
    len = Length@indPat
}];

SymmetryOfIndsBlock[expr_, allIndPos_, dummySpecs_, symDummyPairSelector_] := With[{
    groupedDummySpecs = GroupIndsByType@dummySpecs
},
    {Join @@ Join @@ groupedDummySpecs[[All, 2, All, 2]], Join @@ (SymmetryOfOneIndsBlock[expr, allIndPos, symDummyPairSelector] /@ groupedDummySpecs)}
];

ICanonicalizeOne[expr_, frees_, symDummyPairSelector_] := With[{
    indPos = FindIndicesSlots@expr
}, With[{
    slotsSym = SymmetryOfExpression@expr,
    indSpecs = MapIndexed[Join[SeparateIndexName@#1, #2] &, Extract[expr, Drop[#, 1] & /@ indPos]]
}, With[{
    freeAndDummySpecs = GroupBy[indSpecs, MemberQ[frees, #[[1]]] &]
}, With[{
    freeSpecs = Lookup[freeAndDummySpecs, True, {}],
    groupedDummySpecs = SymmetryOfIndsBlock[expr, indPos, Lookup[freeAndDummySpecs, False, {}], symDummyPairSelector]
},
    {freeSpecs, groupedDummySpecs}
]]]];

End[];

Protect @@ Names@{"`*"};

EndPackage[];
