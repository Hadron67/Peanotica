BeginPackage["Peanotica`Core`", {"Peanotica`Internal`", "Peanotica`Perm`"}];

Scan[Unprotect@#; ClearAll@#; &, Names@{"Peanotica`Core`*"}];

PeanoticaGeneral;
(* interface *)
FindIndicesSlots;
SymmetryOfExpression;
UnorderedProductQ;
AbsIndexQ;
UpIndexQ;
ToUpIndex;

FindIndices;

(* utility functions *)
NoIndicesQ;

(* canonicalization *)
ISort;
ISortedProduct;
ISortedGeneral;
IndexSlot;
CanonicalizationUnitQ;

(* formatting *)
TensorGridBox;

Begin["`Private`"];

PeanoticaGeneral::todo = "TODO: `1`";

UnorderedHeadQ[Times] = True;
AbsIndexQ[_Symbol] = True;

ToUpIndex[-a_] := a;
ToUpIndex[a_] := a;

ApplyHold[fn_, Hold[args__], args2___] := fn[args, args2];
PrependPosToSlotSpec[{l___}][{vb_, pos__}] := {vb, l, pos};

FindIndicesSlots::usage = "";
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

FindIndices[expr_] := Extract[Hold@expr, Prepend[Drop[#, 1], 1] & /@ FindIndicesSlots[expr]];

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

End[];

Protect @@ Names@{"`*"};

EndPackage[];
