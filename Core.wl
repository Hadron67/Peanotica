BeginPackage["Peanotica`Core`", {"Peanotica`Internal`", "Peanotica`Perm`"}];

Scan[Unprotect@#; ClearAll@#; &, Names@{"Peanotica`Core`*"}];

PeanoticaGeneral;
(* interface *)
FindIndicesSlots;
FindIndicesSlotsAndNames;
SymmetryOfExpression;
UnorderedProductQ;
TensorTermQ;
ISort;
RenamingGroupOfIndex::usage = "RenamingGroupOfIndex[a] gives the renaming group of the index name a. Index names with the same renaming group can be interchanged, while the renaming group None cannot be renamed.";
SymmetricIndexPairQ::usage = "SymmetricIndexPairQ[T[..., i, ...], pos, type, index] gives True if T[..., i, ...]F[-i] = \[Epsilon]T[..., -i, ...]F[i].";
SignOfSymmetricPair::usage = "SignOfSymmetricPair[type1, type2, ind] returns 1 or -1.";
InterchangableIndexPairQ::usage = "InterchangableIndexPairQ[type, a] gives true if the index name a and DI[a] can be interchanged.";

(* indices *)
DI::usage = "DI[a] represents a down index, similar to -a or Times[-1, a].";
LabelI::usage = "LabelI[a] represents a label index, with no name.";
DefaultIndex::usage = "DefaultIndex[n] is returned by the default definition of GetIndexOfSlotType[type, indices].";
TempIndex::usage = "TempIndex[n] is a temporary index generated during internal ";
$TempIndexNumber::usage = "$TempIndexNumber is a global variable giving the next global unique id of TempIndex to be used.";
IndexNameSlot;
SeparateIndexName::usage = "SeparateIndexName[index] returns {name, pattern} where name is the name of the index, pattern is it's pattern. An index is determined by its name and pattern.";
GetIndexOfSlotType::usage = "GetIndexOfSlotType[type, inds] returns a new index.";
IndicesCandidateOfSlotType::usage = "IndicesCandidateOfSlotType[type] returns a list, .";
GetUniqueIndexOfSlotType::usage = "GetUniqueIndexOfSlotType[type] returns a globally unique index using the given slot type as a hint. the default definition returns TempIndex[++$TempIndexNumber].";
IndexList::usage = "IndexList[i1, i2, ...] represents a list of indices, similar to List. The purpose of this head is that it may have an overall negative sign.";
DefautRenamingGroup::usage = "DefaultRenamingGroup is the default renaming group of index name.";
RenamableIndicesQ::usage = "RenamableIndicesQ[a, b] returns true if index names a and b can be interchanged.";
GroupedIndexList::usage = "GroupedIndexList[type1 -> {i1 -> {v1, v2, ...}, ...}] represents a grouped list of indices.";
GroupIndexList::usage = "GroupIndexList[{i1 -> v1, i2 -> v2, ...}] groups the indices list into blocks according to their types.";
SymmetryOfGroupedIndices::usage = "SymmetryOfGroupedIndices[GroupedIndices[...], pairSymmetryProvider] gives the generators of the symmetry group of the sorted indices.";
SortGroupedIndexList::usage = "SortGroupedIndexList[GroupedIndexList[...]] returns a sorted version of GroupedIndexList[...].";
FindAndDropFrees::usage = "FindAndDropFrees[GroupedIndexList[...], frees] returns a list containing two GroupedIndexList with the first containing only free indices as specified by frees, the other contains no free indices. FindAndDropFrees[GroupedIndexList[...], Automatic] select frees as those only has one occurance.";
CollectGroupedIndices::usage = "CollectGroupedIndices[GroupedIndexList[...]] returns a list of indices by collecting all the indices in the index list in order.";
FoldGroupedIndexList::usage = "FoldGroupedIndexList[f, x, GroupedIndexList[...]] returns a new GroupedIndexList with the entries being replaced by f[x, name1, poses1], f[f[x, name1, poses1], name2, poses2], where each time f returns {name -> val, x}.";
RenameGroupedIndexList::usage = "RenameGroupedIndexList[GroupedIndexList[...], indices, idToType] renames the indices names.";
PrependPosToSlotSpec::usage = "PrependPosToSlotSpec[]";
$IndexPairPattern::usage = "$IndexPairPattern is global constant with value given by Sort@{IndexNameSlot, DI[IndexNameSlot]}.";
ValidateIndex;

(* utility functions *)
NoIndicesQ;
GroupByTensors::usage = "GroupByTensors[expr] returns an association with key being tensors and values their coefficients.";

(* canonicalization *)
ISortedProduct;
ISortedGeneral;
CanonicalizationUnitQ;
ITensorReduceOneTerm;
SymmetryOfIndicesList;
RenameDummies;
UseMetricOnSlots;
FreeIndices;
FreeIndicesSymmetry;
PeanoticaVerbose;
ReleaseISort;
ExpandToTensorPolynomial;
ITensorReduce;

(* formatting *)
TensorGridBox;
AllowSubsuperscriptBox;
DefTensorFormatings;

Begin["`Private`"];

PeanoticaGeneral::todo = "TODO: `1`";

ValidateIndex::wrongtype = "Contractions of slots of different types: `1` and `2`, assuming the second one to be the same as the first.";

$IndexPairPattern = Sort@{IndexNameSlot, DI@IndexNameSlot};

ToUpIndex[-a_] := a;
ToUpIndex[a_] := a;

ApplyHold[fn_, Hold[args__], args2___] := fn[args, args2];

IndexNameSlot /: MakeBoxes[IndexNameSlot, StandardForm] = InterpretationBox["\[FilledCircle]", IndexNameSlot];

TempIndex /: MakeBoxes[expr : TempIndex[n__], StandardForm] := InterpretationBox[UnderscriptBox[#, "_"], expr] &@If[Length@Hold@n == 1, MakeBoxes@n, RowBox@Riffle[MakeBoxes /@ {n}, ","]];
SyntaxInformation@TempIndex = {"ArgumentsPattern" -> {_}};

DefaultIndex /: MakeBoxes[expr : DefaultIndex[n_], StandardForm] := InterpretationBox[SubscriptBox["\[ImaginaryI]", #], expr] &@MakeBoxes[n, StandardForm];
SyntaxInformation@DefaultIndex = {"ArgumentsPattern" -> {_}};

DI[DI[a_]] := a;
DI[-a_] := a;
SyntaxInformation@DI = {"ArgumentsPattern" -> {_}};

SeparateIndexName[-a_] := MapAt[DI, SeparateIndexName@a, 2];
SeparateIndexName[DI@a_] := MapAt[DI, SeparateIndexName@a, 2];
SeparateIndexName[a_] := {a, IndexNameSlot};
SyntaxInformation@SeparateIndexName = {"ArgumentsPattern" -> {_}};

IndicesCandidateOfSlotType[_] = {{}, DefaultIndex};
SyntaxInformation@IndicesCandidateOfSlotType = {"ArgumentsPattern" -> {_}};

GetIndexOfSlotType[type_, inds_] := With[{
    indCands = IndicesCandidateOfSlotType@type
}, With[{
    inds2 = Complement[indCands[[1]], inds]
}, If[Length@inds2 > 0,
    First@inds2,
    indCands[[2]][1 + Max @@ Append[First /@ Cases[inds, Blank[indCands[[2]]]], 0]]
]]];
SyntaxInformation@GetIndexOfSlotType = {"ArgumentsPattern" -> {_, _}};

GetUniqueIndexOfSlotType[type_] := TempIndex[++$TempIndexNumber];
SyntaxInformation@GetUniqueIndexOfSlotType = {"ArgumentsPattern" -> {_}};

RenamingGroupOfIndex[_Integer] = None;
RenamingGroupOfIndex[_LabelI] = None;
RenamingGroupOfIndex[_] = DefaultRenamingGroup;
SyntaxInformation@RenamingGroupOfIndex = {"ArgumentsPattern" -> {_}};

RenamableIndicesQ[a_, b_] := With[{g1 = RenamingGroupOfIndex@a, g2 = RenamingGroupOfIndex@b}, If[g1 === None || g2 === None, False, g1 === g2]];
SyntaxInformation@RenamableIndicesQ = {"ArgumentsPattern" -> {_, _}};

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

PrependPosToSlotSpec[{vb_, pos__}, {l___}] := {vb, l, pos};
PrependPosToSlotSpec[l_][expr_] := PrependPosToSlotSpec[expr, l];
SyntaxInformation@PrependPosToSlotSpec = {"ArgumentsPattern" -> {_, _.}};

FindIndicesSlots[expr_, pos_] := PrependPosToSlotSpec[pos] /@ FindIndicesSlots@expr;
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
SyntaxInformation@FindIndicesSlots = {"ArgumentsPattern" -> {_, _.}};

FindIndicesSlotsAndNames[expr_] := # -> Extract[Hold@expr, ReplacePart[#, 1 -> 1]] & /@ FindIndicesSlots@expr;
SetAttributes[FindIndicesSlotsAndNames, HoldFirst];
SyntaxInformation@FindIndicesSlotsAndNames = {"ArgumentsPattern" -> {_}};

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

TensorTermQ[expr_Plus] := AllTrue[List @@ expr, Length@FindIndicesSlots@# === 0 &];
TensorTermQ[_List] = False;
TensorTermQ[expr_Times] := And @@ ReleaseHold[Map[TensorTermQ, ReplacePart[Hold@expr, {1, 0} -> List], {2}]];
TensorTermQ[_] = True;
SetAttributes[TensorTermQ, HoldAll];
SyntaxInformation@TensorTermQ = {"ArgumentsPattern" -> {_}};

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

ReleaseISort[expr_] := ReleaseHold[Hold@expr //. {ISortedGeneral[e_] :> e, ISortedProduct[head_, {args___}, _] :> head[args]}];
SyntaxInformation@ReleaseISort = {"ArgumentsPattern" -> {_}};

SetAttributes[{ISortedGeneral, ISortedProduct}, {HoldAll}];

Options@TensorGridBox = {
    AllowSubsuperscriptBox -> False
};
SelectUpDownIndices[{{a_, IndexNameSlot}, {b_, DI@IndexNameSlot}}] := {a, b};
SelectUpDownIndices[{{a_, DI@IndexNameSlot}, {b_, IndexNameSlot}}] := {b, a};
TensorGridBox[t_, inds_List, opt : OptionsPattern[]] := Which[
    Length@inds === 1,
    With[{ind = inds[[1]], indName = inds[[1, 1]]}, Switch[ind[[2]],
        IndexNameSlot, SuperscriptBox[t, MakeBoxes@indName],
        DI@IndexNameSlot, SubscriptBox[t, MakeBoxes@indName]
    ]],

    OptionValue[AllowSubsuperscriptBox] && Sort[inds[[All, 2]]] === $IndexPairPattern,
    SubsuperscriptBox[t, ##] & @@ Reverse@SelectUpDownIndices@inds,

    True,
    GridBox[{{t, StyleBox[
        GridBox[
            Transpose@Map[Replace@{
                {a_, IndexNameSlot} :> {MakeBoxes@a, ""},
                {a_, DI@IndexNameSlot} :> {"", MakeBoxes@a}
            }, inds], RowSpacings -> 0, ColumnSpacings -> 0
        ], FontSize -> 39/4
    ]}}, ColumnSpacings -> 0.05, RowAlignments -> Center]
];
SyntaxInformation@TensorGridBox = {"ArgumentsPattern" -> {_, _, OptionsPattern[]}};

SymbolToIndex[-a_] := {-1, a};
SymbolToIndex[DI[a_]] := {-1, a};
SymbolToIndex[a_] := {1, a};

DefTensorFormatings[sym_] := DefTensorFormatings[sym, MakeBoxes@sym];
DefTensorFormatings[sym_, name_] := sym /: MakeBoxes[expr : sym[inds___], StandardForm] := InterpretationBox[StyleBox[#, ShowAutoStyles -> False], expr, Editable -> False] &@TensorGridBox[name, SeparateIndexName /@ {inds}];
SyntaxInformation@DefTensorFormatings = {"ArgumentsPattern" -> {_, _}};

GroupIndexList[list_] := If[Length@list > 0 && Head@list[[1]] =!= Rule,
    MapIndexed[Join[SeparateIndexName@#1, #2] &, list],
    Join[SeparateIndexName@#2, {#1}] & @@@ list
] (* {name, type, pos} *) //
    GroupBy[First -> Delete[1]] //
    Map[Transpose@Sort@# &] //
    KeyValueMap[{#2[[1]], #1 -> #2[[2]]} &] //
    GroupBy[First -> Extract[2]] //
    KeyValueMap[Rule] //
    Apply[GroupedIndexList] //
    SortGroupedIndexList;
SyntaxInformation@GroupIndexList = {"ArgumentsPattern" -> {_}};

MapSkewedIndexed[fn_, list_] := MapThread[fn, {Delete[list, -1], Delete[list, 1], Range[Length@list - 1]}];

RenamingSymmetryOfInds[inds_] := Join @@ MapShiftedIndexed[
    If[RenamableIndicesQ[#1[[1]], #2[[1]]], With[{len = Length@#1[[2]]}, BlockSymmetricGenSet[len * (#3 - 1) + Range@len, len * #3 + Range@len]], Nothing] &
, inds];

SymmetryOfOneIndsBlock[pairSym_][{IndexNameSlot, DI@IndexNameSlot} -> indsAndPos_] := Join[Join @@ MapIndexed[
    With[{
        pos1 = allIndPos[[ #[[2, 1]] ]],
        pos2 = allIndPos[[ #[[2, 2]] ]],
        indName = #[[1]],
        sign = pairSym[]
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

SymmetryOfOneIndsBlock[pairSym_, indPat_ -> indsAndPos_] := With[{
    baseGenSet = Join @@ MapSkewedIndexed[If[#1 === #2, {SCycles@{#3, #3 + 1}}, {}] &, indPat],
    len = Length@indPat
}, Join[
    Join @@ MapIndexed[ShiftPermutation[baseGenSet, len * (#2[[1]] - 1)] &, indsAndPos],
    If[indPat === $IndexPairPattern,
        Join @@ MapIndexed[With[{sign = pairSym[#1[[1]], #1[[2, 1]], #1[[2, 2]]], p = #2[[1]]}, If[sign =!= None, {SCycles@{2p - 1, 2p}}, {}]] &, indsAndPos]
    , {}],
    RenamingSymmetryOfInds@indsAndPos
]];

SymmetryOfGroupedIndices[list_GroupedIndexList, pairSym_] := ShiftAndJoinGenSets[
    SymmetryOfOneIndsBlock[pairSym, #] & /@ List @@ list,
    Length@#[[2]] * Length@#[[2, 1, 2]] & /@ List @@ list
];
SymmetryOfGroupedIndices[list_] := SymmetryOfGroupedIndices[list, 1 &];
SyntaxInformation@SymmetryOfGroupedIndices = {"ArgumentsPattern" -> {_, _.}};

SortGroupedIndexList[list_GroupedIndexList] := MapAt[SortBy[{RenamingGroupOfIndex@First@#, First@#} &], 2] /@ SortBy[list, First];
SyntaxInformation@SortGroupedIndexList = {"ArgumentsPattern" -> {_}};

FindAndDropFrees[list_GroupedIndexList, Automatic] := With[{
    l = GroupBy[List @@ list, Length@#[[1]] === 1 &]
}, SortGroupedIndexList[GroupedIndexList @@ Lookup[l, #, {}]] & /@ {True, False}];
FindAndDropFrees[list_GroupedIndexList, freeNames_] := With[{
    l = List @@ list,
    pat = Alternatives @@ freeNames
},
    SortGroupedIndexList /@ GroupedIndexList @@@ Select[Length@#[[2]] > 0 &] /@ Transpose[
        Function[{indPat, inds}, indPat -> # & /@ Lookup[GroupBy[inds, MatchQ[#[[1]], pat] &], {True, False}, {}]] @@@ l
    ]
];
SyntaxInformation@FindAndDropFrees = {"ArgumentsPattern" -> {_, _}};

CollectGroupedIndices[list_GroupedIndexList] := Join @@ (Function[{pat, inds},
    Join @@ (Function[{name, val},
        MapThread[#2 -> (#1 /. IndexNameSlot -> name) &, {pat, val}]
    ] @@@ inds)
] @@@ List @@ list);
SyntaxInformation@CollectGroupedIndices = {"ArgumentsPattern" -> {_}};

FoldGroupedIndexList[fn_, x_, list_GroupedIndexList] := GroupedIndexList @@ Delete[FoldList[
    With[{
        res = Delete[FoldList[fn[#1[[2]], #2[[1]], #2[[2]]] &, {None, #1[[2]]}, #2[[2]]], 1] (* {{name1, inds1}, {name2, inds2}, ...} *)
    },
        {#2[[1]] -> res[[All, 1]], res[[-1, 2]]}
    ] &,
    {None, x},
    List @@ list
], 1][[All, 1]];
SyntaxInformation@FoldGroupedIndexList = {"ArgumentsPattern" -> {_, _, _}};

RenameGroupedIndexList[list_GroupedIndexList, inds_, idToType_] := FoldGroupedIndexList[With[{ind = GetIndexOfSlotType[idToType[#3[[1]]], #1]}, {ind -> #3, Append[#1, ind]}] &, inds, list];
SyntaxInformation@RenameGroupedIndexList = {"ArgumentsPattern" -> {_, _, _}};

SymmetryOfIndsBlock[expr_, allIndPos_, dummySpecs_, symDummyPairSelector_] := With[{
    groupedDummySpecs = GroupIndexList@dummySpecs
},
    {
        Join @@ Join @@ groupedDummySpecs[[All, 2, All, 2]],
        ShiftAndJoinGenSets[SymmetryOfOneIndsBlock[expr, allIndPos, symDummyPairSelector] /@ groupedDummySpecs, Length@#[[2]] * Length@#[[2, 1, 2]] & /@ groupedDummySpecs]
    }
];

ExpressionPairSymProvider[expr_, allIndPos_, symDummyPairSelector_][indName_, pos1_, pos2_] := With[{
    type1 = allIndPos[[pos1, 1]],
    type2 = allIndPos[[pos2, 1]]
}, If[
    FilterExprList[symDummyPairSelector, type1] && FilterExprList[symDummyPairSelector, type2] &&
    InterchangableIndexPairQ[type1, indName] && InterchangableIndexPairQ[type2, indName] &&
    SymmetricIndexPairQ[expr, Delete[allIndPos[[pos1]], 1], type1, indName] && SymmetricIndexPairQ[expr, Delete[allIndPos[[pos2]], 1], type2, indName]
,
    SignOfSymmetricPair[type1, type2, indName],
    None
]];

ApplyIndices[_, _, 0] = 0;
ApplyIndices[expr_, indPos_, a_ * newInds_] := a * ApplyIndices[expr, indPos, newInds];
ApplyIndices[expr_, indPos_, newInds_IndexList] := ReplacePart[expr, Thread[(Delete[1] /@ indPos) -> (List @@ newInds)]];
PermuteIndexList[list_, a_ * perm_] := a * PermuteIndexList[list, perm];
PermuteIndexList[list_, perm_Images] := IndexList @@ Permute[list, List @@ perm];
PermuteIndexList[_, 0] = 0;

GetIndicesOfTypes[types_, inds_] := Drop[Fold[Append[#1, GetIndexOfSlotType[#2, #1]] &, inds, types], Length@inds];

MapIf[b_, fn_, expr_] := If[b, fn@expr, expr];
MapIf[b_, fn_][expr_] := MapIf[b, fn, expr];

CanonicalizeOneSorted[expr_, frees_, freesSym_, renameDummies_, symDummyPairSelector_] := With[{
    indPos = FindIndicesSlots@expr,
    slotsSym = SymmetryOfExpression@expr
}, With[{
    actualInds = Extract[expr, Delete[1] /@ indPos]
}, With[{
    groupedInds = FindAndDropFrees[GroupIndexList[actualInds], frees]
}, With[{
    indsSym = Join[
        ShiftPermutation[
            SymmetryOfGroupedIndices[groupedInds[[2]], ExpressionPairSymProvider[expr, indPos, symDummyPairSelector]],
            Length@CollectGroupedIndices@groupedInds[[1]]
        ],
        freesSym
    ]
}, With[{
    canonIndsAndPos = CollectGroupedIndices /@ groupedInds
}, With[{
    perm = InversePermutation@(Join @@ canonIndsAndPos)[[All, 1]],
    canonInds = (Join @@ canonIndsAndPos)[[All, 2]]
}, With[{
    renamedCanonInds = If[renameDummies, (Join @@ (CollectGroupedIndices /@ MapAt[
        RenameGroupedIndexList[#, canonIndsAndPos[[1, All, 2]], indPos[[#, 1]] &] &,
        groupedInds,
        2
    ]))[[All, 2]], canonInds]
},
    xToolsDebugPrint[CanonicalizeOneSorted, "input: ", HoldForm@expr];
    xToolsDebugPrint[CanonicalizeOneSorted, "actual indices: ", actualInds];
    xToolsDebugPrint[CanonicalizeOneSorted, "canonical indices: ", canonInds];
    xToolsDebugPrint[CanonicalizeOneSorted, "renamed indices: ", renamedCanonInds];
    xToolsDebugPrint[CanonicalizeOneSorted, "permutation: ", perm];
    xToolsDebugPrint[CanonicalizeOneSorted, "symmetry of slots: ", slotsSym];
    xToolsDebugPrint[CanonicalizeOneSorted, "symmetry of indices: ", indsSym];
    If[Length@indPos > 0,
        With[{
            canonPerm = Function[{arg}, xToolsDebugPrint[CanonicalizeOneSorted, "calling: ", HoldForm@arg]; arg, {HoldAll}][DoubleCosetRepresentative[slotsSym, #, indsSym]] &[Images @@ perm]
        },
            xToolsDebugPrint[CanonicalizeOneSorted, "canonical permutation: ", canonPerm];
            With[{ret = ApplyIndices[expr, indPos, PermuteIndexList[renamedCanonInds, SignedInversePermutation@canonPerm]]},
                xToolsDebugPrint[CanonicalizeOneSorted, "result: ", ret];
                ret
            ]
        ]
    ,
        xToolsDebugPrint[CanonicalizeOneSorted, "result: ", HoldForm@expr];
        expr
    ]
]]]]]]];

Options@ITensorReduceOneTerm = {
    UseMetricOnSlots -> All,
    FreeIndices -> Automatic,
    FreeIndicesSymmetry -> {},
    RenameDummies -> True
};
ITensorReduceOneTerm::nottensorterm = "`1` is not a tensor term, skipping.";
ITensorReduceOneTerm[expr_, opt : OptionsPattern[]] := If[TensorTermQ@expr,
    ReleaseISort@CanonicalizeOneSorted[
        ISort@expr,
        OptionValue[FreeIndices],
        OptionValue[FreeIndicesSymmetry],
        OptionValue[RenameDummies],
        OptionValue[UseMetricOnSlots]
    ]
,
    Message[ITensorReduceOneTerm::nottensorterm, HoldForm@expr];
    expr
];
SyntaxInformation@ITensorReduceOneTerm = {"ArgumentsPattern" -> {_, OptionsPattern[]}};

ExpandToTensorPolynomial[expr_] := expr //. Times[p_Plus, rest_] :> (# * rest & /@ p) /; !TensorTermQ@p;
SyntaxInformation@ExpandToTensorPolynomial = {"ArgumentsPattern" -> {_}};

End[];

Protect @@ Select[Names["`*"], !StringMatchQ[#, "$" ~~ __] &];
Protect[$IndexPairPattern];

EndPackage[];
