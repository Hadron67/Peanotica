BeginPackage["Peanotica`Core`", {"Peanotica`Internal`", "Peanotica`Perm`"}];

Scan[Unprotect@#; ClearAll@#; &, Names@{"Peanotica`Core`*"}];

PeanoticaGeneral;
(* interface *)
FindIndicesSlots;
FindIndicesSlotsAndNames;
SymmetryOfExpression;
UnorderedProductQ;
ProductQ;
TensorTermQ;
ISort;
RenamingGroupOfIndexName::usage = "RenamingGroupOfIndexName[a] gives the renaming group of the index name a. Index names with the same renaming group can be interchanged, while the renaming group None cannot be renamed.";
MetricPassThroughQ::usage = "MetricPassThroughQ[T[..., i, ...], pos, type, index] gives True if T[..., i, ...]F[-i] = \[Epsilon]T[..., -i, ...]F[i].";
SignOfSymmetricPair::usage = "SignOfSymmetricPair[type1, type2, ind] returns 1 or -1.";
InterchangableIndexPairQ::usage = "InterchangableIndexPairQ[type, a] gives true if the index name a and DI[a] can be interchanged.";
ITensor::usage = "ITensor[expr, indices] represents a general tensor object.";
AbsIndexNameQ::usage = "";
DisplayName::usage = "";

(* indices *)
DI::usage = "DI[a] represents a down index, similar to -a or Times[-1, a].";
LabelI::usage = "LabelI[a] represents a label index, with no name.";
DefaultIndex::usage = "DefaultIndex[n] is returned by the default definition of GetIndexOfSlotType[type, indices].";
TempIndex::usage = "TempIndex[n] is a temporary index generated during internal ";
$TempIndexNumber::usage = "$TempIndexNumber is a global variable giving the next global unique id of TempIndex to be used.";
IndexNameSlot;
SeparateIndexName::usage = "SeparateIndexName[index] returns {name, pattern} where name is the name of the index, pattern is it's pattern. An index is determined by its name and pattern.";
IndexName::usage = "";
GetIndexOfSlotType::usage = "GetIndexOfSlotType[type, inds] returns a new index that is not in inds.";
GetIndicesOfSlotType::usage = "GetIndicesOfSlotType[types, inds] returns a list of index for each type.";
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
GroupedIndexListMapFold::usage = "GroupedIndexListMapFold[f, x, GroupedIndexList[...]] returns a new GroupedIndexList with the entries being replaced by f[x, type1, name1, poses1], where each time f returns {name -> val, x}.";
MapGroupedIndexList::usage = "MapGroupedIndexList[f, GroupedIndexList[...]] returns a list by apply f to every entries of the GroupedIndexList with f[type, name, val].";
MapIndexedGroupedIndexList::usage = "MapIndexedGroupedIndexList[f, GroupedIndexList[...]] returns a list by apply f to every entries of the GroupedIndexList with f[type, name, val, index].";
FoldListGroupedIndexList::usage = "FoldGroupedIndexList[f, x, GroupedIndexList[...]] returns ";
CollectGroupedIndices::usage = "CollectGroupedIndices[GroupedIndexList[...]] returns a list of indices by collecting all the indices in the index list in order.";
CollectGroupedIndicesNames::usage = "CollectGroupedIndices[GroupedIndexList[...]] returns a list containing all the index names in the list.";
RenameGroupedIndexList::usage = "RenameGroupedIndexList[GroupedIndexList[...], indices, idToType] renames the indices names.";
PrependPosToSlotSpec::usage = "PrependPosToSlotSpec[]";
$IndexPairPattern::usage = "$IndexPairPattern is global constant with value given by Sort@{IndexNameSlot, DI[IndexNameSlot]}.";
ToIndexFunction::usage = "ToIndexFunction[expr, frees] converts expr into a function that takes free indices as arguments.";
ReplaceDummiesToUnique::usage = "";
ValidateIndex;
FindAllIndicesNames;
IndexSlot::usage = "IndexSlot[n] represents an index slot used in IndexedObject.";
IndexedObject::usage = "IndexedObject[expr, {name -> type, ...}] represents an indexed object with IndexSlot's and indices.";
ToIndexedObject::usage = "ToIndexedObject[expr, inds] returns an IndexedObject.";
MapIndexSlots::usage = "MapIndexSlots[fn, expr] replaces all IndexSlot[n] in expr as fn[n], skipping IndexedObject's and ETensor's.";
NonDIQ::usage = "NonDIQ[i] returns false on DI[a] or -a, otherwise true.";

(* metric related *)
DimensionOfSlotType::usage = "DimensionOfSlotType[type] represents the dimension of the slot type.";
MetricOfSlotType::usage = "MetricOfSlotType[type] represents the default metric of the slot type. MetricOfSlotType[type] can be used directly as the metric tensor, it also can be assigned other tensors to it. The default MetricOfSlotType[type] assumes indices must be paired.";
DefSimpleMetric::usage = "DefSimpleMetric[symbol, type, symSign, displayName] defines symbol as a metric tensor with common properties.";
ContractableMetricQ::usage = "ContractableMetricQ[expr] returns true of expr is a metric that can be used in ContractMetric.";
ContractionSlotOfMetric::usage = "ContractionSlotOfMetric[metric] returns 1 or 2, specifiying which slot of the metric to be contracted with. Specifically, for 1 we have p[DI@a] = g[DI@b, DI@a]p[b], while for 2 we have p[DI@a] = g[DI@a, DI@b]p[b]. The default rule returns 1. Note that the value is only relevant for non-symmetric metrics.";
ContractMetric::usage = "ContractMetric[expr, metrics] tries to contract all the specified metrics in expr. ContractMetric[expr] or ContractMetric[expr, All] contracts all metrics that returns true when acting ContractableMetricQ on them.";

(* utility functions *)
DefSimpleTensor;
DefSimpleSlotType;
NoIndicesQ;
NonTensorTermQ;
GroupByTensors::usage = "GroupByTensors[expr] returns an association with key being tensors and values their coefficients.";
UseMetricOnIndices::usage = "";

(* canonicalization *)
ISortedProduct;
ISortedGeneral;
CanonicalizationUnitQ;
PreITensorReduce;
PostITensorReduce;
ITensorReduceOneTerm;
SymmetryOfIndicesList;
RenameDummies;
UseMetricOnSlots;
FreeIndexNames;
FreeIndicesSymmetry;
PeanoticaVerbose;
ReleaseISort;
ExpandToTensorPolynomial;
ITensorReduce;

(* formatting *)
TensorGridBox;
TensorInterpretationBox;
AllowSubsuperscriptBox;
DefTensorFormatings;

(* ETensor *)
ETensor::usage = "ETensor[expr, freeCount, dummyCount] represents an expression tensor.";
ETensorIndices;
CETensor::usage = "CETensor[array] represents a concrete tensor whose components are ETensor's.";

(* non-abstract index notation *)
NITensor::usage = "NITensor[expr, indices]";
INATensorConvertableQ;
INATensorOf;
ITensorTranspose::usage = "ITensorTranspose[t, permutation] transposes the tensor according to the given permutation. Similar to Transpose, permutation may contain repeated elements, in which case the resulting tensor has lower rank.";
ITensorOuter::usage = "ITensorOutter[prod, t1, t2, {{s11, s12}, ...}]";
ITensorSum::usage = "ITensorSum[sum, t, {a1, a2, ...}]";
NITensorReduce::usage = "NITensorReduce[expr, frees]";
ReduceNITensorContractions::usage = "ReduceNITensorContractions[prod, factors, frees]";

Begin["`Private`"];

Err;
PeanoticaGeneral::todo = "TODO: `1`";
ValidateIndex::wrongtype = "Contractions of slots of different types: `1` and `2`, assuming the second one to be the same as the first.";

$IndexPairPattern = Sort@{IndexNameSlot, DI@IndexNameSlot};

IndexNameSlot /: MakeBoxes[IndexNameSlot, StandardForm] = InterpretationBox["\[FilledCircle]", IndexNameSlot];

TempIndex /: MakeBoxes[expr : TempIndex[n__], StandardForm] := InterpretationBox[SubscriptBox["\[DifferentialD]", #], expr] &@If[Length@Hold@n == 1, MakeBoxes@n, RowBox@Riffle[MakeBoxes /@ {n}, ","]];
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

IndexName[a_] := SeparateIndexName[a][[1]];
SyntaxInformation@IndexName = {"ArgumentsPattern" -> {_}};

IndicesCandidateOfSlotType[_] = {{}, DefaultIndex};
SyntaxInformation@IndicesCandidateOfSlotType = {"ArgumentsPattern" -> {_}};

GetIndexOfSlotType[type_, inds_List] := With[{
    indCands = IndicesCandidateOfSlotType@type
}, With[{
    inds2 = Complement[indCands[[1]], inds]
}, If[Length@inds2 > 0,
    First@inds2,
    indCands[[2]][1 + Max @@ Append[First /@ Cases[inds, Blank[indCands[[2]]]], 0]]
]]];
SyntaxInformation@GetIndexOfSlotType = {"ArgumentsPattern" -> {_, _}};

GetIndicesOfSlotType[types_List, inds_List] := FoldPairList[
    With[{i = GetIndexOfSlotType[#2, #1]}, {i, Append[#1, i]}] &,
    inds,
    types
];
SyntaxInformation@GetIndicesOfSlotType = {"ArgumentsPattern" -> {_, _}};

$TempIndexNumber = 1;
GetUniqueIndexOfSlotType[type_] := TempIndex[$TempIndexNumber++];
SyntaxInformation@GetUniqueIndexOfSlotType = {"ArgumentsPattern" -> {_}};

RenamingGroupOfIndexName[_Integer] = None;
RenamingGroupOfIndexName[_LabelI] = None;
RenamingGroupOfIndexName[_] = DefaultRenamingGroup;
SyntaxInformation@RenamingGroupOfIndexName = {"ArgumentsPattern" -> {_}};

RenamableIndicesQ[a_, b_] := With[{g1 = RenamingGroupOfIndexName@a, g2 = RenamingGroupOfIndexName@b}, If[g1 === None || g2 === None, False, g1 === g2]];
SyntaxInformation@RenamableIndicesQ = {"ArgumentsPattern" -> {_, _}};

InterchangableIndexPairQ[_, _Integer] = False;
InterchangableIndexPairQ[_, _LabelI] = False;
InterchangableIndexPairQ[_, _] = True;
SyntaxInformation@InterchangableIndexPairQ = {"ArgumentsPattern" -> {_, _}};

AbsIndexNameQ[ind_] := With[{s = SeparateIndexName@ind}, s[[2]] === IndexNameSlot && RenamingGroupOfIndexName@s[[1]] =!= None];
SyntaxInformation@AbsIndexNameQ = {"ArgumentsPattern" -> {_}};

NonDIQ[DI[_]] = False;
NonDIQ[-_] = False;
NonDIQ[_] = True;
SyntaxInformation@NonDIQ = {"ArgumentsPattern" -> {_}};

MetricPassThroughQ[_, {_}, _, _] = True;
MetricPassThroughQ[expr_Times, pos_, type_, ind_] := Extract[Hold@expr, {1, pos[[1]]}, Function[{a}, MetricPassThroughQ[a, Drop[pos, 1], type, ind], {HoldAll}]];
MetricPassThroughQ[ISortedProduct[_, args_, _], {2, pos_, restPos___}, type_, ind_] := Extract[Hold@args, {1, pos}, Function[{a}, MetricPassThroughQ[a, {restPos}, type, ind], {HoldAll}]];
MetricPassThroughQ[ISortedGeneral[arg_], {1, pos__}, type_, ind_] := MetricPassThroughQ[arg, {pos}, type, ind];
SetAttributes[MetricPassThroughQ, HoldFirst];

SignOfSymmetricPair[None, None, _] = 1;
SyntaxInformation@SignOfSymmetricPair = {"ArgumentsPattern" -> {_, _, _}};

PrependPosToSlotSpec[{vb_, pos__}, {l___}] := {vb, l, pos};
PrependPosToSlotSpec[l_][expr_] := PrependPosToSlotSpec[expr, l];
SyntaxInformation@PrependPosToSlotSpec = {"ArgumentsPattern" -> {_, _.}};

JoinSlotPos[pos_][l_Integer] := Append[pos, l];

FindIndicesSlots[expr_, pos_] := MapAt[Join[pos, #] &, 1] /@ FindIndicesSlots@expr;
FindIndicesSlots[fn_[args___]] := Join[
    FindIndicesSlots[fn, {0}]
,
    Join @@ MapIndexed[Function[{elem, pos},
        FindIndicesSlots[elem, pos]
    , {HoldAll}], Hold@args]
];
FindIndicesSlots[Subscript[_, inds__]] := {#} -> None & /@ Range@Length@Hold@inds;
FindIndicesSlots[_] = {};
SetAttributes[FindIndicesSlots, HoldFirst];
SyntaxInformation@FindIndicesSlots = {"ArgumentsPattern" -> {_, _.}};

FindIndicesSlotsAndNames[expr_] := FindIndicesSlotsAndNames[expr, {}];
FindIndicesSlotsAndNames[expr_, pos_] := Join[pos, #] -> {Extract[Hold@expr, Prepend[#1, 1]], #2} & @@@ FindIndicesSlots@expr;
SetAttributes[FindIndicesSlotsAndNames, HoldFirst];
SyntaxInformation@FindIndicesSlotsAndNames = {"ArgumentsPattern" -> {_, _.}};

SymmetryOfExpression[_] = {};
SymmetryOfExpression@ISortedGeneral[expr_] := SymmetryOfExpression@expr;
SymmetryOfExpression@ISortedProduct[head_, args_, symList_] := With[{
    argSlotCounts = Length@FindIndicesSlots@# & /@ args
}, Join[
    ShiftAndJoinGenSets[SymmetryOfExpression /@ args, argSlotCounts],
    Join @@ With[{blocks = PartitionedRange@argSlotCounts}, BlockSymmetricGenSet[blocks[[#]], blocks[[# + 1]]] & /@ symList]
]];
SetAttributes[SymmetryOfExpression, HoldFirst];

ITensor /: FindIndicesSlots[ITensor[expr_, inds_, ___]] := MapIndexed[Join[{2}, #2, 1] -> #1[[2]] &, inds];
ITensor[expr_, inds_, {}] := ITensor[expr, inds];
ITensor /: SymmetryOfExpression[ITensor[expr_, inds_]] = {};
ITensor /: SymmetryOfExpression[ITensor[expr_, inds_, sym_List]] = sym;
SyntaxInformation@ITensor = {"ArgumentsPattern" -> {_, _, _.}};

PartitionedRange[lens_] := FoldPairList[{Range[#1, #1 + #2 - 1], #1 + #2} &, 1, lens];

UnorderedProductQ[Times] = True;
UnorderedProductQ[Wedge] = True;
UnorderedProductQ[Inactive[head_]] := UnorderedProductQ@head;
SyntaxInformation@UnorderedProductQ = {"ArgumentsPattern" -> {_}};

ProductQ[NonCommutativeMultiply] = True;
ProductQ[expr_] := UnorderedProductQ@expr;
SyntaxInformation@ProductQ = {"ArgumentsPattern" -> {_}};

TensorTermQ[expr_Plus] := AllTrue[List @@ expr, Length@FindIndicesSlots@# === 0 &];
TensorTermQ[_List] = False;
TensorTermQ[expr_Times] := And @@ ReleaseHold[Map[TensorTermQ, ReplacePart[Hold@expr, {1, 0} -> List], {2}]];
TensorTermQ[_] = True;
SetAttributes[TensorTermQ, HoldAll];
SyntaxInformation@TensorTermQ = {"ArgumentsPattern" -> {_}};

NonTensorTermQ[term_] := !TensorTermQ@term;
SetAttributes[NonTensorTermQ, HoldAll];
SyntaxInformation@NonTensorTermQ = {"ArgumentsPattern" -> {_}};

CanonicalizationUnitQ[_Plus] = False;
CanonicalizationUnitQ[_List] = False;
CanonicalizationUnitQ[_] = True;
SetAttributes[CanonicalizationUnitQ, HoldAll];

NoIndicesQ[expr_] := Length@FindIndicesSlots === 0;
NoIndicesQ[expr_List] := AllTrue[expr, NoIndicesQ];
NoIndicesQ[expr_Plus] := AllTrue[List @@ expr, NoIndicesQ];
SetAttributes[NoIndicesQ, HoldAll];

Options[DefSimpleTensor] = {
    AllowSubsuperscriptBox -> Automatic,
    DisplayName -> Automatic
};
DefSimpleTensor[sym_, slots_, symmetry_, opt : OptionsPattern[]] := With[{
    slotsAndPos = MapIndexed[#2 -> #1 &, slots],
    allowSubsuperscriptBox = With[{v = OptionValue@AllowSubsuperscriptBox}, If[v === Automatic, Length@slots === 2 && symmetry === {SCycles@{1, 2}}, v]]
},
    (
        sym /: FindIndicesSlots[sym[##]] = slotsAndPos;
        sym /: SymmetryOfExpression[sym[##]] = symmetry;
    ) & @@ ConstantArray[_, Length@slots];
    With[{v = OptionValue@DisplayName}, Which[
        v === Automatic,
        DefTensorFormatings[sym, MakeBoxes@sym, AllowSubsuperscriptBox -> allowSubsuperscriptBox],

        v =!= None,
        DefTensorFormatings[sym, v, AllowSubsuperscriptBox -> allowSubsuperscriptBox]
    ]];
    sym /: NITensorReduce[sym[inds__], frees_] := NITensorReduce[NITensor[sym, IndexName /@ {inds}], frees];
];
SyntaxInformation@DefSimpleTensor = {"ArgumentsPattern" -> {_, _, _, OptionsPattern[]}};

Options[DefSimpleSlotType] = {
    DisplayName -> None
};
DefSimpleSlotType[sym_, dim_, inds_, opt : OptionsPattern[]] := (
    DimensionOfSlotType[sym] ^= dim;
    IndicesCandidateOfSlotType[sym] ^= inds;
    With[{v = OptionValue@DisplayName}, If[v =!= None,
        sym /: MakeBoxes[sym, StandardForm] = InterpretationBox[v, sym];
    ]];
);
SyntaxInformation@DefSimpleSlotType = {"ArgumentsPattern" -> {_, _, _, OptionsPattern[]}};

(* metric related *)
DimensionOfSlotType /: FindIndicesSlots@DimensionOfSlotType[_] = {};
SyntaxInformation@DimensionOfSlotType = {"ArgumentsPattern" -> {_}};

ContractableMetricQ[_] = False;
SyntaxInformation@ContractableMetricQ = {"ArgumentsPattern" -> {_}};

ContractionSlotOfMetric[_] = 1;
SyntaxInformation@ContractionSlotOfMetric = {"ArgumentsPattern" -> {_}};

MetricOfSlotType /: FindIndicesSlots@MetricOfSlotType[type_][_, _] := {{1} -> type, {2} -> type};
MetricOfSlotType /: SymmetryOfExpression@MetricOfSlotType[type_][_, _] = SymmetricGenSet[1, 2];
MetricOfSlotType /: ContractableMetricQ@MetricOfSlotType[_][_, _] = True;
MetricOfSlotType /: MakeBoxes[expr : MetricOfSlotType[type_], StandardForm] := Function[{arg}, InterpretationBox[arg, expr]]@MakeBoxes["g"[type]];
MetricOfSlotType /: MakeBoxes[expr : MetricOfSlotType[type_][a_, b_], StandardForm] := TensorInterpretationBox[
    expr,
    With[{n = SeparateIndexName /@ {a, b}},
        TensorGridBox[
            MakeBoxes[#[type], StandardForm] &@If[Sort@n[[All, 2]] === $IndexPairPattern, "\[Delta]", "g"],
            n,
            AllowSubsuperscriptBox -> True
        ]
    ]
];
MetricOfSlotType[type_][a_?NonDIQ, DI@b_] := MetricOfSlotType[type][DI@b, a];
MetricOfSlotType[type_][a_?AbsIndexNameQ, DI@a_] := DimensionOfSlotType[type];
MetricOfSlotType[type_][DI@a_, a_?AbsIndexNameQ] := DimensionOfSlotType[type];
MetricOfSlotType /: MetricOfSlotType[type_][a_, b_?AbsIndexNameQ]MetricOfSlotType[type_][DI@b_, c_] := MetricOfSlotType[type][a, c];
MetricOfSlotType /: MetricOfSlotType[type_][b_?AbsIndexNameQ, a_]MetricOfSlotType[type_][DI@b_, c_] := MetricOfSlotType[type][a, c];
MetricOfSlotType /: MetricOfSlotType[type_][b_?AbsIndexNameQ, a_]MetricOfSlotType[type_][c_, DI@b_] := MetricOfSlotType[type][a, c];
MetricOfSlotType /: MetricOfSlotType[type_][a_, b_?AbsIndexNameQ]MetricOfSlotType[type_][c_, DI@b_] := MetricOfSlotType[type][a, c];

TryReplaceIndexNames[expr_, reps_] := With[{
    reps2 = Select[reps, AbsIndexNameQ@#[[1]] &]
}, If[Length@reps2 > 0,
    With[{
        indPoses = Select[{#2, Lookup[FindAllIndicesNames@expr, #1, None]} & @@@ reps2, #[[2]] =!= None &]
    }, If[Length@indPoses > 0,
        With[{
            repName = indPoses[[1, 1]]
        }, ReplacePart[expr, #3 -> (#1 /. IndexNameSlot -> repName) & @@@ indPoses[[1, 2]]]]
    ,
        $Failed
    ]]
,
    $Failed
]];
MetricOfSlotType /: Times[MetricOfSlotType[type_][DI@b_, a_?NonDIQ], rest_] := With[{
    newExpr = TryReplaceIndexNames[rest, {a -> b, b -> a}]
}, newExpr /; newExpr =!= $Failed];
MetricOfSlotType /: Times[MetricOfSlotType[type_][a_?NonDIQ, DI@b_], rest_] := With[{
    newExpr = TryReplaceIndexNames[rest, {a -> b, b -> a}]
}, newExpr /; newExpr =!= $Failed];

SyntaxInformation@MetricOfSlotType = {"ArgumentsPattern" -> {_}};

Options[DefSimpleMetric] = Options[DefSimpleTensor];
DefSimpleMetric::invalidsymsign = "Invalid symmetric sign `1`: may only be 1, -1, or 0. Assuming 1.";
DefSimpleMetric[sym_, slotType_, symSign_, opt : OptionsPattern[]] := (
    DefSimpleTensor[
        sym,
        {slotType, slotType},
        Switch[symSign, 1, {SCycles@{1, 2}}, -1, {-SCycles@{1, 2}}, 0, {}, _, Message[DefSimpleMetric::invalidsymsign, symSign]; {}],
        FilterRules[{opt}, Options@DefSimpleTensor]
    ];
    sym[a_?NonDIQ, DI@b_] := sym[DI@b, a];
    sym[a_?AbsIndexNameQ, DI@a_] := DimensionOfSlotType[type];
    sym[DI@a_, a_?AbsIndexNameQ] := DimensionOfSlotType[type];
    sym /: sym[a_, b_?AbsIndexNameQ]sym[DI@b_, c_] := sym[a, c];
    sym /: sym[b_?AbsIndexNameQ, a_]sym[DI@b_, c_] := sym[a, c];
    sym /: sym[b_?AbsIndexNameQ, a_]sym[c_, DI@b_] := sym[a, c];
    sym /: sym[a_, b_?AbsIndexNameQ]sym[c_, DI@b_] := sym[a, c];
    sym /: Times[sym[DI@b_, a_?NonDIQ], rest_] := With[{
        newExpr = TryReplaceIndexNames[rest, {a -> b, b -> a}]
    }, newExpr /; newExpr =!= $Failed];
    sym /: Times[sym[a_?NonDIQ, DI@b_], rest_] := With[{
        newExpr = TryReplaceIndexNames[rest, {a -> b, b -> a}]
    }, newExpr /; newExpr =!= $Failed];
);
SyntaxInformation@DefSimpleMetric = {"ArgumentsPattern" -> {_, _, _, OptionsPattern[]}};

SignOf2SymGroup[{}] = 0;
SignOf2SymGroup[{SCycles@{1, 2}}] = 1;
SignOf2SymGroup[{-SCycles@{1, 2}}] = -1;
SelectMetricForContraction[All][expr_] := ContractableMetricQ@expr;
SelectMetricForContraction[metrics_List][expr_] := MatchQ[expr, Alternatives @@ metrics];
SelectMetricForContraction[metrics_][expr_] := MatchQ[expr, metrics];
ContractMetricExpanded[expr_Plus, metrics_] := Function[{arg}, ContractMetricExpanded[arg, metrics]] & /@ expr;
ContractMetricExpanded[expr_List, metrics_] := Function[{arg}, ContractMetricExpanded[arg, metrics]] & /@ expr;
ContractOneMetric[expr_, metric_] := With[{
    exprIndToPos = First /@ Select[GroupBy[FindIndicesSlotsAndNames@expr, Extract[{2, 1}] -> ({#[[2, 2]], #[[1]]} &)], Length@# === 1 &],
    metricInds = FindIndicesSlotsAndNames[metric][[All, 2, 1]],
    contractionSlot = ContractionSlotOfMetric@metric,
    metricSym = SignOf2SymGroup@SymmetryOfExpression@metric
}, With[{
    pos1 = Lookup[exprIndToPos, DI /@ metricInds, None]
}, Which[
    pos1[[1]] =!= None && (metricSym === 1 || metricSym === -1 || contractionSlot === 1) && MetricPassThroughQ[expr, pos1[[1, 2]], pos1[[1, 1]], SeparateIndexName[metricInds[[1]]][[1]]],
    If[contractionSlot =!= 1, metricSym, 1] * ReplacePart[expr, pos1[[1, 2]] -> metricInds[[2]]],

    pos1[[2]] =!= None && (metricSym === 1 || metricSym === -1 || contractionSlot === 2) && MetricPassThroughQ[expr, pos1[[2, 2]], pos1[[2, 1]], SeparateIndexName[metricInds[[2]]][[1]]],
    If[contractionSlot =!= 2, metricSym, 1] * ReplacePart[expr, pos1[[2, 2]] -> metricInds[[1]]],

    True,
    expr * metric
]]];
ContractMetricExpanded[expr_Times, metrics_] := With[{
    selected = Lookup[GroupBy[List @@ expr, SelectMetricForContraction[metrics]], {True, False}, {}]
},
    Fold[ContractOneMetric, Times @@ selected[[2]], selected[[1]]]
];
ContractMetricExpanded[expr_, _] := expr;

ContractMetric[expr_, metrics_] := ContractMetricExpanded[ExpandToTensorPolynomial@expr, metrics];
ContractMetric[expr_] := ContractMetric[expr, All];
SyntaxInformation@ContractMetric = {"ArgumentsPattern" -> {_, _.}};

MapShiftedIndexed[f_, expr_] := MapThread[f, {Drop[expr, -1], Drop[expr, 1], Range[Length@expr - 1]}];

ISortArgToSortTag[arg_] := With[{
    sym = SymmetryOfExpression@arg,
    indPos = FindIndicesSlots@arg
}, {arg, -GroupOrderFromStrongGenSet@sym, -Length@indPos, ReplacePart[arg, Thread[indPos[[All, 1]] -> IndexSlot[1]]]}];

(* TODO: products like Wedge could produce a negative sign *)
ISort[fn_?UnorderedProductQ[args___]] := With[{
    sortedArgs = SortBy[ISortArgToSortTag /@ (List @@ (ISort /@ Hold@args)), Delete[1]]
}, With[{
    symList = DeleteCases[MapShiftedIndexed[If[#1 === #2, #3, None] &, sortedArgs[[All, 4]]], None]
},
    Function[{arg}, ISortedProduct[fn, arg, symList]]@sortedArgs[[All, 1]]
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
    SubsuperscriptBox[t, ##] & @@ (MakeBoxes[#, StandardForm] & /@ Reverse@SelectUpDownIndices@inds),

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

TensorInterpretationBox[expr_, box_] := InterpretationBox[StyleBox[box, ShowAutoStyles -> False], expr, Editable -> False];
SetAttributes[TensorInterpretationBox, HoldFirst];
SyntaxInformation@TensorInterpretationBox = {"ArgumentsPattern" -> {_, _}};

SymbolToIndex[-a_] := {-1, a};
SymbolToIndex[DI[a_]] := {-1, a};
SymbolToIndex[a_] := {1, a};

Options[DefTensorFormatings] = Options[TensorGridBox];
DefTensorFormatings[sym_] := DefTensorFormatings[sym, MakeBoxes@sym];
DefTensorFormatings[sym_, name_, opt : OptionsPattern[]] := (
    sym /: MakeBoxes[expr : sym[inds___], StandardForm] := TensorInterpretationBox[expr, TensorGridBox[name, SeparateIndexName /@ {inds}, opt]]
);
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

SymmetryOfOneIndsBlock[pairSym_, indPat_ -> indsAndPos_] := With[{
    baseGenSet = Join @@ MapSkewedIndexed[If[#1 === #2, {SCycles@{#3, #3 + 1}}, {}] &, indPat],
    len = Length@indPat
}, Join[
    Join @@ MapIndexed[ShiftPermutation[baseGenSet, len * (#2[[1]] - 1)] &, indsAndPos],
    If[indPat === $IndexPairPattern,
        Join @@ MapIndexed[{entry, index} |-> With[{sign = pairSym[entry[[1]], entry[[2, 1]], entry[[2, 2]]], p = index[[1]]}, If[sign =!= None, {SCycles@{2p - 1, 2p}}, {}]], indsAndPos]
    , {}],
    RenamingSymmetryOfInds@indsAndPos
]];

SymmetryOfGroupedIndices[list_GroupedIndexList, pairSym_] := ShiftAndJoinGenSets[
    Function[{a}, SymmetryOfOneIndsBlock[pairSym, a]] /@ List @@ list,
    Function[{a}, Length@a[[2]] * Length@a[[2, 1, 2]]] /@ List @@ list
];
SymmetryOfGroupedIndices[list_] := SymmetryOfGroupedIndices[list, 1 &];
SyntaxInformation@SymmetryOfGroupedIndices = {"ArgumentsPattern" -> {_, _.}};

SortGroupedIndexList[list_GroupedIndexList] := MapAt[SortBy[{RenamingGroupOfIndexName@First@#, First@#} &], 2] /@ SortBy[list, First];
SyntaxInformation@SortGroupedIndexList = {"ArgumentsPattern" -> {_}};

ToPattern[list_List] := Alternatives @@ (ToPattern /@ list);
ToPattern[patt_] := patt;
FindAndDropFrees[list_GroupedIndexList, Automatic] := With[{
    l = GroupBy[List @@ list, Length@#[[1]] === 1 &]
}, Function[{b}, SortGroupedIndexList[GroupedIndexList @@ Lookup[l, b, {}]]] /@ {True, False}];
FindAndDropFrees[list_GroupedIndexList, freeNames_] := With[{
    l = List @@ list,
    pat = ToPattern@freeNames
},
    SortGroupedIndexList /@ GroupedIndexList @@@ Select[Length@#[[2]] > 0 &] /@ Transpose[
        Function[{indPat, inds}, Function[{a}, indPat -> a] /@ Lookup[GroupBy[inds, a |-> MatchQ[a[[1]], pat]], {True, False}, {}]] @@@ l
    ]
];
SyntaxInformation@FindAndDropFrees = {"ArgumentsPattern" -> {_, _}};

CollectGroupedIndices[list_GroupedIndexList] := Join @@ (Function[{pat, inds},
    Join @@ (Function[{name, val},
        MapThread[Function[{p, v}, v -> (p /. IndexNameSlot -> name)], {pat, val}]
    ] @@@ inds)
] @@@ List @@ list);
SyntaxInformation@CollectGroupedIndices = {"ArgumentsPattern" -> {_}};

CollectGroupedIndicesNames[list_GroupedIndexList] := Join @@ (List @@ list)[[All, 2, All, 1]];
SyntaxInformation@CollectGroupedIndicesNames = {"ArgumentsPattern" -> {_}};

GroupedIndexListMapFold[fn_, x_, list_GroupedIndexList] := GroupedIndexList @@ Delete[FoldList[
    {inds, entry} |-> With[{
        patt = entry[[1]]
    }, With[{
        res = Delete[FoldList[{inds2, indNameAndPos} |-> fn[inds2[[2]], patt, indNameAndPos[[1]], indNameAndPos[[2]]], {None, inds[[2]]}, entry[[2]]], 1] (* {{name1, inds1}, {name2, inds2}, ...} *)
    },
        {patt -> res[[All, 1]], res[[-1, 2]]}
    ]],
    {None, x},
    List @@ list
], 1][[All, 1]];
SyntaxInformation@GroupedIndexListMapFold = {"ArgumentsPattern" -> {_, _, _}};

MapGroupedIndexList[fn_, list_GroupedIndexList] := Join @@ Map[
    entry |-> With[{
        patt = entry[[1]]
    }, Map[
        ind |-> fn[patt, ind[[1]], ind[[2]]] &,
        entry[[2]]
    ]] &,
    List @@ list
];
SyntaxInformation@MapGroupedIndexList = {"ArgumentsPattern" -> {_, _}};

RenameGroupedIndexList[list_GroupedIndexList, inds_, idToType_] := GroupedIndexListMapFold[
    {inds2, type, name, values} |-> With[{
        ind = GetIndexOfSlotType[idToType[values[[1]]], inds2]
    }, {ind -> values, Append[inds2, ind]}]
, inds, list];
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
    type1 = allIndPos[[pos1, 2]],
    type2 = allIndPos[[pos2, 2]]
}, If[
    FilterExprList[symDummyPairSelector, type1] && FilterExprList[symDummyPairSelector, type2] &&
    InterchangableIndexPairQ[type1, indName] && InterchangableIndexPairQ[type2, indName] &&
    MetricPassThroughQ[expr, allIndPos[[pos1, 1]], type1, indName] && MetricPassThroughQ[expr, allIndPos[[pos2, 1]], type2, indName]
,
    SignOfSymmetricPair[type1, type2, indName],
    None
]];

ApplyIndices[_, _, 0] = 0;
ApplyIndices[expr_, indPos_, a_ * newInds_] := a * ApplyIndices[expr, indPos, newInds];
ApplyIndices[expr_, indPos_, newInds_IndexList] := ReplacePart[expr, Thread[indPos -> (List @@ newInds)]];
PermuteIndexList[list_, a_ * perm_] := a * PermuteIndexList[list, perm];
PermuteIndexList[list_, perm_Images] := IndexList @@ Permute[list, List @@ perm];
PermuteIndexList[_, 0] = 0;

MapIf[b_, fn_, expr_] := If[b, fn@expr, expr];
MapIf[b_, fn_][expr_] := MapIf[b, fn, expr];

CanonicalizeOneSorted[expr_, frees_, freesSym_, renameDummies_, symDummyPairSelector_] := With[{
    indPos = FindIndicesSlots@expr,
    slotsSym = SymmetryOfExpression@expr
}, With[{
    actualInds = Extract[expr, First /@ indPos]
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
        indList |-> RenameGroupedIndexList[indList, CollectGroupedIndicesNames@groupedInds[[1]], p |-> indPos[[p, 2]]],
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
            With[{ret = ApplyIndices[expr, indPos[[All, 1]], PermuteIndexList[renamedCanonInds, SignedInversePermutation@canonPerm]]},
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
    FreeIndexNames -> Automatic,
    FreeIndicesSymmetry -> {},
    RenameDummies -> True
};
ITensorReduceOneTerm::nottensorterm = "`1` is not a tensor term, skipping.";
ITensorReduceOneTerm[expr_Plus, opt : OptionsPattern[]] := ITensorReduceOneTerm[#, opt] & /@ expr;
ITensorReduceOneTerm[expr_List, opt : OptionsPattern[]] := ITensorReduceOneTerm[#, opt] & /@ expr;
ITensorReduceOneTerm[expr_, opt : OptionsPattern[]] := If[TensorTermQ@expr,
    ReleaseISort@CanonicalizeOneSorted[
        ISort@expr,
        OptionValue[FreeIndexNames],
        OptionValue[FreeIndicesSymmetry],
        OptionValue[RenameDummies],
        OptionValue[UseMetricOnSlots]
    ]
,
    Message[ITensorReduceOneTerm::nottensorterm, HoldForm@expr];
    expr
];
SyntaxInformation@ITensorReduceOneTerm = {"ArgumentsPattern" -> {_, OptionsPattern[]}};

PreITensorReduce[head_[args___]] := PreITensorReduce[head] @@ Map[PreITensorReduce, {args}];
PreITensorReduce[expr_] := expr;
SyntaxInformation@PreITensorReduce = {"ArgumentsPattern" -> {_}};

PostITensorReduce[head_[args__]] := PostITensorReduce[head] @@ Map[PostITensorReduce, {args}];
PostITensorReduce[expr_] := expr;
SyntaxInformation@PostITensorReduce = {"ArgumentsPattern" -> {_}};

ExpandToTensorPolynomial[expr_] := expr //. {
    prod_?ProductQ[l___, p_Plus?NonTensorTermQ, r___] :> Function[{a}, prod[l, a, r]] /@ p
};
SyntaxInformation@ExpandToTensorPolynomial = {"ArgumentsPattern" -> {_}};

Options[ITensorReduce] = Options[ITensorReduceOneTerm];
ITensorReduce[expr_, opt : OptionsPattern[]] := PostITensorReduce@ITensorReduceOneTerm[ExpandToTensorPolynomial@PreITensorReduce@expr, FilterRules[{opt}, Options@ITensorReduceOneTerm]];
SyntaxInformation@ITensorReduce = {"ArgumentsPattern" -> {_, OptionsPattern[]}};

IndexSlot /: MakeBoxes[expr : IndexSlot[n_], StandardForm] := With[{
    box = MakeBoxes[n, StandardForm]
}, InterpretationBox[SubscriptBox["\[FilledSmallCircle]", box], expr]];
SyntaxInformation@IndexSlot = {"ArgumentsPattern" -> {_}};

ExpressionQ[_[___]] = True;
ExpressionQ[_] = False;
SetAttributes[ExpressionQ, HoldAll];
FindAllIndicesNames[expr_] := FindAllIndicesNames[expr, {}];
FindAllIndicesNames[expr_, pos_] := Which[
    TensorTermQ@expr,
    GroupBy[Join[SeparateIndexName@#2[[1]], {#2[[2]], #1}] & @@@ FindIndicesSlotsAndNames[expr, pos], First -> Delete[1]],

    True,
    FindAllIndicesNamesSubExpression[expr, pos]
];
FindAllIndicesNamesSubExpression[head_[args___], pos_] := Merge[
    ReleaseHold@MapIndexed[Function[{val, i}, FindAllIndicesNames[val, Join[pos, Delete[i, 1]]], {HoldFirst}], Hold@{args}, {2}],
    Apply@Join
];
SetAttributes[{FindAllIndicesNames, FindAllIndicesNamesSubExpression}, HoldFirst];
SyntaxInformation@FindAllIndicesNames = {"ArgumentsPattern" -> {_, _.}};

WrapIntoFunction[inner_, Hold@{args__}] := Function[inner[args]];
ToIndexFunction[expr_, frees_] := With[{
    body = ToIndexFunction0[expr, frees, <||>]
}, With[{
    inner = Function @@ {body[[1]] /. TempSlot -> Slot},
    dummyArgs = Join[##, 2] & @@ (Hold@{GetUniqueIndexOfSlotType@#2} & @@@ body[[2]])
}, If[Length@dummyArgs > 0,
    WrapIntoFunction[inner, Join[Hold@Evaluate@Array[Slot, Length@frees], dummyArgs, 2]],
    inner
]]];
SyntaxInformation@ToIndexFunction = {"ArgumentsPattern" -> {_, _}};

NullableSparseArray[arr_] := If[Length@arr === 0, {}, Normal@SparseArray@arr];
ToIndexedObject[expr_, inds_] := With[{
    body = ToIndexedObjectHelper[expr, Association@MapIndexed[#1 -> {#2[[1]], {}} &, inds]]
}, IndexedObject[
    body[[1]],
    NullableSparseArray@KeyValueMap[#2[[1]] -> (#1 -> #2[[2]]) &, body[[2]]]
]];
SyntaxInformation@ToIndexedObject = {"ArgumentsPattern" -> {_, _}};
ToIndexedObjectHelper[expr_?TensorTermQ, nameToId_] := With[{
    ret = FoldPairList[
        {nameToId2, slotAndName} |-> With[{
            sep = SeparateIndexName@slotAndName[[2]],
            pos = slotAndName[[1]]
        }, With[{
            id = Lookup[nameToId2, sep[[1]], None]
        }, If[id =!= None,
            {pos -> (sep[[2]] /. IndexNameSlot -> IndexSlot[id[[1]]]), MapAt[Append[pos[[1]]], nameToId2, {Key@sep[[1]], 2}]},
            With[{newId = Length@nameToId2 + 1}, {pos -> (sep[[2]] /. IndexNameSlot -> IndexSlot@newId), Append[nameToId2, sep[[1]] -> {newId, {pos[[1]]}}]}]
        ]]],
        nameToId,
        FindIndicesSlotsAndNames@expr,
        Identity
    ]
}, {ReplacePart[expr, MapAt[Delete[1], 1] /@ ret[[All, 1]]], If[Length@ret === 0, nameToId, Last[ret][[2]]]}];
ToIndexedObjectHelper[head_[args__], nameToId_] := With[{
    ret = FoldPairList[
        ToIndexedObjectHelper[#2, #1] &,
        nameToId,
        {args},
        Identity
    ]
}, {head @@ ret[[All, 1]], If[Length@ret === 0, nameToId, Last[ret][[2]]]}] /; !TensorTermQ[head@args];

MapIndexSlots[fn_, expr_] := expr /. {
    expr2_IndexedObject :> expr2,
    expr2_ETensor :> expr2,
    IndexSlot[n_] :> fn[n]
};
SyntaxInformation@MapIndexSlots = {"ArgumentsPattern" -> {_, _}};

ETensor::wrongindlen = "Given index list `1` has different length than `2`.";
ETensor::wrongtransposeperm = "Invalid permutation `1` to transpose ETensor with free count = `2`.";
ETensor::nonmatchfrees = "Encountered free indices of different structures `1` and `2` in sum.";
ETensor[0, _] = 0;
ETensor[expr_, frees_][inds__] := With[{
    indNameToPos = FindAllIndicesNames@expr
}, ReplacePart[expr, Join[
    Join @@ MapThread[Thread[(Delete[1] /@ indNameToPos[#1]) -> #2] &, {frees, {inds}}],
    Join @@ Thread[(Delete[1] /@ #) -> GetUniqueIndexOfSlotType[#[[1, 1]]]] & /@ Values@Delete[indNameToPos, {Key@#} & /@ frees]
]]] /; If[Length@frees === Length@{inds}, True, Message[ETensor::wrongindlen, HoldForm@{inds}, HoldForm@frees]; False];

ReplaceIndicesRules[indPos_, reps_] := Join @@ (With[{
    newInd = #2,
    entry = Lookup[indPos, #1, None]
}, If[entry =!= None, #2 -> (#1 /. IndexNameSlot -> newInd) & @@@ entry, {}]] & @@@ reps);
SameDummies[exprs_] := With[{
    mostDummies = MaximalBy[exprs, Length@#[[2]] &][[1, 2]]
}, With[{
    dummies = #2,
    dummiesToRep = Complement[Keys@#2, Keys@mostDummies]
}, ReplacePart[
    #1,
    ReplaceIndicesRules[dummies, Thread[dummiesToRep -> Take[Keys@mostDummies, Length@dummiesToRep]]]
]] & @@@ exprs];

MultiILength[a_List] := Length@a;
MultiILength[_] = 1;
StructureOfETensorIndices[inds_] := With[{
    lens = MultiILength /@ inds,
    flat = Flatten@inds
}, ETensorIndices[
    DeleteCases[flat, None],
    Position[flat, None, {1}][[All, 1]],
    lens
]];
DeflattenFrees[list_, lens_] := FoldPairList[
    {If[#2 == 1, #1[[1]], Take[#1, #2]], Drop[#1, #2]} &,
    list,
    lens
];
ReconstructETensorIndices[ETensorIndices[inds_, nonePos_, lens_]] := DeflattenFrees[Fold[Insert[#1, None, #2] &, inds, nonePos], lens];
CompatibleETensorIndicesQ[ETensorIndices[inds1_, nonePos1_, lens1_], ETensorIndices[inds2_, nonePos2_, lens2_]] := Length@inds1 === Length@inds2 && nonePos1 === nonePos2 && lens1 === lens2;

ETensorPlusHelper1DeleteTypeAndPrepend1ToPos[{pat_, type_, pos_}] := {pat, Prepend[pos, 1]};
GetIndicesNamesToType[indPoses_Association] := Extract[{1, 2}] /@ indPoses;
ETensor /: FindIndicesSlots[_ETensor] = {};
ETensor /: PreITensorReduce[ETensor[expr_, frees_]] := With[{
    freesObj = StructureOfETensorIndices@frees
}, With[{
    sortedFrees = Sort@freesObj[[1]]
}, ETensor[
    If[sortedFrees =!= freesObj[[1]], ReplacePart[
        expr,
        ReplaceIndicesRules[Map[Delete[2]] /@ FindAllIndicesNames@expr, Thread[freesObj[[1]] -> sortedFrees]]
    ], expr] //
    ITensorReduce[#, FreeIndexNames -> sortedFrees] &,
    ReconstructETensorIndices[ReplacePart[freesObj, 1 -> sortedFrees]]
]]];
ETensor::plusincompat = "Cannot combine ETensor's with incompatible free indices `1` and `2`.";
UncatchedETensorPlus[head_, exprs_] := With[{
    structs = Map@MultiILength /@ exprs[[All, 2]],
    freesFlatten = Flatten /@ exprs[[All, 2]],
    indPoses = FindAllIndicesNames /@ exprs[[All, 1]]
}, With[{
    len = Length@freesFlatten[[1]]
}, MapThread[
    If[Length@#2 =!= len, Message[ETensor::plusincompat, exprs[[1, 2]], #1[[2]]]; Throw@Err;] &,
    {Delete[exprs, 1], Delete[freesFlatten, 1]}
]]; With[{
    indPosesNoType = Map@Map@ETensorPlusHelper1DeleteTypeAndPrepend1ToPos /@ indPoses
}, With[{
    dummies = MapThread[Delete[#1, {Key@#} & /@ DeleteCases[#2, None]] &, {indPosesNoType, freesFlatten}],
    freesFlattenWithTypes = MapThread[With[{indToType = #1}, If[# === None, {None, None}, {#, indToType[#]}] & /@ #2] &, {GetIndicesNamesToType /@ indPoses, freesFlatten}]
}, With[{
    selectedInd = MaximalBy[MapIndexed[List, dummies], Length@#[[1]] &][[1, 2, 1]]
}, With[{
    exprsWithDummiesReplaced = With[{
        selectedDummies = dummies[[selectedInd]]
    }, MapThread[ReplacePart[Hold@#1, ReplaceIndicesRules[#2, With[{
        dummiesToRep = Complement[Keys@#2, Keys@selectedDummies]
    }, Thread[dummiesToRep -> Take[Keys@selectedDummies, Length@dummiesToRep]]]]] &, {exprs[[All, 1]], dummies}]],
    newFrees = With[{
        freeSlotsNeedInds = MapThread[
            With[{noNoneInd = Select[#2, #[[1]] =!= None &]}, If[#1 === None && Length@noNoneInd > 0, {#3, noNoneInd[[1, 2]]}, Nothing]] &, {
                freesFlatten[[selectedInd]],
                Transpose@Delete[freesFlattenWithTypes, selectedInd],
                Range@Length@freesFlatten[[1]]
            }
        ]
    }, ReplacePart[
        freesFlatten[[selectedInd]],
        Thread[freeSlotsNeedInds[[All, 1]] -> GetIndicesOfSlotType[freeSlotsNeedInds[[All, 2]], Keys@indPoses[[selectedInd]]]]
    ]]
},
    ETensor[
        head @@ MapThread[
            ReleaseHold@ReplacePart[#1, ReplaceIndicesRules[#3, MapThread[If[#1 === None || #1 === #2, Nothing, #1 -> #2] &, {#2, newFrees}]]] &,
            {exprsWithDummiesReplaced, freesFlatten, indPosesNoType}
        ],
        DeflattenFrees[newFrees, structs[[selectedInd]]]
    ]
]]]]];
ETensor /: e1_ETensor + e2_ETensor := With[{ret = Catch@UncatchedETensorPlus[Plus, {e1, e2}]}, ret /; ret =!= Err];
ETensor /: prod_?ProductQ[x_, ETensor[expr_, arg__]] := ETensor[prod[x, expr], arg];
ETensor /: D[ETensor[expr_, frees_], args__] := ETensor[D[expr, args], frees];

ETensorProduct[prod_, ETensor[expr1_, frees1_], ETensor[expr2_, frees2_]] := With[{
    indPos1 = FindAllIndicesNames@expr1,
    indPos2 = FindAllIndicesNames@expr2
}, With[{
    allInds = Union[Keys@indPos1, Keys@indPos2],
    inds2ToBeReplaced = Intersection[Keys@indPos2, Keys@indPos1]
}, With[{
    newInds = GetIndicesOfSlotType[indPos2[#][[1, 2]] & /@ inds2ToBeReplaced, allInds]
}, With[{
    newIndsReps = Thread[inds2ToBeReplaced -> newInds]
},
    ETensor[
        prod[expr1, ReplacePart[expr2, ReplaceIndicesRules[Map[Delete[2]] /@ indPos2,newIndsReps]]],
        Join[frees1, ReconstructETensorIndices@MapAt[
            ReplaceAll[newIndsReps],
            StructureOfETensorIndices@frees2,
            1
        ]]
    ]
]]]];

TensorValueQ[_?ArrayQ] = True;
TensorValueQ[_ETensor] = True;
TensorValueQ[_] = False;

GeneralizedPermutationProduct[perm1_, perm2_] := perm1 /. Dispatch@Thread[Range@Length@perm2 -> perm2];

ITensorTranspose[s_?ArrayQ, perm_] := Map[If[TensorValueQ@#, ITensorTranspose[#, perm], #] &, Transpose[s, perm], {ArrayDepth@s}];
ITensorTranspose::emptyslot = "Permutation `1` does not specify destination for level `2`.";
ITensorTranspose::incompatslot = "Cannot combine slots `1` and `2`.";
ConvertTransposeIndsGroup[list_] := If[Head@list[[1]] === List, Sequence @@ Transpose@list, list];
PadPermutation[perm_List, n_Integer] := If[Length@perm < n, Join[perm, Range[Length@perm + 1, n]], perm];
UncatchedETensorTranspose[ETensor[expr_, frees_], perm_List] := With[{
    permPadded = PadPermutation[perm, Length@frees]
}, With[{
    indsBySlots = Fold[MapAt[Append[#2[[2]]], #1, #2[[1]]] &, ConstantArray[{}, Max @@ permPadded], Thread@{permPadded, frees}]
}, With[{
    indPos = If[AnyTrue[indsBySlots, Length@# > 1 &], Map[Delete[2]] /@ FindAllIndicesNames@expr, Throw@Err &]
},
    MapIndexed[If[Length@#1 === 0, Message[ITensorTranspose::emptyslot, perm, #2[[1]]]; Throw@Err;] &, indsBySlots];
    ETensor[
        ReplacePart[
            expr,
            Join @@ (If[Length@# > 1,
                ReplaceIndicesRules[indPos, Thread[Delete[#, 1] -> #[[1]]]],
                {}
            ] & /@ ConvertTransposeIndsGroup /@ indsBySlots)
        ],
        First /@ indsBySlots
    ]
]]];
ITensorTranspose[e1_ETensor, perm_] := With[{ret = UncatchedETensorTranspose[e1, perm]}, ret /; ret =!= Err];
ITensorTranspose[Plus[l___, ITensorTranspose[t2_, perm2_], r___], perm_] := ITensorTranspose[Plus[l, r], perm] + ITensorTranspose[t2, GeneralizedPermutationProduct[perm2, perm]];
ITensorTranspose[ITensorTranspose[t_, perm1_], perm2_] := ITensorTranspose[t, GeneralizedPermutationProduct[perm1, perm2]];
ITensorTranspose[expr_, perm_] := expr /; perm === Range[Length@perm];
SyntaxInformation@ITensorTranspose = {"ArgumentsPattern" -> {_, _}};

SyntaxInformation@ETensor = {"ArgumentsPattern" -> {_, _}};

ITensorOuter[prod_, s1_, s2_] := ITensorOuter[prod, s1, s2, {}];
PermutationOfMovingToFirst[points_List] := If[Length@points === 0, {}, InversePermutation[Join[points, Delete[Range[Max @@ points], Transpose@{points}]]]];
PermutationOfMovingToLast[points_List] := InversePermutation[Join[Delete[Range[Max @@ points], Transpose@{points}], points]];
FillNoneSlots[arr_, l_] := FoldPairList[If[#2 === None, {#1, #1 + 1}, {#2, #1}] &, l, arr];
ITensorOuter::nonzeronontensor = "Multiplication of non-zero non-tensor value `1` and `2` encountered.";
ITensorOuterInner[prod_, s1_, s2_, cont_] := Switch[{TensorValueQ@s1, TensorValueQ@s2},
    {True, True},
    ITensorOuter[prod, s1, s2, cont],

    {False, False},
    prod[s1, s2],

    _,
    If[s1 === 0 || s2 === 0, 0, Message[ITensorOuter::nonzeronontensor, s1, s2]; Throw@Err;]
];
ITensorOuter[prod_, s1_?ArrayQ, s2_?ArrayQ, cont_] := With[{
    ret = Catch@With[{
        perm1 = PermutationOfMovingToFirst[cont[[All, 1]]],
        perm2 = PermutationOfMovingToFirst[cont[[All, 2]]]
    }, With[{
        s22 = Transpose[s2, perm2]
    },
        Transpose[
            MapIndexed[
                Outer[
                    ITensorOuterInner[prod, #1, #2, cont] &,
                    #1,
                    Extract[s22, {#2}][[1]]
                ] &,
                Transpose[s1, perm1],
                {Length@cont}
            ],
            InversePermutation@perm1
        ]
    ]]
}, ret /; ret =!= Err];
ITensorOuter[prod_, e1_ETensor, e2_ETensor, cont_] := ITensorTranspose[
    ETensorProduct[prod, e1, e2],
    With[{
        l1 = Length@e1[[2]],
        l2 = Length@e2[[2]]
    }, Join[
        Range@l1,
        FillNoneSlots[ReplacePart[ConstantArray[None, l2], #2 -> #1 & @@@ cont], l1 + 1]
    ]]
];
ITensorOuter[_, 0, _, _] = 0;
ITensorOuter[_, _, 0, _] = 0;
SyntaxInformation@ITensorOuter = {"ArgumentsPattern" -> {_, _, _, _}};

ITensorSum[expr_, {}] := expr;
ITensorSum[s_?ArrayQ, dims_] := Total@Flatten[
    Transpose[Map[If[TensorValueQ@#, ITensorSum[#, dims], #] &, s, {ArrayDepth@s}], PermutationOfMovingToFirst@dims],
    Length@dims - 1
];
ITensorSum[ETensor[expr_, frees_], dims_] := ETensor[expr, Delete[frees, Transpose@{dims}]];
SyntaxInformation@ITensorSum = {"ArgumentsPattern" -> {_, _}};

OneIndexOfNITensor[ind_ -> _] := ind;
OneIndexOfNITensor[ind_] := ind;
OneIndexPosOfNITensor[ind_, pos_] := pos -> None;
OneIndexPosOfNITensor[ind_ -> type_, pos_] := Append[pos, 1] -> type;
NITensor /: FindIndicesSlots[NITensor[_, inds_]] := MapIndexed[OneIndexPosOfNITensor[#1, Prepend[#2, 2]] &, inds];
NITensor /: prod_?ProductQ[x_, NITensor[t_, inds_]] := NITensor[prod[x, t], inds] /; Head@x =!= NITensor;
NITensor /: Power[NITensor[t_, inds_], x_] := NITensor[t ^ x, inds];
SyntaxInformation@NITensor = {"ArgumentsPattern" -> {_, _}};

FreesByArgs[frees_, freeArgs_] := Array[Union @@ Append[Delete[freeArgs, #], frees] &, Length@freeArgs];
GetINTensorIndexNames[NITensor[_, inds_]] := OneIndexOfNITensor /@ inds;
GetINTensorIndexNames[_] = {};
NITensorReduce::incompatinds = "Cannot combine NITensors `1` and `2` with different indices.";
NITensorReduce[expr_] := NITensorReduce[expr, {}];
NITensorReduce[prod_?ProductQ[args__], frees_] := ReduceNITensorContractions[prod, {args}, frees];
NITensorReduce[t_NITensor, frees_] := ContractOneNITensor[t, frees];
NITensorReduce[expr_Plus, frees_] := With[{
    ret = Catch@UncatchedCombineSummedNITensor[NITensorReduce[#, frees] & /@ expr]
}, ret /; ret =!= Err];
NITensorReduce::unknown = "No rule to transform indexed expression `1`.";
NITensorReduce[expr_, _] := With[{
    inds = FindAllIndicesNames@expr
}, If[Length@inds === 0,
    NITensor[expr, {}]
,
    Message[NITensorReduce::unknown, HoldForm@expr];
    NITensor[Hold@expr, Keys@inds]
]];

PermutationOfMergeIndices[inds_, n_] := With[{
    firstIndToIndGroup = Association @@ (Min @@ # -> # & /@ inds)
}, Fold[
    With[{
        arr = #1[[1]],
        c = #1[[2]]
    }, If[arr[[#2]] === None,
        With[{
            g = Lookup[firstIndToIndGroup, #2, None]
        }, {
            If[g =!= None,
                ReplacePart[arr, Thread[g -> c]],
                ReplacePart[arr, #2 -> c]
            ],
            c + 1
        }]
    ,
        {arr, c}
    ]] &,
    {ConstantArray[None, n], 1},
    Range@n
]][[1]];
ContractOneNITensor[NITensor[t1_, inds_], frees_] := With[{
    indsByName = Sort /@ GroupBy[MapIndexed[{SeparateIndexName[OneIndexOfNITensor@#1][[1]], #2[[1]]} &, inds], First -> Extract[2]]
}, With[{
    toContract = Select[indsByName, Length@# >= 2 &]
}, With[{
    ret = If[Length@toContract > 0,
        With[{
            perm = PermutationOfMergeIndices[Values@toContract, Length@inds]
        }, {
            ITensorTranspose[t1, perm],
            Delete[inds, Transpose@{Flatten[Delete[1] /@ Values@toContract]}],
            Union /@ (indsByName /. Dispatch[Thread[Range@Length@perm -> perm]])
        }]
    ,
        {t1, inds, indsByName}
    ]
}, With[{
    summed = Delete[ret[[3]], {Key@#} & /@ frees]
},
    If[Length@summed > 0,
        NITensor[ITensorSum[ret[[1]], Join @@ (Values@summed)], Delete[ret[[2]], Values@summed]],
        NITensor[ret[[1]], ret[[2]]]
    ]
]]]];

NITensorSlot;

ContractTwoNITensors[prod_, {}, {}, frees_] := NITensor[prod[NITensorSlot[1], NITensorSlot[2]], {}];
ContractTwoNITensors[prod_, inds1_, inds2_, frees_] := With[{
    inds1ByName = Association @@ MapIndexed[SeparateIndexName[OneIndexOfNITensor@#1][[1]] -> #2[[1]] &, inds1],
    inds2ByName = Association @@ MapIndexed[SeparateIndexName[OneIndexOfNITensor@#1][[1]] -> #2[[1]] &, inds2]
}, With[{
    toContract = Intersection[Keys@inds1ByName, Keys@inds2ByName]
}, With[{
    pos1 = inds1ByName /@ toContract,
    pos2 = inds2ByName /@ toContract
}, With[{
    inner = ITensorOuter[prod, NITensorSlot[1], NITensorSlot[2], Thread@{pos1, pos2}],
    summed = Complement[toContract, frees]
},
    If[Length@summed > 0,
        NITensor[ITensorSum[inner, inds1ByName /@ summed], Join[
            Delete[inds1, Transpose@{inds1ByName /@ summed}],
            Delete[inds2, Transpose@{pos2}]
        ]],
        NITensor[inner, Join[inds1, Delete[inds2, Transpose@{pos2}]]]
    ]
]]]];
CombinationPairs[n_] := Join @@ Array[a |-> Array[{a, #} &, n - a, a + 1], n];
(* simple multiplication: use optimized contraction *)
CountContractions[ITensorOuter[_, _, _, cont_]] := Length@Flatten@cont - Length@cont;
CountContractions[ITensorSum[t_, sums_]] := Length@sums + CountContractions@t;
CountContractions[_] = 0;
(* TODO: phase for products like Wedge *)
ReduceNITensorContractionsStep[prod_, factors_, frees_] := With[{
    freesByArg = SeparateIndexName[OneIndexOfNITensor@#][[1]] & /@ factors[[All, 2]]
}, With[{
    choosenCountraction = MaximalBy[
        With[{
            cont = ContractTwoNITensors[
                prod,
                factors[[#1, 2]],
                factors[[#2, 2]],
                Union@Append[Delete[freesByArg, {{#1}, {#2}}], frees]
            ]
        }, {#1, #2, cont, CountContractions@cont[[1]]}] & @@@ CombinationPairs@Length@factors,
        Extract[4]
    ][[1]]
},
    Prepend[
        Delete[factors, {{choosenCountraction[[1]]}, {choosenCountraction[[2]]}}],
        choosenCountraction[[3]] /. {NITensorSlot[1] -> factors[[choosenCountraction[[1]], 1]], NITensorSlot[2] -> factors[[choosenCountraction[[2]], 1]]}
    ]
]];

AbsorbNonNITensorStep[prod_][{factors_, cursor_}] := With[{
    factor1 = factors[[cursor]],
    factor2 = factors[[cursor + 1]]
}, Which[
    Length@factor1[[2]] === 0,
    {ReplacePart[factors, {cursor -> NITensor[prod[factor1[[1]], factor2[[1]]], factor2[[2]]], cursor + 1 -> Nothing}], cursor},

    Length@factor2[[2]] === 0,
    {ReplacePart[factors, {cursor -> NITensor[prod[factor1[[1]], factor2[[1]]], factor1[[2]]], cursor + 1 -> Nothing}], cursor},

    True,
    {factors, cursor + 1}
]];
AbsorbNonNITensor[prod_, factors_] := NestWhile[
    AbsorbNonNITensorStep[prod],
    {factors, 1},
    #[[2]] + 1 <= Length@#[[1]] &
][[1]];
WrapScalarInNITensor[e_NITensor] := e;
WrapScalarInNITensor[e_] := NITensor[e, {}];
ReduceNITensorContractions[prod_, factors_, frees_] := With[{
    freesByArg = FreesByArgs[frees, Keys@FindAllIndicesNames@# & /@ factors]
}, With[{
    factors2 = AbsorbNonNITensor[prod, WrapScalarInNITensor /@ MapThread[NITensorReduce, {factors, freesByArg}]]
},
    Nest[ReduceNITensorContractionsStep[prod, #, frees] &, factors2, Length@factors2 - 1][[1]]
]];
UncatchedCombineSummedNITensor[head_[NITensor[first_, firstInds_], rest__]] := With[{
    inds = OneIndexOfNITensor /@ firstInds
}, NITensor[head @@ Prepend[
    With[{
        inds2 = OneIndexOfNITensor /@ #[[2]]
    }, With[{
        perm = FirstPosition[inds, #, None, {1}][[1]] & /@ inds2
    },
        If[Length@inds2 =!= Length@inds || Length@Cases[perm, None] > 0, Message[NITensorReduce::incompatinds, firstInds, #]; Throw@Err;];
        ITensorTranspose[#[[1]], perm]
    ]] & /@ {rest},
    first
], firstInds]];

End[];

Protect @@ Select[Names["`*"], !StringMatchQ[#, "$" ~~ __] &];
Protect[$IndexPairPattern];

EndPackage[];
