BeginPackage["Peanotica`Core`", {"Peanotica`Internal`", "Peanotica`Perm`"}];

Scan[Unprotect@#; ClearAll@#; &, Names@{"Peanotica`Core`*"}];

PeanoticaGeneral;
(* interface *)
FindIndicesSlots::usage = "FindIndicesSlots[expr]";
SummedIndices;
FindIndicesSlotsAndNames;
SymmetryOfExpression;
UnorderedProductQ;
ProductQ;
TensorTermQ;
ISort::usage = "ISort[expr]";
RenamingGroupOfIndexName::usage = "RenamingGroupOfIndexName[a] gives the renaming group of the index name a. Index names with the same renaming group can be interchanged, while the renaming group None cannot be renamed.";
ExpressionPassThroughQ::usage = "ExpressionPassThroughQ[T[..., i, ...], expr, pos] gives True if T[..., i, ...] expr = T[..., i * expr, ...], where the position of i is given by pos.";
SumPassThroughQ::usage = "SumPassThroughQ[expr, pos]";
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
DefaultRenamingGroup::usage = "DefaultRenamingGroup is the default renaming group of index name.";
RenamableIndicesQ::usage = "RenamableIndicesQ[a, b] returns true if index names a and b can be interchanged.";
GroupedIndexList::usage = "GroupedIndexList[type1 -> {i1 -> {v1, v2, ...}, ...}] represents a grouped list of indices.";
SymmetricIndexGroup::usage = "SymmetricIndexGroup[pattern, symmetrySpec, namingGroups]";
IndexNameGroup::usage = "IndexNameGroup[namingGroup, indices]";
GroupIndexList::usage = "GroupIndexList[{i1 -> v1, i2 -> v2, ...}] groups the indices list into blocks according to their types.";
AddSymmetryToGroupedIndexList;
SymmetryGroupOfSymmetricIndexList;
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
ReplaceDummiesToUnique::usage = "ReplaceDummiesToUnique[expr, frees] or ReplaceDummiesToUnique[expr]";
ValidateIndex;
FindAllIndicesNames;
IndexSlot::usage = "IndexSlot[n] represents an index slot used in IndexedObject.";
IndexedObject::usage = "IndexedObject[expr, {name -> type, ...}] represents an indexed object with IndexSlot's and indices.";
ToIndexedObject::usage = "ToIndexedObject[expr, inds] returns an IndexedObject.";
MapIndexSlots::usage = "MapIndexSlots[fn, expr] replaces all IndexSlot[n] in expr as fn[n], skipping IndexedObject's and ETensor's.";
NonDIQ::usage = "NonDIQ[i] returns false on DI[a] or -a, otherwise true.";
NoDI::usage = "NoDI[DI[a]] gives a, NoDI[a] gives a if Head[a] =!= DI.";
CopyDI::usage = "CopyDI[a, b]";
ReplaceFreesRules::usage = "ReplaceFreesRules[indsAndPos, oldFrees, newFrees]";
ReplaceFrees::usage = "ReplaceFrees[expr, oldFrees, newFrees]";
DummyIndex::usage = "DummyIndex[name, repeats] represents an index with hint that it's a dummy index with specified repeats.";
PopulateDummyIndexHint::usage = "PopulateDummyIndexHint[expr, frees]";
IndexScope::usage = "IndexScope[expr]";
PutIndexScope::usage = "PutIndexScope[expr, frees]";
SignOfUpSlot;
PermuteIndexList::usage = "PermuteIndexList[list, perm]";
ApplyIndices::usage = "ApplyIndices[expr, indPos, indList]";
ReplaceIndexNames::usage = "ReplaceIndexNames[expr, reps]";
TryReplaceIndexNames::usage = "TryReplaceIndexNames[expr, reps]";
MapIndicesSlots::usage = "MapIndicesSlots[fn, inds]";

(* IndexedSum *)
IndexedSum::usage = "IndexedSum[expr, {ind, type}, ...]";
ScalarIndexedSumQ::usage = "ScalarIndexedSumQ[expr]";
MoveIndexedSumToOutermost::usage = "MoveIndexedSumToOutermost[expr]";
MoveIndexedSumToInnermost::usage = "MoveIndexedSumToInnermost[expr]";
IndexedSumTimes::usage = "IndexedSumTimes[{inds...}, factor1, factor2, ...]";
PopulateIndexedSumTimes::usage = "PopulateIndexedSumTimes[expr]";
ReplaceIndexedSumIndsToUnique::usage = "ReplaceIndexedSumIndsToUnique[expr]";
ReplaceAllIndexedSumIndsToUnique::usage = "ReplaceAllIndexedSumIndsToUnique[expr]";

(* metric related *)
DimensionOfSlotType::usage = "DimensionOfSlotType[type] represents the dimension of the slot type.";
MetricOfSlotType::usage = "MetricOfSlotType[type] represents the default metric of the slot type. MetricOfSlotType[type] can be used directly as the metric tensor, it also can be assigned other tensors to it. The default MetricOfSlotType[type] assumes indices must be paired.";
DefSimpleMetric::usage = "DefSimpleMetric[symbol, type, symSign, displayName] defines symbol as a metric tensor with common properties.";
DefSimpleDeltaTensor::usage = "DefSimpleDeltaTensor[symbol, type]";
ContractionRuleMode::usage = "ContractionRuleMode is an option for DefSimpleMetric, specifying whether the indices are assumed to be paired.";
ContractableMetricQ::usage = "ContractableMetricQ[expr] returns true of expr is a metric that can be used in ContractMetric.";
ContractionSlotOfMetric::usage = "ContractionSlotOfMetric[metric] returns 1 or 2, specifiying which slot of the metric to be contracted with. Specifically, for 1 we have p[DI@a] = g[DI@b, DI@a]p[b], while for 2 we have p[DI@a] = g[DI@a, DI@b]p[b]. The default is north-west convention: p[a] = g[a, b]p[DI@b], p[DI@a] = p[b]g[DI@b, DI@a]. Note that the value is only relevant for non-symmetric metrics.";
ContractMetric::usage = "ContractMetric[expr, metrics] tries to contract all the specified metrics in expr. ContractMetric[expr] or ContractMetric[expr, All] contracts all metrics that returns true when acting ContractableMetricQ on them.";
SeparateMetricOne::usage = "SeparateMetricOne[expr, indPattern, metricProvider]";

(* utility functions *)
DefSimpleTensor::usage = "DefSimpleTensor[name, slots, symmetry]";
DefSimpleSlotType::usage = "DefSimpleSlotType[name, dimension, indices]";
NoIndicesQ;
NonTensorTermQ;
GroupByTensors::usage = "GroupByTensors[expr] returns an association with key being tensors and values their coefficients.";
UnionClosures::usage = "UnionClosures[{tag1 -> list1, tag2 -> list2, ...}]";
UnionClosureValues::usage = "UnionClosureValues[{list1, list2, ...}}";
ContractList::usage = "ContractList[list, a, b, slot]";
IndexedExprFunction::usage = "IndexedExprFunction[expr, frees, dummies, dummyTypes]";
ToIndexedExprFunction::usage = "ToIndexedExprFunction[expr, frees]";
RaiseFrees::usage = "RaiseFrees[expr, frees]";
WithTempIndex1::usage = "WithTempIndex1[expr]";
PermuteIndices::usage = "PermuteIndices[expr, inds, perm]";
ImposeSymmetry::usage = "ImposeSymmetry[expr, inds, genset]";
ExpandScalarIndexScope::usage = "ExpandScalarIndexScope[expr]";
MapScalarIndexScopes::usage = "MapScalarIndexScopes[fn, expr] or MapScalarIndexScopes[fn][expr]";
DeepContractMetric::usage = "DeepContractMetric[expr, metrics]";
MetricBarrier::usage = "MetricBarrier[expr]";
ReleaseMetricBarrier::usage = "ReleaseMetricBarrier[expr]";

(* canonicalization *)
ISortedProduct;
ISortedGeneral;
ISortUnorderedArgs::usage = "ISortUnorderedArgs[expr, args]";
CanonicalizationUnitQ;
PreITensorReduce;
PostITensorReduce;
ITensorReduceOneTerm;
RenameDummies;
UseMetricOnSlots;
FreeIndexNames;
FreeIndicesSymmetry::usage = "FreeIndicesSymmetry is an option for ITensorReduceOneTerm and related functions, specifying the symmetry of free indices, as if they are contracted with a tensor with certain symmetry.";
PeanoticaVerbose;
ReleaseISort;
ExpandToTensorPolynomial;
ITensorReduce::usage = "ITensorReduce[expr]";
IndexWrapper::usage = "IndexWrapper[expr, innerInds, outerInds]";
PutIndexWrapper::usage = "PutIndexWrapper[expr]";
ReleaseIndexWrapper::usage = "ReleaseIndexWrapper[expr]";

(* formatting *)
TensorGridBox::usage = "TensorGridBox[head, indices]";
TensorInterpretationBox;
AllowSubsuperscriptBox;
IndexStyle;
DefTensorFormatings::usage = "DefTensorFormatings[name, options...]";

(* ETensor *)
ETensor::usage = "ETensor[expr, freeCount, dummyCount] represents an expression tensor.";
ETensorIndices;
CETensor::usage = "CETensor[array] represents a concrete tensor whose components are ETensor's.";

(* non-abstract index notation *)
NITensor::usage = "NITensor[expr, indices]";
ITensorTranspose::usage = "ITensorTranspose[t, permutation] transposes the tensor according to the given permutation. Similar to Transpose, permutation may contain repeated elements, in which case the resulting tensor has lower rank.";
ITensorOuter::usage = "ITensorOutter[prod, t1, t2, {{s11, s12}, ...}]";
ITensorSum::usage = "ITensorSum[t, {a1, a2, ...}]";
ITensorScalarMultiply::usage = "ITensorScalarMultiply[prod, tensor, scalar]";
ITensorFixedContract::usage = "ITensorFixedContract[t1, t2, n1, n2]";
NITensorReduce::usage = "NITensorReduce[expr, frees]";
ReduceNITensorContractions::usage = "ReduceNITensorContractions[prod, factors, frees]";
ScalarValue::usage = "ScalarValue[expr] represents a scalar value expr.";
ExtractNITensor::usage = "";
ToNITensorIndex::usage = "ToNITensorIndex[ind, slot]";
FromNITensorIndex::usage = "FromNITensorIndex[ind]";
SumIndexOfSlot::usage = "SumIndexOfSlot[slot, ind, expr]";
ETensorArray::usage = "ETensorArray[fn, {inds1, inds2, ...}]";

$PThrowOnError = False;

Begin["`Private`"];

Err;
PeanoticaGeneral::todo = "TODO: `1`";
ValidateIndex::wrongtype = "Contractions of slots of different types: `1` and `2`, assuming the second one to be the same as the first.";

$IndexPairPattern = Sort@{IndexNameSlot, DI@IndexNameSlot};

IndexNameSlot /: MakeBoxes[IndexNameSlot, StandardForm] = InterpretationBox["\[FilledCircle]", IndexNameSlot];

TempIndex /: MakeBoxes[expr : TempIndex[n__], StandardForm] := InterpretationBox[SubscriptBox["\[DifferentialD]", #], expr] &@If[Length@Hold@n === 1, MakeBoxes@n, RowBox@Riffle[MakeBoxes /@ {n}, ","]];
SyntaxInformation@TempIndex = {"ArgumentsPattern" -> {_}};

DefaultIndex /: MakeBoxes[expr : DefaultIndex[n_], StandardForm] := InterpretationBox[SubscriptBox["\[ImaginaryI]", #], expr] &@MakeBoxes[n, StandardForm];
SyntaxInformation@DefaultIndex = {"ArgumentsPattern" -> {_}};

DI[DI[a_]] := a;
DI[-a_] := a;
SyntaxInformation@DI = {"ArgumentsPattern" -> {_}};

LabelI /: MakeBoxes[expr : LabelI[i_], StandardForm] := InterpretationBox[#, expr] &@StyleBox[MakeBoxes@i, FontColor -> Red];
SyntaxInformation@LabelI = {"ArgumentsPattern" -> {_}};

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
    inds2 = DeleteCases[indCands[[1]], Alternatives @@ inds]
}, If[Length@inds2 > 0,
    First@inds2,
    indCands[[2]][1 + Max @@ Append[First /@ Cases[inds, Blank[indCands[[2]]]], 0]]
]]];
SyntaxInformation@GetIndexOfSlotType = {"ArgumentsPattern" -> {_, _}};

GetIndicesOfSlotTypeHelper1[a_] := {a};
GetIndicesOfSlotTypeHelper1[type_ -> n_Integer] := ConstantArray[type, n];
GetIndicesOfSlotType[types_] := GetIndicesOfSlotType[types, {}];
GetIndicesOfSlotType[type_, inds_] := GetIndicesOfSlotType[{type}, inds] /; Head@type =!= List;
GetIndicesOfSlotType[types_List, inds_List] := FoldPairList[
    With[{i = GetIndexOfSlotType[#2, #1]}, {i, Append[#1, i]}] &,
    inds,
    Join @@ (GetIndicesOfSlotTypeHelper1 /@ types)
];
SyntaxInformation@GetIndicesOfSlotType = {"ArgumentsPattern" -> {_, _., _.}};

$TempIndexNumber = 1;
GetUniqueIndexOfSlotType[type_] := TempIndex[$TempIndexNumber++];
SyntaxInformation@GetUniqueIndexOfSlotType = {"ArgumentsPattern" -> {_}};

RenamingGroupOfIndexName[_Integer] = Null;
RenamingGroupOfIndexName[_LabelI] = Null;
RenamingGroupOfIndexName[_] = DefaultRenamingGroup;
SyntaxInformation@RenamingGroupOfIndexName = {"ArgumentsPattern" -> {_}};

RenamableIndicesQ[a_, b_] := With[{g1 = RenamingGroupOfIndexName@a, g2 = RenamingGroupOfIndexName@b}, If[g1 === Null || g2 === Null, False, g1 === g2]];
SyntaxInformation@RenamableIndicesQ = {"ArgumentsPattern" -> {_, _}};

InterchangableIndexPairQ[_, _Integer] = False;
InterchangableIndexPairQ[_, _LabelI] = False;
InterchangableIndexPairQ[_, _] = True;
SyntaxInformation@InterchangableIndexPairQ = {"ArgumentsPattern" -> {_, _}};

AbsIndexNameQ[ind_] := With[{s = SeparateIndexName@ind}, s[[2]] === IndexNameSlot && RenamingGroupOfIndexName@s[[1]] =!= Null];
SyntaxInformation@AbsIndexNameQ = {"ArgumentsPattern" -> {_}};

NonDIQ[DI[_]] = False;
NonDIQ[-_] = False;
NonDIQ[_] = True;
SyntaxInformation@NonDIQ = {"ArgumentsPattern" -> {_}};

NoDI[DI@a_] := a;
NoDI[a_] := a;
SyntaxInformation@NoDI = {"ArgumentsPattern" -> {_}};

CopyDI[_DI, a_] := DI@a;
CopyDI[_, a_] := a;
SyntaxInformation@CopyDI = {"ArgumentsPattern" -> {_, _}};

ExpressionPassThroughQ[_, _, {_}] = True;
ExpressionPassThroughQ[_?ProductQ[args__], tensor_, pos_] := Extract[Hold@{args}, {1, pos[[1]]}, Function[{a}, ExpressionPassThroughQ[a, tensor, Drop[pos, 1]], {HoldAll}]];
ExpressionPassThroughQ[ISortedProduct[_, args_, _], tensor_, {2, pos_, restPos___}] := Extract[Hold@args, {1, pos}, Function[{a}, ExpressionPassThroughQ[a, tensor, {restPos}], {HoldAll}]];
ExpressionPassThroughQ[ISortedGeneral[arg_], tensor_, {1, pos__}] := ExpressionPassThroughQ[arg, tensor, {pos}];
ExpressionPassThroughQ[_, _, _] = False;
SetAttributes[ExpressionPassThroughQ, HoldFirst];
SyntaxInformation@ExpressionPassThroughQ = {"ArgumentsPattern" -> {_, _, _}};

SumPassThroughQ[_, {_}] = True;
SumPassThroughQ[_?ProductQ[args__], pos_] := Extract[Hold@args, pos[[1]], Function[{a}, SumPassThroughQ[a, Drop[pos, 1]], {HoldAll}]];
SumPassThroughQ[ISortedProduct[_, args_, _], {2, pos_, restPos___}] := Extract[Hold@args, {1, pos}, Function[{a}, SumPassThroughQ[a, {restPos}], {HoldAll}]];
SumPassThroughQ[ISortedGeneral[arg_], {1, pos__}] := SumPassThroughQ[arg, {pos}];
SumPassThroughQ[_, _] = False;
SetAttributes[SumPassThroughQ, HoldFirst];
SyntaxInformation@SumPassThroughQ = {"ArgumentsPattern" -> {_, _}};

SignOfSymmetricPair[_, _, _] = 1;
SyntaxInformation@SignOfSymmetricPair = {"ArgumentsPattern" -> {_, _, _}};

PrependPosToSlotSpec[{vb_, pos__}, {l___}] := {vb, l, pos};
PrependPosToSlotSpec[l_][expr_] := PrependPosToSlotSpec[expr, l];
SyntaxInformation@PrependPosToSlotSpec = {"ArgumentsPattern" -> {_, _.}};

MapIndicesSlots[fn_, l_List] := MapIndicesSlots[fn, #] & /@ l;
MapIndicesSlots[fn_, l_SummedIndices] := MapIndicesSlots[fn, #] & /@ l;
MapIndicesSlots[fn_, pos_ -> type_] := fn[pos, type];
SyntaxInformation@MapIndicesSlots = {"ArgumentsPattern" -> {_, _}};

SummedIndices[] = Nothing;

WrapList[list_List] := list;
WrapList[l_] := {l};
FindIndicesSlots[expr_, pos_] := MapIndicesSlots[Join[pos, #1] -> #2 &, FindIndicesSlots@expr];
FindIndicesSlots[HoldPattern@Plus[args__]] := {DeleteCases[
    SummedIndices @@ MapIndexed[Function[{a, pos},
        FindIndicesSlots[a, pos],
    {HoldFirst}], Hold@args],
    {}
]};
FindIndicesSlots[HoldForm@expr_] := FindIndicesSlots[expr, {1}];
FindIndicesSlots[Hold@expr_] := FindIndicesSlots[expr, {1}];
FindIndicesSlots[fn_[args___]] := Join[
    WrapList@FindIndicesSlots[fn, {0}]
,
    Join @@ MapIndexed[Function[{elem, pos},
        FindIndicesSlots[elem, pos]
    , {HoldAll}], Hold@args]
];
FindIndicesSlots[Subscript[_, inds__]] := {#} -> Null & /@ Range@Length@Hold@inds;
FindIndicesSlots[_] = {};
SetAttributes[FindIndicesSlots, HoldFirst];
SyntaxInformation@FindIndicesSlots = {"ArgumentsPattern" -> {_, _.}};

FindIndicesSlotsAndNames[expr_] := FindIndicesSlotsAndNames[expr, {}];
FindIndicesSlotsAndNames[expr_, pos_] := Join[pos, #] -> {Extract[Hold@expr, Prepend[#1, 1]], #2} & @@@ FindIndicesSlots@expr;
SetAttributes[FindIndicesSlotsAndNames, HoldFirst];
SyntaxInformation@FindIndicesSlotsAndNames = {"ArgumentsPattern" -> {_, _.}};

FlattenIndicesSlots[SummedIndices[l__]] := FlattenIndicesSlots /@ Join[l];
FlattenIndicesSlots[list_] := list //. SummedIndices[l__] :> Sequence @@ Join[l];

SymmetryOfExpression[_] = {};
SymmetryOfExpression[Hold@expr_] := SymmetryOfExpression@expr;
SymmetryOfExpression[HoldForm@expr_] := SymmetryOfExpression@expr;
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

TensorTermQ[expr_] := FreeQ[FindIndicesSlots@expr, SummedIndices];
SetAttributes[TensorTermQ, HoldAll];
SyntaxInformation@TensorTermQ = {"ArgumentsPattern" -> {_}};

NonTensorTermQ[term_] := !TensorTermQ@term;
SetAttributes[NonTensorTermQ, HoldAll];
SyntaxInformation@NonTensorTermQ = {"ArgumentsPattern" -> {_}};

TakeOneUnionClosureStep[{list_, takenLists_, elems_}] := With[{
    selected = Lookup[GroupBy[list, Length@Intersection[#[[2]], elems] > 0 &], {True, False}, {}]
}, {
    selected[[2]],
    Join[takenLists, selected[[1, All, 1]]],
    Union[elems, Join @@ selected[[1, All, 2]]]
}];
TakeOneUnionClosure[taggedList_] := Delete[
    FixedPoint[TakeOneUnionClosureStep, {Delete[taggedList, 1], {taggedList[[1, 1]]}, taggedList[[1, 2]]}],
    3
];
UnionClosures[taggedList_] := NestWhile[With[{
    tmp = TakeOneUnionClosure@#[[1]]
}, {tmp[[1]], Append[#[[2]], tmp[[2]]]}] &, {taggedList, {}}, Length@#[[1]] > 0 &][[2]];
SyntaxInformation@UnionClosures = {"ArgumentsPattern" -> {_}};

UnionClosureValues[lists_] := Union @@ Extract[lists, Transpose@{#}] & /@ UnionClosures@MapIndexed[#2[[1]] -> #1 &, lists];
SyntaxInformation@UnionClosureValues = {"ArgumentsPattern" -> {_}};

ContractList[list_, a_, b_, slot_] := With[{
    inds = Join[{a}, Array[GetUniqueIndexOfSlotType@slot &, Length@list - 1], {b}]
}, MapThread[Construct, {list, Delete[inds, -1], Delete[inds, 1]}]];
SyntaxInformation@ContractList = {"ArgumentsPattern" -> {_, _, _, _}};

ToIndexedExprFunction[expr_, frees_] := With[{
    inds = FindAllIndicesNames@expr
}, With[{
    freeLen = Length@frees,
    dummies = Complement[Keys@inds, frees]
}, IndexedExprFunction[
    ReplacePart[expr, ReplaceIndicesRules[Map[Delete[2]] /@ inds, Join[
        MapIndexed[#1 -> IndexSlot @@ #2 &, frees],
        MapIndexed[#1 -> IndexSlot[#2[[1]] + freeLen] &, dummies]
    ]]],
    freeLen,
    Lookup[inds, #][[1, 2]] & /@ dummies
]]];
ToIndexedExprFunction[ETensor[expr_, inds_]] := ToIndexedExprFunction[expr, inds];
SyntaxInformation@ToIndexedExprFunction = {"ArgumentsPattern" -> {_, _.}};

IndexedExprFunction::args = "Expected `1` arguments, found `2`.";
IndexedExprFunction[expr_, frees_, dummyTypes_][inds__] := With[{
    check = If[Length@{inds} === frees, True, Message[IndexedExprFunction::args, frees, Length@{inds}]]
}, With[{
    indNames = Join[{inds}, GetUniqueIndexOfSlotType /@ dummyTypes]
}, expr /. IndexSlot[n_] :> indNames[[n]]] /; check];
SyntaxInformation@IndexedExprFunction = {"ArgumentsPattern" -> {_, _, _}};

RaiseOneFreeNameRule[name_, indsPos_] := With[{
    pos = Lookup[indsPos, name, Null]
}, If[pos =!= Null,
    Thread[pos[[All, 3]] -> name],
    {}
]];
RaiseFrees[expr_, frees_] := ReplacePart[expr, Join @@ With[{indsPos = FindAllIndicesNames@expr}, RaiseOneFreeNameRule[#, indsPos] & /@ frees]];
SyntaxInformation@RaiseFrees = {"ArgumentsPattern" -> {_, _}};

WithTempIndex1[body_] := Block[{$TempIndexNumber = 1}, body];
SetAttributes[WithTempIndex1, HoldAll];
SyntaxInformation@WithTempIndex1 = {"ArgumentsPattern" -> {_}};

GetIndsPos[expr_, inds_] := Thread[inds -> With[{
    indPos = FindAllIndicesNames@expr
}, (Delete[2] /@ Lookup[indPos, #, {}]) & /@ inds]];
SetAttributes[GetIndsPos, HoldFirst];

MakePermutedIndicesRules[name_, poses_] := #2 -> (#1 /. IndexNameSlot -> name) & @@@ poses;
PermuteIndices[expr_, inds_, a_ * img_Images] := a * PermuteIndices[expr, inds, img];
PermuteIndices[expr_, inds_, perm_] := PermuteIndices[expr, GetIndsPos[expr, inds], perm] /; Head@inds[[1]] =!= Rule;
PermuteIndices[expr_, inds_, img_Images] := ReplacePart[
    expr,
    Join @@ MapThread[MakePermutedIndicesRules, {Permute[inds[[All, 1]], List @@ img], inds[[All, 2]]}]
] /; Head@inds[[1]] === Rule;
SyntaxInformation@PermuteIndices = {"ArgumentsPattern" -> {_, _, _}};

ImposeSymmetry[expr_, inds_, genset_] := With[{
    indsWithPos = GetIndsPos[expr, inds],
    elems = PPermGroupElements@genset
},
    Total[PermuteIndices[expr, indsWithPos, #] & /@ elems] / Length@elems
];
SyntaxInformation@ImposeSymmetry = {"ArgumentsPattern" -> {_, _, _}};

ExpandScalarIndexScope[expr_] := expr //. {
    HoldPattern@Power[IndexScope[expr2_], n_] :> Times @@ Table[ReplaceDummiesToUnique@expr2, n],
    HoldPattern@IndexScope[expr2_] :> ReplaceDummiesToUnique@expr2
};
SyntaxInformation@ExpandScalarIndexScope = {"ArgumentsPattern" -> {_}};

MapScalarIndexScopes[fn_, expr_] := expr /. HoldPattern@IndexScope@expr2_ :> IndexScope@fn@expr2;
MapScalarIndexScopes[fn_][expr_] := MapScalarIndexScopes[fn, expr];
SyntaxInformation@MapScalarIndexScopes = {"ArgumentsPattern" -> {_, _.}};

DeepContractMetric[expr_, metrics_] := ContractMetric[expr /. HoldPattern@IndexScope@expr2_ :> IndexScope@DeepContractMetric[expr2, metrics], metrics];
DeepContractMetric[expr_] := DeepContractMetric[expr, All];
SyntaxInformation@DeepContractMetric = {"ArgumentsPattern" -> {_, _.}};

FindIndicesSlots[MetricBarrier[expr_]] ^:= FindIndicesSlots[expr, {1}];
SymmetryOfExpression[MetricBarrier[expr_]] ^:= SymmetryOfExpression@expr;
MetricBarrier /: ExpressionPassThroughQ[_MetricBarrier, _, _] = False;
SyntaxInformation@MetricBarrier = {"ArgumentsPattern" -> {_}};

ReleaseMetricBarrier[expr_] := expr /. MetricBarrier[e_] :> e;
SyntaxInformation@ReleaseMetricBarrier = {"ArgumentsPattern" -> {_}};

CanonicalizationUnitQ[_Plus] = False;
CanonicalizationUnitQ[_List] = False;
CanonicalizationUnitQ[_] = True;
SetAttributes[CanonicalizationUnitQ, HoldAll];

NoIndicesQ[expr_] := Length@FindIndicesSlots === 0;
NoIndicesQ[expr_List] := AllTrue[expr, NoIndicesQ];
NoIndicesQ[expr_Plus] := AllTrue[List @@ expr, NoIndicesQ];
SetAttributes[NoIndicesQ, HoldAll];

Options@TensorGridBox = {
    AllowSubsuperscriptBox -> False,
    IndexStyle -> "Default"
};
SelectUpDownIndices[{{a_, IndexNameSlot}, {b_, DI@IndexNameSlot}}] := {a, b};
SelectUpDownIndices[{{a_, DI@IndexNameSlot}, {b_, IndexNameSlot}}] := {b, a};
ConvertIndexStyle["Default"][ind_] := ind;
ConvertIndexStyle["AllUp"][{a_, _}] := {a, IndexNameSlot};
ConvertIndexStyle["AllDown"][{a_, _}] := {a, DI@IndexNameSlot};
IndexToGridRow[{a_, IndexNameSlot}] := {MakeBoxes@a, ""};
IndexToGridRow[{a_, DI@IndexNameSlot}] := {"", MakeBoxes@a};
TensorGridBox[t_, {}, ___] := t;
TensorGridBox[t_, inds_List, opt : OptionsPattern[]] := With[{
    inds2 = ConvertIndexStyle[OptionValue@IndexStyle] /@ inds
}, Which[
    Length@inds === 1,
    With[{ind = inds2[[1]], indName = inds2[[1, 1]]}, Switch[ind[[2]],
        IndexNameSlot, SuperscriptBox[t, MakeBoxes@indName],
        DI@IndexNameSlot, SubscriptBox[t, MakeBoxes@indName]
    ]],

    OptionValue[AllowSubsuperscriptBox] && Sort[inds[[All, 2]]] === $IndexPairPattern,
    SubsuperscriptBox[t, ##] & @@ (MakeBoxes[#, StandardForm] & /@ Reverse@SelectUpDownIndices@inds),

    True,
    GridBox[{{t, StyleBox[
        GridBox[
            Transpose[IndexToGridRow /@ inds2], RowSpacings -> 0, ColumnSpacings -> 0
        ], FontSize -> 39/4
    ]}}, ColumnSpacings -> 0.05, RowAlignments -> Center]
]];
SyntaxInformation@TensorGridBox = {"ArgumentsPattern" -> {_, _, OptionsPattern[]}};

TensorInterpretationBox[expr_, box_] := InterpretationBox[StyleBox[box, ShowAutoStyles -> False], expr, Editable -> False];
SetAttributes[TensorInterpretationBox, HoldFirst];
SyntaxInformation@TensorInterpretationBox = {"ArgumentsPattern" -> {_, _}};

Options[DefTensorFormatings] = Union[Options[TensorGridBox], {
    DisplayName -> None
}];
DefTensorFormatings[symbol_, opt : OptionsPattern[]] := With[{
    name = OptionValue@DisplayName,
    fn = With[{
        opt2 = FilterRules[{opt}, Options@TensorGridBox]
    }, Function[{tname}, symbol /: MakeBoxes[expr : symbol[inds___], StandardForm] := TensorInterpretationBox[expr, TensorGridBox[tname, SeparateIndexName /@ {inds}, opt2]], {HoldAll}]]
}, If[name === None, fn@MakeBoxes@symbol, fn@name]];
SyntaxInformation@DefTensorFormatings = {"ArgumentsPattern" -> {_, OptionsPattern[]}};

Options[DefSimpleTensor] = Options@DefTensorFormatings;
DefSimpleTensor[sym_, slots_, symmetry_, opt : OptionsPattern[]] := With[{
    slotsAndPos = MapIndexed[#2 -> #1 &, slots]
},
    (
        sym /: FindIndicesSlots[sym[##]] = slotsAndPos;
        If[symmetry =!= {},
            sym /: SymmetryOfExpression[sym[##]] = symmetry;
        ];
    ) & @@ ConstantArray[_, Length@slots];
    If[OptionValue@DisplayName =!= None, DefTensorFormatings[sym, opt]];
    sym /: NITensorReduce[sym[inds__], frees_] := NITensorReduce[NITensor[sym, IndexName /@ {inds}], frees];
    SyntaxInformation@sym = {"ArgumentsPattern" -> ConstantArray[_, Length@slots]};
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

ContractionSlotOfMetric[_[DI@_, DI@_]] = 1;
ContractionSlotOfMetric[_[_?NonDIQ, _?NonDIQ]] = 2;
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

ReplaceIndexNames[expr_, reps_] := ReplacePart[expr, ReplaceIndicesRules[Map@Delete[2] /@ FindAllIndicesNames@expr, reps]];
SyntaxInformation@ReplaceIndexNames = {"ArgumentsPattern" -> {_, _}};

TryReplaceIndexNames[expr_, reps_] := With[{
    reps2 = Select[reps, AbsIndexNameQ@#[[1]] &]
}, If[Length@reps2 > 0,
    With[{
        indPoses = Select[{#2, Lookup[FindAllIndicesNames@expr, #1, Null]} & @@@ reps2, #[[2]] =!= Null &]
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
SyntaxInformation@TryReplaceIndexNames = {"ArgumentsPattern" -> {_, _}};

MetricOfSlotType /: Times[MetricOfSlotType[type_][DI@b_, a_?NonDIQ], rest_] := With[{
    newExpr = TryReplaceIndexNames[rest, {a -> b, b -> a}]
}, newExpr /; newExpr =!= $Failed];
MetricOfSlotType /: Times[MetricOfSlotType[type_][a_?NonDIQ, DI@b_], rest_] := With[{
    newExpr = TryReplaceIndexNames[rest, {a -> b, b -> a}]
}, newExpr /; newExpr =!= $Failed];

SyntaxInformation@MetricOfSlotType = {"ArgumentsPattern" -> {_}};

NonDummyIndexQ[_DummyIndex] = False;
NonDummyIndexQ[_] = True;
ReduceDummyCount[DummyIndex[d_, _]] := DummyIndex[d, 1];
ReduceDummyCount[d_] := d;

Options[DefSimpleMetric] = Union[
    Options[DefSimpleTensor], {
        ContractionRuleMode -> "Normal"
    }
];
TimesQ[a_] := a === Times;
DefSimpleMetric::invalidsymsign = "Invalid symmetric sign `1`: may only be 1, -1, or 0. Assuming 1.";
DefSimpleMetric::unknowncontractrulemode = "Invalid contraction rule mode `1`.";
DefSimpleMetric[sym_, slotType_, symSign_, opt : OptionsPattern[]] := (
    DefSimpleTensor[
        sym,
        {slotType, slotType},
        Switch[symSign, 1, {SCycles@{1, 2}}, -1, {-SCycles@{1, 2}}, 0, {}, _, Message[DefSimpleMetric::invalidsymsign, symSign]; {}],
        FilterRules[{opt}, Options@DefSimpleTensor]
    ];
    (* TODO: anti-symmetric metric *)
    Switch[OptionValue@ContractionRuleMode,
        "Normal",
        sym[a_?NonDIQ, DI@b_] := sym[DI@b, a];
        sym[a_?AbsIndexNameQ, DI@a_] := DimensionOfSlotType[slotType];
        sym[DI@a_, a_?AbsIndexNameQ] := DimensionOfSlotType[slotType];
        sym /: ContractableMetricQ[sym[_, _]] = True;
        sym /: HoldPattern[sym[a_, b_?AbsIndexNameQ]sym[DI@b_, c_]] := sym[a, c];
        sym /: HoldPattern[sym[a_, b_?AbsIndexNameQ]sym[c_, DI@b_]] := sym[a, c];
        sym /: HoldPattern[sym[b_?AbsIndexNameQ, a_]sym[DI@b_, c_]] := sym[a, c];
        sym /: HoldPattern[sym[b_?AbsIndexNameQ, a_]sym[c_, DI@b_]] := sym[a, c];
        sym /: _?TimesQ[l___, sym[DI@b_, a_?NonDIQ], r___] := With[{
            newExpr = TryReplaceIndexNames[Times[l, r], {a -> b, b -> a}]
        }, newExpr /; newExpr =!= $Failed];
        sym /: _?TimesQ[l___, sym[a_?NonDIQ, DI@b_], r___] := With[{
            newExpr = TryReplaceIndexNames[Times[l, r], {a -> b, b -> a}]
        }, newExpr /; newExpr =!= $Failed];
    ,
        "AllUp",
        sym[a_?AbsIndexNameQ, a_] := DimensionOfSlotType@slotType;
        sym /: _?TimesQ[l___, sym[a_, b_], r___] := With[{
            newExpr = TryReplaceIndexNames[Times[l, r], {a -> b, b -> a}]
        }, newExpr /; newExpr =!= $Failed];
    ,
        _,
        Message[DefSimpleMetric::unknowncontractrulemode, OptionValue@ContractionRuleMode]
    ];
);
SyntaxInformation@DefSimpleMetric = {"ArgumentsPattern" -> {_, _, _, OptionsPattern[]}};

Options@DefSimpleDeltaTensor = Options@DefTensorFormatings;
CheckOneSummableIndex[l_List, a_] := AnyTrue[l, CheckOneSummableIndex[#, a] &];
CheckOneSummableIndex[expr_, a_] := TensorTermQ@expr && AnyTrue[Lookup[FindAllIndicesNames@expr, a, {}], SumPassThroughQ[expr, #[[3]]] &];
DefSimpleDeltaTensor[symbol_, type_, opt : OptionsPattern[]] := (
    FindIndicesSlots[symbol[_]] ^= {{1} -> type};
    FindIndicesSlots[symbol[_, _]] ^= {{1} -> type, {2} -> type};
    SymmetryOfExpression[symbol[_, _]] ^= {SCycles@{1, 2}};

    If[OptionValue@DisplayName =!= None, DefTensorFormatings[symbol, opt]];

    symbol[a_, a_] = 1;
    symbol /: _?TimesQ[l___, symbol[a_, b_], r___] := With[{
        newExpr = TryReplaceIndexNames[Times[l, r], {a -> b}]
    }, symbol[a, b] * newExpr /; a =!= b && newExpr =!= $Failed];
    symbol /: IndexedSumTimes[inds_, r1___, symbol[a_, b_], r2___] := With[{
        summedInds = Intersection[{a, b}, inds[[All, 1]]]
    }, With[{
        summedInd = summedInds[[1]],
        otherInd = First@DeleteCases[{a, b}, summedInds[[1]]]
    },
        IndexedSumTimes[DeleteCases[inds, {summedInd, _}], ##] & @@ (ReplaceIndexNames[#, {summedInd -> otherInd}] & /@ {r1, r2})] /; Length@summedInds > 0
    ];
    RenamingSymmetryOfExpression@symbol[a_, b_] := {{a, b}};
);
SyntaxInformation@DefSimpleDeltaTensor = {"ArgumentsPattern" -> {_, _, OptionsPattern[]}};

SignOf2SymGroup[{}] = 0;
SignOf2SymGroup[{SCycles@{1, 2}}] = 1;
SignOf2SymGroup[{-SCycles@{1, 2}}] = -1;
SelectMetricForContraction[All][expr_] := ContractableMetricQ@expr;
SelectMetricForContraction[metrics_List][expr_] := MatchQ[expr, Alternatives @@ metrics];
SelectMetricForContraction[metrics_][expr_] := MatchQ[expr, metrics];
ContractMetricExpanded[expr_Plus, metrics_] := Function[{arg}, ContractMetricExpanded[arg, metrics]] /@ expr;
ContractOneMetric[expr_, metric_] := With[{
    exprIndToPos = First /@ Select[GroupBy[FindIndicesSlotsAndNames@expr, Extract[{2, 1}] -> ({#[[2, 2]], #[[1]]} &)], Length@# === 1 &],
    metricInds = FindIndicesSlotsAndNames[metric][[All, 2, 1]],
    contractionSlot = ContractionSlotOfMetric@metric,
    metricSym = SignOf2SymGroup@SymmetryOfExpression@metric
}, With[{
    pos1 = Lookup[exprIndToPos, DI /@ metricInds, Null]
}, Which[
    pos1[[1]] =!= Null && (metricSym === 1 || metricSym === -1 || contractionSlot === 1) && ExpressionPassThroughQ[expr, metric, pos1[[1, 2]]] && SumPassThroughQ[expr, pos1[[1, 2]]],
    If[contractionSlot =!= 1, metricSym, 1] * ReplacePart[expr, pos1[[1, 2]] -> metricInds[[2]]],

    pos1[[2]] =!= Null && (metricSym === 1 || metricSym === -1 || contractionSlot === 2) && ExpressionPassThroughQ[expr, metric, pos1[[2, 2]]] && SumPassThroughQ[expr, pos1[[2, 2]]],
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

ContractMetric[expr_?ArrayQ, metrics_] := Map[ContractMetric[#, metrics] &, expr, {ArrayDepth@expr}];
ContractMetric[expr_Association, metrics_] := ContractMetric[#, metrics] & /@ expr;
ContractMetric[ETensor[expr_, inds_], metrics_] := ETensor[ContractMetric[expr, metrics], inds];
ContractMetric[expr_, metrics_] := ContractMetricExpanded[ExpandToTensorPolynomial@expr, metrics];
ContractMetric[expr_] := ContractMetric[expr, All];
SyntaxInformation@ContractMetric = {"ArgumentsPattern" -> {_, _.}};

LookupListOrApply[list_List, elem_] := If[Length@list === 1 && Head@list[[1]] =!= Rule, list[[1]], elem /. list];
LookupListOrApply[list_, elem_] := list@elem;
MoveMetricContractSlotTo1[metric_[a_, b_]] := If[ContractionSlotOfMetric@metric[a, b] === 2, metric[b, a], metric[a, b]];
SeparateMetricOneIndex[pos_ -> type_, metric_, {indName_, IndexNameSlot}, -1] := With[{dummy = GetUniqueIndexOfSlotType@type}, {pos -> DI@dummy, MoveMetricContractSlotTo1@metric[dummy, indName]}];
SeparateMetricOneIndex[pos_ -> type_, metric_, {indName_, DI@IndexNameSlot}, 1] := With[{dummy = GetUniqueIndexOfSlotType@type}, {pos -> dummy, MoveMetricContractSlotTo1@metric[DI@dummy, DI@indName]}];
SeparateMetricOneIndex[_, _, _, _] = Nothing;
SeparateMetricOne[expr_, indsPat_] := SeparateMetricOne[expr, indsPat, MetricOfSlotType];
SeparateMetricOne[expr_, indsPat_, metricProvider_] := With[{
    indPos = FindIndicesSlots@expr
}, With[{
    reps = Transpose@MapThread[SeparateMetricOneIndex[#1, LookupListOrApply[metricProvider, #1[[2]]], SeparateIndexName@#2, #3] &, {indPos, Extract[expr, indPos[[All, 1]]], indsPat}]
}, If[Length@reps =!= 0, ReplacePart[expr, reps[[1]]] * Times @@ reps[[2]], expr]]];
SyntaxInformation@SeparateMetricOne = {"ArgumentsPattern" -> {_, _, _.}};

MapShiftedIndexed[f_, expr_] := MapThread[f, {Drop[expr, -1], Drop[expr, 1], Range[Length@expr - 1]}];

ISortArgToSortTag[arg_] := With[{
    sym = SymmetryOfExpression@arg,
    indPos = FindIndicesSlots@arg
}, {arg, -GroupOrderFromStrongGenSet@sym, -Length@indPos, ReplacePart[arg, Thread[indPos[[All, 1]] -> IndexSlot[1]]]}];

ISortUnorderedArgs[head_, {args__}] := With[{
    sortedArgs = SortBy[ISortArgToSortTag /@ (List @@ (ISort /@ Hold@args)), Delete[1]]
}, With[{
    symList = MapShiftedIndexed[If[#1 === #2, #3, Nothing] &, sortedArgs[[All, 4]]]
},
    Function[{arg}, ISortedProduct[head, arg, symList]]@sortedArgs[[All, 1]]
]];
SyntaxInformation@ISortUnorderedArgs = {"ArgumentsPattern" -> {_, _}};

(* TODO: products like Wedge could produce a negative sign *)
ISort[fn_?UnorderedProductQ[args___]] := ISortUnorderedArgs[fn, {args}];
ISort[expr_] := ISortedGeneral@expr;
SetAttributes[ISort, {HoldAll}];

ReleaseISort[expr_] := ReleaseHold[Hold@expr //. {ISortedGeneral[e_] :> e, ISortedProduct[head_, {args___}, _] :> head[args]}];
SyntaxInformation@ReleaseISort = {"ArgumentsPattern" -> {_}};

SetAttributes[{ISortedGeneral, ISortedProduct}, {HoldAll}];

SymbolToIndex[-a_] := {-1, a};
SymbolToIndex[DI[a_]] := {-1, a};
SymbolToIndex[a_] := {1, a};

GroupIndexList[list_] := If[Length@list > 0 && Head@list[[1]] =!= Rule,
    MapIndexed[Join[SeparateIndexName@#1, #2] &, list],
    Join[SeparateIndexName@#2, {#1}] & @@@ list
] (* {name, type, pos} *) //
    GroupBy[First -> Delete[1]] //
    (* name -> {{type, pos}, ...} *)
    Map[Transpose@SortBy[#, First, IndexOrder] &] //
    (* name -> {{type1, type2, ...}, {pos1, pos2, ...}} *)
    KeyValueMap[{#2[[1]], #1 -> #2[[2]]} &] //
    (* {{type1, type2, ...}, name -> {pos1, pos2, ...}} *)
    GroupBy[First -> Extract[2]] //
    (* <|{type1, type2, ...} -> {name1 -> {pos1, pos2, ...}, ...}|> *)
    KeyValueMap[Rule];
SyntaxInformation@GroupIndexList = {"ArgumentsPattern" -> {_}};

IndexOrder[DI@a_, DI@b_] := Order[a, b];
IndexOrder[a_?NonDIQ, DI@b_] := 1;
IndexOrder[DI@a_, b_?NonDIQ] := -1;
IndexOrder[a_?NonDIQ, b_?NonDIQ] := Order[a, b];
SyntaxInformation@IndexOrder = {"ArgumentsPattern" -> {_, _}};

AddSymmetryToOneIndex[pat : {a_, DI@a_}, indToPos : (indName_ -> {pos1_, pos2_}), pairSym_] := {
    pat,
    With[{s = pairSym[indName, pos1, pos2]}, If[s === 0, {}, {s}]],
    RenamingGroupOfIndexName@indName
} -> indToPos;
AddSymmetryToOneIndex[pat : {DI@a_, a_}, indToPos : (indName_ -> {pos1_, pos2_}), pairSym_] := {
    pat,
    With[{s = pairSym[indName, pos2, pos1]}, If[s === 0, {}, {s}]],
    RenamingGroupOfIndexName@indName
} -> indToPos;
AddSymmetryToOneIndex[pat_, indToPos : (indName_ -> _), _] := {
    pat,
    MapShiftedIndexed[If[#1 === #2, #3, Nothing] &, pat],
    RenamingGroupOfIndexName@indName
} -> indToPos;

AddSymmetryToOneIndexGroup[pairSym_][pat_ -> indsToPos_] := AddSymmetryToOneIndex[pat, #, pairSym] & /@ indsToPos;
AddSymmetryToGroupedIndexList[list_List, pairSym_] := Join @@ (AddSymmetryToOneIndexGroup[pairSym] /@ list) //
    GroupBy[First -> Extract[2]] //
    Map[SortBy[First]] //
    KeySort //
    KeyValueMap[Rule];
SyntaxInformation@AddSymmetryToGroupedIndexList = {"ArgumentsPattern" -> {_, _}};

SymmetryGroupOfSymmetricIndexGroup[i_, {pat_, symList_, renamingGroup_} -> inds_] := With[{
    blockLen = Length@pat,
    baseGenSet = If[# > 0, SCycles@{#, # + 1}, -SCycles@{-#, -# + 1}] & /@ symList
}, {Join[
    Join @@ Array[ShiftPermutation[baseGenSet, blockLen * (# - 1) + i] &, Length@inds],
    If[renamingGroup =!= Null,
        Join @@ With[{
            block = Range@blockLen + i
        }, Array[BlockSymmetricGenSet[blockLen * (# - 1) + block, blockLen * # + block] &, Length@inds - 1]],
        {}
    ]
], i + blockLen * Length@inds}];
SymmetryGroupOfSymmetricIndexList[list_List] := Join @@ FoldPairList[SymmetryGroupOfSymmetricIndexGroup, 0, list];
SyntaxInformation@SymmetryGroupOfSymmetricIndexList = {"ArgumentsPattern" -> {_}};

MapSkewedIndexed[fn_, list_] := MapThread[fn, {Delete[list, -1], Delete[list, 1], Range[Length@list - 1]}];

IndexPairPatternQ[{a_, DI@a_}] = True;
IndexPairPatternQ[{DI@a_, a_}] = True;
IndexPairPatternQ[_] = False;

SortGroupedIndexList[list_List] := MapAt[Sort, 2] /@ SortBy[list, First];
SyntaxInformation@SortGroupedIndexList = {"ArgumentsPattern" -> {_}};

ToPattern[list_List] := Alternatives @@ (ToPattern /@ list);
ToPattern[patt_] := patt;
FindAndDropFrees[list_List, Automatic] := FindAndDropFrees[list, Length@#1 === 1 &];
FindAndDropFrees[list_List, frees_List] := FindAndDropFrees[list, MemberQ[frees, #2] &];
FindAndDropFreesOneGroup[indPat_ -> inds_List, freePredicate_] := indPat -> # & /@ Lookup[GroupBy[inds, freePredicate[indPat, #[[1]]] &], {True, False}, {}];
FindAndDropFrees[list_List, freePredicate_] := If[Length@list > 0,
    SortGroupedIndexList /@ Select[Length@#[[2]] > 0 &] /@ Transpose[
        FindAndDropFreesOneGroup[#, freePredicate] & /@ list
    ],
    {{}, {}}
];
SyntaxInformation@FindAndDropFrees = {"ArgumentsPattern" -> {_, _}};

CollectGroupedIndices[list_List] := Join @@ (Function[{pat, inds},
    Join @@ (Function[{name, val},
        MapThread[Function[{p, v}, v -> (p /. IndexNameSlot -> name)], {pat, val}]
    ] @@@ inds)
] @@@ List @@ list);
SyntaxInformation@CollectGroupedIndices = {"ArgumentsPattern" -> {_}};

CollectGroupedIndicesNames[list_List] := Join @@ list[[All, 2, All, 1]];
SyntaxInformation@CollectGroupedIndicesNames = {"ArgumentsPattern" -> {_}};

GroupedIndexListMapFold[fn_, x_, list_List] := Delete[FoldList[
    {inds, entry} |-> With[{
        patt = entry[[1]]
    }, With[{
        res = Delete[FoldList[{inds2, indNameAndPos} |-> fn[inds2[[2]], patt, indNameAndPos[[1]], indNameAndPos[[2]]], {Null, inds[[2]]}, entry[[2]]], 1] (* {{name1, inds1}, {name2, inds2}, ...} *)
    },
        {patt -> res[[All, 1]], res[[-1, 2]]}
    ]],
    {Null, x},
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

RenameGroupedIndexList[list_List, inds_, idToType_] := GroupedIndexListMapFold[
    {inds2, type, name, values} |-> If[RenamingGroupOfIndexName@name =!= Null,
        With[{
            ind = GetIndexOfSlotType[idToType[values[[1]]], inds2]
        }, {ind -> values, Append[inds2, ind]}]
    ,
        {name -> values, inds2}
    ]
, inds, list];
SyntaxInformation@RenameGroupedIndexList = {"ArgumentsPattern" -> {_, _, _}};

ExpressionPairSymProvider[expr_, allIndPos_, symDummyPairSelector_][indName_, pos1_, pos2_] := With[{
    type1 = allIndPos[[pos1, 2]],
    type2 = allIndPos[[pos2, 2]]
}, If[
    FilterExprList[symDummyPairSelector, type1] && FilterExprList[symDummyPairSelector, type2] &&
    InterchangableIndexPairQ[type1, indName] && InterchangableIndexPairQ[type2, indName] &&
    SumPassThroughQ[expr, allIndPos[[pos1, 1]]] && SumPassThroughQ[expr, allIndPos[[pos2, 1]]] &&
    ExpressionPassThroughQ[expr, MetricOfSlotType[type1][DefaultIndex[1], DefaultIndex[2]], allIndPos[[pos1, 1]]] && ExpressionPassThroughQ[expr, MetricOfSlotType[type1][DefaultIndex[1], DefaultIndex[2]], allIndPos[[pos2, 1]]]
,
    SignOfSymmetricPair[type1, type2, indName],
    0
]];

ApplyIndices[_, _, 0] = 0;
ApplyIndices[expr_, indPos_, a_ * newInds_] := a * ApplyIndices[expr, indPos, newInds];
ApplyIndices[expr_, indPos_, newInds_IndexList] := ReplacePart[expr, Thread[indPos -> (List @@ newInds)]];
PermuteIndexList[list_, a_ * perm_] := a * PermuteIndexList[list, perm];
PermuteIndexList[list_, perm_Images] := IndexList @@ Permute[list, List @@ perm];
PermuteIndexList[_, 0] = 0;

ConvertFreeIndicesSym[{inds_, sym_}, frees_] := With[{
    perm = FillNoneSlots[Images @@ (FirstPosition[inds, #, {Null}][[1]] & /@ frees), Length@inds + 1]
}, With[{
    invPerm = SignedInversePermutation@perm
},
    PPermPermutationProduct[perm, #, invPerm] & /@ sym
]];
ConvertFreeIndicesSym[sym_List, frees_] := sym;

CheckNoSummedIndices[expr_] := If[FreeQ[expr, SummedIndices], expr, Throw@$Failed];
CanonicalizeOneSorted[expr_, frees_, freesSym_, renameDummies_, symDummyPairSelector_] := With[{
    indPos = FindIndicesSlots@expr,
    slotsSym = SymmetryOfExpression@expr
}, With[{
    actualInds = Extract[expr, First /@ indPos]
}, With[{
    groupedInds = FindAndDropFrees[GroupIndexList[actualInds], frees]
}, With[{
    dummySymList = AddSymmetryToGroupedIndexList[groupedInds[[2]], ExpressionPairSymProvider[expr, indPos, symDummyPairSelector]],
    freeInds = CollectGroupedIndices@groupedInds[[1]]
}, With[{
    indsSym = Join[
        ShiftPermutation[
            SymmetryGroupOfSymmetricIndexList@dummySymList,
            Length@freeInds
        ],
        ConvertFreeIndicesSym[freesSym, freeInds[[All, 2]]]
    ],
    groupedDummies = MapAt[Extract[1], 1] /@ dummySymList
}, With[{
    canonIndsAndPos = {freeInds, CollectGroupedIndices[MapAt[First, 1] /@ dummySymList]}
}, With[{
    perm = InversePermutation@(Join @@ canonIndsAndPos)[[All, 1]],
    canonInds = (Join @@ canonIndsAndPos)[[All, 2]],
    renamedDummyGroup = RenameGroupedIndexList[groupedDummies, CollectGroupedIndicesNames@groupedInds[[1]], indPos[[#, 2]] &]
}, With[{
    renamedCanonInds = If[renameDummies, Join[
        freeInds,
        CollectGroupedIndices@renamedDummyGroup
    ][[All, 2]], canonInds]
},
    xToolsDebugPrint[CanonicalizeOneSorted, "input: ", HoldForm@expr];
    xToolsDebugPrint[CanonicalizeOneSorted, "actual indices: ", actualInds];
    xToolsDebugPrint[CanonicalizeOneSorted, "gropued indices: ", dummySymList];
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
                {ret, Association@Thread[CollectGroupedIndicesNames@groupedDummies -> CollectGroupedIndicesNames@renamedDummyGroup]}
            ]
        ]
    ,
        xToolsDebugPrint[CanonicalizeOneSorted, "result: ", HoldForm@expr];
        {expr, <||>}
    ]
]]]]]]]];

Options@ITensorReduceOneTerm = {
    UseMetricOnSlots -> All,
    FreeIndexNames -> Automatic,
    FreeIndicesSymmetry -> {},
    RenameDummies -> True,
    PostprocessCanonicalIndicesList -> Identity,
    FixedPoint -> False
};
ITensorReduceOneTerm::nottensorterm = "`1` is not a tensor term, skipping.";
ITensorReduceOneTerm[expr_Plus, opt : OptionsPattern[]] := ITensorReduceOneTerm[#, opt] & /@ expr;
ITensorReduceOneTerm[expr_?ArrayQ, opt : OptionsPattern[]] := Map[ITensorReduceOneTerm[#, opt] &, expr, {ArrayDepth@expr}];
ITensorReduceOneTerm[expr_List, opt : OptionsPattern[]] := ITensorReduceOneTerm[#, opt] & /@ expr;
PutIndexWrapperIfNeeded[expr_Times] := PutIndexWrapperIfNeeded /@ expr;
PutIndexWrapperIfNeeded[expr_] := If[!FreeQ[FindIndicesSlots@expr, SummedIndices], PutIndexWrapper@expr, expr];
CollectIndexedSumTermAndPopulateIndexedSumTimes[expr_] := With[{
    ret = CollectIndexedSumTerm[expr, False]
}, CollectIndexedSumTerm[
    IndexedSumTimes[ret[[2]], ret[[1]]] /. IndexedSumTimes[{inds__}, factors___] :> IndexedSum[Times@factors, inds],
    False
]];
ITensorReduceOneTermInner[expr_, opt : OptionsPattern[]] := With[{
    exprAndSummedInds = CollectIndexedSumTermAndPopulateIndexedSumTimes@expr
}, With[{
    ret = CanonicalizeOneSorted[
        With[{expr2 =
            PreITensorReduce[exprAndSummedInds[[1]], opt] //
                PutIndexWrapperIfNeeded
        }, ISort@expr2],
        With[{frees = OptionValue[ITensorReduceOneTerm, opt, FreeIndexNames]},
            If[Length@exprAndSummedInds[[2]] > 0 && frees === Automatic,
                With[{summed = exprAndSummedInds[[2, All, 1]]}, Length@#1 === 1 && !MemberQ[summed, #2] &],
                frees
            ]
        ],
        OptionValue[ITensorReduceOneTerm, opt, FreeIndicesSymmetry],
        OptionValue[ITensorReduceOneTerm, opt, RenameDummies],
        OptionValue[ITensorReduceOneTerm, opt, UseMetricOnSlots]
    ]
}, With[{
    exprRet = ret[[1]] //
        ReleaseISort //
        PostITensorReduce //
        ReleaseIndexWrapper,
    summedIndsRet = {Lookup[ret[[2]], #1, #1], #2} & @@@ exprAndSummedInds[[2]]
},
    If[Length@summedIndsRet > 0,
        IndexedSum[exprRet, ##] & @@ summedIndsRet,
        exprRet
    ]
]]];
ITensorReduceOneTerm[expr_, opt : OptionsPattern[]] := With[{
    ret = If[OptionValue@FixedPoint,
        FixedPoint[ITensorReduceOneTermInner[#, opt] &, expr],
        ITensorReduceOneTermInner[expr, opt]
    ]
},
    If[ret === $Failed, Message[ITensorReduceOneTerm::nottensorterm, HoldForm@expr]; expr, ret]
];
SyntaxInformation@ITensorReduceOneTerm = {"ArgumentsPattern" -> {_, OptionsPattern[]}};

RenamingSymmetryOfExpression[_] = {};
SyntaxInformation@RenamingSymmetryOfExpression = {"ArgumentsPattern" -> {_}};

ExpandToTensorPolynomial[expr_] := expr //. {
    e_IndexScope :> e,
    prod_?ProductQ[l___, p_Plus?NonTensorTermQ, r___] :> Function[{a}, prod[l, a, r]] /@ p,
    Power[a_Plus?NonTensorTermQ, b_Integer] :> Total@Flatten[(Outer[Times, ##] & @@ ConstantArray[List @@ a, b]), b - 1] /; b > 0
};
SyntaxInformation@ExpandToTensorPolynomial = {"ArgumentsPattern" -> {_}};

Options[ITensorReduce] = Options[ITensorReduceOneTerm];
ITensorReduce[NITensor[expr_, inds_], opt : OptionsPattern[]] := NITensor[ITensorReduce[expr, opt], inds];
ITensorReduce[expr_Rule, opt___] := ITensorReduce[#, opt] & /@ expr;
ITensorReduce[expr_List, opt___] := ITensorReduce[#, opt] & /@ expr;
ITensorReduce[expr_, opt : OptionsPattern[]] := ITensorReduceOneTerm[ExpandToTensorPolynomial@expr, FilterRules[{opt}, Options@ITensorReduceOneTerm]];
SyntaxInformation@ITensorReduce = {"ArgumentsPattern" -> {_, OptionsPattern[]}};

IndexSlot /: MakeBoxes[expr : IndexSlot[n_], StandardForm] := With[{
    box = MakeBoxes[n, StandardForm]
}, InterpretationBox[SubscriptBox["\[FilledSmallCircle]", box], expr]];
SyntaxInformation@IndexSlot = {"ArgumentsPattern" -> {_}};

Options@PreITensorReduce = Options@ITensorReduce;
PreITensorReduce[arr_?ArrayQ, opt___] := Map[PreITensorReduce[#, opt] &, arr, {ArrayDepth@arr}];
PreITensorReduce[expr_Association, opt___] := PreITensorReduce[#, opt] & /@ expr;
PreITensorReduce[expr_IndexedSum?ScalarIndexedSumQ, opt___] := ITensorReduce[expr, opt];
PreITensorReduce[head_[args___], opt___] := PreITensorReduce[head, opt] @@ Map[PreITensorReduce[#, opt] &, {args}];
PreITensorReduce[expr_, ___] := With[{
    allInds = FindAllIndicesNames@expr
}, If[Length@allInds > 0,
    IndexScope[expr, Keys@allInds, Keys@allInds],
    expr
]];
SyntaxInformation@PreITensorReduce = {"ArgumentsPattern" -> {_}};

Options@PostITensorReduce = Options@ITensorReduce;
PostITensorReduce[head_[args__], opt___] := PostITensorReduce[head, opt] @@ Map[PostITensorReduce[#, opt] &, {args}];
PostITensorReduce[expr_, ___] := expr;
SyntaxInformation@PostITensorReduce = {"ArgumentsPattern" -> {_}};

FindAllIndicesNames[expr_] := FindAllIndicesNames[expr, {}];
FindAllIndicesNamesHelper1[ind_, pos_ -> slot_] := Join[SeparateIndexName@ind, {slot, pos}];
FindAllIndicesNames[expr_, pos_] := With[{
    slots = FlattenIndicesSlots@FindIndicesSlots[expr, pos]
}, With[{
    inds = Extract[Hold@expr, Prepend[1] /@ slots[[All, 1]]]
},
    GroupBy[MapThread[FindAllIndicesNamesHelper1, {inds, slots}], First -> Delete[1]]
]];
SetAttributes[{FindAllIndicesNames, FindAllIndicesNamesSubExpression}, HoldFirst];
SyntaxInformation@FindAllIndicesNames = {"ArgumentsPattern" -> {_, _.}};

ReplaceFreesRules[allIndsPos_, frees0_, newFrees0_] := With[{
    frees = Cases[frees0, Alternatives @@ Keys@allIndsPos]
}, With[{
    dummies = Select[Complement[Keys@allIndsPos, frees], RenamingGroupOfIndexName@# =!= Null &],
    newFrees = If[newFrees0 === Automatic, GetIndicesOfSlotType[allIndsPos[#][[1, 2]] & /@ frees, {}], newFrees0]
}, With[{
    dummiesToKeep = Complement[dummies, newFrees],
    dummiesNeedsReplacing = Intersection[dummies, Complement[newFrees, frees]]
}, With[{
    replacementDummies = GetIndicesOfSlotType[allIndsPos[#][[1, 2]] & /@ dummiesNeedsReplacing, Union[dummiesToKeep, newFrees]]
}, {
    Join[
        ReplaceIndicesRules[Map[Delete[2]] /@ allIndsPos, Thread[frees -> Take[newFrees, Length@frees]]],
        ReplaceIndicesRules[Map[Delete[2]] /@ allIndsPos, Thread[dummiesNeedsReplacing -> replacementDummies]]
    ],
    FoldPairList[If[MemberQ[frees, #2], {newFrees[[#1]], #1 + 1}, {Null, #1}] &, 1, frees0]
}]]]];
SyntaxInformation@ReplaceFreesRules = {"ArgumentsPattern" -> {_, _, _}};

ReplaceFrees[expr_, frees0_, newFrees0_] := With[{
    rules = ReplaceFreesRules[FindAllIndicesNames@expr, frees0, newFrees0]
}, {
    ReplacePart[expr, rules[[1]]],
    rules[[2]]
}];
SyntaxInformation@ReplaceFrees = {"ArgumentsPattern" -> {_, _, _}};

PopulateDummyIndexHintExpanded[expr_Plus, frees_] := PopulateDummyIndexHintExpanded[#, frees] & /@ expr;
PopulateDummyIndexHintExpanded[expr_List, frees_] := PopulateDummyIndexHintExpanded[#, frees] & /@ expr;
ReplaceDummyHint1[name_, len_][{type_, slot_, pos_}] := pos -> (type /. IndexNameSlot -> DummyIndex[name, len]);
PopulateDummyIndexHintExpanded[expr_, frees_] := With[{
    dummies = KeyDrop[FindAllIndicesNames@expr, frees]
}, ReplacePart[expr,
    Join @@ KeyValueMap[ReplaceDummyHint1[#1, Length@#2] /@ #2 &, dummies]
]];
PopulateDummyIndexHint[expr_, frees_] := PopulateDummyIndexHintExpanded[ExpandToTensorPolynomial@expr, frees];
SyntaxInformation@PopulateDummyIndexHint = {"ArgumentsPattern" -> {_, _}};

SplitIndexScopeFactorIndexList[indNames_] := With[{
    splitNoInds = Lookup[GroupBy[MapIndexed[#2[[1]] -> #1 &, indNames], Length@#[[2]] > 0 &], {True, False}, {}]
}, {
    splitNoInds[[2, All, 1]],
    UnionClosures@splitNoInds[[1]]
}];

TrySplitIndexScopeFactors[factors_, frees0_] := With[{
    inds = Keys@FindAllIndicesNames@# & /@ factors
}, With[{
    frees = If[Head@frees0 === "Not", Complement[Union @@ inds, frees0[[1]]], frees0]
}, With[{
    dummies = Complement[#, frees] & /@ inds
}, With[{
    inds2 = Lookup[GroupBy[MapIndexed[#2[[1]] -> #1 &, dummies], Length@#[[2]] > 0 &], {True, False}, {}]
}, With[{
    splittedInds = UnionClosures@inds2[[1]],
    constsInds = inds2[[2, All, 1]]
}, If[Length@splittedInds > 1 || Length@constsInds > 0,
    {Times @@ Extract[factors, Thread@{constsInds}], {Times @@ Extract[factors, Thread@{#}], Union @@ Extract[dummies, Thread@{#}]} & /@ splittedInds},
    $Failed
]]]]]];

IndexScope[expr_, {}, {}] := IndexScope[expr];
IndexScope[expr_Plus] := IndexScope /@ expr;
IndexScope[expr_Times] := With[{
    ret = TrySplitIndexScopeFactors[List @@ expr, {}]
}, ret[[1]] * Times @@ (IndexScope /@ ret[[2, All, 1]]) /; ret =!= $Failed];
IndexScope[expr_] := expr /; Length@FindIndicesSlots@expr === 0;
IndexScope /: PreITensorReduce[HoldPattern@IndexScope[expr_], opt___] := IndexScope@ITensorReduce[expr, FreeIndexNames -> {}, FilterRules[{opt}, DeleteCases[Options[ITensorReduce], FreeIndexNames -> _]]];
IndexScope /: FindIndicesSlots@IndexScope[expr_] = {};
IndexScope /: FindIndicesSlots@IndexScope[_, _, inds_List] := MapIndexed[Prepend[#2, 3] -> Null &, inds];
IndexScope /: MakeBoxes[expr : IndexScope[expr2_], StandardForm] := With[{
    box = MakeBoxes@expr2,
    red = Red
}, InterpretationBox[RowBox@{StyleBox["(", FontColor -> red], box, StyleBox[")", FontColor -> red]}, expr]];
SyntaxInformation@IndexScope = {"ArgumentsPattern" -> {_, _., _.}};

PutIndexScope[expr_, {}] := IndexScope@expr;
PutIndexScope[expr_Plus, frees_] := PutIndexScope[#, frees] & /@ expr;
PutIndexScope[expr_List, frees_] := PutIndexScope[#, frees] & /@ expr;
PutIndexScope[expr_Times, frees_] := With[{
    splitInds = SplitIndexScopeFactorIndexList[Join[{frees}, Keys@FindAllIndicesNames@# & /@ List @@ expr]]
}, With[{
    freesAndDummies = MapThread[Construct, {{
        DeleteCases[Join[#[[1]], splitInds[[1]]], 1] - 1 &,
        # - 1 &
    }, Lookup[GroupBy[splitInds[[2]], MemberQ[1]], {True, False}, {}]}]
},
    (Times @@ Extract[expr, Thread@{freesAndDummies[[1]]}]) * (Times @@ (IndexScope[Times @@ Extract[expr, Thread@{#}]] & /@ freesAndDummies[[2]]))
]];
PutIndexScope[expr_, frees_] := If[Length@Intersection[frees, Keys@FindAllIndicesNames@expr] === 0, IndexScope[expr], expr];
PutIndexScope[ETensor[expr_, inds_]] := ETensor[PutIndexScope[expr, DeleteCases[inds, Null]], inds];
PutIndexScope[0] = 0;
PutIndexScope[expr_?ArrayQ] := Map[PutIndexScope, expr, {ArrayDepth@expr}];
PutIndexScope[frees_][expr_] := PutIndexScope[expr, frees];
SyntaxInformation@PutIndexScope = {"ArgumentsPattern" -> {_, _.}};

FindIndicesSlots[IndexWrapper[_, innerInds_List, inds_List]] ^:= MapIndexed[Prepend[#2, 3] -> #1[[2]] &, innerInds];
SyntaxInformation@IndexWrapper = {"ArgumentsPattern" -> {_, _, _}};

PutIndexWrapper[expr_] := With[{
    inds = Normal[Extract[{1, 2}] /@ FindAllIndicesNames@expr]
}, IndexWrapper[expr, inds, inds[[All, 1]]]];
SyntaxInformation@PutIndexWrapper = {"ArgumentsPattern" -> {_}};

ReleaseIndexWrapper[expr_] := expr //. {
    IndexWrapper[expr2_, inner_, outer_] :> ReplacePart[expr2, ReplaceIndicesRules[Map@Delete[2] /@ FindAllIndicesNames@expr2, Thread[inner[[All, 1]] -> outer]]]
};
SyntaxInformation@ReleaseIndexWrapper = {"ArgumentsPattern" -> {_}};

IndexedSum[expr_] := expr;
IndexedSum[expr_, inds__List] := With[{inds2 = SortBy[{inds}, First]}, IndexedSum[expr, ##] & @@ inds2 /; inds2 =!= {inds}];
IndexedSum[expr_, inds__List] := With[{
    freeInds = Complement[{inds}[[All, 1]], Keys@FindAllIndicesNames@expr]
},
    With[{nameToType = Association[Rule @@@ {inds}]}, Times @@ (DimensionOfSlotType@nameToType@# & /@ freeInds)] IndexedSum[expr, ##] & @@ DeleteCases[{inds}, {Alternatives @@ freeInds, _}] /; Length@freeInds > 0
];
IndexedSum[expr_, inds__List] := expr Times @@ (DimensionOfSlotType@#2 & @@@ {inds}) /; Length@Intersection[Keys@FindAllIndicesNames@expr, {inds}[[All, 1]]] === 0;
IndexedSum[expr_Plus, inds__] := IndexedSum[#, inds] & /@ expr;
IndexedSum[IndexedSum[expr_, inds1__List], inds2__List] := IndexedSum[expr, inds1, inds2];
IndexedSum[expr_Times, inds__List] := With[{
    ret = TrySplitIndexScopeFactors[List @@ expr, "Not"@{inds}[[All, 1]]],
    indToType = Association[Rule @@@ {inds}]
}, ret[[1]] Times @@ IndexedSum @@@ (MapAt[Sequence @@ ({#, indToType@#} & /@ #) &, #, 2] & /@ ret[[2]]) /; ret =!= $Failed];
RemoveIndexNamesFromSlotList[expr_, e : (pos_ -> _), indsToRemove_] := If[indsToRemove@Extract[expr, pos], Nothing, e];
RemoveIndexNamesFromSlotList[expr_, e_List, indsToRemove_] := RemoveIndexNamesFromSlotList[expr, #, indsToRemove] & /@ e;
RemoveIndexNamesFromSlotList[expr_, e_SummedIndices, indsToRemove_] := With[{ret = DeleteCases[RemoveIndexNamesFromSlotList[expr, #, indsToRemove] & /@ List @@ e, {}]}, If[Length@ret > 0, SummedIndices @@ ret, Nothing]];
expr_RemoveIndexNamesFromSlotList := Throw@Hold@expr /; $PThrowOnError;
FindIndicesSlots[IndexedSum[expr_, inds__]] ^:= With[{
    poses = FindIndicesSlots[expr, {1}]
}, RemoveIndexNamesFromSlotList[Hold@expr, poses, MatchQ[Alternatives @@ {inds}[[All, 1]]]]];
ListBoxNoBrace[list_] := If[Length@list === 1, ToBoxes@list[[1]], RowBox@Riffle[ToBoxes /@ list, ","]];
IndexedSum /: MakeBoxes[expr : IndexedSum[expr2_, inds__List], StandardForm] := With[{
    box = RowBox@{UnderscriptBox["\[Sum]", ListBoxNoBrace@{inds}[[All, 1]]], MakeBoxes@expr2}
}, InterpretationBox[box, expr]];
SyntaxInformation@IndexedSum = {"ArgumentsPattern" -> {_, ___}};

CollectIndexedSumTerm[expr_] := CollectIndexedSumTerm[expr, True];
CollectIndexedSumTerm[IndexedSum[expr_, inds__], skipScalars_] := With[{
    ret = CollectIndexedSumTerm@expr
}, If[!skipScalars || Length@Complement[Keys@FindAllIndicesNames@ret[[1]], Join[ret[[2, All, 1]], {inds}[[All, 1]]]] > 0,
    With[{
        newInds = {GetUniqueIndexOfSlotType@#2, #2} & @@@ {inds}
    }, {
        ReplaceIndexNames[ret[[1]], Thread[{inds}[[All, 1]] -> newInds[[All, 1]]]],
        Join[ret[[2]], newInds]
    }]
,
    {IndexedSum[ret[[1]], ##] & @@ Join[ret[[2]], {inds}], {}}
]];
CollectIndexedSumTerm[expr_IndexedSum ^ power_Integer, skipScalars_] := With[{
    ret1 = CollectIndexedSumTerm@expr
}, If[Length@ret1[[2]] > 0,
    With[{
        rest = Prepend[Table[CollectIndexedSumTerm[expr], power - 1], ret1]
    }, {
        Times @@ rest[[All, 1]],
        Join @@ rest[[All, 2]]
    }],
    MapAt[# ^ power &, ret1, 1]
]];
CollectIndexedSumTerm[prod_?ProductQ[args__], _] := With[{
    ret = CollectIndexedSumTerm /@ {args}
}, {
    prod @@ ret[[All, 1]],
    Join @@ ret[[All, 2]]
}];
CollectIndexedSumTerm[expr_, _] := {expr, {}};

NormalizeIndexedSumExprHelper[terms_, {ind_, type_}] := With[{
    sel = Select[MapIndexed[Join, terms], MemberQ[#[[2]], ind] &]
}, Join[{{IndexedSum[Times @@ sel[[All, 1]], {ind, type}], DeleteCases[Union @@ sel[[All, 2]], ind]}}, Delete[terms, Transpose@{sel[[All, 3]]}]]];
NormalizeIndexedSumExpr[terms_, inds_] := Fold[NormalizeIndexedSumExprHelper, {#, Keys@FindAllIndicesNames@#} & /@ terms, inds];

NormalizeOneIndexedSum[IndexedSum[expr_Times, inds__]] := Times @@ NormalizeIndexedSumExpr[List @@ expr, {inds}][[All, 1]];
SyntaxInformation@NormalizeOneIndexedSum = {"ArgumentsPattern" -> {_}};

MoveIndexedSumToOutermost[expr_] := expr //. prod_?ProductQ[l___, IndexedSum[expr2_, inds__], r___] :> With[{
    newInds = {GetUniqueIndexOfSlotType@#2, #2} & @@@ {inds}
}, IndexedSum[
    prod[l, ReplaceIndexNames[expr2, Thread[{inds}[[All, 1]] -> newInds[[All, 1]]]], r],
    ##
] & @@ newInds];
SyntaxInformation@MoveIndexedSumToOutermost = {"ArgumentsPattern" -> {_}};

MoveIndexedSumToInnermost[expr_] := expr //. {
    IndexedSum[expr2_Times, inds__] :> With[{
        ret = TrySplitIndexScopeFactors[List @@ expr2, "Not"@{inds}[[All, 1]]],
        indNameToSlot = First /@ GroupBy[{inds}, First -> Extract[2]]
    }, ret[[1]] * Times @@ ((IndexedSum @@ Join[{#1}, {#, indNameToSlot@#} & /@ #2]) & @@@ ret[[2]]) /; ret =!= $Failed]
};
SyntaxInformation@MoveIndexedSumToInnermost = {"ArgumentsPattern" -> {_}};

ScalarIndexedSumQ[expr_IndexedSum] := Length@FindIndicesSlots@expr === 0;
SyntaxInformation@ScalarIndexedSumQ = {"ArgumentsPattern" -> {_}};

IndexedSumTimes[{}, factors___] := Times[factors];
IndexedSumTimes[inds_, r1___, HoldPattern@Times[args__], r2___] := IndexedSumTimes[inds, r1, args, r2];
SyntaxInformation@IndexedSumTimes = {"ArgumentsPattern" -> {_, ___}};

PopulateIndexedSumTimes[expr_] := expr //. {
    IndexedSum[expr2_, inds2__] :> IndexedSumTimes[{inds2}, expr2]
} //. IndexedSumTimes[{inds__}, factors___] :> IndexedSum[Times[factors], inds];
SyntaxInformation@PopulateIndexedSumTimes = {"ArgumentsPattern" -> {_}};

ReplaceIndexedSumIndsToUnique[expr_] := ReplaceIndexedSumIndsToUnique[expr, Identity];
ReplaceIndexedSumIndsToUnique[IndexedSum[expr_, inds__], fn_] := With[{
    newInds = {GetUniqueIndexOfSlotType@#2, #2} & @@@ {inds}
}, With[{
    expr2 = fn@ReplaceIndexNames[expr, Thread[{inds}[[All, 1]] -> newInds[[All, 1]]]]
}, IndexedSum[expr2, ##] & @@ newInds]];
SyntaxInformation@ReplaceIndexedSumIndsToUnique = {"ArgumentsPattern" -> {_, _.}};

ReplaceAllIndexedSumIndsToUnique[expr_] := expr /. expr2_IndexedSum :> ReplaceIndexedSumIndsToUnique[expr2, ReplaceAllIndexedSumIndsToUnique];
SyntaxInformation@ReplaceAllIndexedSumIndsToUnique = {"ArgumentsPattern" -> {_}};

SignOfUpSlot[_DI] = -1;
SignOfUpSlot[_] = 1;
SyntaxInformation@SignOfUpSlot = {"ArgumentsPattern" -> {_}};

ReplaceDummiesToUnique[expr_, frees_List] := With[{
    allInds = Values@KeyDrop[FindAllIndicesNames@expr, frees]
}, ReplacePart[expr, Join[
    Join @@ (With[{newName = GetUniqueIndexOfSlotType[#[[1, 2]]]}, #3 -> (#1 /. IndexNameSlot -> newName) & @@@ #] & /@ allInds)
]]];
ReplaceDummiesToUnique[expr_] := ReplaceDummiesToUnique[expr, {}];
SyntaxInformation@ReplaceDummiesToUnique = {"ArgumentsPattern" -> {_, _.}};

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
ETensor[expr_, {}] := IndexScope@expr;
ETensor[expr_, frees_][inds__] := ToIndexedExprFunction[expr, frees][inds];

ReplaceIndicesRules[indPos_, reps_] := Join @@ (With[{
    newInd = #2,
    entry = Lookup[indPos, #1, Null]
}, If[entry =!= Null, #2 -> (#1 /. IndexNameSlot -> newInd) & @@@ entry, {}]] & @@@ reps);
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
    DeleteCases[flat, Null],
    Position[flat, Null, {1}][[All, 1]],
    lens
]];
DeflattenFrees[list_, lens_] := FoldPairList[
    {If[#2 == 1, #1[[1]], Take[#1, #2]], Drop[#1, #2]} &,
    list,
    lens
];
ReconstructETensorIndices[ETensorIndices[inds_, nonePos_, lens_]] := DeflattenFrees[Fold[Insert[#1, Null, #2] &, inds, nonePos], lens];
CompatibleETensorIndicesQ[ETensorIndices[inds1_, nonePos1_, lens1_], ETensorIndices[inds2_, nonePos2_, lens2_]] := Length@inds1 === Length@inds2 && nonePos1 === nonePos2 && lens1 === lens2;

ETensorPlusHelper1DeleteTypeAndPrepend1ToPos[{pat_, type_, pos_}] := {pat, Prepend[pos, 1]};
GetIndicesNamesToType[indPoses_Association] := Extract[{1, 2}] /@ indPoses;
ETensor /: FindIndicesSlots[_ETensor] = {};
ETensor /: PreITensorReduce[ETensor[expr_, frees_], opt : OptionsPattern[]] := With[{
    freesObj = StructureOfETensorIndices@frees
}, With[{
    newExprAndFrees = ReplaceFrees[ReplaceAllIndexedSumIndsToUnique@expr, freesObj[[1]], Automatic]
}, ETensor[
    ITensorReduce[newExprAndFrees[[1]], FreeIndexNames -> newExprAndFrees[[2]], FilterRules[{opt}, DeleteCases[Options@ITensorReduce, FreeIndexNames -> _]]],
    ReconstructETensorIndices[ReplacePart[freesObj, 1 -> newExprAndFrees[[2]]]]
]]];
ETensor::plusincompat = "Cannot combine ETensor's with incompatible free indices `1` and `2`.";

NoneSlot;
CheckUnequalFreesLength[frees_] := With[{
    len = Length@frees[[1]]
},
    If[Length@# =!= len, Message[ETensor::plusincompat, frees[[1]], #]; Throw@Err;] & /@ Delete[frees, 1];
];
CheckExistsLists[list_] := If[Head@list[[1]] === List,
    If[Head@# =!= List, Message[ETensor::plusincompat, list[[1]], #]; Throw@Err;] & /@ Delete[list, 1];
    True
,
    If[Head@# === List, Message[ETensor::plusincompat, list[[1]], #]; Throw@Err;] & /@ Delete[list, 1];
    False
];
CombineETensorFreesHelper[{allInds_, curInds_}, slots_] := With[{
    nonNull = DeleteCases[slots, NoneSlot]
}, If[Length@nonNull > 0,
    If[CheckExistsLists@nonNull,
        MapAt[Append[curInds, #] &, CombineETensorFrees0[nonNull, allInds], 2]
    ,
        With[{
            newInd = GetIndexOfSlotType[nonNull[[1]], allInds]
        }, {Append[allInds, newInd], Append[curInds, newInd]}]
    ]
,
    {allInds, Append[curInds, Null]}
]];
CombineETensorFrees0[inds_, allInds_] := Fold[CombineETensorFreesHelper, {allInds, {}}, CheckUnequalFreesLength[inds]; Transpose@inds];
CombineETensorFrees[inds_] := CombineETensorFrees0[inds, {}][[2]];

LookupOneFreeTypeOrNone[frees_List, poses_] := LookupOneFreeTypeOrNone[#, poses] & /@ frees;
LookupOneFreeTypeOrNone[Null, _] := NoneSlot;
LookupOneFreeTypeOrNone[free_, poses_] := poses[free][[1, 2]];
MakeETensorReplaceFreesInner[{reps1_, reps2_, others_}, {Null, inds_}] := {reps1, reps2, Append[others, inds]};
MakeETensorReplaceFreesInner[{reps1_, reps2_, others_}, {inds1_List, inds2_List}] := {Join[reps1, inds1], Join[reps2, inds2], others};
MakeETensorReplaceFreesInner[{reps1_, reps2_, others_}, {ind1_, ind2_}] := {Append[reps1, ind1], Append[reps2, ind2], others};
MakeETensorReplaceFrees[inds1_, inds2_] := With[{
    ret = Fold[MakeETensorReplaceFreesInner, {{}, {}, {}}, Thread@{inds1, inds2}]
}, {ret[[1]], Join @@ Delete[ret, 1]}];
ReplaceETensorFrees[ETensor[expr_, inds_], indPos_, frees_] := ReplacePart[
    expr,
    With[{reps = MakeETensorReplaceFrees[inds, frees]}, ReplaceFreesRules[indPos, reps[[1]], reps[[2]]][[1]]]
];

ScalarETensorQ[ETensor[_, inds_]] := AllTrue[inds, # === Null &];
UncatchedETensorPlus[head_, exprs_] := If[AllTrue[exprs, ScalarETensorQ],
    CheckUnequalFreesLength@exprs[[All, 2]];
    ETensor[head @@ exprs[[All, 1]], exprs[[1, 2]]]
,
    UncatchedETensorPlusNonNullInds[head, exprs]
];
UncatchedETensorPlusNonNullInds[head_, exprs_] := With[{
    indPoses = FindAllIndicesNames /@ exprs[[All, 1]]
}, With[{
    combinedFrees = CombineETensorFrees[MapThread[LookupOneFreeTypeOrNone, {exprs[[All, 2]], indPoses}]]
}, ETensor[
    head @@ MapThread[
        ReplaceETensorFrees[#1, #2, combinedFrees] &,
        {exprs, indPoses}
    ],
    combinedFrees
]]];
PlusQ[a_] := a === Plus;
ETensor /: _?PlusQ[e1__ETensor] := With[{ret = Catch@UncatchedETensorPlus[Plus, ReplaceAllIndexedSumIndsToUnique /@ {e1}]}, ret /; ret =!= Err];
ETensor /: prod_?ProductQ[l___, ETensor[expr_, arg__], r___] := ETensor[prod[l, expr, r], arg];
ETensor /: (head_ /; head === TensorProduct)[l___, e1_ETensor, e2_ETensor, r___] := TensorProduct[l, ITensorOuter[Times, e1, e2, {}], r];
ETensor /: D[ETensor[expr_, frees_], args__] := ETensor[D[expr, args], frees];

SyntaxInformation@ETensor = {"ArgumentsPattern" -> {_, _}};

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
        prod[expr1, ReplacePart[expr2, ReplaceIndicesRules[Map[Delete[2]] /@ indPos2, newIndsReps]]],
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

ITensorTranspose[s_?ArrayQ, perm_] := With[{
    newArr = Transpose[s, perm]
}, Map[If[TensorValueQ@#, ITensorTranspose[#, perm], #] &, newArr, {ArrayDepth@newArr}]];
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
ITensorTranspose[expr_, perm_] := expr /; perm === Range[Length@perm];
ITensorTranspose[e1_ETensor, perm_] := With[{ret = UncatchedETensorTranspose[e1, perm]}, ret /; ret =!= Err];
ITensorTranspose[Plus[l___, ITensorTranspose[t2_, perm2_], r___], perm_] := ITensorTranspose[Plus[l, r], perm] + ITensorTranspose[t2, GeneralizedPermutationProduct[perm2, perm]];
ITensorTranspose[ITensorTranspose[t_, perm1_], perm2_] := ITensorTranspose[t, GeneralizedPermutationProduct[perm1, perm2]];
SyntaxInformation@ITensorTranspose = {"ArgumentsPattern" -> {_, _}};

ITensorOuter[prod_, s1_, s2_] := ITensorOuter[prod, s1, s2, {}];
PermutationOfMovingToFirst[points_List] := If[Length@points === 0, {}, InversePermutation[Join[points, Delete[Range[Max @@ points], Transpose@{points}]]]];
PermutationOfMovingToLast[points_List] := InversePermutation[Join[Delete[Range[Max @@ points], Transpose@{points}], points]];
FillNoneSlots[arr_, l_] := FoldPairList[If[#2 === Null, {#1, #1 + 1}, {#2, #1}] &, l, arr];
ITensorOuter::nonzeronontensor = "Multiplication of non-zero non-tensor value `1` and `2` encountered.";
ITensorOuterInner[prod_, s1_, s2_, cont_] := Switch[{TensorValueQ@s1, TensorValueQ@s2},
    {True, True},
    ITensorOuter[prod, s1, s2, cont],

    {False, False},
    prod[s1, s2],

    _,
    If[s1 === 0 || s2 === 0, 0, Message[ITensorOuter::nonzeronontensor, s1, s2]; Throw@Err;]
];
OuterOrMap[0, 0] := {fn, list1, list2} |-> fn[list1, list2];
OuterOrMap[0, n_] := {fn, list1, list2} |-> Map[fn[list1, #] &, list2, {n}];
OuterOrMap[n_, 0] := {fn, list1, list2} |-> Map[fn[#, list2] &, list1, {n}];
OuterOrMap[n1_, n2_] := {fn, list1, list2} |-> Outer[fn, list1, list2, n1, n2];

ITensorOuter[prod_, s1_?ArrayQ, s2_?ArrayQ, cont_] := With[{
    ret = Catch@With[{
        perm1 = PermutationOfMovingToFirst[cont[[All, 1]]],
        perm2 = PermutationOfMovingToFirst[cont[[All, 2]]]
    }, With[{
        s22 = Transpose[s2, perm2]
    },
        Transpose[
            MapIndexed[
                With[{
                    map = OuterOrMap[
                        ArrayDepth@s1 - Length@cont,
                        ArrayDepth@s2 - Length@cont
                    ]
                }, map[
                    ITensorOuterInner[prod, #1, #2, cont] &,
                    #1,
                    Extract[s22, {#2}][[1]]
                ] &],
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
        FillNoneSlots[ReplacePart[ConstantArray[Null, l2], #2 -> #1 & @@@ cont], l1 + 1]
    ]]
];
ITensorOuter[_, 0, _, _] = 0;
ITensorOuter[_, _, 0, _] = 0;
SyntaxInformation@ITensorOuter = {"ArgumentsPattern" -> {_, _, _, _}};

SumIndexOfSlot[slot_, ind_, expr_] := expr;
SyntaxInformation@SumIndexOfSlot = {"ArgumentsPattern" -> {_, _, _}};

ITensorSum[expr_, {}] := expr;
ITensorSum[s_?ArrayQ, dims_] := Total@Flatten[
    Transpose[Map[If[TensorValueQ@#, ITensorSum[#, dims], #] &, s, {ArrayDepth@s}], PermutationOfMovingToFirst@dims],
    Length@dims - 1
];
(* ITensorSum[ETensor[expr_, frees_], dims_] := ETensor[expr, Delete[frees, Transpose@{dims}]]; *)
ITensorSum[ETensor[expr_, frees_], dims_] := With[{
    allInds = Extract[{1, 2}] /@ FindAllIndicesNames@expr
}, ETensor[
    Fold[SumIndexOfSlot[#2[[1]], #2[[2]], #1] &, expr, If[KeyExistsQ[allInds, #], {allInds@#, #}, Nothing] & /@ StructureOfETensorIndices[Extract[frees, Transpose@{dims}]][[1]]],
    Delete[frees, Transpose@{dims}]
]];
ITensorSum[0, _] = 0;
SyntaxInformation@ITensorSum = {"ArgumentsPattern" -> {_, _}};

ITensorScalarMultiply[prod_, tensor_?ArrayQ, scalar_] := Map[If[TensorValueQ@#, ITensorScalarMultiply[prod, #, scalar], prod[#, scalar]] &, tensor, {ArrayDepth@tensor}];
ITensorScalarMultiply[prod_, ETensor[expr_, inds_], scalar_] := ETensor[prod[expr, scalar], inds];
SyntaxInformation@ITensorScalarMultiply = {"ArgumentsPattern" -> {_, _, _}};

PermutationOfMoveTo[n_, n_] := {1};
PermutationOfMoveTo[from_Integer, to_Integer] := If[
    from < to,
    Join[Range[from - 1], {to}, Range[from, to - 1]],
    Join[Range[to - 1], Range[to + 1, from], {to}]
];
ITensorFixedContract[prod_, t1_, t2_, n1_, n2_] := ITensorTranspose[
    ITensorSum[ITensorOuter[prod, t1, t2, {{n1, n2}}], {n1}],
    PermutationOfMoveTo[1, n2]
];
SyntaxInformation@ITensorFixedContract = {"ArgumentsPattern" -> {_, _, _, _, _}};

OneIndexOfNITensor[ind_ -> _] := ind;
OneIndexOfNITensor[ind_] := ind;
OneIndexPosOfNITensor[ind_, pos_] := pos -> Null;
OneIndexPosOfNITensor[ind_ -> type_, pos_] := Append[pos, 1] -> type;
NITensor /: FindIndicesSlots[NITensor[_, inds_]] := MapIndexed[OneIndexPosOfNITensor[#1, Prepend[#2, 2]] &, inds];
NITensor /: ExpressionPassThroughQ[_NITensor, _, {2, __}] = True;
NITensor /: prod_?ProductQ[x_, NITensor[t_, inds_]] := NITensor[prod[x, t], inds] /; Head@x =!= NITensor;
NITensor /: Power[NITensor[t_, inds_], x_] := NITensor[t ^ x, inds];
SyntaxInformation@NITensor = {"ArgumentsPattern" -> {_, _}};

TakeFrees[inds_] := Cases[KeyValueMap[List, Counts@inds], {a_, 1} :> a];
FreesByArgs[frees_, freeArgs_] := Array[Union @@ Append[Delete[freeArgs, #], frees] &, Length@freeArgs];
GetINTensorIndexNames[NITensor[_, inds_]] := OneIndexOfNITensor /@ inds;
GetINTensorIndexNames[_] = {};
NITensorReduce::incompatinds = "Cannot combine NITensors `1` and `2` with different indices.";
NITensorReduce[expr_] := NITensorReduce[expr, Automatic];
NITensorReduce[prod_?ProductQ[args__], frees_] := ReduceNITensorContractions[prod, {args}, frees];
NITensorReduce[t_NITensor, frees_] := ContractOneNITensor[t, frees];
NITensorReduce[expr_Plus, frees_] := With[{
    ret = Catch@UncatchedCombineSummedNITensor[NITensorReduce[#, frees] & /@ expr]
}, ret /; ret =!= Err];
NITensorReduce[HoldPattern@IndexScope@expr_, frees_] := IndexScope@NITensorReduce[expr, {}];
NITensorReduce::unknown = "No rule to transform indexed expression `1`.";
NITensorReduce[expr_, _] := With[{
    inds = FindAllIndicesNames@expr
}, If[Length@inds === 0,
    NITensor[expr, {}]
,
    Message[NITensorReduce::unknown, HoldForm@expr];
    NITensor[Hold@expr, Keys@inds]
]];
SyntaxInformation@NITensorReduce = {"ArgumentsPattern" -> {_, _.}};

PermutationOfMergeIndices[inds_, n_] := With[{
    firstIndToIndGroup = Association @@ (Min @@ # -> # & /@ inds)
}, Fold[
    With[{
        arr = #1[[1]],
        c = #1[[2]]
    }, If[arr[[#2]] === Null,
        With[{
            g = Lookup[firstIndToIndGroup, #2, Null]
        }, {
            If[g =!= Null,
                ReplacePart[arr, Thread[g -> c]],
                ReplacePart[arr, #2 -> c]
            ],
            c + 1
        }]
    ,
        {arr, c}
    ]] &,
    {ConstantArray[Null, n], 1},
    Range@n
]][[1]];
ContractOneNITensor[NITensor[t1_, inds_], frees0_] := With[{
    indsByName = Sort /@ GroupBy[MapIndexed[{SeparateIndexName[OneIndexOfNITensor@#1][[1]], #2[[1]]} &, inds], First -> Extract[2]]
}, With[{
    frees = If[frees0 === Automatic, Cases[KeyValueMap[List, indsByName], {ind_, {_}} :> ind], frees0]
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
]]]]];

NITensorSlot;

ContractTwoNITensors[prod_, {}, {}, frees_] := NITensor[prod[NITensorSlot[1], NITensorSlot[2]], {}];
ContractTwoNITensors[prod_, inds1_, {}, _] := NITensor[ITensorScalarMultiply[prod, NITensorSlot[1], NITensorSlot[2]], inds1];
ContractTwoNITensors[prod_, {}, inds2_, _] := NITensor[ITensorScalarMultiply[ReverseApplied@prod, NITensorSlot[2], NITensorSlot[1]], inds2];
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
ReduceNITensorContractionsStep[prod_, factors_, frees0_] := With[{
    freesByArg = SeparateIndexName[OneIndexOfNITensor@#][[1]] & /@ factors[[All, 2]]
}, With[{
    frees = If[frees0 === Automatic, TakeFrees[Join @@ freesByArg], frees0]
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
]]];

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
    indsByArgs = Keys@FindAllIndicesNames@# & /@ factors
}, With[{
    factors2 = AbsorbNonNITensor[
        prod, WrapScalarInNITensor /@ MapThread[
            NITensorReduce,
            {factors, If[frees === Automatic, ConstantArray[Automatic, Length@factors], FreesByArgs[frees, indsByArgs]]}
        ]
    ]
},
    Nest[ReduceNITensorContractionsStep[prod, #, frees] &, factors2, Length@factors2 - 1][[1]]
]];
UncatchedCombineSummedNITensor[head_[NITensor[first_, firstInds_], rest__]] := With[{
    inds = OneIndexOfNITensor /@ firstInds
}, NITensor[head @@ Prepend[
    With[{
        inds2 = OneIndexOfNITensor /@ #[[2]]
    }, With[{
        perm = FirstPosition[inds, #, Null, {1}][[1]] & /@ inds2
    },
        If[Length@inds2 =!= Length@inds || Length@Cases[perm, Null] > 0, Message[NITensorReduce::incompatinds, firstInds, #]; Throw@Err;];
        ITensorTranspose[#[[1]], perm]
    ]] & /@ {rest},
    first
], firstInds]];

ExtractNITensor::incompat = "Incompatible indices `1` and `2`.";
ExtractNITensor[NITensor[0, _], _] = 0;
ExtractNITensor[NITensor[expr_, inds1_], inds2_] := With[{ret = Catch@With[{
    perm = FirstPosition[inds2, OneIndexOfNITensor@#, Null, {1}][[1]] & /@ inds1
},
    If[Length@inds1 =!= Length@inds2 || AnyTrue[perm, # === Null &], Message[ExtractNITensor::incompat, inds1, inds2]; Throw@Err;];
    ITensorTranspose[expr, perm]
]}, ret /; ret =!= Err];
ExtractNITensor[inds_][expr_] := ExtractNITensor[expr, inds];
SyntaxInformation@ExtractNITensor = {"ArgumentsPattern" -> {_, _.}};

ToNITensorIndex[a_List, slots_List] := MapThread[ToNITensorIndex, {a, slots}];
ToNITensorIndex[a_, slot_] := With[{ind = SeparateIndexName@a}, ind[[1]] -> (ind[[2]] /. IndexNameSlot -> slot)];
SyntaxInformation@ToNITensorIndex = {"ArgumentsPattern" -> {_, _}};

FromNITensorIndex[a_List] := FromNITensorIndex /@ a;
FromNITensorIndex[name_ -> slot_] := SeparateIndexName[slot][[2]] /. IndexNameSlot -> name;
SyntaxInformation@FromNITensorIndex = {"ArgumentsPattern" -> {_}};

GetIndexForETensor[type_, inds_] := With[{ind = GetIndexOfSlotType[type, inds]}, {ind, ind}];
GetIndexForETensor[t_LabelI, _] := {t, Null};
GetIndicesForETensor[indSpecs_, inds_] := Transpose@Fold[Append[#1, GetIndexForETensor[#2, DeleteCases[Join[inds, #1[[All, 1]]], _LabelI]]] &, {}, indSpecs];
ETensorArray[fn_, inds_] := Outer[With[{inds2 = GetIndicesForETensor[{##}, {}]}, ETensor[fn @@ inds2[[1]], inds2[[2]]]] &, ##] & @@ inds;
SyntaxInformation@ETensorArray = {"ArgumentsPattern" -> {_, _}};

End[];

Protect @@ Select[Names["`*"], !StringMatchQ[#, "$" ~~ __] &];
Protect[$IndexPairPattern];

EndPackage[];
