BeginPackage["Peanotica`Core`", {"Peanotica`Internal`", "Peanotica`xPerm`"}];

Scan[Unprotect@#; ClearAll@#; &, Names@{"Peanotica`Core`*"}];

ISum::usage = "ISum[expr, {i1, ...}, {i2, ...}, ...] represents a summation expression with dummy indices i1, i2...";
IProd::usage = "";
DummyIndexScope::usage = "";
DummyIndexScopeQ::usage = "";
SymmetryGroupOfExpression::usage = "";
IndexSlot::usage = "";
ToxPermSGS::usage = "";
CommutivityQ::usage = "";
TensorProductHeadQ::usage = "";
PhaseOfOrdering::usage = "";
ExpandableWithDummyScopeQ::usage = "";
CommutesWithDummyScopeQ::usage = "";
OrderlessDummyScopeQ::usage = "";
DefDummyIndexScope::usage = "";
FlattenedISum::usage = "";
FreeIndexScope::usage = "";
DummyInfos::usage = "";
IndexBarrier::usage = "";
NRange::usage = "VRange[min, max] represents the range of a dummy index.";

LabelI::usage = "LabelI[...] represents a label (generic) index.";
DI::usage = "DI[...] represents a down index.";
FindDummyIndices::usage = "";
AllowedIndexPositions::usage = "";
IndexOfCompoundIndex::usage = "";
IndexPosition::usage = "";
IndicesBySlots::usage = "";
ReplaceDummies::usage = "";
MapIndex::usage = "";
ReplaceIndex::usage = "";
JoinIndexScopes::usage = "";
SameDummies::usage = "";
NonIndexExpressionQ::usage = "";

DollarQ::usage = "";
NoDollar::usage = "";
UniqueTempSymbol::usage = "";
UniqueIndexSpec::usage = "";

DeltaTensor::usage = "";

(* canonicalization *)
SortDummyScopes::usage = "";
CombineDummyScopes::usage = "";
MinimalWrapDummyScopeOne::usage = "";
ISort::usage = "";
SymmetryOfSortedObject::usage = "";
ISortedObject::usage = "";
ISortedIndex::usage = "";
ISortedOther::usage = "";
SCycles::usage = "";
FindDummies::usage = "";
SameDummies::usage = "";
IndexReduce::usage = "";
IndexExpand::usage = "";
DistributivePairQ::usage = "";

Begin["`Private`"];

IndexName[{a_}] := a;
IndexName[{a_, _}] := a;
IndexName[a_] := a;
DummyIndexInfo[{_, info_}] := info;
DummyIndexInfo[_] = None;
MapIndexName[fn_, {a_, info_}] := {fn@a, info};
MapIndexName[fn_, a_] := fn@a;
SetIndexSpecName[{_, info_}, n_] := {n, info};
SetIndexSpecName[_, n_] := n;

DI[DI[a_]] := a;
SyntaxInformation@DI = {"ArgumentsPattern" -> {__}};

DummyInfos[inds__] := Association @@ (IndexName@# -> DummyIndexInfo@# & /@ {inds});
SyntaxInformation@DummyInfos = {"ArgumentsPattern" -> {__}};

NonIndexExpressionQ[_ISum | _IProd] = False;
NonIndexExpressionQ[_] = True;
SyntaxInformation@NonIndexExpressionQ = {"ArgumentsPattern" -> {_}};

DummyIndexScope@Plus = ISum;
DummyIndexScope@Times = IProd;
DummyIndexScope[_][expr_] := expr;
DummyIndexScope[a_][expr_, inds__] := With[{sortedInds = SortBy[{inds}, IndexName]}, DummyIndexScope[a] @@ Prepend[sortedInds, expr] /; sortedInds =!= {inds}];
DummyIndexScope[a_][DummyIndexScope[a_][expr_, inds1__], inds2__] := DummyIndexScope[a][expr, inds1, inds2];
SyntaxInformation@DummyIndexScope = {"ArgumentsPattern" -> {_}};

DummyIndexScopeQ@_DummyIndexScope = True;
DummyIndexScopeQ@_ = False;
SyntaxInformation@DummyIndexScopeQ = {"ArgumentsPattern" -> {_}};

SymmetryGroupOfExpression@_ = {};
SetAttributes[SymmetryGroupOfExpression, HoldAll];
SyntaxInformation@SymmetryGroupOfExpression = {"ArgumentsPattern" -> {_}};

MaxSCycleNum[SCycles[a__]] := Max @@ Join[a];
MaxSCycleNum[-SCycles[a__]] := Max @@ Join[a];
ToxPermSGS[gs_] := StrongGenSet[Max @@ (MaxSCycleNum /@ gs), GenSet @@ (gs /. SCycles -> Peanotica`xPerm`Cycles)];
SyntaxInformation@ToxPermSGS = {"ArgumentsPattern" -> {_}};

CommutivityQ[Times | Plus | Wedge] = True;
CommutivityQ[_] = False;
SyntaxInformation@CommutivityQ = {"ArgumentsPattern" -> {_}};

TensorProductHeadQ[Plus] = False;
TensorProductHeadQ[_] = True;
SetAttributes[TensorProductHeadQ, HoldAll];
SyntaxInformation@TensorProductHeadQ = {"ArgumentsPattern" -> {_}};

PhaseOfOrdering[expr_, ordering_] = 1;
SetAttributes[PhaseOfOrdering, HoldFirst];
SyntaxInformation@PhaseOfOrdering = {"ArgumentsPattern" -> {_, _}};

CommutesWithDummyScopeQ[ISum, Times | NonCommutativeMultiply | Wedge, _] = True;
CommutesWithDummyScopeQ[IProd, Power, 1] = True;
SyntaxInformation@CommutesWithDummyScopeQ = {"ArgumentsPattern" -> {_, _, _}};

ExpandableWithDummyScopeQ[ISum, Plus] = True;
ExpandableWithDummyScopeQ[IProd, Times] = True;
SyntaxInformation@ExpandableWithDummyScopeQ = {"ArgumentsPattern" -> {_, _}};

SyntaxInformation@OrderlessDummyScopeQ = {"ArgumentsPattern" -> {_}};

DefDummyIndexScope[sym_Symbol, optr_] := (
    DummyIndexScopeQ@sym ^= True;
    sym[expr_] := expr;
    sym[0, __] = 0;
    sym[expr_, inds__] := With[{sortedInds = SortBy[{inds}, IndexName]}, sym @@ Prepend[sortedInds, expr] /; sortedInds =!= {inds}];
    sym[sym[expr_, inds1__], inds2__] := sym[expr, inds1, inds2];
    SyntaxInformation@sym = {"ArgumentsPattern" -> {__}};
);

DummyIndexScopeQ@FreeIndexScope ^= True;
FreeIndexScope[0, __] = 0;
FreeIndexScope[expr_] := expr;
FreeIndexScope /: expr_ * FreeIndexScope[expr2_, inds__] := FreeIndexScope[expr * expr2, inds];
SyntaxInformation@FreeIndexScope = {"ArgumentsPattern" -> {__}};

DefDummyIndexScope[ISum, Plus];
ISum[0, __] = 0;
ISum[Times[DeltaTensor[a_, b_], rest___], inds__] := With[{
    pos = FirstPosition[IndexName /@ {inds}, a, None]
},
    ISum[MapIndex[Times[rest], a -> b], ##] & @@ Delete[{inds}, pos[[1]]] /; pos =!= None
];
ISum[Times[DeltaTensor[a_, b_], rest___], inds__] := With[{
    pos = FirstPosition[IndexName /@ {inds}, b, None]
},
    ISum[MapIndex[Times[rest], b -> a], ##] & @@ Delete[{inds}, pos[[1]]] /; pos =!= None
];
OrderlessDummyScopeQ@ISum ^= True;

DefDummyIndexScope[IProd, Times];
IProd[a : 0 | 1, __] := a;
OrderlessDummyScopeQ@IProd ^= True;

FlattenedISum[{inds___}, terms___] := ISum[Times[terms], inds];
SyntaxInformation@FlattenedISum = {"ArgumentsPattern" -> {__}};

FindDummyIndices[ISum[expr_, inds__]] := Union[FindDummyIndices[expr], IndexName /@ {inds}];
SetAttributes[FindDummyIndices, HoldFirst];
SyntaxInformation@FindDummyIndices = {"ArgumentsPattern" -> {_}};

AllowedIndexPositions[_IndexBarrier] = {};
AllowedIndexPositions[_[args___]] := Thread@{Range[0, Length@{args}]};
SetAttributes[AllowedIndexPositions, HoldAll];
SyntaxInformation@AllowedIndexPositions = {"ArgumentsPattern" -> {_}};

IndexOfCompoundIndex[DI[a_]] := a;
IndexOfCompoundIndex[_] = None;
SetAttributes[IndexOfCompoundIndex, HoldAll];
SyntaxInformation@IndexOfCompoundIndex = {"ArgumentsPattern" -> {_}};

ApplyHold[fn_, Hold[e1__], e2___] := fn[e1, e2];
ApplyHold[fn_, e___] := fn[e];

IndexPosition[expr_] := IndexPosition[expr, {}];
IndexPosition[expr_, inds_] := IndexPosition[expr, inds, {}];
IndexPosition[_?DummyIndexScopeQ[expr_, inds__], inds2_, pos_] := IndexPosition[expr, Join[inds2, IndexName /@ {inds}], Append[pos, 1]];
IndexPosition[expr: _[___], inds_, pos_] := With[{
    pat = Alternatives @@ inds
}, If[MatchQ[IndexOfCompoundIndex@expr, pat],
    <|expr -> {pos}|>
,
    Merge[ApplyHold[IndexPosition, Extract[Hold@expr, Prepend[#, 1], Hold], inds, Join[pos, #]] & /@ AllowedIndexPositions@expr, Apply@Join]
]];
IndexPosition[l_LabelI, _, pos_] := <|l -> {pos}|>;
IndexPosition[ind_, inds_, pos_] := If[MatchQ[Hold@ind, Hold@Evaluate[Alternatives @@ inds]], <|ind -> {pos}|>, <||>];
SetAttributes[IndexPosition, HoldFirst];
SyntaxInformation@IndexPosition = {"ArgumentsPattern" -> {_, _., _.}};

IndicesBySlots[expr_] := IndicesBySlots[expr, {}];
IndicesBySlots[expr_, inds_] := IndicesBySlots[expr, inds, {}];
IndicesBySlots[_?DummyIndexScopeQ[expr_, inds__], inds2_, pos_] := IndicesBySlots[expr, Join[inds2, IndexName /@ {inds}], Append[pos, 1]];
IndicesBySlots[expr : fn_[a___], inds_, pos_] := If[With[{pat = Alternatives @@ inds}, IndexMatchQ[expr, inds]],
    {{expr, pos}}
, IndicesBySlotsCompoundExpression[fn, {a}, inds, pos]];
IndicesBySlots[ISortedObject[fn_, args_, _], inds_, pos_] := IndicesBySlotsCompoundExpression[fn, args, inds, pos];
IndicesBySlots[ind_, inds_, pos_] := If[IndexMatchQ[ind, inds], {{ind, pos}}, {}];
IndicesBySlots[e_LabelI, _, pos_] := {{e, pos}};
SetAttributes[IndicesBySlots, HoldFirst];
SyntaxInformation@IndicesBySlots = {"ArgumentsPattern" -> {_, _., _.}};

IndexMatchQ[expr_, inds_] := With[{pat = Alternatives @@ inds}, MatchQ[Hold@expr, Hold@pat] || MatchQ[IndexOfCompoundIndex@expr, pat]];
SetAttributes[IndexMatchQ, HoldFirst];

IndicesBySlotsCompoundExpression[fn_, args_, inds_, pos_] := Join[
    IndicesBySlots[fn, inds],
    With[{
        args2 = MapIndexed[IndicesBySlots[#1, inds, Join[pos, #2]] &, args]
    }, If[ISortedTensorProductHeadQ@fn, Join @@ args2, CombineSummedIndexSlots@args2]]
];

BroadcastIndicesSlots[from_, to_] := Fold[With[{
    pos = FirstPosition[#1[[1]], {_, #2}, {1}][[1]]
},
    {Delete[#1[[1]], pos], Append[#1[[2]], #1[[1, pos, 1]]]}
] &, {MapIndexed[{#2[[1]], #1} &, to], {}}, from][[2]];
BroadcastIndicesSlotsRule[from_, to_] := Thread[Range@Length@from -> BroadcastIndicesSlots[from, to]];
CombineSummedIndexSlots[args_] := With[{
    allInds = Sort[Join @@ KeyValueMap[ConstantArray, Merge[Merge[With[{ind = #[[1]]}, <|ind -> 1|>] & /@ #, Total] & /@ args, Apply@Max]]]
}, Fold[Function[{ret, inds},
    Fold[With[{
        last = #1,
        pos = #2[[2]],
        indPos = #2[[1]]
    }, MapAt[Join[#, indPos] &, last, {pos}]]&, ret, Thread@{inds[[All, 2 ;;]], BroadcastIndicesSlots[inds[[All, 1]], allInds]}]
], {#} & /@ allInds, args]];

(* MapIndex *)
MapIndex[expr_, e_Rule] := MapIndex[expr, Identity, {e}];
MapIndex[expr_, e_List] := MapIndex[expr, Identity, e];
MapIndex[expr_, fn_] := MapIndex[expr, fn, {}];
MapIndex[head_?DummyIndexScopeQ[expr_, inds__], fn_, rules_] := With[{
    currentInds = With[{n = IndexName@#}, n -> fn@n] & /@ {inds}
},
    head[MapIndex[expr, fn, Join[rules, currentInds]], ##] & @@ ({inds} /. currentInds)
];
MapIndex[e_IndexBarrier, _, _] := e;
MapIndex[head_[args__], fn_, rules_] := MapIndex[head, fn, rules] @@ (MapIndex[#, fn, rules] & /@ {args});
MapIndex[e_, fn_, rules_] := e /. rules;
SyntaxInformation@MapIndex = {"ArgumentsPattern" -> {_, _, _.}};

ReplaceDummies[expr_] := MapIndex[expr, UniqueTempSymbol];
SyntaxInformation@ReplaceDummies = {"ArgumentsPattern" -> {_}};

ReplaceFrees1[FreeIndexScope[expr_, inds__]] := With[{
    inds2 = UniqueIndexSpec /@ {inds}
},
    {MapIndex[expr, Thread[{inds} -> inds2]], inds2}
];
ReplaceFrees1[e_] := {e, {}};

JoinIndexScopes[fn_, t___] := With[{args = ReplaceFrees1 /@ {t}}, FreeIndexScope[fn @@ args[[All, 1]], ##] & @@ Join @@ args[[All, 2]]];
SyntaxInformation@JoinIndexScopes = {"ArgumentsPattern" -> {__}};

ReplaceIndex[expr_, rules_] := MapIndex[expr, # /. rules &];
SyntaxInformation@ReplaceIndex = {"ArgumentsPattern" -> {_, _}};

DollarQ[s_Symbol] := Length@StringPosition[SymbolName@s, "$"] > 0;
SyntaxInformation@DollarQ = {"ArgumentsPattern" -> {_}};

NoDollar[s_Symbol] := Symbol[Context@s <> First@StringSplit[SymbolName@s, "$"]];
SyntaxInformation@NoDollar = {"ArgumentsPattern" -> {_}};

UniqueTempSymbol[s_Symbol] := With[{s0 = NoDollar@s}, Module @@ {{s0}, s0}]; (* use Module instead of Unique because we want the Temporary attribute *)
SyntaxInformation@UniqueTempSymbol = {"ArgumentsPattern" -> {_}};

UniqueIndexSpec[s_] := MapIndexName[UniqueTempSymbol, s];
SyntaxInformation@UniqueIndexSpec = {"ArgumentsPattern" -> {_}};

sortDummyScopeRule = {
    fn_[l___, expr: head_?DummyIndexScopeQ[__], r___] :> With[{expr2 = List @@ ReplaceDummies@expr}, head[fn[l, expr2[[1]], r], ##] & @@ expr2[[2 ;;]]] /; CommutesWithDummyScopeQ[head, fn, Length@{l} + 1],
    head_?DummyIndexScopeQ[fn_[args__], inds__] :> fn @@ (head[#, inds] & /@ {args}) /; ExpandableWithDummyScopeQ[head, fn]
};

SortDummyScopes[expr_] := expr //. sortDummyScopeRule;
SyntaxInformation@SortDummyScopes = {"ArgumentsPattern" -> {_}};

combineDummyScopeRule = {
    fn_[l1___, head_?DummyIndexScopeQ[expr1_, inds__], head_?DummyIndexScopeQ[expr2_, inds__], l2___] :> fn[l1, head[fn[expr1, expr2], inds], l2] /; ExpandableWithDummyScopeQ[head, fn]
};

CombinableIndicesQ[inds1_, inds2_] := Sort[DummyIndexInfo /@ inds1] === Sort[DummyIndexInfo /@ inds];
CombineDummyScopes[expr_, heads_List] := With[{
    pat = Alternatives @@ heads
},
    expr //. fn_[l1___, (head : pat)[expr1_, inds1__], (head : pat)[expr2_, inds2__], l2___] :> fn[
        l1,
        head[fn[expr1, MapIndex[expr2, Thread[(IndexName /@ {inds2}) -> (IndexName /@ {inds1})]]], inds1],
        l2
    ] /; ExpandableWithDummyScopeQ[head, fn] && CombinableIndicesQ[{inds1}, {inds2}]
];
CombineDummyScopes[expr_, All] := expr //. fn[l1___, head_?DummyIndexScopeQ[expr1_, inds1__], head_?DummyIndexScopeQ[expr2_, inds2__], l2___] :> fn[
    l1,
    head[fn[expr1, MapIndex[expr2, Thread[(IndexName /@ {inds2}) -> (IndexName /@ {inds1})]]], inds1],
    l2
] /; ExpandableWithDummyScopeQ[head, fn] && CombinableIndicesQ[{inds1}, {inds2}];
CombineDummyScopes[expr_] := CombineDummyScopes[expr, All];
SyntaxInformation@CombineDummyScopes = {"ArgumentsPattern" -> {_, _}};

dsReachablePosition[head_, expr_, pos_] := With[{
    len = Length@pos
},
    pos[[ ;; NestWhile[# + 1 &, 0, # < len && With[{head2 = Head@First@Extract[expr, {pos[[;; #]]}]}, head2 === head || CommutesWithDummyScopeQ[head, head2, pos[[# + 1]]]] &] ]]
];
ConstantArrayQ[{}] = True;
ConstantArrayQ[arr_] := With[{a = First@arr}, AllTrue[arr[[2 ;;]], # === a &]];
identicalPrefix[] = {};
identicalPrefix[lists__] := {lists}[[1, ;; With[{len = Min @@ Length /@ {lists}}, NestWhile[# + 1 &, 0, # < len && ConstantArrayQ@{lists}[[All, # + 1]] &]] ]];
MinimalWrapDummyScopeOne[expr: _?DummyIndexScopeQ[e_, inds__]] := MinimalWrapDummyScopeOne[expr, IndexPosition[e, IndexName /@ {inds}]];
MinimalWrapDummyScopeOne[head_[expr_, inds__], pos_] := With[{
    (* firstPos = Replace[IndexName /@ {inds}, identicalPrefix @@@ Map[dsReachablePosition[head, expr, #] &, pos, {2}], {1}] *)
    firstPos = Map[dsReachablePosition[head, expr, #] &, pos, {2}]
},
    firstPos
];

AssertEvaluation::noeval = "Expression `1` is not evaluating.";
AssertEvaluation[expr_] := With[{expr2 = expr}, If[Hold@expr === Hold@expr2, Message[AssertEvaluation::noeval, HoldForm@expr]]; expr2];
SetAttributes[AssertEvaluation, HoldAll];

ISort[expr_] := ISortAllScopes[Evaluate@SortDummyScopes@expr, {}];
SetAttributes[ISort, HoldFirst];
SyntaxInformation@ISort = {"ArgumentsPattern" -> {_}};
ISortAllScopes[fn_?DummyIndexScopeQ[expr_, inds__], inds2_] := fn[ISortAllScopes[expr, Join[inds2, IndexName /@ {inds}]], inds];
ISortAllScopes[IndexBarrier[expr_]] := IndexBarrier@ISortAllScopes[expr, {}];
ISortAllScopes[fn_[], inds_] := ISortAllScopes[fn, inds][];
ISortAllScopes[fn_[a_], inds_] := ISortAllScopes[fn, inds]@ISortAllScopes[a, inds];
ISortAllScopes[-a_, inds_] := -ISortAllScopes[a, inds];
ISortAllScopes[fn_[args__], inds_] := If[CommutivityQ@fn,
    ISortedObject[
        ISortAllScopes[fn, inds],
        ##
    ] & @@ SortArgList[fn, ISortAllScopes[#, inds] & /@ {args}, inds]
,
    ISortAllScopes[fn, inds] @@ (ISortAllScopes[#, inds] & /@ {args})
];
ISortAllScopes[expr_, _] := expr;

ToSortableIObject[inds_][expr_] := With[{
    indsPos = IndexPosition[expr, inds]
}, {
    expr,
    OrderOfGroup@ToxPermSGS@SymmetryGroupOfExpression@expr,
    indsPos,
    RemoveIObjectIndices[expr, indsPos]
}];

SortArgList[head_, args_, inds_] := If[CommutivityQ@head, With[{
    sortingArgs = ToSortableIObject[inds] /@ args
}, With[{
    ordering = InversePermutation@Ordering[sortingArgs, Length@args, IObjectOrder]
}, {Permute[args, ordering], CommutingIObjectList@Permute[sortingArgs, ordering]}]], {args, {}}]; (* TODO: phase *)

IObjectOrder[{expr1_, sym1_, inds1_, noInds1_}, {expr2_, sym2_, inds2_, noInds2_}] := OrderChain[
    -Order[sym1, sym2],
    IObjectIndexOrder[inds1, inds2],
    Order[noInds1, noInds2]
];

CommutingIObjectList[objs_] := With[{objs2 = objs[[All, 4]]}, If[objs2[[#]] === objs2[[# + 1]], {#, # + 1}, Nothing] & /@ Range[Length@objs2 - 1]];

RemoveIObjectIndices[expr_, inds_] := ReplacePart[
    Hold@expr /. _LabelI -> None,
    Prepend[#, 1] -> None & /@ Join @@ Values@inds
];
SetAttributes[RemoveIObjectIndices, HoldFirst];

InverseIndexCountMap[inds_] := Merge[KeyValueMap[<|#2 -> #1|> &, Length /@ inds], Length@Union@# &];
IObjectIndexOrder[inds1_, inds2_] := With[{
    c1 = InverseIndexCountMap@inds1,
    c2 = InverseIndexCountMap@inds2
}, With[{
    maxLen = Max @@ Join[{0}, Keys@c1, Keys@c2]
},
    OrderChain @@ Map[-Order[Lookup[c1, #, 0], Lookup[c2, #, 0]] &, Reverse@Range[0, maxLen]]
]];

ISortedTensorProductHeadQ[ISortedObject[head_, args_, _]] := TensorProductHeadQ@head;
ISortedTensorProductHeadQ[a_] := TensorProductHeadQ@a;
GSListProduct[{n1_, s1_}, {n2_, s2_}, rest___] := GSListProduct[{n1 + n2, Join[s1, AddSCyclesElement[n1] /@ s2]}, rest];
GSListProduct[a_] := a;
GSListProduct[] = {0, {}};
AddSCyclesElement[n_][SCycles[list___]] := SCycles @@ Map[# + n &, {list}, {2}];
AddSCyclesElement[n_][-SCycles[list___]] := -(SCycles @@ Map[# + n &, {list}, {2}]);
MergeGSList[{n_, s1_}, {n_, s2_}, rest___] := MergeGSList[{n, Join[s1, s2]}, rest];
MergeGSList[a_] := a;
GSListSum[list___] := {Max @@ {list}[[All, 1]], Intersection @@ {list}[[All, 2]]};
ReplaceCycleNumbers[{n_, cycles_}, rep_] := {n, cycles /. rep};
CanonicalizeOneCycle[list_] := With[{minPos = FirstPosition[list, Min @@ list, {1}][[1]]}, Join[list[[minPos ;;]], list[[;; minPos - 1]]]];

SymmetryOfSortedObject[expr_] := SymmetryOfSortedObject[expr, IndicesBySlots@expr];
SymmetryOfSortedObject[expr_, slotPos_] := SymmetryOfSortedObject[expr, slotPos, {}];
SymmetryOfSortedObject[expr_, slotPos_List, pos_] := SymmetryOfSortedObject[expr, Association @@ Flatten[MapIndexed[Thread[Drop[#1, 1] -> #2[[1]]] &, slotPos], 1], pos];
SymmetryOfSortedObject[fn_?DummyIndexScopeQ[expr_, __], slotPos_Association, pos_] := SymmetryOfSortedObject[expr, slotPos, Append[pos, 1]]; (* assumed no duplicated indices, so must call ReplaceDummies first on the expression *)
SymmetryOfSortedObject[_IndexBarrier, _, _] = {};
SymmetryOfSortedObject[fn_[], slotPos_Association, pos_] := SymmetryOfSortedObject[fn, slotPos, Append[pos, 0]];
SymmetryOfSortedObject[fn_[a__], slotPos_Association, pos_] := SymmetryOfSortedCompoundExpression[fn, {a}, {}, slotPos, pos];
SymmetryOfSortedObject[ISortedObject[head_, args_, syms_], slotPos_Association, pos_] := SymmetryOfSortedCompoundExpression[head, args, syms, slotPos, pos];
SymmetryOfSortedObject[_, _, _] = {};
SyntaxInformation@SymmetryOfSortedObject = {"ArgumentsPattern" -> {_, _.}};

FixSymmetryList[sym_, toSlot_] := sym /. Thread[Range@Length@toSlot -> toSlot];
SamePrefixQ[pref_, list_] := With[{len = Length@pref}, Length@list >= len && pref === list[[;; len]]];
SymmetryOfSortedCompoundExpression[head_, args_, syms_, slotPos_, pos_] := With[{
    indSlots = DeleteCases[{#, Lookup[slotPos, Key@Append[pos, #], None]} & /@ Range@Length@args, {_, None}],
    prod = ISortedTensorProductHeadQ@head
}, Join[
    SymmetryOfSortedObject[head, slotPos, Append[pos, 0]],
    If[prod, Join, Intersection] @@ (SymmetryOfSortedObject[args[[#]], slotPos, Append[pos, #]] & /@ Delete[Range@Length@args, Transpose@{indSlots[[All, 1]]}]),
    FixSymmetryList[SymmetryGroupOfExpression@head[##], indSlots[[All, 2]]] & @@ ReplacePart[args, Thread[indSlots[[All, 1]] -> IndexSlot]],
    If[prod, Map[p |-> With[{
        pos1 = Lookup[slotPos, Key@#] & /@ Select[Keys@slotPos, SamePrefixQ[Append[pos, p[[1]]], #] &],
        pos2 = Lookup[slotPos, Key@#] & /@ Select[Keys@slotPos, SamePrefixQ[Append[pos, p[[2]]], #] &]
    }, (* TODO: phase *) SCycles @@ Thread@{Sort@pos1, Sort@pos2}], syms], {}]
]];

FlattenEmpty[{{}, a___}] := Sequence@a;
FlattenEmpty[a_] := a;
DeleteFirstEmpty[{{}, r___}] := {r};
DeleteFirstEmpty[r_] := r;
DeleteFirstEmpty[] = {};
ToSimpleIndex[idx_] := With[{s = IndexOfCompoundIndex@idx}, If[s === None, idx, s]];
FindDummies[head_?DummyIndexScopeQ[expr_, inds__]] := With[{
    (* TODO: just counting the indices is not enough when compound indices are involved, we need a set of all index forms *)
    counts = Counts[ToSimpleIndex /@ IndicesBySlots[expr, IndexName /@ {inds}][[All, 1]]]
}, Join[
    {{IndexName@#, DummyIndexInfo@#, counts@IndexName@#} & /@ {inds}},
    DeleteFirstEmpty@FindDummies@expr
]];
FindDummies[fn_[a___]] := Join[{{}, FlattenEmpty@FindDummies@fn}, FlattenEmpty /@ FindDummies /@ {a}];
FindDummies[_] = {{}};
SetAttributes[FindDummies, HoldAll];
SyntaxInformation@FindDummies = {"ArgumentsPattern" -> {_}};

reinitDummies[allDummiesByType_][dummies_] := FoldPairList[With[{
    indPos = FirstPosition[#1, Prepend[#2, _], {1}]
}, With[{ind = Extract[#1, indPos]}, {ind, DeleteCases[#1, {ind[[1]], __}]}]] &, allDummiesByType, dummies[[All, 2 ;;]]];
MinimizeDummySet[{inds_, subs___}] := With[{
    sets = MinimizeDummySet /@ {subs}
}, With[{
    allDummiesByType = Join @@ sets[[All, 1]]
}, Join[
    {inds},
    MapAt[reinitDummies[allDummiesByType], {subs}, Thread@{Range@Length@{subs}, 1}]
]]];

CountDummyScopes[_?DummyIndexScopeQ[expr_, inds__]] := {1};
CountDummyScopes[fn_[a___]] := Join[{Total@CountDummyScopes@fn}, Total@CountDummyScopes@# & /@ {a}];
CountDummyScopes[_] = 0;
SetAttributes[CountDummyScopes, HoldFirst];

ApplyDummies0[expr_, inds_] := ApplyDummies0[expr, inds, {}];
ApplyDummies0[expr_, {{}, subs___}, rep_] := ApplyDummies1[expr, {subs}, rep];
ApplyDummies0[expr_, inds_, rep_] := ApplyDummies1[expr, {inds}, rep];
ApplyDummies1[fn_?DummyIndexScopeQ[expr_, inds__], {inds2_}, rep_] := With[{
    newInds = inds2[[1, All, 1]]
},
    fn[ApplyDummies1[expr, Drop[inds2, 1], Join[rep, Thread[(IndexName /@ {inds}) -> newInds]]], ##] & @@ MapThread[SetIndexSpecName, {{inds}, newInds}]
];
ApplyDummies1[expr : fn_[args___], inds_, rep_] := If[MatchQ[Hold@expr, Hold@Evaluate[Alternatives @@ rep[[All, 1]]]],
    expr /. rep,
    With[{
        subs = FoldPairList[{#1[[;; #2]], Drop[#1, #2]} &, inds, CountDummyScopes@expr]
    },
        ApplyDummies1[fn, subs[[1]], rep] @@ MapThread[ApplyDummies1[#1, #2, rep] &, {{args}, Drop[subs, 1]}]
    ]
];
ApplyDummies1[e_, inds_, rep_] := e /. rep;

SameDummies[expr_] := ApplyDummies0[expr, MinimizeDummySet@FindDummies@expr];
SyntaxInformation@SameDummies = {"ArgumentsPattern" -> {_}};

DistributivePairQ[Times | NonCommutativeMultiply | Wedge, Plus] = True;
SyntaxInformation@DistributivePairQ = {"ArgumentsPattern" -> {_, _}};

End[];

Protect @@ Names@{"`*"};

EndPackage[];
