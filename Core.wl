BeginPackage["Peanotica`Core`", {"Peanotica`Internal`", "Peanotica`xPerm`"}];

Scan[Unprotect@#; ClearAll@#; &, Names@{"Peanotica`Core`*"}];

(* interface *)
StructureOfExpression::usage = "";
SymmetryGroupOfATensor::usage = "";
ICommutativityQ::usage = "";
PhaseOfFunctionSlotOrdering::usage = "";
IObjectHead::usage = "";
IIndexSlot::usage = "";
IFunctionSlot::usage = "";
PrimitiveIndex::usage = "";
AbsIndexQ::usage = "";

(* basic functions *)
ETensor::usage = "";
IndexSlotPosition::usage = "";
MapIndicesOnce::usage = "";
ScalarExpr::usage = "";
UniqueAbsIndex::usage = "";

(* canonicalization *)
ISortExpression::usage = "";
ISorted::usage = "";

Begin["`Private`"];

StructureOfExpression[fn_[args___]] := IObjectHead @@ ConstantArray[IFunctionSlot[1], Length@Hold@args];
HoldPattern@StructureOfExpression[(Plus | List | Equal)[args___]] := IObjectHead @@ Array[IFunctionSlot, Length@Hold@args];
StructureOfExpression[_ScalarExpr] = IObjectHead[];
SetAttributes[StructureOfExpression, HoldAll];
SyntaxInformation@StructureOfExpression = {"ArgumentsPattern" -> {_}};

SymmetryGroupOfATensor[{}] = {};
SyntaxInformation@SymmetryGroupOfATensor = {"ArgumentsPattern" -> {_}};

ICommutativityQ[Plus | Times | Wedge] = True;
SetAttributes[ICommutativityQ, HoldAll];
SyntaxInformation@ICommutativityQ = {"ArgumentsPattern" -> {_}};

PhaseOfFunctionSlotOrdering[(Plus | Times)[___], _] = 1;
SetAttributes[PhaseOfFunctionSlotOrdering, HoldFirst];
SyntaxInformation@PhaseOfFunctionSlotOrdering = {"ArgumentsPattern" -> {_, _}};

AbsIndexQ[_Symbol] = True;
SyntaxInformation@AbsIndexQ = {"ArgumentsPattern" -> {_}};

PrimitiveIndex[-a_] := PrimitiveIndex@a;
PrimitiveIndex[a_] := a;
SyntaxInformation@PrimitiveIndex = {"ArgumentsPattern" -> {_}};

MapIndicesOnce[expr_, fn_] := MapIndicesOnce[expr, fn, Identity];
MapIndicesOnce[expr_, fn_, head_] := MapIndicesOnce1[expr, fn, head, <||>];
SetAttributes[MapIndicesOnce, HoldFirst];
SyntaxInformation@MapIndicesOnce = {"ArgumentsPattern" -> {_, _, _.}};

MapIndicesOnce1[expr_, fn_, head_, indMap_] := With[{
    struct = StructureOfExpression@expr
}, With[{
    indPos = Position[struct, _IIndexSlot]
},
    TODO[]
]];
SetAttributes[MapIndicesOnce1, HoldFirst];

FunctionStructure[struct_] := With[{
    pos = Position[struct, _IFunctionSlot]
}, Values[GroupBy[Thread@{First /@ Extract[struct, pos], pos}, First]][[All, All, 2]]];

ISortExpression[expr_, inds_] := With[{
    struct = StructureOfExpression@expr
}, With[{
    indPos = Position[struct, _IIndexSlot],
    funcPos = Position[struct, _IFunctionSlot]
}, With[{
    args = SortArgs[expr, ISortExpression /@ Extract[expr, funcPos]]
},
    ReplacePart[ISorted@expr, MapThread[Prepend[#1, 1] -> #2 &, {funcPos, args}]]
]]];

SortArgs[expr_, args_] := If[ICommutativityQ@expr,
    TODO[],
    args
];
SetAttributes[SortArgs, HoldFirst];

SetAttributes[ISorted, HoldAll];

IndexSlotPosition[expr_] := IndexSlotPosition[expr, {}];
IndexSlotPosition[expr_, inds_] := IndexSlotPosition[expr, inds, {}];
IndexSlotPosition[expr_, inds_, pos_] := With[{
    struct = StructureOfExpression@expr
}, With[{
    indPos = Position[struct, _IIndexSlot],
    funcPos = FunctionStructure@struct
}, With[{
    exprInds = Extract[Hold@expr, Prepend[1] /@ indPos]
},
    Join[
        With[{
            newInds = Union@Join[inds, Select[PrimitiveIndex /@ exprInds, AbsIndexQ]]
        }, MergeBroadcastedIndices[
            Join /@ Map[IndexSlotPosition[Extract[Hold@expr, Prepend[#, 1]], newInds, Join[pos, #]] &, funcPos, {2}],
            newInds
        ]]
    ,
        MapThread[Prepend, {indPos, exprInds}]
    ]
]]];
SetAttributes[IndexSlotPosition, HoldFirst];

MergeBroadcastedIndices[indAndPos_, inds_] := With[{
    ind = Map[First, indAndPos, {2}]
},
    
];

UniqueAbsIndex[s_Symbol] := Module @@ {{s}, s};

End[];

Protect @@ Names@{"`*"};

EndPackage[];
