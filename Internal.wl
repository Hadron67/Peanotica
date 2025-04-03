BeginPackage["Peanotica`Internal`"];

Scan[Unprotect@#; ClearAll@#; &, Names@{"`*", "`Private`*"}];

xToolsDebugPrint::usage = "xToolsDebugPrint[tag, msg] prints debug message with specified tag.";
FilterExprList::usage = "FilterExprList[filter, expr] returns true if expr is not filtered by filter.";
ThreadableQ;
MakeThreadableRules::usage = "MakeThreadableRules[lhs :> rhs]";
NestWith;
OrderOr::usage = "OrderOr[terms, ...]";
SymmetryList::usage = "SymmetryList[elems, comp]";

$xToolsDebugFilter;
$xToolsDebugPrint;

ArraySimplify;

RelationClosureTableToArray::usage = "";

Begin["`Private`"];

If[Hold@$xToolsDebugPrint === Hold@Evaluate@$xToolsDebugPrint, $xToolsDebugPrint = Print];

FilterExprList[All, __] = True;
FilterExprList[None, __] = False;
FilterExprList[l_List, expr_, ___] := MemberQ[l, expr];
FilterExprList[func_, args__] := func[args];

xToolsDebugPrint[tag_, msg___] := If[FilterExprList[$xToolsDebugFilter, tag], $xToolsDebugPrint["[", tag, "] ", msg]];
SetAttributes[DebugPrint, HoldRest];

Options[ArraySimplify] = Options@Simplify;
ArraySimplify[arr_?ArrayQ, opt : OptionsPattern[]] := Map[ArraySimplify[#, opt] &, arr, {ArrayDepth@arr}];
ArraySimplify[expr_, opt : OptionsPattern[]] := Simplify[expr, opt];
SyntaxInformation@ArraySimplify = {"ArgumentsPattern" -> {_, OptionsPattern[]}};

MakeThreadableRules[lhs_ :> rhs_, patternName_] := With[{
    lhsPos = Position[lhs, patternName],
    rhsPos = Drop[1] /@ Position[Hold@rhs, patternName]
}, {

}];

OrderOr[0, rest__] := OrderOr[rest];
OrderOr[1, ___] = 1;
OrderOr[-1, ___] = -1;
OrderOr[a_] := a;
SetAttributes[OrderOr, HoldRest];
SyntaxInformation@OrderOr = {"ArgumentsPattern" -> {__}};

SymmetryList[list_, comp_] := MapThread[If[comp[#1, #2], #3, Nothing] &, {Drop[list, -1], Drop[list, 1], Range[Length@list - 1]}];
SyntaxInformation@SymmetryList = {"ArgumentsPattern" -> {_, _}};

End[];

Protect @@ Select[Names["`*"], !StringMatchQ[#, "$" ~~ __] &];

EndPackage[];
