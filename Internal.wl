BeginPackage["Peanotica`Internal`"];

Scan[Unprotect@#; ClearAll@#; &, Names@{"`*", "`Private`*"}];

xToolsDebugPrint::usage = "xToolsDebugPrint[tag, msg] prints debug message with specified tag.";
FilterExprList::usage = "FilterExprList[filter, expr] returns true if expr is not filtered by filter.";
ThreadableQ;
ExpandThreadableRules::usage = "ExpandThreadableRules[lhs :> rhs]";
NestWith;

$xToolsDebugFilter;
$xToolsDebugPrint;

ArraySimplify;

RelationClosureTable::usage = "RelationClosureTable[initial, relationFn, collector]";
RelationClosureTableToArray::usage = "";

Begin["`Private`"];

If[Hold@$xToolsDebugPrint === Hold@Evaluate@$xToolsDebugPrint, $xToolsDebugPrint = Print];

FilterExprList[All, __] = True;
FilterExprList[None, __] = False;
FilterExprList[l_List, expr_, ___] := MemberQ[l, expr];
FilterExprList[func_, args__] := func[args];

xToolsDebugPrint[tag_, msg___] := If[FilterExprList[$xToolsDebugFilter, tag], $xToolsDebugPrint["[", tag, "] ", msg]];
SetAttributes[DebugPrint, HoldRest];

DefDerivativeOperator[sym_, prodq_, funcq_] := (
    sym[expr_Plus, args___] := sym[#, args] & /@ expr;
    sym[expr_?prodq, args___] := Total@MapIndexed[ReplacePart[expr, #2 -> sym[#, args]] &, expr];
    sym[fn_?funcq[args__], args2___] := With[{mat = IdentityMatrix@Length@{args}}, Total@MapIndexed[(Derivative @@ mat[[#2[[1]]]])[fn][args] * sym[#, args2] &, {args}]];
    sym[Power[a_, b_], args___] := b * Power[a, b - 1] * sym[a, args] + Power[a, b] * Log[b] * sym[b, args];
    sym[_?NumberQ, ___] = 0;
);
SyntaxInformation@DefDerivativeOperator = {"ArgumentsPattern" -> {_, _}};

Options[ArraySimplify] = Options@Simplify;
ArraySimplify[arr_?ArrayQ, opt : OptionsPattern[]] := Map[ArraySimplify[#, opt] &, arr, {ArrayDepth@arr}];
ArraySimplify[expr_, opt : OptionsPattern[]] := Simplify[expr, opt];
SyntaxInformation@ArraySimplify = {"ArgumentsPattern" -> {_, OptionsPattern[]}};

ExpandThreadableRules[lhs :> rhs] := With[{
    exprPatName = Cases[lhs, Verbatim[PatternTest][Verbatim[Pattern][p_, Blank[]], ThreadableQ] :> p, {0, Infinity}][[1]]
}];

RelationClosureTableStep[relationFn_, collector_][{current_, newTerms_}] := With[{
    newTermRels = AssociationMap[relationFn, newTerms]
}, With[{
    newCurrent = Join[current, newTermRels]
}, {
    newCurrent,
    Complement[Union @@ (collector /@ Values@newTermRels), Keys@newCurrent]
}]];
RelationClosureTable[initial_, relationFn_, collector_] := First@NestWhile[RelationClosureTableStep[relationFn, collector], {<||>, initial}, Length@#[[2]] > 0 &];
SyntaxInformation@RelationClosureTable = {"ArgumentsPattern" -> {_, _, _}};

End[];

Protect @@ Select[Names["`*"], !StringMatchQ[#, "$" ~~ __] &];

EndPackage[];
