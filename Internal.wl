BeginPackage["Peanotica`Internal`"];

Scan[Unprotect@#; ClearAll@#; &, Names@{"`*", "`Private`*"}];

xToolsDebugPrint::usage = "xToolsDebugPrint[tag, msg] prints debug message with specified tag.";
FilterExprList::usage = "FilterExprList[filter, expr] returns true if expr is not filtered by filter.";

$xToolsDebugFilter;
$xToolsDebugPrint;

Begin["`Private`"];

If[Hold@$xToolsDebugPrint === Hold@Evaluate@$xToolsDebugPrint, $xToolsDebugPrint = Print];

FilterExprList[All, __] = True;
FilterExprList[l_List, expr_, ___] := MemberQ[l, expr];
FilterExprList[func_, args__] := func[args];

xToolsDebugPrint[tag_, msg___] := If[FilterExprList[$xToolsDebugFilter, tag], $xToolsDebugPrint["[", tag, "] ", msg]];
SetAttributes[DebugPrint, HoldRest];

End[];

Protect @@ Select[Names["`*"], !MatchQ["$" ~~ _]];

EndPackage[];
