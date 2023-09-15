BeginPackage["Peanotica`Internal`"];

Scan[Unprotect@#; ClearAll@#; &, Names@{"`*", "`Private`*"}];

xToolsDebugPrint::usage = "xToolsDebugPrint[tag, msg] prints debug message with specified tag.";
FilterExprList::usage = "FilterExprList[filter, expr] returns true if expr is not filtered by filter.";

Begin["`Private`"];

FilterExprList[All, __] = True;
FilterExprList[l_List, expr_, ___] := MemberQ[l, expr];
FilterExprList[func_, args__] := func[args];

xToolsDebugPrint[tag_, msg___] := If[FilterExprList[Peanotica`$xToolsDebugFilter, tag], Peanotica`$xToolsDebugPrint[tag, " ", msg]];
SetAttributes[DebugPrint, HoldRest];

End[];

Protect @@ Names["`*"];

EndPackage[];
