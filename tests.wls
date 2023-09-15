xTools`$SkipLongTest = !MemberQ[$ScriptCommandLine[[2 ;;]], "--all"];

result = TestReport[TestReport /@ FileNames[__ ~~ ".wlt", $ScriptCommandLine[[2]]]];

SymbolsToString[expr_] := expr /. name_Symbol :> With[{e = SymbolName@name, ctx = Context@name}, e /; ctx =!= "System`"];

If[result["AllTestsSucceeded"],
    Print["All tests passed."]
,
    failed = result["TestsFailed"];
    KeyValueMap[Function[{name, ret},
        Print[name," :"];
        Scan[Function[val,
            Print["    Input: ", SymbolsToString@val["Input"]];
            Print["        Expected: ", SymbolsToString@val["ExpectedOutput"]];
            Print["        Actual: ", SymbolsToString@val["ActualOutput"]];
        ], Values@ret];
    ], failed];
];
