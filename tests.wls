xTools`$SkipLongTest = !MemberQ[$ScriptCommandLine[[2 ;;]], "--all"];

result = TestReport[TestReport /@ FileNames[__ ~~ ".wlt", $ScriptCommandLine[[2]]]];

If[result["AllTestsSucceeded"],
    Print["All tests passed."]
,
    KeyValueMap[Function[{name, ret},
        Print[name," :"];
        Scan[Function[val,
            Print["    Input: ", val["Input"]];
            Print["        Expected: ", val["ExpectedOutput"]];
            Print["        Actual: ", val["ActualOutput"]];
        ], Values@ret];
    ], result["TestsFailed"]];
];
