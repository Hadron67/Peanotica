BeginPackage["Peanotica`Presets`", {"Peanotica`Core`", "Peanotica`DiffGeo`"}];

Scan[Unprotect@#; ClearAll@#; &, Names@{"Peanotica`Presets`*"}];

SetupRiemannManifold::usage = "SetupRiemannManifold[slot, dim, metric, cd, {riemann, ricci, ricciScalar}]";
PPRiemannDisplayName::usage = "PPRiemannDisplayName is an option of SetupRiemannManifold.";

SetupPerturbation::usage = "SetupPerturbation[pert, vard]";
PPMetricPert::usage = "PPMetricPert is an option of SetupPerturbation.";

Begin["`Private`"];

End[];

Options@SetupRiemannManifold = {
    ClearAll -> True,
    PPRiemannDisplayName -> "R"
};
SetupRiemannManifold[slot_, dim_, metric_, cd_, riemanns_List, opt : OptionsPattern[]] := With[{
    cls = OptionValue@ClearAll
},
    If[cls, ClearAll[slot]];
    DefSimpleSlotType[slot, dim, {Symbol /@ CharacterRange["a", "z"], DefaultIndex}];
    If[cls, ClearAll[metric]];
    DefSimpleMetric[metric, slot, 1];
    If[cls, ClearAll[cd]];
    DefTensorDerivativeOperator[cd, {slot}, {}];
    cd[_metric, _] = 0;
    MetricOfSlotType@slot ^= metric;
    If[Length@riemanns >= 3,
        If[cls, ClearAll /@ Take[riemanns, 3]];
        DefRiemannCurvature[slot, riemanns[[1]], riemanns[[2]], riemanns[[3]], DisplayName -> OptionValue@PPRiemannDisplayName];
    ];
];
SyntaxInformation@SetupRiemannManifold = {"ArgumentsPattern" -> {_, _, _, _, _, OptionsPattern[]}};

Options@SetupPerturbation = Join[Options@DefPerturbationOperator, {
    ClearAll -> True
}];
SetupPerturbation[pert_, vard_, info_] := With[{
    cls = OptionValue@ClearAll
},
    If[cls, ClearAll@pert];
    If[cls, ClearAll@vard];
    DefPerturbationOperator[pert];

];

EndPackage[];
