BeginPackage["Peanotica`Presets`", {"Peanotica`Core`", "Peanotica`DiffGeo`", "Peanotica`Perm`"}];

Scan[Unprotect@#; ClearAll@#; &, Names@{"Peanotica`Presets`*"}];

SetupRiemannManifold::usage = "SetupRiemannManifold[slot, dim, metric, cd, {riemann, ricci, ricciScalar}]";
PPRiemannDisplayName::usage = "PPRiemannDisplayName is an option of SetupRiemannManifold.";

SetupPerturbation::usage = "SetupPerturbation[pert, vard]";
PPMetricPert::usage = "PPMetricPert is an option of SetupPerturbation.";
ExpandAllPerturbations::usage = "ExpandAllPerturbations[expr, pert, info]";

PostCurvatureCompute::usage = "PostCurvatureCompute is an option for ComputeCurvature";
ComputeCurvature::usage = "ComputeCurvature[input]";
MakeMetricAdapter::usage = "MakeMetricAdapter[symbols, values]";
MakeCurvatureValueRules::usage = "MakeCurvatureValueRules[symbols, values]";
MakeTensorValueRules::usage = "MakeTensorValueRules[symbols, values]";
DefLabelledTensor::usage = "DefLabelledTensor[name, slots, symmetry]";
VarCovDs::usage = "VarCovDs is an option for DefTensorVariationOperator.";
VarParamDs::usage = "VarParamDs is an option for DefTensorVariationOperator.";
DefTensorVariationOperator::usage = "DefTensorVariationOperator[name]";
DefScalarFunction::usage = "DefScalarFunction[name]";

Begin["`Private`"];

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
    <|"Slot" -> slot, "Metric" -> metric, "CovD" -> cd, "Riemann" -> #1, "Ricci" -> #2, "RicciScalar" -> #3|> & @@ riemanns
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

ExpandAllPerturbations[expr_, pert_, info_Association, action_] := With[{
    metric = info["Metric"],
    metricDet = info["DetMetric"],
    cd = info["CovD"],
    riemann = info["Riemann"],
    ricci = info["Ricci"],
    ricciScalar = info["RicciScalar"]
}, If[Head@cd =!= Missing && Head@metric =!= Missing,
    action@ExpandCovDPerturbation[expr, {pert, cd, metric, pert@metric}],
    expr
] // If[Head@metric =!= Missing && Head@cd =!= Missing && Head@riemann =!= Missing,
    action@ExpandRiemannPerturbation[#, {pert, metric, pert@metric}, {cd, riemann, ricci, ricciScalar}],
    #
] & // If[Head@metric =!= Missing && Head@metricDet =!= Missing,
    action@ExpandTensorDetPerturbation[#, {pert, metric, metricDet}],
    #
] & // If[Head@metric =!= Missing,
    action@ExpandMetricPerturbation[#, {pert, metric, pert@metric}],
    #
] &];
ExpandAllPerturbations[pert_, info_][expr_] := ExpandAllPerturbations[expr, pert, info, Identity];
ExpandAllPerturbations[pert_, info_, action_][expr_] := ExpandAllPerturbations[expr, pert, info, action];
SyntaxInformation@ExpandAllPerturbations = {"ArgumentsPattern" -> {_, _, _., _.}};

Options@ComputeCurvature = {
    PostCurvatureCompute -> Simplify @* ITensorReduce @* ContractMetric
};
ComputeCurvature[input_, opt: OptionsPattern[]] := With[{
    metric = input["Metric"],
    metricInv = input["InverseMetric"],
    pdSpec = input["PdSpec"],
    postCompute = OptionValue@PostCurvatureCompute
}, With[{
    pd = If[Head@pdSpec =!= Missing, PdArrayFromPdSpec@pdSpec, input["Pd"]],
    subRiemann = If[Head@pdSpec =!= Missing, RiemannPdFromPdSpec@pdSpec, Lookup[input, "RiemannPd", 0]]
}, With[{
    chris = postCompute@LeviCivitaChristoffelValue[Null, {pd, 0}, metric, metricInv]
}, With[{
    riemann = postCompute[RiemannDifferenceValue[Null, {pd, 0}, chris] + subRiemann]
}, With[{
    ricci = postCompute@ITensorSum[ITensorTranspose[riemann, {1, 3, 2, 3}], {3}]
}, With[{
    ricciScalar = postCompute@ITensorSum[ITensorTranspose[ITensorFixedContract[Times, metricInv, ricci, 1, 1], {1, 1}], {1}]
}, Join[input, <|
    "Pd" -> pd,
    "RiemannPd" -> subRiemann,
    "Christoffel" -> chris,
    "Riemann" -> riemann,
    "Ricci" -> ricci,
    "RicciScalar" -> ricciScalar
|>]]]]]]];
SyntaxInformation@ComputeCurvature = {"ArgumentsPattern" -> {_, OptionsPattern[]}};

MakeMetricAdapter[syms_, values_] := MetricAdapter@{syms["Slot"] -> {values["Metric"], values["InverseMetric"]}};
SyntaxInformation@MakeMetricAdapter = {"ArgumentsPattern" -> {_, _}};

MakeCurvatureValueRules[syms_, values_] := With[{
    slot = syms["Slot"],
    gpd = values["PdInfo"],
    mat = MetricAdapter@{syms["Slot"] -> {values["Metric"], values["InverseMetric"]}},
    metric = syms["Metric"],
    covd = syms["CovD"],
    riemann = syms["Riemann"],
    ricci = syms["Ricci"],
    ricciScalar = syms["RicciScalar"],
    metricValue = values["Metric"],
    metricInvValue = values["InverseMetric"],
    pdValue = values["Pd"],
    chrisValue = values["Christoffel"],
    riemannValue = values["Riemann"],
    ricciValue = values["Ricci"],
    ricciScalarValue = values["RicciScalar"]
}, Join[{
    metric[a_?NonDIQ, b_?NonDIQ] :> NITensor[metricInvValue, {a -> slot, b -> slot}],
    metric[DI@a_, DI@b_] :> NITensor[metricValue, {a -> DI@slot, b -> DI@slot}],
    covd -> Function[{expr, a}, AdaptNITensorCovD[expr, a, slot, {pdValue, {{slot, slot} -> chrisValue}}, mat]],
    riemann[inds__] :> AdaptNITensor[riemannValue, {DI@slot, DI@slot, DI@slot, slot}, {inds}, mat],
    ricci[inds__] :> AdaptNITensor[ricciValue, {DI@slot, DI@slot}, {inds}, mat],
    ricciScalar -> ricciScalarValue
}, MakeTensorValueRules[syms, values, KeySelect[values, Head@# === Symbol &]]]];
SyntaxInformation@MakeCurvatureValueRules = {"ArgumentsPattern" -> {_, _}};

MakeTensorValueRules$OneRule[mat_][name_, {value_, slots_}] := name[inds__] :> AdaptNITensor[value, slots, {inds}, mat];
MakeTensorValueRules[syms_, values_, tensors_] := With[{
    slot = syms["Slot"],
    mat = MetricAdapter@{syms["Slot"] -> {values["Metric"], values["InverseMetric"]}}
}, KeyValueMap[MakeTensorValueRules$OneRule[mat], tensors]];
SyntaxInformation@MakeTensorValueRules = {"ArgumentsPattern" -> {_, _}};

Options@DefLabelledTensor = Options@DefSimpleTensor;
DefLabelledTensor[name_, slots_, sym_, opt : OptionsPattern[]] := DefSimpleTensor[name, Prepend[slots, TFLabelSlot["(", ")"]], sym, opt];
SyntaxInformation@DefLabelledTensor = {"ArgumentsPattern" -> {_, _, _, OptionsPattern[]}};

Options@DefTensorVariationOperator = Join[Options@ExpandDerivativeRules, {
    VarCovDs -> {},
    VarParamDs -> {}
}];
DefTensorVariationOperator$TensorD[perms_List, args__] := Total[DefTensorVariationOperator$TensorD[#, args] & /@ perms] / Length@perms;
DefTensorVariationOperator$TensorD[a_ * perm_Images, args__] := a * DefTensorVariationOperator$TensorD[perm, args];
DefTensorVariationOperator$TensorD[Images[perm__], metrics_, inds1_, inds2_] := Times @@ MapThread[Construct, {metrics, inds1, Permute[inds2, {perm}]}];
DefTensorVariationOperator[name_, opt : OptionsPattern[]] := (
    SetDelayed @@@ ExpandDerivativeRules[name[expr_?DerivativeExpandableQ, var_, rest_] :> ExpandDerivativeWithRest[name[#1, var, #2 * rest] &, expr], FilterRules[{opt}, Options@ExpandDerivativeRules]];
    name[expr_, var_] := name[expr, var, 1];
    name[var_][expr_] := name[expr, var];
    Scan[name[Pattern[cd, #1][expr_, a_], var_, rest_] := name[expr, var, -cd[rest, a]]; &, OptionValue@VarCovDs];
    Scan[name[Pattern[paramd, #1][expr_, params__], var_, rest_] := name[expr, var, Power[-1, Length@{params}] * paramd[rest, params]]; &, OptionValue@VarParamDs];
    name[var_, var_, rest_] := rest;
    name[tensor_[inds1__], expr : tensor_[inds2__], rest_] := With[{
        indPos = FindIndicesSlots@tensor@inds2,
        symmetry = SymmetryOfExpression@tensor@inds2
    }, With[{
        restPos = Delete[Range@Length@{inds1}, indPos[[All, 1]]]
    }, If[Extract[{inds1}, restPos] === Extract[{inds2}, restPos],
        rest * DefTensorVariationOperator$TensorD[
            PPermGroupElements@symmetry,
            MetricOfSlotType /@ indPos[[All, 2]],
            Extract[{inds1}, indPos[[All, 1]]],
            Extract[{inds2}, indPos[[All, 1]]]
        ]
    ,
        0
    ]]] /; Length@{inds1} === Length@{inds2};
    name[_, _, _] = 0;
    SyntaxInformation@name = {"ArgumentsPattern" -> {_, _., _.}};
);
SyntaxInformation@DefTensorVariationOperator = {"ArgumentsPattern" -> {_, OptionsPattern[]}};

DefScalarFunction[name_, displayName_] := (
    DerFunctionQ[name] ^= True;
    If[displayName =!= None, name /: MakeBoxes[name, StandardForm] = InterpretationBox[displayName, name]];
);
SetAttributes[DefScalarFunction, Listable];
SyntaxInformation@DefScalarFunction = {"ArgumentsPattern" -> {_, _}};

JoinSymbolName[str_, symbol_Symbol] := Context@symbol <> str <> SymbolName@symbol;

End[];

EndPackage[];
