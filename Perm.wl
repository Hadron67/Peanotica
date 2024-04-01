BeginPackage["Peanotica`Perm`"];

PPermDisconnect[];

Scan[Unprotect@#; ClearAll@#; &, Names@{"Peanotica`Perm`*"}];

(* basic types and functions *)
SCycles;
Images;
SCyclesToImages;
ImagesToSCycles;
MinPermutationLength;
ShiftPermutation;
ShiftAndJoinGenSets;
SignedInversePermutation;

(* predefined symmetry groups *)
SymmetricGenSet;
RiemannSymmetricGenSet;
BlockSymmetricGenSet;
RiemannMonomialGenSet;
ScalarMonomialDummiesGenSet;
DummiesGenSet;
RubiksCubeGenSet;

(* mathlink functions *)
ConstructStrongGenSet::usage = "ConstructStrongGenSet[G] returns a strong generating set relative to the base [1, 2, ..., n] of the group \[LeftAngleBracket]G\[RightAngleBracket], using Jerrum's variant of Schreier-Sims algorithm.";
DoubleCosetRepresentative::usage = "DoubleCosetRepresentative[S, g, D] returns a canonical representative of the double coset \[LeftAngleBracket]S\[RightAngleBracket]\[CenterDot]g\[CenterDot]\[LeftAngleBracket]D\[RightAngleBracket] using Butler's algorithm. If two identical permutations with opposite signs are in the double coset, 0 is returned. S and D are assumed to be strong generating sets relative to the base [1, 2, ..., n] where n is the degree. The criterium is the minimum permutation in the lexicographic order of its images.";
GroupOrderFromStrongGenSet::usage = "GroupOrderFromStrongGenSet[g] gives the order of the group \[LeftAngleBracket]g\[RightAngleBracket], where g is assumed to be a strong generating set relative to the base [1, 2, ..., n].";
MoveBasePoint;
PPermGroupElements::usage = "PPermGroupElements[g] gives all the group elements generated by g.";
PPermOpenLogFile::usage = "PPermOpenLogFile[path] opens a log file for the logs to be printed.";
PPermCloseLogFile::usage = "PPermCloseLogFile[] closes the current log file."; (* defined in mathlink directly *)

UseTwoStep::usage = "UseTwoStep is a boolean option for DoubleCosetRepresentative specifying whether to use the two-step method described by Portugal: apply the right coset representative algorithm on the stable points of \[LeftAngleBracket]D\[RightAngleBracket] first, and then apply Butler's algorithm on the result. It's claimed that such method is more efficient, while profiling result shows otherwise. The default is False.";
PPermVerbose::usage = "PPermVerbose is a boolean option for various PPerm functions specifying whether to print messages to log file. This option is maining for debug purpose, and the messages are only enabled in debug builds. The default is False.";

(* mathlink management *)
$PPermLink::usage = "$PPermLink is a global variable holding the LinkObject of the external executable of PPerm.";
PPermConnect::usage = "PPermConnect[] creates the connection to the external executable of PPerm.";
PPermDisconnect::usage = "PPermDisconnect[] disconnects the external executable.";
PPermEnsureLink::usage = "PPermEnsureLink[] checks if $PPermLink is valid, and calls PPermConnect[] if it's not.";

Begin["`Private`"];

CheckLink;
MathLinkConstructStrongGenSet;
MathLinkDoubleCosetRepresentative;
MathLinkGroupOrderFromStrongGenSet;
MathLinkPPermGroupElements;
MathLinkMoveBasePoint;
MathLinkOpenLogFile;

FormatOneCycle[cyc_List] := RowBox@Join[{"("}, Riffle[MakeBoxes /@ cyc, ","], {")"}];
SCycles /: MakeBoxes[expr_SCycles, StandardForm] := InterpretationBox[RowBox@#, expr] &@If[Length@expr === 0, {"I"}, FormatOneCycle /@ Apply[List, expr]];

SCyclesToImages[-a_, n_] := -SCyclesToImages[a, n];
SCyclesToImages[SCycles[inds__], n_] := Images @@ PermutationList[Cycles@{inds}, n];
SCyclesToImages[SCycles[], n_] := Images @@ Range[n];
SyntaxInformation@SCyclesToImages = {"ArgumentsPattern" -> {_, _}};

ImagesToSCycles[-a_] := -ImagesToSCycles[a];
ImagesToSCycles[Images[inds__]] := SCycles @@ First@PermutationCycles@{inds};
SyntaxInformation@ImagesToSCycles = {"ArgumentsPattern" -> {_}};

MinPermutationLength[-a_] := MinPermutationLength@a;
MinPermutationLength[SCycles[]] = 0;
MinPermutationLength[SCycles[inds__]] := Max @@ Apply[Max, {inds}, {2}];
MinPermutationLength[Images[inds__]] := Max[inds];
MinPermutationLength[expr_List] := Max @@ Map[MinPermutationLength, expr];
SyntaxInformation@MinPermutationLength = {"ArgumentsPattern" -> {_}};

ShiftPermutation[-a_, by_] := -ShiftPermutation[a, by];
ShiftPermutation[expr_List, by_] := ShiftPermutation[#, by] & /@ expr;
ShiftPermutation[expr_Images, by_] := Join[Images @@ Range@by, by + # & /@ expr];
ShiftPermutation[expr_SCycles, by_] := by + # & /@ expr;
SyntaxInformation@ShiftPermutation = {"ArgumentsPattern" -> {_, _}};

ShiftAndJoinGenSets[gensets_, lengths_] := Join @@ MapThread[ShiftPermutation, {gensets, Drop[FoldList[Plus, 0, lengths], -1]}];
SyntaxInformation@ShiftAndJoinGenSets = {"ArgumentsPattern" -> {_, _}};

SignedInversePermutation[-expr_] := -SignedInversePermutation[expr];
SignedInversePermutation[0] = 0;
SignedInversePermutation[expr_Images] := Images @@ InversePermutation[List @@ expr];
SyntaxInformation@SignedInversePermutation = {"ArgumentsPattern" -> {_}};

SymmetricGenSet[points__] := MapThread[SCycles[{#1, #2}] &, {Drop[{points}, -1], Drop[{points}, 1]}];
SyntaxInformation@SymmetricGenSet = {"ArgumentsPattern" -> {__}};

RiemannSymmetricGenSet[n_] := RiemannSymmetricGenSet[n, n + 1, n + 2, n + 3];
RiemannSymmetricGenSet[n1_, n2_, n3_, n4_] := {-SCycles@{n1, n2}, -SCycles@{n3, n4}, SCycles[{n1, n3}, {n2, n4}]};
SyntaxInformation@RiemannSymmetricGenSet = {"ArgumentsPattern" -> {_, _., _., _.}};

BlockSymmetricGenSet[blocks__] := MapThread[SCycles @@ Thread[{#1, #2}] &, {Drop[{blocks}, -1], Drop[{blocks}, 1]}];
BlockSymmetricGenSet[] = {};
SyntaxInformation@BlockSymmetricGenSet = {"ArgumentsPattern" -> {__}};

RiemannMonomialGenSet[n_] := With[{blocks = Partition[Range[4n], 4]}, Join[Join @@ RiemannSymmetricGenSet @@@ blocks, BlockSymmetricGenSet @@ blocks]];
SyntaxInformation@RiemannMonomialGenSet = {"ArgumentsPattern" -> {_}};

ScalarMonomialDummiesGenSet[n_] := With[{inds = Partition[Range[n], 2]}, Join[Join @@ (SymmetricGenSet @@@ inds), BlockSymmetricGenSet @@ inds]];
SyntaxInformation@ScalarMonomialDummiesGenSet = {"ArgumentsPattern" -> {_}};

DummiesGenSet[sign_, dummies_] := Join[sign * Join @@ (SymmetricGenSet @@@ dummies), BlockSymmetricGenSet @@ dummies];
SyntaxInformation@DummiesGenSet = {"ArgumentsPattern" -> {_, _}};

RubiksCubeGenSet[n_]

PermToMLPerm[Images[inds__], _] := {1, inds};
PermToMLPerm[-Images[inds__], _] := {-1, inds};
PermToMLPerm[e_SCycles, n_] := PermToMLPerm[SCyclesToImages[e, n], n];
PermToMLPerm[-e_SCycles, n_] := PermToMLPerm[SCyclesToImages[-e, n], n];
PermToMLPerm[e_List, n_] := PermToMLPerm[#, n] & /@ e;
MLPermToPerm[{n_, inds__}] := n * Images[inds];
MLPermToPerm[0] = 0;

ConstructStrongGenSet[gs_] := With[{n = MinPermutationLength@gs},
    PPermEnsureLink[];
    ImagesToSCycles /@ MLPermToPerm /@ MathLinkConstructStrongGenSet[PermToMLPerm[gs, n], n]
];
SyntaxInformation@ConstructStrongGenSet = {"ArgumentsPattern" -> {_, _.}};

Options[DoubleCosetRepresentative] = {
    UseTwoStep -> False,
    PPermVerbose -> False
};
DoubleCosetRepresentative[s_, g_, d_, opt : OptionsPattern[]] := With[{
    n = Max[MinPermutationLength[s], MinPermutationLength[g], MinPermutationLength[d]]
},
    PPermEnsureLink[];
    MLPermToPerm@MathLinkDoubleCosetRepresentative[PermToMLPerm[s, n], PermToMLPerm[g, n], PermToMLPerm[d, n], n, Boole[OptionValue[UseTwoStep]], Boole[OptionValue[PPermVerbose]]]
];
SyntaxInformation@DoubleCosetRepresentative = {"ArgumentsPattern" -> {_, _, _, OptionsPattern[]}};

GroupOrderFromStrongGenSet[gs_] := With[{
    n = MinPermutationLength@gs
},
    PPermEnsureLink[];
    MathLinkGroupOrderFromStrongGenSet[PermToMLPerm[gs, n], n]
];
SyntaxInformation@GroupOrderFromStrongGenSet = {"ArgumentsPattern" -> {_}};

MoveBasePoint[base_, gs_, pos_] := With[{
    n = Max[MinPermutationLength@gs, Max @@ base]
},
    PPermEnsureLink[];
    ImagesToSCycles /@ MLPermToPerm /@ MathLinkMoveBasePoint[base, PermToMLPerm[gs, n], pos, n]
];

PPermGroupElements[genset_List] := With[{
    n = MinPermutationLength@genset
},
    PPermEnsureLink[];
    MLPermToPerm /@ MathLinkPPermGroupElements[PermToMLPerm[genset, n], n]
];
SyntaxInformation@PPermGroupElements = {"ArgumentsPattern" -> {_}};

PPermDisconnect[] := If[Head@$PPermLink === LinkObject, Quiet[Uninstall@$PPermLink, LinkObject::linkn]; $PPermLink =.];
SyntaxInformation@PPermDisconnect = {"ArgumentsPattern" -> {}};

PPermConnect[] := (
    PPermDisconnect[];
    $PPermLink = Install@FileNameJoin[{DirectoryName@FindFile["Peanotica`Perm`"], "mathlink", "pperm-ml"}]
);
SyntaxInformation@PPermConnect = {"ArgumentsPattern" -> {}};

PPermEnsureLink::reconn = "Link is dead, trying to reconnect.";
PPermEnsureLink[] := With[{chk = CheckLink[]}, If[chk =!= True, If[chk === $Failed, Message[PPermEnsureLink::reconn]]; PPermConnect[]]];
SyntaxInformation@PPermEnsureLink = {"ArgumentsPattern" -> {}};

PPermOpenLogFile[path_] := (
    PPermEnsureLink[];
    MathLinkOpenLogFile[path];
);
SyntaxInformation@PPermOpenLogFile = {"ArgumentsPattern" -> {_}};

End[];

EndPackage[];