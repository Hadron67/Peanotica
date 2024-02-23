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

(* predefined symmetry groups *)
SymmetricGenSet;
RiemannSymmetricGenSet;
BlockSymmetricGenSet;
RiemannMonomialGenSet;
DummyIndicesGenSet;
RubiksCubeGenSet;

(* mathlink functions *)
ConstructStrongGenSet;
DoubleCosetRepresentative;
GroupOrderFromStrongGenSet;

(* mathlink management *)
$PPermLink;
PPermConnect;
PPermDisconnect;
PPermEnsureLink;

Begin["`Private`"];

CheckLink;
MathLinkConstructStrongGenSet;
MathLinkDoubleCosetRepresentative;
MathLinkGroupOrderFromStrongGenSet;

FormatOneCycle[cyc_List] := RowBox@Join[{"("}, Riffle[MakeBoxes /@ cyc, ","], {")"}];
SCycles /: MakeBoxes[expr_SCycles, StandardForm] := InterpretationBox[RowBox@#, expr] &[FormatOneCycle /@ Apply[List, expr]];

SCyclesToImages[-a_, n_] := -SCyclesToImages[a, n];
SCyclesToImages[SCycles[inds__], n_] := Images @@ PermutationList[Cycles@{inds}, n];
SyntaxInformation@SCyclesToImages = {"ArgumentsPattern" -> {_, _}};

ImagesToSCycles[-a_] := -ImagesToSCycles[a];
ImagesToSCycles[Images[inds__]] := SCycles @@ First@PermutationCycles@{inds};
SyntaxInformation@ImagesToSCycles = {"ArgumentsPattern" -> {_}};

MinPermutationLength[-a_] := MinPermutationLength@a;
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

SymmetricGenSet[points__] := MapThread[SCycles[{#1, #2}] &, {Drop[{points}, -1], Drop[{points}, 1]}];
SyntaxInformation@SymmetricGenSet = {"ArgumentsPattern" -> {__}};

RiemannSymmetricGenSet[n_] := RiemannSymmetricGenSet[n, n + 1, n + 2, n + 3];
RiemannSymmetricGenSet[n1_, n2_, n3_, n4_] := {-SCycles@{n1, n2}, -SCycles@{n3, n4}, SCycles[{n1, n3}, {n2, n4}]};
SyntaxInformation@RiemannSymmetricGenSet = {"ArgumentsPattern" -> {_, _., _., _.}};

BlockSymmetricGenSet[blocks__] := MapThread[SCycles @@ Thread[{#1, #2}] &, {Drop[{blocks}, -1], Drop[{blocks}, 1]}];
SyntaxInformation@BlockSymmetricGenSet = {"ArgumentsPattern" -> {__}};

RiemannMonomialGenSet[n_] := With[{blocks = Partition[Range[4n], 4]}, Join[Join @@ RiemannSymmetricGenSet @@@ blocks, BlockSymmetricGenSet @@ blocks]];
SyntaxInformation@RiemannMonomialGenSet = {"ArgumentsPattern" -> {_}};

DummyIndicesGenSet[sign : 0 | 1 | -1, indGroups_List];

RubiksCubeGenSet[n_]

PermToMLPerm[Images[inds__], _] := {1, inds};
PermToMLPerm[-Images[inds__], _] := {-1, inds};
PermToMLPerm[e_SCycles, n_] := PermToMLPerm[SCyclesToImages[e, n], n];
PermToMLPerm[-e_SCycles, n_] := PermToMLPerm[SCyclesToImages[-e, n], n];
PermToMLPerm[e_List, n_] := PermToMLPerm[#, n] & /@ e;
MLPermToPerm[{n_, inds__}] := n * Images[inds];
MLPermToPerm[0] = 0;

ConstructStrongGenSet::usage = "ConstructStrongGenSet[G, n] returns a strong generating set relative to the base [1, 2, ..., n] of the group \[LeftAngleBracket]G\[RightAngleBracket], using Jerrum's variant of Schreier-Sims algorithm. ConstructStrongGenSet[g] infers the argument n from g.";
ConstructStrongGenSet[gs_] := ConstructStrongGenSet[gs, MinPermutationLength@gs];
ConstructStrongGenSet[gs_, n_] := (
    PPermEnsureLink[];
    ImagesToSCycles /@ MLPermToPerm /@ MathLinkConstructStrongGenSet[PermToMLPerm[gs, n], n]
);
SyntaxInformation@ConstructStrongGenSet = {"ArgumentsPattern" -> {_, _.}};

DoubleCosetRepresentative::usage = "DoubleCosetRepresentative[S, g, D, n] returns a canonical representative of the double coset \[LeftAngleBracket]S\[RightAngleBracket]\[CenterDot]g\[CenterDot]\[LeftAngleBracket]D\[RightAngleBracket], with n being the permutation length. S and D are assumed to be strong generating sets relative to the base [1, 2, ..., n]. DoubleCosetRepresentative[S, g, D] infers the argument n from the other three arguments.";
DoubleCosetRepresentative[s_, g_, d_] := DoubleCosetRepresentative[s, g, d, Max[MinPermutationLength[s], MinPermutationLength[g], MinPermutationLength[d]]];
DoubleCosetRepresentative[s_, g_, d_, n_] := (
    PPermEnsureLink[];
    MLPermToPerm@MathLinkDoubleCosetRepresentative[PermToMLPerm[s, n], PermToMLPerm[g, n], PermToMLPerm[d, n], n]
);
SyntaxInformation@DoubleCosetRepresentative = {"ArgumentsPattern" -> {_, _, _, _.}};

GroupOrderFromStrongGenSet::usage = "GroupOrderFromStrongGenSet[g, n] gives the order of the group \[LeftAngleBracket]g\[RightAngleBracket], where g is assumed to be a strong generating set relative to the base [1, 2, ..., n]. GroupOrderFromStrongGenSet[g] infers n from g.";
GroupOrderFromStrongGenSet[gs_] := GroupOrderFromStrongGenSet[gs, MinPermutationLength@gs];
GroupOrderFromStrongGenSet[gs_, n_] := (
    PPermEnsureLink[];
    MathLinkGroupOrderFromStrongGenSet[PermToMLPerm[gs, n], n]
);
SyntaxInformation@GroupOrderFromStrongGenSet = {"ArgumentsPattern" -> {_, _.}};

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

End[];

EndPackage[];