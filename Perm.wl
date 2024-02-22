BeginPackage["Peanotica`Perm`"];

PPermDisconnect[];

Scan[Unprotect@#; ClearAll@#; &, Names@{"Peanotica`Perm`*"}];

SCycles;
Images;
MathLinkConstructStrongGenSet;
ConstructStrongGenSet;
TestFunction;
SCyclesToImages;
ImagesToSCycles;

$PPermLink;
PPermConnect;
PPermDisconnect;

Begin["`Private`"];

PermToMLPerm[Images[inds__], _] := {1, inds};
PermToMLPerm[-Images[inds__], _] := {-1, inds};
PermToMLPerm[e_, n_] := PermToMLPerm[SCyclesToImages[e, n], n];
MLPermToPerm[{n_, inds__}] := n * Images[inds];

SCyclesToImages[-a_, n_] := -SCyclesToImages[a, n];
SCyclesToImages[SCycles[inds__], n_] := Images @@ PermutationList[Cycles@{inds}, n];
ImagesToSCycles[-a_] := -ImagesToSCycles[a];
ImagesToSCycles[Images[inds__]] := SCycles @@ First@PermutationCycles@{inds};

CheckDeadLink[expr_] := Check[expr, PPermConnect[]; expr];
SetAttributes[CheckDeadLink, HoldAll];

ConstructStrongGenSet[gs_, n_] := MLPermToPerm /@ CheckDeadLink@MathLinkConstructStrongGenSet[PermToMLPerm[#, n] & /@ gs, n];

PPermDisconnect[] := If[Head@$PPermLink === LinkObject, Uninstall@$PPermLink; $PPermLink =.];

PPermConnect[] := (
    PPermDisconnect[];
    $PPermLink = Install@FileNameJoin[{DirectoryName@FindFile["Peanotica`Perm`"], "mathlink", "pperm-ml"}]
);

PPermConnect[];

End[];

EndPackage[];