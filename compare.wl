<< Peanotica`Perm`
<< xAct`xPerm`

ZeroRiemannMonomialIndicesQ[l_, freen_] := AnyTrue[(Min @@ # > freen) && OddQ[Min @@ #] && EvenQ[Max @@ #] &, l];

RandomNonZeroRiemannMonomialIndices[n_, freen_] := NestWhile[
    PermutationList[RandomPermutation[4 n], 4 n] &,
    PermutationList[RandomPermutation[4 n], 4 n],
    ZeroRiemannMonomialIndicesQ[#, freen] &
];

TestRiemannMonomialCanon::wrong = "Different results on input `1`: Peanotica: `2`, xPerm: `3`.";

TestRiemannMonomialCanon[n_, freen_] := TestRiemannMonomialCanon[n, freen, RandomNonZeroRiemannMonomialIndices[n, freen]];
TestRiemannMonomialCanon[n_, freen_, testPerm_] := Module[
    {dummySet, peanoticaResult, xPermResult},
    dummySet = Partition[Range[freen + 1, 4 n], 2];
    peanoticaResult = Peanotica`Perm`DoubleCosetRepresentative[
        RiemannMonomialGenSet[n],
        Peanotica`Perm`Images @@ testPerm,
        DummiesGenSet[1, dummySet]
    ] // AbsoluteTiming;
    xPermResult = CanonicalPerm[
        xAct`xPerm`Images@testPerm,
        4 n,
        StrongGenSet[
            Range[4 n],
            GenSet @@ (RiemannMonomialGenSet[n] /. SCycles -> xAct`xPerm`Cycles)
        ],
        Range@freen,
        {DummySet[0, dummySet, 1]}
    ] /. Peanotica`Perm`Images -> xAct`xPerm`Images /. xAct`xPerm`Images[{inds__}] :> Peanotica`Perm`Images[inds] // AbsoluteTiming;
    If[peanoticaResult[[2]] =!= xPermResult[[2]], Message[TestRiemannMonomialCanon::wrong, testPerm, peanoticaResult[[2]], xPermResult[[2]]]];
    If[peanoticaResult[[1]] === $Failed, Abort[]];
    {peanoticaResult[[1]], xPermResult[[1]]}
];
(* SetPPermVerbose[True]; *)
(* TestRiemannMonomialCanon[1, 2, {2, 4, 1, 3}]; *)
Scan[TestRiemannMonomialCanon[10, 20] &, Range@1000];
Print["Completed 1000 tests"];