(* This file is copied from xAct`xPerm`, with license unmodified. *)

(************************ 0. Info and copyright ***********************)


xAct`xPerm`$Version={"1.2.3",{2015,8,23}}


(* xPerm, a free package for permutation groups in Mathematica *)

(* Copyright (C) 2003-2020 Jose M. Martin-Garcia *)

(* This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2of the License,
  or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
 for more details.

You should have received a copy of the GNU General Public License along
 with this program; if not, write to the Free Software Foundation, Inc.,
  59 Temple Place-Suite 330, Boston, MA 02111-1307, USA.
*)


(* :Title: xPerm *)

(* :Author: Jose M. Martin-Garcia *)

(* :Summary: Computational group theory for index canonicalization *)

(* :Brief Discussion:
    - Notations for permutations: It is not clear which one is better,
      so that four are encoded: Perm, Images, Cycles and Rules.
    - Permutations act on the right of lists or other permutations.
      This is the choice in GAP and Butler's book.
    - We follow Portugal's notation: permutations map slots to indices.
    - There are two main objectives in this package:
        a) Schreier-Sims algorithm (Butler's book)
        b) choice of coset and double-coset representatives
           (Portugal's papers).
    - This is the Mathematica code of the package. An external C code
      with a subset of capabilities can be linked via MathLink.
    - Mathematica 9 contains the Butler-Portugal algorithm, but xPerm
      does not yet link to it by default.  
*)
  
(* :Context: xAct`xPerm` *)

(* :Package Version: 1.2.3 *)

(* :Copyright: Jose M. Martin-Garcia 2003-2020 *)

(* :History: (see xPerm.History for details)
    - Version 0.1 (May 2003): Schreier-Sims coded from Butler's book.
      Portugal's routines encoded.
    - Version 0.2 (July 2003): Recoding in C, using Images notations,
      and connection through MathLink. Much faster.
    - Version 0.3 (August 2003): Recoded signed permutations. The basic
      mathematical structure is an algebra, and not the {-1,1} x Sn group.
    - Version 0.4 (March 2004): First public release.
    - Version 0.5 (July 2006) 
    - Version 0.6 (August 2007)
    - Version 0.7 (November 2007)
    - Version 1.0 (March 2008): Release for publication.
    - Version 1.1 (September 2010)
    - Version 1.2 (January 2013)
*)

(* :Keywords: permutations, index canonicalization, Butler-Portugal algorithm *)

(* :Source: xPerm.nb *)

(* :Mathematica Version: 5.0 and later *)

(* :Limitations: Only permutation symmetries, not multiterm symmetries *)


(************************ 1. Begin package ***********************)

Scan[Unprotect@#; ClearAll@#; &, DeleteCases[Join[Names["Peanotica`xPerm`*"],Names["Peanotica`xPerm`Private`*"]],"$Version"|"xAct`xPerm`$Version"]];

Peanotica`xPerm`Cycles;
Peanotica`xPerm`RightCosetRepresentative;

System`Cycles;
Off[System`Cycles::shdw];

BeginPackage["Peanotica`xPerm`"];

Off[General::shdw]
Peanotica`xPerm`Disclaimer[]:=Print["These are points 11 and 12 of the General Public License:\n\nBECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM `AS IS\.b4 WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.\n\nIN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES."]
On[General::shdw]

(* Notations and translation *)
Perm::usage="Perm is the head for permutation lists of the form Perm[{n1, ..., nk}]. Example: Perm[{6, 3, 2, 1, 5, 4}] means that the object in the first place goes to the fourth place and so on. Perm[{}] represents the identity.";
Cycles::usage="Cycles is the head for permutations expressed in disjoint cyclic notation. Singletons are not included. Example: Cycles[{1, 4, 6}, {2, 3}] represents the exchange of points 2 and 3, and the cycle 1->4->6->1. Cycles[] represents the identity.";
Rules::usage="Rules is the head for permutations expressed in rule notation. Singletons are not included. Example: Rules[1->4, 4->6, 6->1, 2->3, 3->2]. Rules[] represents the identity.";
Images::usage="Images is the head for permutations expressed as a list of images. Example: Images[{4, 3, 2, 6, 5, 1}]. Images[{}] represents the identity.";
PermQ::usage="PermQ[ Perm[{n1, ..., nk}] ] yields True if the list {n1, ..., nk} of length k is a rearrangement of the numbers {1, ..., k}. PermQ[ Cycles[cyc1, cyc2, ...] ] gives True if there are no repeated points among cycles cyci. PermQ[ Rules[i1->i2, ...] ] gives True if the rules map a set of integers to itself. PermQ[ Images[{n1, ..., nk}] ] yields True if the list {n1, ..., nk} of length k is a rearrangement of the numbers {1, ..., k}. PermQ[ID] gives True. PermQ gives True on a linear combination of valid permutations. PermQ returns False otherwise.";
ID::usage="ID represents the identity in all notations. ID[perm] gives the identity permutation in the notation used by perm.";
NotationOfPerm::usage="NotationOfPerm[perm] gives the notation of permutation perm. It can be one of {Perm, length}, {Images, length}, Cyles or Rules.";
TranslatePerm::usage="TranslatePerm[perm, notation] translates permutation perm, in any format, to the given notation, which must be one of Perm, {Perm, length}, Cycles, Rules, Images or {Images, length}. TranslatePerm[set, notation] returns the set (with head GenSet, StrongGenSet, Group or Coset) with all its permutations translated to notation.";
RandomPerm::usage="RandomPerm[deg] gives a random permutation in Perm notation. RandomPerm[deg, notation] constructs RandomPerm[deg] and then translates it to the given notation using TranslatePerm[perm, notation].";


(* Operations with permutations *)
PermLength::usage="PermLength[perm] gives the so-called length of permutation perm. If expressed in Perm or Images notation, the length of perm is the length of the list of points. In other cases, it returns PermDeg[perm].";
PermDeg::usage="PermDeg[perm] gives the degree of permutation perm, defined as the largest point moved. The degree of (any form of) the identity is 0. PermDeg[set] gives the largest degree among the permutations of the set (a group, generating set, strong generating set or linear combination).";
PermProduct::usage="PermProduct[perm1, perm2] gives the composition of permutations perm1 and perm2, applied from left to right, that is, the result is equivalent to the application of perm2 after perm1 on a list of points. PermProduct[perm1, perm2, perm3, ...] composes all permutations, assuming that they all use the same notation. PermProduct[perm] gives perm and PermProduct[] gives ID.";
InversePerm::usage="InversePerm[perm] gives the inverse permutation of perm.";
PermPower::usage="PermPower[perm, n] for positive n gives the composition of n copies of perm. For negative n it gives the composition of n copies of InversePerm[perm]. For zero n it gives ID[perm].";
OnPoints::usage="OnPoints[p, perm] yields the image of point p under permutation perm. OnPoints[{p1, ...}, perm] yields the list of images of points pi. Signs of permutations are not considered.";
PermSignature::usage="PermSignature[perm] gives the signature of perm: +1 if perm can be written as an even number of transpositions, -1 if perm can be written as an odd number of transpositions.";

(* Stabilization *)
StablePoints::usage="StablePoints[perm, n] gives the points in {1, ..., n} that are fixed by permutation perm. StablePoints[GS, n] gives the points in {1, ..., n} that are fixed by all permutations in GS.";
NonStablePoints::usage="NonStablePoints[list, GS] enlarges list until none of the permutations in GS fixes all points in list. It tries to give the shortest result possible, not necessarily ordered.";
Stabilizer::usage="Stabilizer[{p1, ...}, GS] gives the subset of permutations in GS that fix all points pi. Stabilizer[{p1, ...}, SGS] returns a strong generating set for the stabilizer subgroup of the group SGS fixing all points pi. In this latter case a third argument can be given with the length of the internal Schreier vectors.";
SetStabilizer::usage="SetStabilizer[{p1, ...}, GS] gives the subset of permutations in GS that keep the points pi inside the list. SetStabilizer[{p1, ...}, SGS] returns a strong generating set for the setwise stabilizer of the list of points pi under the group generated by the strong generating set SGS.";

(* Permuting lists *)
PermutationFromTo::usage="PermutationFromTo[list1, list2] gives the permutation (in Images notation) that converts list1 into list2. Points in list1 or list2 can be repeated.";
PermuteList::usage="PermuteList[list, perm] permutes the points of list according to the permutation perm.";

(* Sorting *)
SortB::usage="SortB[{e1, e2, ...}, B] sorts the elements ei according to the canonical order defined by list B. Elements not in B are sorted last, using Sort.";
MinB::usage="MinB[list, B] gives the mininum element of list with respect to base B. If none of the elements of B is in list then the minimum of list with respect to the canonical order is returned.";
PermOrderedQ::usage="PermOrderedQ[h[perm1, perm2]] gives True if perm1 maps integers to smaller points than perm2. It gives False if perm1 maps integers to larger points than perm2. It gives Null if perm1 and perm2 maps integers to the same points. PermOrderedQ[h[perm1, perm2], B] gives True if perm1 maps the points of base B to smaller (according to B) images than perm2. It gives False if perm1 maps the points of B to larger (according to B) images than perm2. It gives Null if perm1 and perm2 map the points of B to the same images.";
PermSort::usage="PermSort[{perm1, perm2, ...}] sorts permutations permi according to PermOrderedQ. PermSort[{perm1, perm2, ...}, B] sorts permutations permi according to PermOrderedQ using base B.";
PermEqual::usage="PermEqual[perm1, perm2] gives True if both permutations map all points to the same images, and False otherwise.";
PermLess::usage="PermLess[perm1, perm2] gives True if perm1 maps sorted integers to smaller points than perm2, and False otherwise.";
PermGreater::usage="PermGreater[perm1, perm2] gives True if perm1 maps sorted integers to larger points than perm2, and False otherwise.";
PermLessEqual::usage="PermLessEqual[perm1, perm2] gives True if perm1 maps sorted integers to smaller or equal points than perm2, and False otherwise.";
PermGreaterEqual::usage="PermGreaterEqual[perm1, perm2] gives True if perm1 maps sorted integers to larger of equal points than perm2, and False otherwise.";


(* Structures *)
GenSet::usage="GenSet is the head for a generating set.";
Group::usage="Group is the head for a group of permutations.";
Coset::usage="Coset is the head for a coset of permutations.";
Schreier::usage="Schreier is the head for a Schreier vector. The format is Schreier[orbits, nu, w], where nu is the actual Schreier vector and w is the associated vector of backward pointers.";
StrongGenSet::usage="StrongGenSet is the head for a strong generating set. The format is StrongGenSet[base, GS].";
Dimino::usage="Dimino[GenSet[perm1, ...]] gives the group generated by permutations permi, using Dimino's algorithm.";


(* Strong generating sets *)
Orbit::usage="Orbit[p, GS] gives the orbit of point p under the group generated by GS. The orbit is given as a (not necessarily ordered) list having p as first point.";
Orbits::usage="Orbits[GS, len] gives a list with all orbits of the group generated by permutations in GS, a partition of Range[len]. Orbits[GS, len, list] makes points in list to be the first points of the orbits. Orbits[GS] and Orbits[GS, list] compute len from GS using PermLength. GS can be a generating set (head GenSet) or a strong generating set (head StrongGenSet).";
SchreierOrbit::usage="SchreierOrbit[p, GS, len] gives the orbit of point p under the group generated by GS, and its Schreier vector (of lenght len). SchreierOrbit[p, SGS, len] gives the same output, but the the first point of the orbit is now the minimum with respect to the base of the SGS. SchreierOrbit[p, gs] computes the length of the vectors from gs using PermLength. See notes for Schreier.";
SchreierOrbits::usage="SchreierOrbits[GS, len] gives a compound Schreier vector (length len) of the orbits of the group generated by GS. SchreierOrbits[GS, len, list] makes points in list to be the first points of the orbits. SchreierOrbits[SGS, len] gives the same output, but the orbits now start with the points of the base of the SGS. SchreierOrbits[gs] computes len from gs using PermLength. See notes for Schreier.";
TraceSchreier::usage="TraceSchreier[p, Schreier[orbits, nu, w]] finds a permutation such that the image of the first point of the orbit of p is p.";
StabilizerChain::usage="StabilizerChain[SGS] gives a list of strong generating sets corresponding to the chain of stabilizers of the group generated by SGS. The first one is SGS and the last one is the generating set of Group[ID].";
OrderOfGroup::usage="OrderOfGroup[SGS] gives the order of the group generated by SGS.";
PermMemberQ::usage="PermMemberQ[perm, SGS] gives True if permutation perm belongs to the group generated by SGS, and False otherwise.";
PermWord::usage="PermWord[perm, SGS] decompose the permutation perm as {pk, ..., p0} where each of the permutations pi belong to the stabilizer subgroup of the first i points of the base of SGS. If pk == ID then perm belongs to the group and not otherwise. There is a third argument, used internally for iteration.";
FromBaseImage::usage="FromBaseImage[list, SGS] gives the permutation in the group described by SGS such that the images of the points in the base are the points in list (assumed to have the length of the base of the SGS). If no permutation in the group gives the list of points then an error message is thrown. A third argument can be given with the length of the internal Schreier vectors used.";
AllBaseImages::usage="AllBaseImages[SGS] gives a complete list of rules baseimages -> perm, where baseimages is a list of images of the base of SGS and perm is its corresponding permutation, as would be given by FromBaseImage[baseimages, SGS]. A second argument can be given with the length of the Schreier vectors used.";
Search::usage="Search[SGS, P, s, SGSK] returns the s-th stabilizer in the stabilizer-chain of the subgroup K of permutations obeying the property P in the group G (described by the strong generating set SGS). It is assumed P[perm] returns True or False on any permutation of the group G. The fourth argument is a strong generating set for a subgroup of K, possibly a deeper stabilizer in its stabilizer-chain.";
BaseChange::usage="BaseChange[SGS, newbase] gives a new strong generating set for the group described by SGS, having newbase as the first elements of its base. A third argument can be given specifying the length of the internal Schreier vectors used.";
DeleteRedundantGenerators::usage="DeleteRedundantGenerators[SGS] returns an equivalent strong generating set removing redundant generators from the generating set.";
SchreierSims::usage="SchreierSims[initbase, GS, deg] generates a strong generating set for the group generated by GS (permutations of degree deg), using list initbase as the first points for the base. The final SGS is not reduced in general.";
UseRules::usage="UseRules is an option for SchreierSims giving a set of rules replacing the permutations by strings. By default it is {}.";
xPermVerbose::usage="xPermVerbose is an option for SchreierSims, RightCosetRepresentative, DoubleCosetRepresentative and CanonicalPerm. xPermVerbose->True gives lots of information about the intermediate status of the process.";


(* Canonicalization *)
RightCosetRepresentative::usage="RightCosetRepresentative[perm, n, SGS] for an unsigned permutation perm of degree n and a SGS for group S gives a canonical representative of the right coset S.perm of perm with respect to subgroup S of the symmetric group Sn. The criterium is the minimization of images of points of the base of SGS under the elements of the coset, following the order given by the base. A fourth argument can be used to give additional priority to some points (the free slots in the tensorial context)..";
DummySet::usage="DummySet is the head of expressions DummySet[manifold, {{d1u, d1d}, {d2u, d2d}, ...}, metricsym] denoting a set of pairs of (names of) dummies {diu, did} on manifold, whose metric has symmetry metricsym (an integer with value 1 if the metric is symmetric, -1 if the metric is antisymmetric, or 0 if there is no metric).";
RepeatedSet::usage="RepeatedSet[{i1, i2, ...}] represents a list of names of repeated indices in the canonical configuration.";
JoinSGS::usage="JoinSGS[StrongGenSet[base1, GS1], StrongGenSet[base2, GS2]] gives a strong generating set having base Join[base1, base2] (with base1 and base2 assumed to be disjoint) and generating set Union[GS1, GS2] (with GS1 and GS2 assumed to move disjoint sets of points).";
SGSOfDummySet::usage="SGSOfDummySet[ DummySet[...] ] gives a Strong Generating Set for the group of permutations associated to the given DummySet. There are always permutations coming from the exchange of dummies. There are permutations coming from the exchange of up/down indices in a pair if there is a metric for those indices. The GS is given in Cycles notation. See notes for DummySet.";
DoubleCosetRepresentative::usage="DoubleCosetRepresentative[perm, n, SGS, dummysets] returns a canonical representative of the double coset S.perm.D, where S is generated by the strong generating set SGS and D is the group of symmetries of the dummysets. The criterium is the sequential sorting of slots of permutation perm (from first to last), choosing the least index not yet used consistent with the symmetries S and D. The indices are chosen assuming that indices are numbered according to their canonical order.";
DoubleCosetRepresentative2::usage = "DoubleCosetRepresentative2[perm, n, S, D] = Canonical(S.perm.D)";
CanonicalPerm::usage="CanonicalPerm[perm, n, GS, {f1, f2, ...}, dummysets] gives a canonical representative of the double coset S.perm.D, where S is generated by GS and D is the group of symmetries of the dummysets of dummies. The algorithm RightCosetRepresentative is first applied to canonicalize free indices f1, f2, ..., and then algorithm DoubleCosetRepresentative is applied on the result of the latter.";
OrderedBase::usage="OrderedBase is an option for CanonicalPerm specifying that the base must be filled with the missing integers, in normal order.";
BaseChangeCheck::usage="BaseChangeCheck is a Boolean option for RightCosetRepresentative that forces checking the need of a change of base to stabilize the group of symmetry slots. The default is False.";
TimeVerbose::usage="TimeVerbose is an option for CanonicalPerm. TimeVerbose->True outputs the time spent in the computation of the SGS, in the computation of the free-indices representative and in the computation of the dummy-indices representative.";
MathLink::usage="MathLink is a Boolean option for CanonicalPerm, SchreierSims and Orbit, indicating whether the external C executable xperm must be used to speed up computations. Its default value is given by the global variable $xpermQ for CanonicalPerm, and it is False for the other two functions.";

(* MathLink connection *)
$xpermQ::usage="$xpermQ is a Boolean global variable containing whether the connection to the external C executable xperm has been possible or not.";
$xpermExecutable::usage="$xpermExecutable is a global variable containing the name of the xperm executable to which we have connected. If the connection was not possible then it is not assigned.";
$xpermLink::usage="$xpermLink is a global variable containing the link identification of the connection to the external excutable xperm. If the connection was not possible then it is not assigned.";

(* Predefined strong generating sets *)
If[System`$VersionNumber<8.5,
Symmetric::usage="Symmetric[{p1, p2, ...}] represents the symmetry of a tensor that is symmetric in the slots pi. Symmetric[{p1, p2, ...}, notation] returns a strong generating set for the symmetric group on the set of points pi, using the indicated notation for signed permutations.";
Antisymmetric::usage="Antisymmetric[{p1, p2, ...}] represents the symmetry of a tensor that is antisymmetric in the slots pi. Antisymmetric[{p1, p2, ...}, notation] returns a strong generating set for the antisymmetric group on the set of points pi, using the indicated notation for signed permutations.";,
Symmetric::usage=Symmetric::usage<>"\n\nxAct extension:\nSymmetric[{p1, p2, ...}, notation] returns a strong generating set for the symmetric group on the set of points pi, using the indicated notation for signed permutations.";
Antisymmetric::usage=Antisymmetric::usage<>"\n\nxAct extension:\nAntisymmetric[{p1, p2, ...}, notation] returns a strong generating set for the antisymmetric group on the set of points pi, using the indicated notation for signed permutations.";
];
PairSymmetric::usage="PairSymmetric[{{p1a, p1b}, {p2a, p2b}, ...}, sym1, sym2, notation] returns a Strong Generating Set for the symmetric (if sym1=1) or antisymmetric (if sym1=-1) group of permutations of the pairs {pia, pib}. The switch sym is an integer: 1 adds permutations Cycles[{pia, pib}]; -1 adds permutations -Cycles[{pia, pib}]; other values do nothing. The result is given in the using the indicated notation (Cycles by default).";
RiemannSymmetric::usage="RiemannSymmetric[{p1, p2, p3, p4}, notation] gives a strong generating set implementing the symmetries of the Riemann tensor R_{p1,p2,p3,p4}, using the indicated notation for signed permutations (Cycles by default).";
RiemannSymmetry::usage="RiemannSymmetry = RiemannSymmetric. Kept for backward compatibility.";

(* Transversals *)
RightTransversal::usage="RightTransversal[SGS, deg] returns a sorted list of permutations containing the canonical representative of each right coset of SGS in the symmetric group of degree deg. The choice of representative is based in the order induced on the permutations by the base of the SGS. RightTransversal[GS, deg] returns the same thing by first using SchreierSims.";
LeftTransversal::usage="LeftTransversal[SGS, deg] returns a sorted list of permutations containing the canonical representative of each left coset of SGS in the symmetric group of degree deg. The choice of representative is based in the order induced on the **inverse** of the permutations by the base of the SGS. Efectively the transversal is simply the inverse of the corresponding right transversal. LeftTransversal[GS, deg] returns the same thing by first using SchreierSims.";
DoubleTransversal::usage="DoubleTransversal[SGS, dummysets] returns a sorted list of permutations containing the canonical representative of each double coset of SGS and the D-group of the dummysets in the symmetric group of degree deg.";


Begin["`Private`"]


$xPermNames=Names["Peanotica`xPerm`*"];


(************** 2. Permutations. Notation dependent **************)


If[$ReadingVerbose,Print["Reading section 2: Permutations. Notation dependent"],Null,Null]


If[System`$VersionNumber<8.,

PermutationListQ[list_]:=SameQ[Sort[list],Range[Length[list]]];

PermutationCyclesQ[System`Cycles[{cycs___}]]:=Union[cycs]===Sort[Join[cycs]];

lastFalse[{___,{pos_}}]:=pos;
lastFalse[{}]:=0;
PermutationMax[list_List]:=lastFalse[Position[Inner[SameQ,list,Sort[list],List],False]];

InversePermutation[list_List]:=Ordering[list];
InversePermutation[System`Cycles[cyclist_List]]:=System`Cycles[Reverse/@cyclist];
]


SetAttributes[IfMathematica789,HoldAll];
IfMathematica789[expr7_,expr8_,expr9_]:=Which[
System`$VersionNumber<7.5,expr7,
System`$VersionNumber<8.5,expr8,
True,expr9
];


IfMathematica789[
(* Mathematica 7*)
addsingletons[cycs_,length_]:=Join[cycs,Partition[Complement[Range@length,Flatten@cycs],1]];PermutationFromCycles[cycs_List]:=Last/@Sort@Transpose[Flatten/@{RotateRight/@cycs,cycs}];
PermList[System`Cycles[cycslist_List],length_]:=PermutationFromCycles[addsingletons[cycslist,length]],
(* Mathematica 8 *)
PermList[System`Cycles[cycslist_List], length_]:=PermutationList[System`Cycles[Reverse/@cycslist],length],
(* Mathematica 9 *)
PermList[perm_System`Cycles,length_]:=PermutationList[perm,length]
]


IfMathematica789[
(* Mathematica 7*)
PermList[list_List,length_]:=Take[list~Join~Range[Length[list]+1,length],length],
(* Mathematica 8 *)
PermList[list_List, length_]:=PermutationList[list,length],
(* Mathematica 9 *)
PermList[list_List,length_]:=PermutationList[list,length]
]


IfMathematica789[
(* Mathematica 7 *)
ToCycles[list_List]:=Take[#,Position[Rest@#,First@#][[1,1]]]&/@Fold[If[MemberQ[Flatten@#1,#2],#1,Append[#1,NestList[list[[#1]]&,#2,Length@list]]]&,{},list];
PermCycles[list_List]:=System`Cycles[Reverse/@ToCycles[Ordering@list]],
(* Mathematica 8 *)
PermCycles[list_List]:=Map[Reverse,PermutationCycles[list],{2}],
(* Mathematica 9 *)
PermCycles[list_List]:=PermutationCycles[list]
]


IfMathematica789[
(* Mathematica 7 *)
PermutationProduct[System`Cycles[cycslist1_],System`Cycles[cycslist2_]]:=System`Cycles[List@@TranslatePerm[PermProduct[TranslatePerm[Cycles@@cycslist1,Rules],TranslatePerm[Cycles@@cycslist2,Rules]],Cycles]],
(* Mathematica 8 *)
Null,
(* Mathematica 9 *)
Null
]


Clear[test,testcounter,testresult];
SetAttributes[test,HoldAll];
testcounter[symbol_]:=(testcounter[symbol]=0);
test[symbol_,expr_,result_,messageQ_:False]:=
{symbol,1+testcounter[symbol]++,testresult[symbol,testcounter[symbol]]=Check[SameQ[expr,result],messageQ]};


alltests[]:=Last/@DownValues[testresult];
alltests[symbol_]:=
Cases[DownValues[testresult],_[_[testresult[symbol,_]],result_]:>result]


MathToxPermSym[sym:(_Symmetric|_Antisymmetric)]:=SchreierSims[{},sym];
MathToxPermSym[gs:{___List}]:=SchreierSims[{},GenSet@@(MathToxPermGen/@gs)];


MathToxPermGen[{System`Cycles[{cycs___}],sign_}]:=sign Cycles[cycs];


xPermToMathSym[sym:(_Symmetric|_Antisymmetric)]:=sym;
xPermToMathSym[StrongGenSet[base_List,GS_GenSet]]:={xPermToMathGen/@List@@GS,base};


xPermToMathGen[sign_. Cycles[cycs___]]:={System`Cycles[{cycs}],sign};


PermQ[ID]:=True;


PermQ[Perm[list:{___Integer}]]:=PermutationListQ[list];


PermQ[Images[list:{___Integer}]]:=PermutationListQ[list];


PermQ[Cycles[cycs:{___Integer?Positive}...]]:=PermutationCyclesQ[System`Cycles[{cycs}]];


PermQ[Rules[rules:Rule[_Integer?Positive,_Integer?Positive]..]]:=Apply[Union[#1]===Sort[Join[#2]]&,Transpose[{rules}/.Rule->List]
];PermQ[Rules[]]:=True;


PermQ[expr_Plus]:=Apply[And,PermQ/@List@@expr];
PermQ[x_?NumericQ perm_]:=PermQ[perm];


PermQ[_]:=False;


Protect[PermQ];


Cycles[cycs1___,{},cycs2___]:=Cycles[cycs1,cycs2];
Cycles[cycs1___,{_},cycs2___]:=Cycles[cycs1,cycs2];


Rules[rules1___,HoldPattern[x_->x_],rules2___]:=Rules[rules1,rules2];


Protect[Perm,Cycles,Rules,Images];


SortCycles[perm_Cycles]:=Sort[SortCycle/@perm];
SortCycle[cyc_List]:=Nest[RotateLeft,cyc,Position[cyc,Min[cyc]][[1,1]]-1];
SortCycles[expr_]:=expr/.perm_Cycles:>SortCycles[perm];


(* Unprotect[Cycles];
MakeBoxes[Cycles[],StandardForm]:=xAct`xTensor`Private`interpretbox[Cycles[],"id"];
formatcycle[cyc_List]:=RowBox@Join[{"("},Riffle[MakeBoxes/@cyc,","],{")"}];
MakeBoxes[-Cycles[cycs__List],StandardForm]:=xAct`xTensor`Private`interpretbox[-Cycles[cycs],RowBox[Prepend[formatcycle/@{cycs},"-"]]];
MakeBoxes[coeff_?NumberQ Cycles[cycs__List],StandardForm]:=xAct`xTensor`Private`interpretbox[coeff Cycles[cycs],RowBox[Prepend[formatcycle/@{cycs},MakeBoxes[coeff,StandardForm]]]];
MakeBoxes[Cycles[cycs__List],StandardForm]:=xAct`xTensor`Private`interpretbox[Cycles[cycs],RowBox[formatcycle/@{cycs}]];
Protect[Cycles]; *)


PermLength[Perm[list_List]]:=Length[list];
PermLength[Images[list_List]]:=Length[list];


PermLength[group_Group]:=Max[0,PermLength/@List@@group]
PermLength[GS_GenSet]:=Max[0,PermLength/@List@@GS];
PermLength[StrongGenSet[base_,GS_]]:=Max[base,PermLength[GS]];
PermLength[Symmetric[list_]]:=Max[0,list];
PermLength[Antisymmetric[list_]]:=Max[0,list];


PermLength[x_]:=PermDeg[x];
Protect[PermLength];


PermDeg[Perm[list_List]]:=PermutationMax[list];
PermDeg[Images[list_List]]:=PermutationMax[list];
PermDeg[Cycles[cycs___List]]:=Max[0,cycs];
PermDeg[Rules[rules___Rule]]:=Max[0,Apply[List,{rules},{1}]];
PermDeg[ID]:=0;


PermDeg[Group[perms___]]:=Max[0,PermDeg/@{perms}];
PermDeg[GenSet[perms___]]:=Max[0,PermDeg/@{perms}];
PermDeg[StrongGenSet[_List,GenSet[perms___]]]:=Max[0,PermDeg/@{perms}];
PermDeg[Symmetric[list_]]:=Max[0,list];
PermDeg[Antisymmetric[list_]]:=Max[0,list];


PermDeg[expr_Plus]:=Max[PermDeg/@List@@expr];
PermDeg[x_?NumericQ perm_]:=PermDeg[perm];


PermDeg[x_] := Null /; (Message[PermDeg::undef,"degree",x]; False);
Protect[PermDeg];


NotationOfPerm[-g_]:=NotationOfPerm[g];
NotationOfPerm[g:Perm[_List]]:={Perm,PermLength[g]};
NotationOfPerm[g:Images[_List]]:={Images,PermLength[g]};
NotationOfPerm[Cycles[___List]]:=Cycles;
NotationOfPerm[Rules[___Rule]]:=Rules;
NotationOfPerm[GenSet[]]:=Cycles;
NotationOfPerm[GenSet[perm_,___]]:=NotationOfPerm[perm];
NotationOfPerm[StrongGenSet[_,GS_]]:=NotationOfPerm[GS];
(* Arbitrary decision *)
NotationOfPerm[Symmetric[list_]]:=Cycles;
NotationOfPerm[Antisymmetric[list_]]:=Cycles;


NotationOfPerm[x_]:= Null /; (Message[NotationOfPerm::undef,"notation",x]; False);
Protect[NotationOfPerm];


ID[Perm[list_List]]:=Perm[Sort@list];
ID[Images[list_List]]:=Images[Sort@list];
ID[Cycles[___List]]:=Cycles[];
ID[Rules[___Rule]]:=Rules[];
ID[ID]:=ID;


ID[x_?NumericQ perm_]:=ID[perm];


ID[x_]:=Throw[Message[ID::undef,"identity for notation",x]];
Protect[ID];


TranslatePerm[set:(_List|_GenSet|_Group|_Coset),notation_]:=TranslatePerm[#,notation]&/@set;
TranslatePerm[StrongGenSet[base_List,GS_GenSet],notation_]:=StrongGenSet[base,TranslatePerm[GS,notation]];
TranslatePerm[Symmetric[list_],notation_]:=Symmetric[list];
TranslatePerm[Antisymmetric[list_],notation_]:=Antisymmetric[list];


TranslatePerm[expr_Plus,notation_]:=TranslatePerm[#,notation]&/@expr;
TranslatePerm[x_?NumericQ perm_,notation_]:=x TranslatePerm[perm,notation];


TranslatePerm[perm_,Perm]:=TranslatePerm[perm,{Perm,PermLength@perm}];
TranslatePerm[perm_,Images]:=TranslatePerm[perm,{Images,PermLength@perm}];


TranslatePerm[ID,{Perm,length_Integer}]:=Perm[Range[length]];
TranslatePerm[ID,{Images,length_Integer}]:=Images[Range[length]];
TranslatePerm[ID,Cycles]:=Cycles[];
TranslatePerm[ID,Rules]:=Rules[];


TranslatePerm[perm:Cycles[___List],Cycles]:=Cycles@@First[System`Cycles[List@@perm]];


TranslatePerm[Cycles[cycs___List],Rules]:=Inner[Rule,Flatten[{cycs}],Flatten[RotateLeft/@{cycs}],Rules];


TranslatePerm[Cycles[cycs___List],{Perm,length_Integer}]:=Perm@PermList[System`Cycles[Reverse/@{cycs}],length]


TranslatePerm[perm:Cycles[cycs___List],{Images,length_Integer}]:=Images@PermList[System`Cycles[{cycs}],length];


TranslatePerm[Perm[list_List],{Perm,length_Integer}]:=Perm@PermList[list,length];


TranslatePerm[Perm[list_List],Cycles]:=Cycles@@(Reverse/@First[PermCycles[list]]);


TranslatePerm[perm:Perm[_List],Rules]:=TranslatePerm[TranslatePerm[perm,Cycles],Rules];


TranslatePerm[Perm[list_List],{Images,length_Integer}]:=Images[Take[Ordering[list]~Join~Range[Length[list]+1,length],length]];


TranslatePerm[perm:Rules[___Rule],Rules]:=perm;


m1c[{n_,n_},_]:=Sequence[];
m1c[{n_,other__,n_},_]:={n,other};
m1c[{other___,n_},rules_]:=m1c[{other,n,n/.rules},rules];
mcs[{n_,other___},rules_,{cycs___}]:=With[{cyc=m1c[{n},rules]},mcs[Complement[{other},cyc],rules,{cycs,cyc}]];
mcs[{},_,cycs_]:=cycs;
TranslatePerm[Rules[rules___Rule],Cycles]:=Cycles@@mcs[Range[PermDeg[Rules[rules]]],{rules},{}];


TranslatePerm[Rules[rules___Rule],{Perm,length_Integer}]:=Perm@ReplacePart[Range@length,First/@{rules},({#[[2]]}&/@{rules})/.{}->{{}},Partition[Range[Length[{rules}]],1]/.{}->{{}}]


TranslatePerm[perm:Rules[___Rule],{Images,length_Integer}]:=Images[Range@length/.List@@perm]


TranslatePerm[Images[list_List],{Images,length_Integer}]:=Images[Take[list~Join~Range[Length[list]+1,length],length]];


TranslatePerm[Images[list_List],{Perm,length_}]:=Perm[Take[Ordering[list]~Join~Range[Length[list]+1,length],length]];


TranslatePerm[Images[list_List],Cycles]:=Cycles@@First[PermCycles[list]];


TranslatePerm[Images[perm_List],Rules]:=Inner[Rule,Sort[perm],perm,Rules];


TranslatePerm[perm_,Cycles|Rules|Perm|Images|{Perm,_Integer}|{Images,_Integer}]:=Throw[Message[TranslatePerm::invalid,perm,"permutation"]];
TranslatePerm[_,notation_]:=Throw[Message[TranslatePerm::unknown,"notation",notation]];
Protect[TranslatePerm];


OnPoints[list_List,perm_]:=OnPoints[#,perm]&/@list;


OnPoints[p_Integer,ID]:=p;


(* Sign points *)
OnPoints[0,-perm_]:=-1;
OnPoints[-1,-perm_]:=0;
OnPoints[0,perm_]:=0;
OnPoints[-1,perm_]:=-1;
(* Other points *)
OnPoints[p_Integer,-perm_]:=OnPoints[p,perm];


OnPoints[p_Integer,Perm[list:{___,p_,___}]]:=Position[list,p][[1,1]];
OnPoints[p_Integer,Perm[{___}]]:=p;


OnPoints[p_Integer,Rules[rules___Rule]]:=p/.{rules};


OnPoints[p_Integer,Cycles[___List,{___,p_,q_,___},___List]]:=q;
OnPoints[p_Integer,Cycles[___List,{q_,___,p_},___List]]:=q;
OnPoints[p_Integer,Cycles[___List]]:=p;


OnPoints[p_Integer,Images[list:{___,p_,___}]]:=list[[p]];
OnPoints[p_Integer,Images[{___}]]:=p;


Protect[OnPoints];


PermProduct[perm1_,expr_Plus]:=PermProduct[perm1,#]&/@expr;
PermProduct[expr_Plus,perm2_]:=PermProduct[#,perm2]&/@expr;
PermProduct[x_?NumericQ perm1_,perm2_]:=x PermProduct[perm1,perm2];
PermProduct[perm1_,y_?NumericQ perm2_]:=y PermProduct[perm1,perm2];
PermProduct[perm_,0]:=0;
PermProduct[0,perm_]:=0;


PermProduct[]:=ID;
PermProduct[perm_,ID]:=perm;
PermProduct[ID,perm_]:=perm;


PermProduct[perm_]:=perm;


PermProduct[perm1_,perm2_,perm3__]:=PermProduct[perm1,PermProduct[perm2,perm3]];


PermProduct[list_List,perm_]:=PermProduct[#,perm]&/@list;
PermProduct[perm_,list_List]:=PermProduct[perm,#]&/@list;


PermProduct[Perm[list_List],Perm[{}]]:=Perm[list];
PermProduct[Perm[{}],Perm[list_List]]:=Perm[list];
PermProduct[Perm[list1_List],Perm[list2_List]]:=Perm[list1[[list2]]];


PermProduct[Rules[r1___Rule],Rules[r2___Rule]]:=Inner[Rule,#,#/.{r1}/.{r2},Rules]&@Union[Level[{r1,r2},{-1}]]


PermProduct[Cycles[],perm:Cycles[___List]]:=perm;
PermProduct[perm:Cycles[___List],Cycles[]]:=perm;
PermProduct[Cycles[cycs1___],Cycles[cycs2___]]:=Apply[Cycles,First@PermutationProduct[System`Cycles[{cycs1}],System`Cycles[{cycs2}]]];


PermProduct[Images[list1_List],Images[list2_List]]:=Images[Union[list1,list2]/.Inner[Rule,Sort[list1],list1,List]/.Inner[Rule,Sort[list2],list2,List]];


PermProduct[Group[perms___],perm_]:=If[Or@@(PermEqual[#,perm]&/@{perms}),Group[perms],PermProduct[#,perm]&/@Coset[perms]];
PermProduct[perm_,Group[perms___]]:=If[Or@@(PermEqual[perm,#]&/@{perms}),Group[perms],PermProduct[perm,#]&/@Coset[perms]];


Protection[PermProduct];


InversePerm[Perm[list_List]]:=Perm[InversePermutation[list]];
InversePerm[Images[list_List]]:=Images[InversePermutation[list]];
InversePerm[Cycles[cycs___List]]:=Apply[Cycles,First@InversePermutation[System`Cycles[{cycs}]]];
InversePerm[Rules[rules___Rule]]:=Reverse/@Rules[rules];
InversePerm[ID]:=ID;
InversePerm[x_?NumericQ perm_]:=1/x InversePerm[perm];
Protect[InversePerm];


PermuteList[list_,-perm_]:=PermuteList[list,perm];
PermuteList[list_,perm_]:=list[[First@TranslatePerm[perm,{Perm,Length[list]}]]];
Protect[PermuteList];


PermutationFromTo::diff="Lists`1` and `2` contain different points";
PermutationFromTo[list1_List,list2_List]:=PermProduct[InversePerm@Images@Ordering[list1],Images@Ordering[list2]]/;Sort[list1]===Sort[list2];
PermutationFromTo[list1:head_[___],list2:head_[___]]:=PermutationFromTo[List@@list1,List@@list2];
PermutationFromTo[list1_List,list2_List]:=Throw[Message[PermutationFromTo::diff,list1,list2]]/;Sort[list1]=!=Sort[list2];
Protect[PermutationFromTo];


rand[n_]:=Random[Integer,{1,n}];
addrand[n_][list_List,m_]:=Append[list,Complement[Range[n],list][[rand[m]]]];
RandomPerm[deg_]:=RandomPerm[deg,Perm];
RandomPerm[deg_,Perm]:=Perm[Fold[addrand[deg],{},Reverse@Range[deg]]];
RandomPerm[deg_,notation_]:=TranslatePerm[RandomPerm[deg,Perm],notation];
Protect[RandomPerm];


PermSignature[Perm[list_List]]:=Signature[list];
PermSignature[Images[list_List]]:=Signature[list];
signofcycle[cyc_List]:=-(-1)^Length[cyc];
PermSignature[perm_Cycles]:=Apply[Times,signofcycle/@perm];
PermSignature[perm_Rules]:=PermSignature[TranslatePerm[perm,Cycles]];
PermSignature[-perm_]:=-PermSignature[perm];
Protect[PermSignature];


(************* 3. Permutations. Notation independent *************)


If[$ReadingVerbose,Print["Reading section 3: Permutations. Notation independent"],Null,Null]


SortB[list_List,B_List]:=DeleteCases[B,Alternatives@@Complement[B,list]]~Join~Complement[list,B];
SortB[list:{_},_]:=list;
SortB[{},_]:={};
SortB[list_List]:=Sort[list];
Protect[SortB];


MinB[{___,p_,___},{p_,___}]:=p;
MinB[list_List,{_,x___}]:=MinB[list,{x}];
MinB[list_List,{}]:=Min[list];
MinB[list_List]:=Min[list];
Protect[MinB];


LessB[{x_,y_,z__},base_List]:=LessB[{x,y},base]&&LessB[{y,z},base];
LessB[{x_,x_},_List]:=False;
LessB[{x_,y_},{___,x_,___,y_,___}]:=True;
LessB[{x_,y_},{___,y_,___,x_,___}]:=False;
LessB[{x_,_},{___,x_,___}]:=True;
LessB[{_,x_},{___,x_,___}]:=False;
LessB[{x_,y_},_List]:=Less[x,y];


PermOrderedQ[head_[perm1_/;Head[perm1]=!=Times,-perm2_]]:=True;
PermOrderedQ[head_[-perm1_,perm2_/;Head[perm2]=!=Times]]:=False;
PermOrderedQ[head_[-perm1_,-perm2_]]:=PermOrderedQ[head[perm1,perm2]];


PermOrderedQ[head_[perm1_,perm2_]]:=Module[{len=Max[PermLength@perm1,PermLength@perm2],i,p1,p2},
For[i=1,i<=len,++i,
p1=OnPoints[i,perm1];
p2=OnPoints[i,perm2];
Which[
p1<p2,Return[True],
p1>p2,Return[False]]
];
Null
];


PermOrderedQ[head_[perm1_,perm2_],base_List]:=Module[{len=Max[PermLength@perm1,PermLength@perm2],i,b,p1,p2,base2},
base2=Join[base,Complement[Range[len],base]];
For[i=1,i<=len,++i,b=base2[[i]];
p1=OnPoints[b,perm1];
p2=OnPoints[b,perm2];
Which[
LessB[{p1,p2},base],Return[True],
LessB[{p2,p1},base],Return[False]]
];
Null
];


PermSort[head_[perms___]]:=head@@Sort[{perms},PermOrderedQ[{#1,#2}]&];
PermSort[head_[perms___],base_List]:=head@@Sort[{perms},PermOrderedQ[{#1,#2},base]&];


Protect[PermOrderedQ,PermSort];


PermEqual[perms__]:=And@@(#===Null&/@PermOrderedQ/@Partition[{perms},2,1]);
PermLessEqual[perms__]:={perms}===PermSort[{perms}];
PermGreaterEqual[perms__]:=PermLessEqual@@Reverse[{perms}];
PermLess[perms__]:=And@@(PermOrderedQ/@Partition[{perms},2,1]/.Null->False);
PermGreater[perms__]:=PermLess@@Reverse[{perms}];
Protect[PermEqual,PermLess,PermGreater,PermLessEqual,PermGreaterEqual];


GenSet[x___,GenSet[y___],z___]:=GenSet[x,y,z];


GenSet[StrongGenSet[_,GS_GenSet]]:=GS;


GenSet[(Symmetric|Antisymmetric)[{}|{_}]]:=GenSet[];
GenSet[Symmetric[list_List]]:=DeleteDuplicates@GenSet[Cycles[list[[{1,2}]]],Cycles[list]];
GenSet[Antisymmetric[list_List]]:=DeleteDuplicates@GenSet[-Cycles[list[[{1,2}]]],-(-1)^Length[list]Cycles[list]];


Protect[GenSet];


PermPower[perm_,0]:=ID[perm];
PermPower[perm_,1]:=perm;
PermPower[perm_,n_Integer?Negative]:=PermPower[InversePerm[perm],-n];
PermPower[perm_,n_Integer?Positive]:=PermProduct@@Table[perm,{n}]/;n<10;
PermPower[perm_,n_Integer?Positive]:=With[{m=IntegerPart[n/2]},PermProduct[PermPower[perm,m],PermPower[perm,n-m]]];
PermPower[perm_,n_Integer?Positive]:=Module[{pp,p},
pp[p_,0]:=IP[p];
pp[p_,1]:=p;
pp[p_,k_]:=pp[p,k]=With[{m=IntegerPart[k/2]},PermProduct[pp[p,m],pp[p,k-m]]];
pp[perm,n]
];
Protect[PermPower];


cyclicGroup[perm_]:=With[{id=ID[perm]},Group@@RotateRight[NestWhileList[PermProduct[#,perm]&,perm,(#=!=id)&]]];
RightCoset[subgroup_Group,perm_]:=Map[PermProduct[#,perm]&,subgroup];
MakeGroup[H_Group,gs_GenSet]:=If[MemberQ[H,Last[gs]],H,Module[{
ordH=Length[H],
elts=H~Join~RightCoset[H,Last[gs]],
ncoset=2},
While[ordH ncoset<=Length[elts],
Map[If[FreeQ[elts,#],elts=elts~Join~RightCoset[H,#]]&,Map[PermProduct[elts[[ordH (ncoset-1)+1]],#]&,gs]];
++ncoset
];
elts]];


Dimino[StrongGenSet[base_,GS_GenSet]]:=Dimino[GS];
Dimino[GS_GenSet]:=Module[{GSPerm=TranslatePerm[GS,{Perm,PermDeg[GS]}]},
TranslatePerm[
Fold[
MakeGroup[#1,#2]&,
cyclicGroup[First[GSPerm]],
Rest[FoldList[Append[#1,#2]&,GenSet[First[GSPerm]],List@@Rest[GSPerm]]]
],NotationOfPerm[GS]]
];
Dimino[GenSet[]]:=Group[ID];


Dimino[Symmetric[list_]]:=Group@@(Images/@Permutations[list]);
Dimino[Antisymmetric[list_]]:=Group@@((Signature[#]Images[#])&/@Permutations[list]);


Protect[Dimino,Group];


StablePoints[perm_]:=StablePoints[perm,PermLength[perm]];
StablePoints[perm_?PermQ,n_Integer]:=Cases[Transpose[{Range[n],OnPoints[Range[n],perm]}],{x_,x_}->x];
StablePoints[GS:GenSet[__],n_Integer]:=Intersection@@(StablePoints[#,n]&/@GS);
StablePoints[GenSet[],n_Integer]:=Range[n];
Protect[StablePoints];


NonStablePoints[initB_List,GenSet[]]:=initB
NonStablePoints[initB_List,GS:GenSet[__]]:=Module[{
SubsetQ,stablepoints,base=initB,candidates,check,len=PermLength[GS]},
SubsetQ[large_List,small_List]:=And@@(MemberQ[large,#]&)/@small;
stablepoints=List@@(StablePoints[#,len]&/@GS);
candidates=DeleteCases[Join[Complement[Range[len],#],#]&@Flatten[Union/@Sort@Split@Sort@Flatten@stablepoints],Alternatives@@Intersection@@stablepoints];
While[True,
candidates=DeleteCases[candidates,Alternatives@@base];
check=SubsetQ[#,base]&/@stablepoints;
Which[
(* Correct base. Break while loop *)
Not[Or@@check],Break[],
(* No candidate elements. Throw error *)
candidates==={},Throw["Unable to find enough nonstable points."],
(* Incorrect base. Add new point *)
True,For [i=1,i<=Length[check],++i,
If[check[[i]],AppendTo[base,First[Cases[candidates,x_/;FreeQ[stablepoints[[i]],x]]]];Break[]]
]]];
base
];
Protect[NonStablePoints];


Stabilizer[{},GS_GenSet]:=GS;
Stabilizer[{p_Integer},GS_GenSet]:=GenSet@@Cases[{p,OnPoints[p,#],#}&/@GS,{x_,x_,y_}->y];
Stabilizer[{p1_Integer,ps__Integer},GS_GenSet]:=Stabilizer[{ps},Stabilizer[{p1},GS]];
Stabilizer[ps_List,Symmetric[list_]]:=Symmetric[Complement[list,ps]];
Stabilizer[ps_List,Antisymmetric[list_]]:=Antisymmetric[Complement[list,ps]];

SetStabilizer[{},GS_GenSet]:=GS;
SetStabilizer[{p_Integer},GS_GenSet]:=Stabilizer[{p},GS];
SetStabilizer[list_List,GS_GenSet]:=setstabilizer[list,GS,GenSet[]];
setstabilizer[list_List,GenSet[perm1_,perms___],GenSet[stperms___]]:=setstabilizer[list,GenSet[perms],If[Complement[OnPoints[list,perm1],list]==={},GenSet[stperms,perm1],GenSet[stperms]]];
setstabilizer[list_List,GenSet[],stableGS_]:=stableGS;


(*************** 4. Strong generating sets ***************)


If[$ReadingVerbose,Print["Reading section 4: Strong generating sets"],Null,Null]


(* Driver *)
Options[Orbit]={MathLink:>$xpermQ};
Orbit[point_Integer?Positive,StrongGenSet[base_,GS_],options___]:=Orbit[point,GS,options];
Orbit[point_Integer?Positive,GenSet[],options___]:={point};
Orbit[point_Integer?Positive,GS_GenSet,options:OptionsPattern[]]:=If[OptionValue[MathLink],MathLinkOrbit,MathOrbit][point,GS];
Orbit[point_Integer?Positive,GS_] := Null /; (Message[Orbit::unknown,"generating set",GS]; False);
Orbit[point_,GS_] := Null /; (Message[Orbit::unknown,"point",point]; False);
Protect[Orbit];
(* Mathematica code *)
MathOrbit[point_,GS_]:=Module[{orbit={point},np=1,gamma,newgamma},
While[np<=Length[orbit],
gamma=orbit[[np]];
Map[If[FreeQ[orbit,newgamma=OnPoints[gamma,#]],AppendTo[orbit,newgamma]]&,GS];
++np];
orbit
];


(* Compute PermLength if not given *)
Orbits[gs_,initpoints_List:{}]:=Orbits[gs,PermLength[gs],initpoints];
(* Transform SGS to GS *)
Orbits[StrongGenSet[base_,GS_],len_Integer,initpoints_List:{}]:=Orbits[GS,len,initpoints];
(* Compute orbits using Orbit *)
Orbits[GS_GenSet,len_Integer,initpoints_List:{}]:=Fold[If[MemberQ[Flatten[#1],#2],#1,Append[#1,Orbit[#2,GS]]]&,{},initpoints~Join~Complement[Range[len],initpoints]];
(* Special cases *)
Orbits[(Symmetric|Antisymmetric)[list_List],len_Integer]:=Join[{list},List/@Complement[Range[len],list]];
Orbits[(Symmetric|Antisymmetric)[list_List],len_Integer,initpoints_List]:=Join[{SortB[list,initpoints]},List/@Complement[Range[len],list]];
Protect[Orbits];


Protect[Schreier];


SchreierOrbit[point_Integer,StrongGenSet[B_List,GS_GenSet],deg_Integer]:=SchreierOrbit[MinB[Orbit[point,GS],B],GS,deg];


SchreierOrbit[point_Integer,sym:(_Symmetric|_Antisymmetric),deg_Integer]:=SchreierOrbit[point,SchreierSims[{},sym],deg];


SchreierOrbit::infty="Found Infinity as a point, with generating set `1`.";


SchreierOrbit[point_Integer?Positive,gs_]:=SchreierOrbit[point,gs,PermLength[gs]];
SchreierOrbit[point_Integer?Positive,GenSet[],len_Integer]:=Schreier[{point},Table[0,{len}],Table[0,{len}]];
SchreierOrbit[point_Integer?Positive,GS:GenSet[__],len_Integer]:=Module[{orbit={point},nu,w,np=1,gamma,newgamma},
nu=w=Table[0,{len}];
While[np<=Length[orbit],
gamma=orbit[[np]];
Map[If[FreeQ[orbit,newgamma=OnPoints[gamma,#]],AppendTo[orbit,newgamma];nu[[newgamma]]=#;w[[newgamma]]=gamma]&,GS];
++np];
Schreier[orbit,nu,w]
];
SchreierOrbit[Infinity,GS_,len_] := Null /; (Message[SchreierOrbit::infty,GS]; False);
Protect[SchreierOrbit];


(* Compute len from gs using PermLength *)
SchreierOrbits[gs_,initpoints_List:{}]:=SchreierOrbits[gs,PermLength[gs],initpoints];
(* GS: Compute all Schreier orbits from SchreierOrbit and Orbits *)
SchreierOrbits[GS:GenSet[__],len_Integer,initpoints_List:{}]:=Schreier[Sequence@@#[[1]],Plus@@#[[2]],Plus@@#[[3]]]&@Transpose[SchreierOrbit[#,GS,len]&/@First/@Orbits[GS,len,initpoints]/.Schreier->List];
(* SGS: Use base as initpoints if initpoints not specified *)
SchreierOrbits[StrongGenSet[B_List,GS:GenSet[__]],len_Integer]:=SchreierOrbits[GS,len,B];
SchreierOrbits[StrongGenSet[B_List,GS:GenSet[__]],len_Integer,initpoints_List]:=SchreierOrbits[GS,len,initpoints];
(* Special cases *)
SchreierOrbits[sym:(_Symmetric|_Antisymmetric),len_Integer,initpoints_:{}]:=SchreierOrbits[GenSet[sym],len,initpoints];
SchreierOrbits[GenSet[],len_Integer,initpoints_:{}]:=Schreier[Sequence@@Orbits[GenSet[],len],Table[0,{len}],Table[0,{len}]];
SchreierOrbits[StrongGenSet[_,GenSet[]],len_Integer]:=Schreier[Sequence@@Orbits[GenSet[],len],Table[0,{len}],Table[0,{len}]];
Protect[SchreierOrbits];


TraceSchreier::trace="Cannot trace `1` with orbit `2`";
TraceSchreier[p_Integer,oS:Schreier[orbits___List,nu_,w_]]:=Which[
MemberQ[First/@{orbits},p],ID,
p===0,Throw[Message[TraceSchreier::trace,0,oS]],
Length[nu]<p,ID,
True,PermProduct[TraceSchreier[w[[p]],oS],nu[[p]]]
];
TraceSchreier[p_Integer,os_Schreier] := Null /; (Message[TraceSchreier::trace,p,os]; False);
Protect[TraceSchreier];


Protect[StrongGenSet];

StabilizerChain[StrongGenSet[base_List,GS_GenSet]]:=StrongGenSet[DeleteCases[base,Alternatives@@#],Stabilizer[#,GS]]&/@FoldList[Append[#1,#2]&,{},base];
StabilizerChain[sym:(_Symmetric|_Antisymmetric)]:=StabilizerChain[SchreierSims[{},sym]];
Protect[StabilizerChain];


OrderOfGroup::nobase="Found strong generating set with empty base but generators `1`. Assigning ficticious order 0.";
OrderOfGroup[StrongGenSet[{___},GenSet[]],options___]:=1;
OrderOfGroup[StrongGenSet[{},GS_GenSet]]:=(Message[OrderOfGroup::nobase,GS];0);
OrderOfGroup[StrongGenSet[base_List,GS_GenSet],options___]:=Length[Orbit[First[base],GS,options]]OrderOfGroup[StrongGenSet[Rest[base],Stabilizer[{First[base]},GS]],options];
OrderOfGroup[(Symmetric|Antisymmetric)[list_List]]:=Length[list]!;
Protect[OrderOfGroup];


PermWord[perm_,StrongGenSet[base_List,GS_GenSet],word_List:{}]:=If[Length[base]===0,
Prepend[word,perm],
Module[{
Sorbit=SchreierOrbit[First[base],GS,Max[PermDeg[perm],PermDeg[GS]]],
point=OnPoints[First[base],perm],
u},
If[MemberQ[First[Sorbit],point],
u=TraceSchreier[point,Sorbit];
PermWord[PermProduct[perm,InversePerm[u]],StrongGenSet[Rest[base],Stabilizer[{First[base]},GS]],Prepend[word,u]],
Prepend[word,perm]]
]
];
PermMemberQ[perm_,sym:(_Symmetric|_Antisymmetric)]:=PermMemberQ[perm,SchreierSims[{},sym]];
PermMemberQ[perm_,SGS_]:=With[{id=ID[perm],first=First@PermWord[perm,SGS]},Or[PermEqual[first,id],PermEqual[first===-id]&&MemberQ[SGS[[2]],first]]];
Protect[PermWord,PermMemberQ];


FromBaseImage::noimgs="Invalid list of images `1`.";
FromBaseImage[images_List,sym:(_Symmetric|_Antisymmetric),len___]:=FromBaseImage[images,SchreierSims[{},sym],len];
FromBaseImage[images_List,SGS_StrongGenSet]:=FromBaseImage[images,SGS,PermLength[SGS]];
FromBaseImage[{},StrongGenSet[{},GenSet[]],0]:=ID;
FromBaseImage[images_List,SGS:StrongGenSet[base_List,GS_GenSet],len_Integer]:=Module[{i,u,g=ID,schvecs=SchreierOrbits[#,len]&/@StabilizerChain[SGS],imgs=images},
If[Length[images]=!=Length[base],Throw[Message[FromBaseImage::noimgs,images]]];
For[i=1,i<=Length[imgs],i++,
(* If[MemberQ[schvecs[[i,1]],imgs[[i]]],*)
u=TraceSchreier[imgs[[i]],schvecs[[i]]];
g=PermProduct[u,g];
imgs=OnPoints[imgs,InversePerm[u]]
(* ,
Throw[Message[FromBaseImage::noimgs,images]];
]*)
];
If[imgs==base,g,Throw[Message[FromBaseImage::noimgs,images]]]];
Protect[FromBaseImage];


AllBaseImages[sym:(_Symmetric|_Antisymmetric)]:=AllBaseImages[SchreierSims[{},sym]];
AllBaseImages[SGS:StrongGenSet[base_List,GS_GenSet]]:=AllBaseImages[SGS,PermDeg[SGS]];
AllBaseImages[SGS:StrongGenSet[base_List,GS_GenSet],len_Integer]:=
BaseImage[1,Length[base],{},{ID},SchreierOrbits[#,len]&/@StabilizerChain[SGS]];
Protect[AllBaseImages];


BaseImage[i_,k_,points_List,word_List,schvecs_]:=Module[{g=PermProduct@@word,ig,j,u,Deltab},
If[i==k+1,{Rule[points,g]},
ig=InversePerm[g];
Deltab=OnPoints[schvecs[[i,1]],g];
Flatten[Table[u=TraceSchreier[OnPoints[Deltab[[j]],ig],schvecs[[i]]];
BaseImage[i+1,k,Append[points,Deltab[[j]]],{u,g},schvecs],
{j,1,Length[Deltab]}]]
]
];


Options[Search]={xPermVerbose->False};
Search[SGS_StrongGenSet,property_,s_Integer,SGSK_,options___]:=Search[StabilizerChain[SGS],property,s,SGSK,options];
Search[chain_List,property_,s_Integer,SGSK_,options:OptionsPattern[]]:=Module[{base=chain[[1,1]],newSGSK=SGSK,k=Length[chain]-1,Deltas,gammas,Korbit,verb},
verb=OptionValue[xPermVerbose];
If[s==k+1,
newSGSK=StrongGenSet[base,GenSet[]],
newSGSK=Search[chain,property,s+1,SGSK,options];
Deltas=Orbit[base[[s]],chain[[s,2]]];
If[verb,Print["Branching over points ",Deltas]];
(* Avoid rechecking permutations. Not in Butler's algorithm *)
If[s=!=k,Deltas=Drop[Deltas,1]];
Do[
gammas=Deltas[[j]];
Korbit=Orbit[gammas,newSGSK[[2]]];
If[gammas===MinB[Korbit,base],
newSGSK=Generate[chain,property,s,s+1,Append[Take[base,s-1],gammas],newSGSK,verb];
If[Head[newSGSK]===Times,newSGSK=-newSGSK]]
,{j,1,Length[Deltas]}];
];
newSGSK
];
Protect[Search];


Generate[chain_List,property_,s_,i_,list_,SGSK_,verb_]:=Module[{base=chain[[1,1]],k=Length[chain]-1,g,newSGSK=SGSK,Deltag,gammai,otherSGSK},
g=FromBaseImage[list,ReplacePart[chain[[1]],Take[base,Length[list]],1]];
If[i==k+1,
If[verb,Print["Generate at level ",s," with i=",i,". We have list ",list," and permutation ",g]];
If[Not@PermEqual[g,ID]&&property[g],
newSGSK=-StrongGenSet[First@SGSK,Append[Last[SGSK],g]];
If[verb,Print["  Added permutation ",g]]],
Deltag=OnPoints[Orbit[base[[i]],chain[[i,2]]],g];
If[verb,Print["Generating over points ",Deltag]];
Do[
gammai=Deltag[[j]];
newSGSK=Generate[chain,property,s,i+1,Append[list,gammai],newSGSK,verb];
If[Head[newSGSK]===Times,Break[]],
{j,1,Length[Deltag]}];
];
newSGSK
];


BasicOrbit[j_Integer,k_Integer,StrongGenSet[base_List,GS_GenSet]]:=Orbit[base[[j]],Stabilizer[Take[base,k],GS]]/;k<j;


Interchange[SGS:StrongGenSet[base_List,GS_GenSet],j_Integer,len_Integer]:=Module[{Deltaj,Deltajp1,LDeltaBarjp1,T,Gamma,Delta,gamma,p,g1,g2},
Deltaj=BasicOrbit[j,j-1,SGS];
Deltajp1=BasicOrbit[j+1,j,SGS];
LDeltaBarjp1=Length[Deltajp1]Length[Deltaj]/Length[BasicOrbit[j+1,j-1,SGS]];
T=Stabilizer[Take[base,j+1],GS];
Gamma=Complement[Deltaj,base[[{j,j+1}]]];
Delta={base[[j]]};
While[Length[Delta]!=LDeltaBarjp1,
gamma=First[Gamma];
g1=TraceSchreier[gamma,SchreierOrbit[base[[j]],StrongGenSet[base[[Range[j,Length[base]]]],Stabilizer[base[[Range[j-1]]],GS]],len]];
p=OnPoints[base[[j+1]],InversePerm[g1]];
If[MemberQ[Deltajp1,p],
g2=TraceSchreier[p,SchreierOrbit[base[[j+1]],StrongGenSet[base[[Range[j+1,Length[base]]]],Stabilizer[base[[Range[j]]],GS]],len]];
AppendTo[T,PermProduct[g2,g1]];
Delta=Orbit[base[[j]],T];
Gamma=Complement[Gamma,Delta],
Gamma=Complement[Gamma,Orbit[gamma,T]]
]
];
StrongGenSet[base/.{base[[j]]->base[[j+1]],base[[j+1]]->base[[j]]},Union[GS,T]]
]


BaseChange[SGS_,base_]:=BaseChange[SGS,base,PermLength[SGS]];
BaseChange[StrongGenSet[{},GenSet[]],base_,len_]:=StrongGenSet[base,GenSet[]];
BaseChange[SGS:StrongGenSet[{b_,___},_],{b_},len_]:=SGS;
BaseChange[SGS:StrongGenSet[base_List,GS_GenSet],newbase_List,len_]:=Module[{i=0,j,g=ID,more,B=base,gs=GS,pos,newSGS},
more=If[Length[newbase]>0,
MemberQ[BasicOrbit[1,0,SGS],newbase[[1]]],
False];
While[more,
i=i+1;
g=PermProduct[TraceSchreier[OnPoints[newbase[[i]],InversePerm[g]],SchreierOrbits[SGS,len]],g];
more=(i+1)<=Min[Length[B],Length[newbase]];
If[more,more=MemberQ[OnPoints[newbase[[i+1]],InversePerm[g]],BasicOrbit[i+1,i,SGS]]]
];
If[Not@PermEqual[g,ID],B=OnPoints[B,g];gs=Map[PermProduct[InversePerm[g],#,g]&,gs]];
For[j=i+1,j<=Length[newbase],j++,
If[MemberQ[B,newbase[[j]]],pos=Position[B,newbase[[j]]][[1,1]],AppendTo[B,newbase[[j]]];pos=Length[B]];
While[pos!=j &&pos>1,newSGS=Interchange[StrongGenSet[B,gs],pos-1,len];B=newSGS[[1]];gs=newSGS[[2]];pos=pos-1];
];
StrongGenSet[B,gs]
];
Protect[BaseChange];


DeleteRedundantGenerators[StrongGenSet[base_,GS_GenSet]]:=Module[{Si,Sip1,i,T=GenSet[],orbit,gens,pgens,check,toadd},
Sip1=GenSet[];
check[i_,gen_,t_]:=FreeQ[Orbit[base[[i]],t],OnPoints[base[[i]],gen]];
toadd[i_,gs_,T_]:=Module[{TT=T},If[check[i,#,TT],AppendTo[TT,#]]&/@gs;Complement[TT,T]];
For[i=Length[base],i>=1,i--,
Si=Stabilizer[Take[base,i-1],GS];
gens=Complement[Si,Sip1];
pgens=Permutations[gens];
T=Join[T,First@Sort[toadd[i,#,T]&/@pgens]];
Sip1=Si;
];
StrongGenSet[base,T]
];
Protect[DeleteRedundantGenerators];


SchreierSimsStep[B_List,S_GenSet,len_Integer,i_Integer,T_GenSet,options:OptionsPattern[]]:=Module[{Si,oldSi,genset,Deltai,oldDeltai,gn,j,k,gamma,sn,s,g,gbar,newB,newS,level,n,rules,method,verb,word,junk},

{rules,method,verb}=OptionValue[SchreierSims,{options},{UseRules,Method,xPermVerbose}];

newB=B;newS=S;
If[verb,Print["Schreier-Sims-Step called with B=",B,", S=",S/.rules,", i=",i,", T=",T/.rules]];

(* Original generating sets *)
Si=Stabilizer[Take[B,i-1],S];
oldSi=Complement[Si,T];
(* Basic orbits *)
oldDeltai=SchreierOrbit[B[[i]],oldSi,len];
Deltai=SchreierOrbit[B[[i]],Si,len];
(* Check that Deltai is an extension of oldDeltai *)
For[n=1,n<=Length[oldDeltai[[2]]],++n,
If[(Deltai[[2,n]]=!=oldDeltai[[2,n]])&&(oldDeltai[[2,n]]=!=0),
If[verb,Print["Modifying orbit Delta to extend previous value."]];
Deltai[[2,n]]=oldDeltai[[2,n]];
Deltai[[3,n]]=oldDeltai[[3,n]]
]
];

(* Loop over elements gamma of basic orbit *)
For[gn=1,gn<=Length[Deltai[[1]]],++gn,gamma=Deltai[[1,gn]];

If[MemberQ[First[oldDeltai],gamma],genset=T,genset=Si];

(* Loop sn over generators s in genset *)
For[sn=1,sn<=Length[genset],++sn,s=genset[[sn]];

(* Compute Schreier generator *)
g=PermProduct[TraceSchreier[gamma,Deltai],s,InversePerm[TraceSchreier[OnPoints[gamma,s],Deltai]]];++num;

(* If g is not in subgroup H^(i+1) *)
If[Not@PermMemberQ[g,StrongGenSet[Drop[newB,i],Stabilizer[Take[newB,i],newS]]],
k=Length[newB];
Switch[method,
"Butler3",
gbar=g;
j=1;While[j<=k&&OnPoints[newB[[j]],gbar]===newB[[j]],j++],
"Butler4",
word=PermWord[g,StrongGenSet[Drop[newB,i],Stabilizer[Take[newB,i],newS]]];
gbar=First[word];
j=i+Length[word],
_,
Throw @Print["Unknown Schreier-Sims variant method: ",method]
];
(* Extend GS *)
AppendTo[newS,gbar];
If[verb,Print["Appended S[[",Length[newS],"]]=",gbar/.rules]];
(* Extend base, if necessary, so that no strong generator fixes all base points *)
If[j===k+1,
AppendTo[newB,Min[Complement[Range[len],StablePoints[gbar,len],newB,{0}]]];
If[verb,Print["Appended B[[",k+1,"]]=",Last[newB]]]
];
(* Ensure we still have a base and SGS for H^(i+1) *)
For[level=j,level>=i+1,--level,{newB,newS}=SchreierSimsStep[newB,newS,len,level,GenSet[gbar],options]];
If[verb,Print["Finished check of H(",i+1,") with base ",Drop[newB,i]," and SGS ",Stabilizer[Take[newB,i],newS]/.rules]];
]

]

];{newB,newS}];


(* Main driver *)
Options[SchreierSims]={MathLink:>$xpermQ,UseRules:>{},Method->"Butler3",xPermVerbose->False};
SchreierSims[n_Integer,other__]:=SchreierSims[Range[n],other];
SchreierSims[B_List,Symmetric[inds_List,not_:Peanotica`xPerm`Cycles],options___]:=StrongGenSet[Sort@inds,TranslatePerm[GenSet@@(Peanotica`xPerm`Cycles/@Partition[Sort@inds,2,1]),not]];
SchreierSims[B_List,Antisymmetric[inds_List,not_:Peanotica`xPerm`Cycles],options___]:=StrongGenSet[Sort@inds,TranslatePerm[GenSet@@(-Peanotica`xPerm`Cycles[#]&/@Partition[Sort@inds,2,1]),not]];
SchreierSims[_,GenSet[],len_Integer,options___]:=StrongGenSet[{},GenSet[]];
SchreierSims[B_List,GS_GenSet,options___?OptionQ]:=SchreierSims[B,GS,PermLength[StrongGenSet[B,GS]],options];
SchreierSims[B_List,GS_GenSet,len_Integer,options:OptionsPattern[]]:=If[OptionValue[MathLink],MathLinkSchreierSims,MathSchreierSims][B,GS,len,options];
Protect[SchreierSims];
(* Mathematica code *)
MathSchreierSims[B_List,GS_GenSet,len_Integer,options:OptionsPattern[]]:=Module[{k,base=B,PBQ,genset,i,rules,verb},

{rules,verb}=OptionValue[SchreierSims,{options},{UseRules,xPermVerbose}];

num=0;

(* Eliminate identity from GS, if present *)
genset=DeleteCases[GS,ID[First[GS]]];

(* Check initial base *)
base=NonStablePoints[B,genset];
k=Length[base];

(* Recursive call to SchreierSims *)
For[i=k,i>=1,--i,
{base,genset}=Evaluate[SchreierSimsStep[base,genset,len,i,Stabilizer[Take[base,i-1],genset],options]];
];

(* Results *)
If[verb,Print["Checked ",num," generators and obtained group of order ",OrderOfGroup[StrongGenSet[base,genset]]]];
StrongGenSet[DeleteCases[base,0],genset/.rules]

];


Unprotect[Symmetric];
Symmetric[inds_List,not_]:=StrongGenSet[Sort@inds,TranslatePerm[GenSet@@(Cycles/@Partition[Sort@inds,2,1]),not]];
Symmetric[{},_]:=StrongGenSet[{},GenSet[]];
Protect[Symmetric];


Unprotect[Antisymmetric];
Antisymmetric[inds_List,not_]:=StrongGenSet[Sort@inds,TranslatePerm[GenSet@@(-Cycles[#]&/@Partition[Sort@inds,2,1]),not]];
Antisymmetric[{},_]:=StrongGenSet[{},GenSet[-ID]];
Protect[Antisymmetric];


RiemannSymmetric[inds:{i1_,i2_,i3_,i4_},not_:Cycles]:=StrongGenSet[Sort@inds,TranslatePerm[GenSet[Cycles[{i1,i3},{i2,i4}],-Cycles[{i1,i2}],-Cycles[{i3,i4}]],not]]
RiemannSymmetry=RiemannSymmetric;


addperm[sign:(1|-1)][{p1a_,p1b_},{p2a_,p2b_}]:=GenSet[sign Cycles[{p1a,p2a},{p1b,p2b}]]
addperm[sign:(1|-1)][pair_List,pairs__List]:=GenSet[sign^Length[{pairs}] Cycles@@Transpose@RotateRight[{pair,pairs}],addperm[sign][pairs]]
addperm[_][__]:=GenSet[]


PairSymmetric[pairs:{{_,_}...},sym1_,sym2_,not_:Cycles]:=StrongGenSet[Sort[Flatten@pairs],TranslatePerm[GenSet[addperm[sym1]@@pairs,Switch[sym2,1,GenSet@@Map[Cycles,pairs],-1,GenSet@@Map[-Cycles[#]&,pairs],_,GenSet[]]],not]];


Stabilizer[points_List,SGS_]:=Stabilizer[points,SGS,PermLength[SGS]];
Stabilizer[points_List,SGS:StrongGenSet[base_,GS_],len_Integer]:=StrongGenSet[DeleteCases[#1,Alternatives@@points],Stabilizer[points,#2]]&@@BaseChange[SGS,points~Join~DeleteCases[base,Alternatives@@points],len];
Protect[Stabilizer];


SetStabilizerProperty[points_List,perm_]:=Complement[OnPoints[points,perm],points]==={};


Options[SetStabilizer]={MathLink:>$xpermQ,xPermVerbose->False,OrderedBase->False};


SetStabilizer[points_,sgs:StrongGenSet[base_,GS_],options:OptionsPattern[]]:=If[OptionValue[MathLink],
MathLinkSetStabilizer[points,Max[points,base,PermLength[GS]],sgs,options],Search[StrongGenSet[base,GS],SetStabilizerProperty[points,#]&,1,StrongGenSet[base,GenSet[]],options]];
Protect[SetStabilizer];


(************************ 5. Canonicalization ***********************)


If[$ReadingVerbose,Print["Reading section 5: Canonicalization"],Null,Null]


Options[Peanotica`xPerm`RightCosetRepresentative]={
xPermVerbose->False,
BaseChangeCheck->False,
MathLink:>$xpermQ
};
Peanotica`xPerm`RightCosetRepresentative[p_,len_,sym:(_Symmetric|_Antisymmetric),rest___]:=Peanotica`xPerm`RightCosetRepresentative[p,len,SchreierSims[{},sym],rest];
Peanotica`xPerm`RightCosetRepresentative[p_?PermQ,len_Integer,StrongGenSet[base_List,GS_GenSet],free_List,options:OptionsPattern[]]:=Module[{perm=p,newGS=GS,newbase=base,newbase2,newGS2,newfree=free,Delta,Delta1,Deltap,k,pk,pp,om,bi,n=0,verb,bcc},
{verb,bcc}=OptionValue[{xPermVerbose,BaseChangeCheck}];
If[verb,Print["RIGHT-COSET-REPRESENTATIVE ALGORITHM for ",p]];
If[verb,Print["which corresponds to the index list: ",TranslatePerm[InversePerm[p],{Perm,len}]]];
If[verb,Print["base: ",base]];
(* Loop over elements of base *)
For[i=1,i<=Length[base]&&n<Length[free],++i,
bi=base[[i]];
If[verb,Print["****** Analysing element i=",i," of base: slot ",bi," ******"]];
Delta=SchreierOrbit[bi,StrongGenSet[newbase,newGS],len];
If[verb,Print["Symmetry orbit Delta of slots: ",First[Delta]]];
If[verb,Print["Free slots: ",newfree]];
Delta1=Cases[First[Delta],Alternatives@@newfree];
If[verb,Print["Free slots that can go to that slot: ",Delta1]];
If[Delta1=={},Continue[]];
(* Tensor notation: perm acts on slots giving indices *)
Deltap=OnPoints[Delta1,perm];
If[verb,Print["At those slots we respectively find indices Deltap: ",Deltap]];
k=MinB[Deltap,base];
pk=Position[Deltap,k][[1,1]];
If[verb,Print["The least index is ",k,", found at position pk: ",pk," of Deltap"]];
pp=Delta1[[pk]];
If[verb,Print["That index is found in tensor at slot pp: ",pp]];
om=TraceSchreier[pp,Delta];
If[verb,Print["We can move slot ",pp," to slot ",bi," using permutation om: ",om, " in S"]];
perm=PermProduct[om,perm];
If[verb,Print["New indices list: ",TranslatePerm[InversePerm[perm],{Perm,len}]]];
(* New position of free indices *)
newfree=OnPoints[newfree,InversePerm[om]];
If[verb,Print["Computing stabilizer in S of slot ",bi]];
(* Ensure bi is the first point of the base *)
If[verb,Print["SGS before change: ",StrongGenSet[newbase,newGS]]];
{newbase2,newGS2}=List@@BaseChange[StrongGenSet[newbase,newGS],{bi},len];
If[verb,Print["SGS after change: ",StrongGenSet[newbase2,newGS2]]];
newbase2=Drop[newbase2,1];
newGS2=Stabilizer[{bi},newGS2];
If[verb,Print["SGS after stabilization: ",StrongGenSet[newbase2,newGS2]]];
(* Check necessity of the base change *)
If[bcc&&newbase2=!=DeleteCases[newbase,bi],
Print["Checking ",newbase," vs. ", newbase2," with point ",bi];
Block[{$RecursionLimit=Infinity},If[OrderOfGroup[StrongGenSet[newbase2,newGS2]]=!=OrderOfGroup[StrongGenSet[DeleteCases[newbase,bi],Stabilizer[{bi},newGS]]],Print["Unavoidable base change. Contact JMM."]]]];
newbase=newbase2;newGS=newGS2;
If[verb,Print["newbase after change: ",newbase]];
++n;
];
(* We return the canonical permutation and the new SGS *)
{perm,StrongGenSet[newbase,newGS],newfree}
];
(* Default definition *)
Peanotica`xPerm`RightCosetRepresentative[p_?PermQ,len_Integer,SGS_StrongGenSet,options___]:=Peanotica`xPerm`RightCosetRepresentative[p,len,SGS,Range[len],options];
Protect[Peanotica`xPerm`RightCosetRepresentative];


SGSOfDummySet[DummySet[_,pairs_List,metricQ_Integer]]:=StrongGenSet[First/@pairs,
GenSet@@Join[Switch[metricQ,
1,Cycles/@pairs,
-1,Minus/@Cycles/@pairs,
0,{},
_,Throw[Print["Invalid value for metricQ in SGSOfDummySet."]]
],
Flatten[Cycles@@#&/@Transpose/@Partition[pairs,2,1]]]];
Protect[DummySet];


RemovePairOf[i_Integer,DummySet[m_,pairs_List,metricQ_Integer]]:=DummySet[m,DeleteCases[pairs,{___,i,___}],metricQ];


MovePairOf[i_Integer,DummySet[m_,{p1___,{i_,j_},p2___},metricQ_Integer]]:=DummySet[m,{{i,j},p1,p2},metricQ];
MovePairOf[i_Integer,DummySet[m_,{p1___,{j_,i_},p2___},metricQ_Integer]]:=DummySet[m,Reverse/@{{j,i},p1,p2},metricQ];
MovePairOf[i_Integer,DS_DummySet]:=DS;


RepeatedSet[{}]:=Sequence[];
SGSOfDummySet[RepeatedSet[list_List]]:=SchreierSims[{},Symmetric[list]];
SGSOfDummySet[list_List]:=Apply[JoinSGS,SGSOfDummySet/@list];
MovePairOf[i_Integer,RepeatedSet[{p1___,i_,p2___}]]:=RepeatedSet[{i,p1,p2}];
MovePairOf[i_Integer,repe_RepeatedSet]:=repe;
RemovePairOf[i_Integer,RepeatedSet[{p1___,i_,p2___}]]:=RepeatedSet[{p1,p2}];
RemovePairOf[i_Integer,repe_RepeatedSet]:=repe;
Protect[SGSOfDummySet,RepeatedSet];


JoinSGS[prev___,sym:(_Symmetric|_Antisymmetric),after___]:=JoinSGS[prev,SchreierSims[{},sym],after];
JoinSGS[SGSs__StrongGenSet]:=StrongGenSet[Join@@#1,Union@@#2]&@@Transpose[Apply[List,{SGSs},1]];
JoinSGS[]:=StrongGenSet[{},GenSet[]];


takenext[{a_,other___},{found___},list_List]:=With[{sublist=Complement[Select[list,#<a&],{other}]},takenext[{other},Flatten[{found,sublist,a}],Complement[list,sublist,{a}]]];
takenext[{},found_,list_List]:=Join[found,list];
ExtendBase[base_,dummyslots_,ob_]:=Module[{nondummybase=Complement[base,dummyslots],otherdummyslots=Complement[dummyslots,base]},
If[nondummybase=!={},Print["ExtendBase: non-dummy base points: ",nondummybase]];
If[ob,
takenext[base,{},dummyslots],
Join[Cases[base,Alternatives@@dummyslots],otherdummyslots]
]
];


orbitof[i_Integer,h_[___,list:{___,i_,___},___]]:=list
orbitof[i_Integer,_[___]]:={}


Options[DoubleCosetRepresentative]:={
xPermVerbose->False,
MathLink:>$xpermQ
};
(* 
permutation: degree deg;
base={b1, ..., bk}: base of group S ;
GS: strong generating set of S relative to base ;
dummysets : list of sets (head DummySet or RepeatedSet) of pairs of names
(positions in the canonical configuration) of dummies, or a single list
with repeated indices. Each set is associated with a manifold ;
*)
DoubleCosetRepresentative[permutation_?PermQ,len_Integer,StrongGenSet[base_List,GS_GenSet],dummysets:{(_DummySet|_RepeatedSet)...},options:OptionsPattern[]]:=Module[{n,perm,notation,F1,F2,bS,SGSS,SGSD,i,TAB,ALPHA={{}},nuS,Deltab,DeltaD,IMAGES,p,nuD,Deltap,s,d,NEXT,j,jj,s1,d1,L1,tmp,alphaindices,KS=GS,result,dummyindices,newdummysets=dummysets,dummyslots,bi,pi,ob=OptionValue[CanonicalPerm,{options},OrderedBase],verb=OptionValue[xPermVerbose],bSsort},

(* 1. The routine works in the notation given by permutation. If it is ID, change to Cycles[] *)
perm=permutation/.ID->Peanotica`xPerm`Cycles[];
(* Note that perm is always kept fixed *)
notation=NotationOfPerm[perm];
If[verb,Print["DOUBLE-COSET-REPRESENTATIVE ALGORITHM for ",permutation]];

(* 2. Dummies and repeated indices go into the D group *)
If[verb,Print["index-dummysets: ",dummysets]];
dummyindices=Flatten[dummysets/.{DummySet[_,dums_,_]->dums,RepeatedSet[list_]->list}];
If[verb,Print["dummyindices: ",dummyindices]];
dummyslots=OnPoints[dummyindices,InversePerm[perm]];
If[verb,Print["dummyslots: ",dummyslots]];
newdummysets=dummysets;

(* 3. Extend base. base contains some of the dummy slots, but not all. We must cover all slots of dummies, keeping the order of the points of the original base *)
bS=ExtendBase[base,Sort@dummyslots,ob];
If[verb,Print["Extended ",base," to ",bS]];
SGSS=StrongGenSet[bS,GS];
If[verb,Print["Initial SGSS: ",SGSS]];
(* Initialize base images *)
p=0 bS;

(* 4. Adapt base for sorting of dummy indices. This is a particular choice, for aesthetical reasons *)
bSsort=bS/.Inner[Rule,Sort[bS],Sort[dummyindices],List];
If[verb,Print["base for sorting: ",bSsort]];

(* 5. Initialize TAB *)
TAB[{}]:={ID[perm],ID[perm]};

(* 6. Subroutines. Note that elements of TAB are {s, d} *)
F2[L_,TAB_,g_]:=PermProduct[TAB[L][[1]],g,TAB[L][[2]]];
F1[L_,TAB_,DeltaD_,Deltab_,g_]:=Module[{sgd=F2[L,TAB,g],list},
If[verb,Print["With L=",L," we get sgd: ",sgd]];
list=OnPoints[Deltab,sgd];
If[verb,Print["which maps slots in Deltab to indices list: ",list]];
list=Union[Flatten[Cases[DeltaD,x_/;Intersection[x,list]=!={}]]];
If[verb,Print["whose points belong to orbits ",list]];
list
];

(* 7. Strong Generating Set of group D *)
SGSD=TranslatePerm[JoinSGS@@(SGSOfDummySet/@dummysets),notation];
If[verb,Print["Initial SGSD: ",SGSD]];

(* 8. Main loop on the slots of bS *)
For[i=1,i<=Length[bS],++i,
If[verb,Print["******************* Loop i= ",i," *********************"]];
bi=bS[[i]];
If[verb,Print["Analyzing slot ",bi," of tensor"]];

(* A. Schreier vector of S *)
nuS=SchreierOrbits[SGSS,len];
(* Orbits are ordered according to base of SGSS, which has bi as first element *)
If[verb,Print["nuS: ",nuS," with first element ",bi]];
Deltab=orbitof[bi,Drop[List@@nuS,-2]];
If[verb,Print["Under S, slot ",bi," can go to slots Deltab: ",Deltab]];

(* B. Orbits of D *)
DeltaD=Orbits[SGSD[[2]],len];
If[verb,Print["Orbits of indices under D: DeltaD: ",DeltaD]];

(* C. Images of bi under elements of S.perm.D *)
IMAGES=Union[Flatten[Map[F1[#,TAB,DeltaD,Deltab,perm]&,ALPHA]]];
If[verb,Print["Therefore at slot ",bi," we can have indices IMAGES: ",IMAGES]];
(* The minimal element is taken with respect to bSsort *)
p[[i]]=pi=MinB[IMAGES,bSsort];
If[verb,Print["The least of them is p[[",i,"]]: ",pi]];

(* D. Rearrange SGS of D. Now pi will be the first element of the base *)
newdummysets=MovePairOf[pi,#]&/@newdummysets;
If[verb,Print["Moved pairs ",newdummysets]];
(* Full reconstruction of the SGSD starting from the new dummysets, with reordered indices *)
SGSD=TranslatePerm[JoinSGS@@(SGSOfDummySet/@newdummysets),notation];
If[verb,Print["New SGS of D: ",SGSD]];
(* Schreier vector of D *)
nuD=SchreierOrbits[SGSD,len];
If[verb,Print["with Schreier vector nuD: ",nuD]];
(* Orbit of pi under D *)
Deltap=orbitof[pi,Drop[List@@nuD,-2]];
If[verb,Print["In particular, the orbit of index ",pi," is Deltap: ",Deltap]];

(* E. Calculate ALPHA and TAB. Inner double loop *)
If[verb,Print["Now looking for all permutations sgd that move index ",pi," to slot ",bi]];
alphaindices={};
For[l=1,l<=Length[ALPHA],++l,
If[verb,Print["Loop with l=",l]];
L=ALPHA[[l]];
If[verb,Print["L= ",L]];
s=TAB[L][[1]];
d=TAB[L][[2]];
If[verb,Print["TAB[L] = {s, d} = ",{s,d}]];
If[verb,Print["Calculating NEXT. We need the intersection of sets of slots ", OnPoints[Deltab,s]," and ",OnPoints[Deltap,InversePerm[PermProduct[perm,d]]]]];
NEXT=Intersection[OnPoints[Deltab,s],OnPoints[Deltap,InversePerm[PermProduct[perm,d]]]];
If[verb,Print["Intermediate slots NEXT= ",NEXT]];
For[jj=1,jj<=Length[NEXT],++jj,
j=NEXT[[jj]];
s1=PermProduct[TraceSchreier[OnPoints[j,InversePerm@s],nuS],s];
If[verb,Print["From slot ",bi," to intermediate slot ",j," use s1=", s1]];
d1=PermProduct[d,InversePerm@TraceSchreier[OnPoints[j,PermProduct[perm,d]],nuD]];
If[verb,Print["d1= ",d1]];
L1=Append[L,j];
If[verb,Print["L1= ",L1]];
TAB[L1]={s1,d1};
AppendTo[alphaindices,L1];
If[verb,Print["This gives us the new index configuration: ",TranslatePerm[InversePerm[F2[L1,TAB,perm]],{Perm,len}]]];
(* Checks *)
For[ii=1,ii<=i,++ii,If[OnPoints[bS[[ii]],PermProduct[s1,perm,d1]]=!=p[[ii]],Print["WRONG check of slot ",bS[[ii]]," with point ",p[[ii]]]]
]];
TAB[L]=.;
];
ALPHA=alphaindices;
If[verb,Print["New ALPHA: ",ALPHA]];

(* F. Verify if there are 2 equal permutations of opposite sign in S.perm.D *)
tmp=Map[InversePerm[F2[#,TAB,perm]]&,ALPHA];
If[verb,Print["Checking consistency in set ",tmp]];
If[Intersection[tmp,-tmp]=!={},result=0;Break[]];

(* G. Find the stabilizers S^(i+1) and D^(i+1) *)
SGSS=StrongGenSet[Drop[SGSS[[1]],1],Stabilizer[{bi},SGSS[[2]]]];
If[verb,Print["Removing permutations from SGS of S that move slot ",bi]];
If[verb,Print["New SGS of S: ",SGSS]];
(* Construct new SGSD from the new dummysets, and not by stabilization *)
newdummysets=RemovePairOf[pi,#]&/@newdummysets;
SGSD=TranslatePerm[JoinSGS@@(SGSOfDummySet/@newdummysets),notation];
If[verb,Print["Removing permutations from SGS of D that move index ",pi]];
If[verb,Print["New SGS of D: ",SGSD]];
]; (* End of main loop *);

(* 9. Result *)
Switch[result,
0,0,
_,F2[ALPHA[[1]],TAB,perm]/.If[permutation===ID,Cycles[]->ID,{}]
]

];
Protect[DoubleCosetRepresentative];

DoubleCosetRepresentative2[
    permutation_?PermQ,
    len_Integer,
    StrongGenSet[base_List, GS_GenSet],
    StrongGenSet[basek_List, GSD_GenSet],
    options:OptionsPattern[]
] := Module[
    {n,perm,notation,F1,F2,bS,SGSS,SGSD,i,TAB,ALPHA={{}},nuS,Deltab,DeltaD,IMAGES,p,nuD,Deltap,s,d,NEXT,j,jj,s1,d1,L1,tmp,alphaindices,KS=GS,result,dummyindices,newdummysets=dummysets,dummyslots,bi,pi,ob=OptionValue[CanonicalPerm,{options},OrderedBase],verb=OptionValue[xPermVerbose],bSsort},

    (* 1. The routine works in the notation given by permutation. If it is ID, change to Cycles[] *)
    perm=permutation/.ID->Peanotica`xPerm`Cycles[];
    (* Note that perm is always kept fixed *)
    notation=NotationOfPerm[perm];
    If[verb,Print["DOUBLE-COSET-REPRESENTATIVE ALGORITHM for ",permutation]];

    (* 2. Dummies and repeated indices go into the D group *)
    If[verb,Print["index-dummysets: ",dummysets]];
    dummyindices=Flatten[dummysets/.{DummySet[_,dums_,_]->dums,RepeatedSet[list_]->list}];
    If[verb,Print["dummyindices: ",dummyindices]];
    dummyslots=OnPoints[dummyindices,InversePerm[perm]];
    If[verb,Print["dummyslots: ",dummyslots]];
    newdummysets=dummysets;

    (* 3. Extend base. base contains some of the dummy slots, but not all. We must cover all slots of dummies, keeping the order of the points of the original base *)
    bS=ExtendBase[base,Sort@dummyslots,ob];
    If[verb,Print["Extended ",base," to ",bS]];
    SGSS=StrongGenSet[bS,GS];
    If[verb,Print["Initial SGSS: ",SGSS]];
    (* Initialize base images *)
    p=0 bS;

    (* 4. Adapt base for sorting of dummy indices. This is a particular choice, for aesthetical reasons *)
    bSsort=bS/.Inner[Rule,Sort[bS],Sort[dummyindices],List];
    If[verb,Print["base for sorting: ",bSsort]];

    (* 5. Initialize TAB *)
    TAB[{}]:={ID[perm],ID[perm]};

    (* 6. Subroutines. Note that elements of TAB are {s, d} *)
    F2[L_,TAB_,g_]:=PermProduct[TAB[L][[1]],g,TAB[L][[2]]];
    F1[L_,TAB_,DeltaD_,Deltab_,g_]:=Module[
        {sgd=F2[L,TAB,g],list},
        If[verb,Print["With L=",L," we get sgd: ",sgd]];
        list=OnPoints[Deltab,sgd];
        If[verb,Print["which maps slots in Deltab to indices list: ",list]];
        list=Union[Flatten[Cases[DeltaD,x_/;Intersection[x,list]=!={}]]];
        If[verb,Print["whose points belong to orbits ",list]];
        list
    ];

    (* 7. Strong Generating Set of group D *)
    SGSD=TranslatePerm[JoinSGS@@(SGSOfDummySet/@dummysets),notation];
    If[verb,Print["Initial SGSD: ",SGSD]];

    (* 8. Main loop on the slots of bS *)
    For[i=1,i<=Length[bS],++i,
        If[verb,Print["******************* Loop i= ",i," *********************"]];
        bi=bS[[i]];
        If[verb,Print["Analyzing slot ",bi," of tensor"]];

        (* A. Schreier vector of S *)
        nuS=SchreierOrbits[SGSS,len];
        (* Orbits are ordered according to base of SGSS, which has bi as first element *)
        If[verb,Print["nuS: ",nuS," with first element ",bi]];
        Deltab=orbitof[bi,Drop[List@@nuS,-2]];
        If[verb,Print["Under S, slot ",bi," can go to slots Deltab: ",Deltab]];

        (* B. Orbits of D *)
        DeltaD=Orbits[SGSD[[2]],len];
        If[verb,Print["Orbits of indices under D: DeltaD: ",DeltaD]];

        (* C. Images of bi under elements of S.perm.D *)
        IMAGES=Union[Flatten[Map[F1[#,TAB,DeltaD,Deltab,perm]&,ALPHA]]];
        If[verb,Print["Therefore at slot ",bi," we can have indices IMAGES: ",IMAGES]];
        (* The minimal element is taken with respect to bSsort *)
        p[[i]]=pi=MinB[IMAGES,bSsort];
        If[verb,Print["The least of them is p[[",i,"]]: ",pi]];

        (* D. Rearrange SGS of D. Now pi will be the first element of the base *)
        newdummysets=MovePairOf[pi,#]&/@newdummysets;
        If[verb,Print["Moved pairs ",newdummysets]];
        (* Full reconstruction of the SGSD starting from the new dummysets, with reordered indices *)
        SGSD=TranslatePerm[JoinSGS@@(SGSOfDummySet/@newdummysets),notation];
        If[verb,Print["New SGS of D: ",SGSD]];
        (* Schreier vector of D *)
        nuD=SchreierOrbits[SGSD,len];
        If[verb,Print["with Schreier vector nuD: ",nuD]];
        (* Orbit of pi under D *)
        Deltap=orbitof[pi,Drop[List@@nuD,-2]];
        If[verb,Print["In particular, the orbit of index ",pi," is Deltap: ",Deltap]];

        (* E. Calculate ALPHA and TAB. Inner double loop *)
        If[verb,Print["Now looking for all permutations sgd that move index ",pi," to slot ",bi]];
        alphaindices={};
        For[l=1,l<=Length[ALPHA],++l,
            If[verb,Print["Loop with l=",l]];
            L=ALPHA[[l]];
            If[verb,Print["L= ",L]];
            s=TAB[L][[1]];
            d=TAB[L][[2]];
            If[verb,Print["TAB[L] = {s, d} = ",{s,d}]];
            If[verb,Print["Calculating NEXT. We need the intersection of sets of slots ", OnPoints[Deltab,s]," and ",OnPoints[Deltap,InversePerm[PermProduct[perm,d]]]]];
            NEXT=Intersection[OnPoints[Deltab,s],OnPoints[Deltap,InversePerm[PermProduct[perm,d]]]];
            If[verb,Print["Intermediate slots NEXT= ",NEXT]];
            For[jj=1,jj<=Length[NEXT],++jj,
                j=NEXT[[jj]];
                s1=PermProduct[TraceSchreier[OnPoints[j,InversePerm@s],nuS],s];
                If[verb,Print["From slot ",bi," to intermediate slot ",j," use s1=", s1]];
                d1=PermProduct[d,InversePerm@TraceSchreier[OnPoints[j,PermProduct[perm,d]],nuD]];
                If[verb,Print["d1= ",d1]];
                L1=Append[L,j];
                If[verb,Print["L1= ",L1]];
                TAB[L1]={s1,d1};
                AppendTo[alphaindices,L1];
                If[verb,Print["This gives us the new index configuration: ",TranslatePerm[InversePerm[F2[L1,TAB,perm]],{Perm,len}]]];
                (* Checks *)
                For[ii=1,ii<=i,++ii,
                    If[OnPoints[bS[[ii]],PermProduct[s1,perm,d1]]=!=p[[ii]],Print["WRONG check of slot ",bS[[ii]]," with point ",p[[ii]]]]
                ]
            ];
            TAB[L]=.;
        ];
        ALPHA=alphaindices;
        If[verb,Print["New ALPHA: ",ALPHA]];

        (* F. Verify if there are 2 equal permutations of opposite sign in S.perm.D *)
        tmp=Map[InversePerm[F2[#,TAB,perm]]&,ALPHA];
        If[verb,Print["Checking consistency in set ",tmp]];
        If[Intersection[tmp,-tmp]=!={},result=0;Break[]];

        (* G. Find the stabilizers S^(i+1) and D^(i+1) *)
        SGSS=StrongGenSet[Drop[SGSS[[1]],1],Stabilizer[{bi},SGSS[[2]]]];
        If[verb,Print["Removing permutations from SGS of S that move slot ",bi]];
        If[verb,Print["New SGS of S: ",SGSS]];
        (* Construct new SGSD from the new dummysets, and not by stabilization *)
        newdummysets=RemovePairOf[pi,#]&/@newdummysets;
        SGSD=TranslatePerm[JoinSGS@@(SGSOfDummySet/@newdummysets),notation];
        If[verb,Print["Removing permutations from SGS of D that move index ",pi]];
        If[verb,Print["New SGS of D: ",SGSD]];
    ]; (* End of main loop *)

    (* 9. Result *)
    Switch[result,
        0,0,
        _,F2[ALPHA[[1]],TAB,perm]/.If[permutation===ID,Cycles[]->ID,{}]
    ]
];
Protect[DoubleCosetRepresentative2];

length[dummysets_List]:=Plus@@(length/@dummysets);
length[DummySet[_,pairs_List,_]]:=2Length[pairs];
length[RepeatedSet[list_List]]:=Length[list];


Options[CanonicalPerm]={MathLink:>$xpermQ,TimeVerbose->False,xPermVerbose->False,OrderedBase->True};
CanonicalPerm[perm_?PermQ,len_Integer,GS_,frees_List,dummysets:{(_DummySet|_RepeatedSet)...},options:OptionsPattern[]]:=If[OptionValue[MathLink],MathLinkCanonicalPerm,MathCanonicalPerm][perm,len,GS,frees,dummysets,options];
Protect[CanonicalPerm];


MathCanonicalPerm[perm_,len_,GS_GenSet,frees_,dummysets_,options:OptionsPattern[]]:=Module[{notation,SGS,time=TimeUsed[],timeverb,verb,ob},

{timeverb,verb,ob}=OptionValue[CanonicalPerm,{options},{TimeVerbose,xPermVerbose,OrderedBase}];

(* Compute Strong Generating Set *)
notation=NotationOfPerm[perm];
If[verb,Print["Using notation ",notation]];
SGS=Sequence[If[ob,Range[len],NonStablePoints[{},GS]],TranslatePerm[GS,notation]];
If[verb,Print["Before SchreierSims: ",{SGS}]];
SGS=SchreierSims[SGS,len,options];
If[verb,Print["After SchreierSims: ",SGS]];

If[timeverb,Print["SGS for group of order ",OrderOfGroup[SGS]," computed in ",-time+(time=TimeUsed[])," secs."]];

(* {perm, frees, dummysets} come directly here *)
MathCanonicalPerm[perm,len,SGS,frees,dummysets,options]
];


MathCanonicalPerm[perm_,len_,sym:(_Symmetric|_Antisymmetric),rest__]:=MathCanonicalPerm[perm,len,SchreierSims[{},sym],rest];
MathCanonicalPerm[perm_,len_,SGS_StrongGenSet,frees_,dummysets_,options:OptionsPattern[]]:=Module[{notation,freeps,newperm,newSGS,newfreeps,time=TimeUsed[],timeverb,verb,ob},

{timeverb,verb,ob}=OptionValue[CanonicalPerm,{options},{TimeVerbose,xPermVerbose,OrderedBase}];

(* Length of permutations (without considering the sign) and notation *)
If[len=!=length[dummysets]+Length[frees],Throw@Print["Wrong permutation length."]];
notation=NotationOfPerm[perm];

(* Strong generating set *)
newSGS=TranslatePerm[SGS,notation];
If[ob,newSGS=ReplacePart[newSGS,takenext[First@newSGS,{},Range[len]],1]];

(* First, we apply the free-indices algorithm. Compute slots of frees *)
freeps=OnPoints[frees,InversePerm[perm]];
If[verb,Print["Free indices at slots: ",freeps]];
{newperm,newSGS,newfreeps}=RightCosetRepresentative[perm,len,newSGS,freeps,options];
If[timeverb,Print["Free algorithm applied in ",-time+(time=TimeUsed[])," secs."]];
If[verb,Print["Canonical Permutation after RightCosetRepresentative: ",newperm]];

(* If there are dummies apply dummy-indices algorithm*)
If[dummysets=!={},
newperm=DoubleCosetRepresentative[newperm,len,newSGS,dummysets,options];
If[timeverb,Print["Dummy algorithm applied in ",-time+TimeUsed[]," secs."]];
];

(* Final result (could be zero) *)
newperm
];


nosign[-x_]:=x;
nosign[x_]:=x;


RightTransversal[x_]:=RightTransversal[x,PermLength[x]];
RightTransversal[GS_GenSet,len_Integer]:=RightTransversal[SchreierSims[{},GS,len],len];
RightTransversal[SGS:StrongGenSet[base_,GS_GenSet],len_Integer]:=Module[{length=len!/OrderOfGroup[SGS],list={},perm,not=NotationOfPerm[SGS]},
While[Length[list]<length,
perm=nosign[First@RightCosetRepresentative[RandomPerm[len,not],len,SGS]];
If[FreeQ[list,perm],AppendTo[list,perm]]
];
PermSort[list]
];
Protect[RightTransversal];


LeftTransversal[x_]:=LeftTransversal[x,PermLength[x]];
LeftTransversal[GS_GenSet,len_Integer]:=LeftTransversal[SchreierSims[{},GS,len],len];
LeftTransversal[SGS:StrongGenSet[base_,GS_GenSet],len_Integer]:=InversePerm/@RightTransversal[SGS,len];
Protect[LeftTransversal];


(********************** 6. MathLink connection *********************)


If[$ReadingVerbose,Print["Reading section 6: MathLink connection"],Null,Null]


xpermConnect:=Module[{version=System`$Version,number=System`$VersionNumber,result,message,dir=StringJoin[$xActDirectory,"/xPerm/mathlink/"]},
Which[

StringMatchQ[version,"*Windows*"],
If[number>=6.0,
message="Connecting to external MinGW executable...";
result=True;
$xpermExecutable=If[StringMatchQ[version,"*64-bit*"],
StringJoin[dir,"xperm.win64"],
StringJoin[dir,"xperm.win32"]
],
message="Pre-6 versions of Mathematica for Windows do not link to MinGW.";
result=False
],

StringMatchQ[version,"*Linux*"],
message="Connecting to external linux executable...";result=True;
$xpermExecutable=If[number>=6.0,
If[StringMatchQ[version,"*64-bit*"],
StringJoin[dir,"xperm.linux.64-bit"],
StringJoin[dir,"xperm.linux.32-bit"]
],
StringJoin[dir,"xperm.linux.pre6"]
],

StringMatchQ[version,"*Mac*"],
message="Connecting to external mac executable...";result=True;$xpermExecutable=StringJoin[$xActDirectory,"/xPerm/mathlink/xperm.mac"],

True,
message="Unsupported system. Contact JMM to compile.";result=False];
Print[message];
If[result,
Check[$xpermLink=Install[$xpermExecutable],result=False];
Print[If[result,"Connection established.","Connection failed."]]
];result
];
(* $xpermQ=xpermConnect; *)
$xpermQ = False;


SetAttributes[CheckDeadLink,HoldFirst];
CheckDeadLink[expr_]:=Check[expr,Print["Failed computing ",Hold[expr]];$xpermQ=xpermConnect;Print["Reconnected and recomputing"];Module[{result=CheckDeadLink[expr]},Print["Result is ",result];result],LinkObject::linkd];


ToSign[perm:Perm[list_],len_]:=If[OnPoints[len+1,perm]===len+1,1,-1]Perm[Drop[list,-2]];
ToSign[perm_Rules,len_]:=If[OnPoints[len+1,perm]===len+1,1,-1]DeleteCases[perm,_[___,len+1,___]];
ToSign[perm_Cycles,len_]:=If[OnPoints[len+1,perm]===len+1,1,-1]DeleteCases[perm,{___,len+1,___}];
ToSign[perm:Images[list_],len_]:=If[OnPoints[len+1,perm]===len+1,1,-1]Images[Drop[list,-2]];
ToSign[GS_GenSet,len_]:=ToSign[#,len]&/@GS;
ToSign[StrongGenSet[base_,GS_],len_]:=StrongGenSet[DeleteCases[base,(len+1)|(len+2)],ToSign[GS,len]];


FromSign[s_. Perm[perm_],len_]:=Perm[perm~Join~(len+If[s>0,{1,2},{2,1}])];
FromSign[s_. Rules[rules___],len_]:=Rules@@If[s<0,{rules,len+1->len+2,len+2->len+1},{rules}];
FromSign[s_. Cycles[cycs___],len_]:=Cycles@@If[s<0,{cycs,len+{1,2}},{cycs}];
FromSign[s_. Images[perm_],len_]:=Images[perm~Join~(len+If[s>0,{1,2},{2,1}])];
FromSign[GS_GenSet,len_]:=FromSign[#,len]&/@GS;
FromSign[StrongGenSet[base_,GS_],len_]:=StrongGenSet[If[!FreeQ[GS,-1],Append[base,len+1],base],FromSign[GS,len]];


toimagelist[len_][GS_GenSet]:=Flatten@Apply[List,toimagelist[len]/@GS];
toimagelist[len_][sperm_]:=Identity@@TranslatePerm[FromSign[sperm,len],{Images,len+2}];


MathLinkOrbit[point_,GS_GenSet]:=With[{len=Max[point,PermDeg[GS]]},MLOrbit[point,toimagelist[len][GS],len+2]];


MathLinkSchreierSims[initbase_List,GS_GenSet,len_Integer,options___]:=TranslatePerm[ToSign[MLSchreierSims[initbase,toimagelist[len][GS],len+2],len],NotationOfPerm[GS]];


Unprotect[StrongGenSet];
StrongGenSet[base:{___Integer},GS:{___Integer},len_Integer]:=(If[Select[base,#>len&]=!={},Print["Computed SGS contains -Cycles[]."]];StrongGenSet[base,GenSet@@(Images/@Partition[GS,len])]);
Protect[StrongGenSet];


TranslateSet[DummySet[_,dums_,sym_]]:={Flatten[{dums}],sym};
TranslateSet[RepeatedSet[list_]]:=Flatten[{list}];
transpose[{}]:={{},{}};
transpose[x_]:=Transpose[x];
TranslateSet[list_List]:=TranslateSetStructure[{transpose[TranslateSet/@Cases[list,DummySet[_,{__},_]]],TranslateSet/@Cases[list,_RepeatedSet]}];
TranslateSetStructure[{{dummysets_,mQ_},repeatedsets_}]:=Sequence[Length/@dummysets,Flatten[dummysets],mQ,Length/@repeatedsets,Flatten[repeatedsets]];


tosgslist[GS_GenSet,len_,ob_]:=Sequence[0,If[ob,Range[len],{}],toimagelist[len][GS]];
tosgslist[StrongGenSet[base_List,GS_GenSet],len_,ob_]:=Sequence[1,If[ob,takenext[base,{},Range[len]],Append[base,len+1]],toimagelist[len][GS]];


MathLinkCanonicalPerm[sperm_?PermQ,len_Integer,sgs_,frees_List,sets_List,options:OptionsPattern[]]:=Module[{verb,ob,tmp},
{verb,ob}=OptionValue[CanonicalPerm,{options},{xPermVerbose,OrderedBase}];
tmp=TMPHead[
toimagelist[len][sperm],len+2,
tosgslist[sgs,len,ob],
frees,
TranslateSet[sets]
];
If[verb,Print["MathLinkCanonicalPerm:: ",ReplacePart[TMPHead[tmp],{HoldForm,MLCanonicalPerm},{{0},{1,0}},{{1},{2}}]]];
CheckDeadLink[Apply[MLCanonicalPerm,tmp]]/.Images[{(0)..}]->0]


MathLinkSetStabilizer[list_List,len_Integer,sgs_,options:OptionsPattern[]]:=TranslatePerm[ToSign[
Module[{verb,ob,tmp},
{verb,ob}=OptionValue[SetStabilizer,{options},{xPermVerbose,OrderedBase}];
tmp=Drop[TMPHead[
list,
len+2,
tosgslist[sgs,len,ob]
],{3}];
If[verb,Print["MathLinkSetStabilizer:: ",ReplacePart[TMPHead[tmp],{HoldForm,MathLinkSetStabilizer},{{0},{1,0}},{{1},{2}}]]];
CheckDeadLink[Apply[MLSetStabilizer,tmp]]/.Images[{(0)..}]->0
],
len],NotationOfPerm[sgs]];


End[];


EndPackage[];


On[System`Cycles::shdw];
