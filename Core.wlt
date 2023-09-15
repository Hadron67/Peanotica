<< Peanotica`xPerm`
<< Peanotica`Core`

SymmetryGroupOfExpression@F[a__] ^:= {-SCycles@{1, 2}};
SymmetryGroupOfExpression@RiemannTensor[___, Repeated[IndexSlot, {4}]] ^= {-SCycles@{1, 2}, -SCycles@{3, 4}, SCycles[{1, 3}, {2, 4}]};
SymmetryGroupOfExpression@RicciTensor[___, Repeated[IndexSlot, {2}]] ^= {SCycles@{1, 2}};

Print@OrderOfGroup@StrongGenSet[{1, 2, 3, 4}, GenSet[-Peanotica`xPerm`Cycles@{1, 2}, -Peanotica`xPerm`Cycles@{3, 4}, Peanotica`xPerm`Cycles[{1, 3}, {2, 4}]]];

VerificationTest[
    SymmetryOfSortedObject[ISort[ISum[a[i, i] a[j, j] b[k] b[l], i, j, k, l]]],
    {SCycles[{1, 3}, {2, 4}], SCycles[{5, 6}]}
];

VerificationTest[
    SymmetryOfSortedObject@ISort[ISum[F[1, DI@i, j] RicciTensor[i, j], i, j]],
    {-SCycles[{1, 2}], SCycles[{3, 4}]}
];

VerificationTest[
    ISum[a[i] b[i], i] - ISum[a[j] b[j], j] // SameDummies,
    0
];

VerificationTest[
    ISum[a[i, i, i, j], i, j] - ISum[a[j, j, j, i], i, j] // SameDummies,
    0
];
