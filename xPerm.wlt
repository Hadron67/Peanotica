<< Peanotica`xPerm`

VerificationTest[
    OrderOfGroup@{-SCycles@{1, 2}, -SCycles@{3, 4}, SCycles[{1, 3}, {2, 4}]},
    8
];

VerificationTest[
    DoubleCosetRepresentative[Images@{2, 1}, 2, StrongGenSet[{1}, GenSet[-SCycles@{1, 2}]], StrongGenSet[{1, 2}, GenSet[]]],
    -Images@{1, 2}
];

VerificationTest[
    DoubleCosetRepresentative[Images@{2, 1}, 2, StrongGenSet[{1}, GenSet[SCycles@{1, 2}]], StrongGenSet[{1, 2}, GenSet[-SCycles@{1, 2}]]],
    0
];

VerificationTest[
    DoubleCosetRepresentative[Images@{2, 4, 3, 1}, 4, RiemannSymmetric@{1, 2, 3, 4}, StrongGenSet[{2}, GenSet[SCycles@{2, 3}]]],
    -Images@{1, 2, 3, 4}
];

VerificationTest[
    DoubleCosetRepresentative[Images@{2, 4, 3, 1}, 4, StrongGenSet[{2}, GenSet[]], StrongGenSet[{3}, GenSet[SCycles@{3, 4}]]],
    Images@{2, 3, 4, 1}
];