<< Peanotica`xPerm`

VerificationTest[
    DoubleCosetRepresentative[Images@{2, 1}, 2, StrongGenSet[{1, 2}, GenSet[-SCycles@{1, 2}]], StrongGenSet[{1, 2}, GenSet[]]],
    -Images@{1, 2}
];

VerificationTest[
    DoubleCosetRepresentative[Images@{2, 1}, 2, StrongGenSet[{1, 2}, GenSet[SCycles@{1, 2}]], StrongGenSet[{1, 2}, GenSet[-SCycles@{1, 2}]]],
    0
];

