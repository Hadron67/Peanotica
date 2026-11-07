#!/usr/bin/env python3
"""Generate ``docs/api.md`` from the ``::usage`` strings in the package.

Run from the repository root::

    python3 docs/gen_api.py

The generated file is a reference: one entry per exported symbol, grouped by
package, with the usage string, a source link and a status tag.  Curated
descriptions, option tables and examples live in the ``OVERRIDES`` table
below; edit that table (not ``api.md``) and regenerate.

The generator is intentionally simple: it only parses single-line
``Name::usage = "...";`` assignments, which is how the package declares all of
its messages.  Symbols that are exported but have no usage string are listed
in ``EXTRA``.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
OUTPUT = REPO_ROOT / "docs" / "api.md"

USAGE_RE = re.compile(r'^\s*([A-Za-z$][A-Za-z0-9$]*)::usage\s*=\s*"(.*)";')

MODULES = [
    ("Peanotica`Internal`", "Internal.wl", "内部工具：调试打印、排序辅助、数组化简。"),
    ("Peanotica`Perm`", "Perm.wl", "置换代数、预定义对称群、WSTP 后端封装。"),
    ("Peanotica`Core`", "Core.wl", "指标/槽位协议、规范化、度规、ETensor/NITensor、显示。"),
    ("Peanotica`DiffGeo`", "DiffGeo.wl", "协变导数、曲率、指标适配、扰动。"),
    ("Peanotica`AlgSimp`", "AlgSimp.wl", "Bianchi 等多重线性关系的枚举与化简。"),
    ("Peanotica`Presets`", "Presets.wl", "流形设置、曲率计算、分量代入、变分。"),
]

# Symbols that are exported without a usage string, or that deserve an entry
# despite not having one.  (module file, name, one-line description)
EXTRA = {
    "Perm.wl": [
        ("Images", "单行像记号：`Images[p1, p2, ...]`；可短写，未写出的点为不动点。"),
        ("SCycles", "轮换记号：`SCycles[{...}, ...]`；前面加 `-` 表示奇置换。"),
        ("RubiksCubeGenSet", "（未实现）占位符号。"),
    ],
    "Core.wl": [
        ("SummedIndices", "`FindIndicesSlots` 对 `Plus` 返回的内部标记，表示指标是跨项求和的。"),
        ("IndexNameSlot", "上指标的模板（`DI[IndexNameSlot]` 是下指标模板）。"),
        ("FindIndicesSlotsAndNames", "`FindIndicesSlots` 的变体，同时返回槽位上的指标表达式。"),
        ("AddSymmetryToGroupedIndexList", "给分组指标列表附加对称性，供规范化内部使用。"),
        ("ReleaseISort", "把 `ISortedProduct`/`ISortedGeneral` 还原成普通表达式。"),
        ("PreITensorReduce", "`ITensorReduce` 进入规范化前的扩展点（可写 UpValue）。"),
        ("PostITensorReduce", "`ITensorReduce` 离开规范化时的扩展点（可写 UpValue）。"),
        ("ExpandToTensorPolynomial", "把非张量因子乘 `Plus`、正整数幂展开成单项之和。"),
        ("SignOfUpSlot", "上指标返回 1，下指标返回 -1。"),
        ("CanonicalizationUnitQ", "判断一个表达式是否适合作为规范化的最小单位。"),
    ],
    "DiffGeo.wl": [
        ("DerivativeExpandableQ", "只是 `ExpandDerivativeRules` 用来定位被求导表达式的语法标记，本身没有定义。"),
        ("SymmetriedDer", "`DefTensorDerivativeOperator` 的选项：构造对称化导数的对称性。"),
        ("AllowPassThrough", "导数/扰动算子是否允许把被作用表达式看作可通过的因子。"),
        ("PredefinedSlotType", "扰动公式里使用的占位槽位类型。"),
        ("PredefinedMetric", "扰动公式里使用的占位度规。"),
        ("PredefinedCovD", "扰动公式里使用的占位协变导数。"),
        ("ShiftedMetric", "扰动展开中的 `g + δg + ...` 记号。"),
        ("ShiftedInverseMetric", "扰动展开中的逆度规记号。"),
    ],
    "AlgSimp.wl": [],
    "Presets.wl": [],
    "Internal.wl": [],
}

STATUS = {
    # 未实现（只有声明）
    "ConvertPermutationNotation": "未实现",
    "RelationClosureTableToArray": "未实现",
    "SymmetricIndexGroup": "未实现",
    "IndexNameGroup": "未实现",
    "MapIndexedGroupedIndexList": "未实现",
    "FoldListGroupedIndexList": "未实现",
    "IndexedToFunction": "未实现",
    "GroupByTensors": "未实现",
    "CETensor": "未实现",
    "ScalarValue": "未实现",
    "WrapETensor": "未实现",
    "ITensorRowReduce": "未实现",
    "CoRiemannValue": "未实现",
    "DefTensorPerturbation": "未实现",
    "PerturbationCovD": "未实现",
    "Cocurvature": "未实现",
    "Cotorsion": "未实现",
    "ToAlgTensor": "未实现",
    "RiemannToAlgTensor": "未实现",
    "AlgTensorToRiemann": "未实现",
    "RubiksCubeGenSet": "未实现",
    # 已知问题
    "NoIndicesQ": "已知问题",
    "RiemannScalars": "已知问题",
    "SetupPerturbation": "已知问题",
    # 实验性
    "AlgTensorSimplifyData": "实验性",
    "AlgTensorReduceRules": "实验性",
    "AlgRelations": "实验性",
    "EnumerateAlgTensorTerms": "实验性",
    "AlgTensorTermReduce": "实验性",
    "AlgTensorTerm": "实验性",
    "AlgTensorOrder": "实验性",
    "AlgDRiemann": "实验性",
    "AlgDRicci": "实验性",
    "AlgEpsilon": "实验性",
    "AlgDRiemannList": "实验性",
    "AlgDRiemannETensor": "实验性",
    "IndexedAlgTensor": "实验性",
    "IndexedAlgTensorCovD": "实验性",
    "ExtractAlgTensor": "实验性",
    "ExtractAllAlgTensors": "实验性",
    "RiemannTensorOrder": "实验性",
    "PopulateCurvatureTReducerRules": "实验性",
    "StoredTensorData": "实验性",
    "MetricAdapterOfTReducer": "实验性",
    "DefTensorVariationOperator": "实验性",
    "VarCovDs": "实验性",
    "VarParamDs": "实验性",
    # 高级/内部
    "GroupedIndexList": "高级",
    "GroupIndexList": "高级",
    "GroupIndexListWithSlotTypes": "高级",
    "SortGroupedIndexList": "高级",
    "FindAndDropFrees": "高级",
    "GroupedIndexListMapFold": "高级",
    "MapGroupedIndexList": "高级",
    "CollectGroupedIndices": "高级",
    "CollectGroupedIndicesNames": "高级",
    "RenameGroupedIndexList": "高级",
    "PrependPosToSlotSpec": "高级",
    "IndexSlot": "高级",
    "IndexedObject": "高级",
    "ToIndexedObject": "高级",
    "MapIndexSlots": "高级",
    "IndexedExprFunction": "高级",
    "ToIndexedExprFunction": "高级",
    "RaiseFrees": "高级",
    "WithTempIndex1": "高级",
    "PermuteIndices": "高级",
    "ImposeSymmetry": "高级",
    "ExpandScalarIndexScope": "高级",
    "MapScalarIndexScopes": "高级",
    "MetricBarrier": "高级",
    "ReleaseMetricBarrier": "高级",
    "TDagger": "高级",
    "SymmetryListGenSet": "高级",
    "HeldTimes": "高级",
    "ISortedProduct": "高级",
    "ISortedGeneral": "高级",
    "ISortUnorderedArgs": "高级",
    "IndexWrapper": "高级",
    "PutIndexWrapper": "高级",
    "ReleaseIndexWrapper": "高级",
    "TensorGridBox": "高级",
    "DefTensorFormatings": "高级",
    "TFIndexSlot": "高级",
    "TFLabelSlot": "高级",
    "TFFunctionSlot": "高级",
    "ETensorIndices": "高级",
    "NITensorSlot": "高级",
    "ToNITensorIndex": "高级",
    "FromNITensorIndex": "高级",
    "SumIndexOfSlot": "高级",
    "ETensorArray": "高级",
    "ExpandedGDelta": "高级",
    "xToolsDebugPrint": "内部",
    "OrderOr": "内部",
    "SymmetryList": "内部",
    "FilterExprList": "内部",
    "MakeThreadableRules": "内部",
    "NestWith": "内部",
    "DefDisplayAs": "内部",
    "ArraySimplify": "内部",
    "DebugPrint": "内部",
    "$xToolsDebugFilter": "内部",
    "$xToolsDebugPrint": "内部",
    "SummedIndices": "高级",
    "IndexNameSlot": "高级",
    "FindIndicesSlotsAndNames": "高级",
    "AddSymmetryToGroupedIndexList": "高级",
    "PreITensorReduce": "高级",
    "PostITensorReduce": "高级",
    "ExpandToTensorPolynomial": "高级",
    "SignOfUpSlot": "高级",
    "CanonicalizationUnitQ": "内部",
    "DerivativeExpandableQ": "内部",
    "SymmetriedDer": "高级",
    "AllowPassThrough": "高级",
    "PredefinedSlotType": "高级",
    "PredefinedMetric": "高级",
    "PredefinedCovD": "高级",
    "ShiftedMetric": "高级",
    "ShiftedInverseMetric": "高级",
    "RubiksCubeGenSet": "未实现",
}

OVERRIDES: dict[str, str] = {
    "FindIndicesSlots": """
**签名**：`FindIndicesSlots[expr]`、`FindIndicesSlots[expr, prefix]`

返回 `{{pos...} -> slotType, ...}`：每个指标槽位在表达式中的位置，以及槽位类型。`prefix` 会把给定位置前缀加到每个结果上，便于在自定义对象里复用子表达式的槽位。

**示例**：

~~~text
FindIndicesSlots[T[a, b]]        -> {{1} -> mf, {2} -> mf}
FindIndicesSlots[T[a, b], {9}]   -> {{9, 1} -> mf, {9, 2} -> mf}
~~~

**注意**：有 `HoldFirst` 属性，可用模式定义；自定义对象请用 `TagSet`/`UpValue`。`Plus` 返回的槽位会带 `SummedIndices` 标记；`IndexedSum` 会删掉被求和的槽位。
""",
    "SymmetryOfExpression": """
**签名**：`SymmetryOfExpression[expr]`

返回作用在槽位序号上的**带符号置换生成元**列表。槽位序号按 `FindIndicesSlots` 返回的顺序编号。

**示例**：

~~~text
SymmetryOfExpression[T[a, b]]        -> {SCycles[{1, 2}]}
SymmetryOfExpression[R[a, b, c, d]]  -> RiemannSymmetricGenSet[1]
SymmetryOfExpression[T[a, b] T[c, d]] -> {}  (* 乘积的块对称性由 ISort 单独处理 *)
~~~

**注意**：返回值必须是**强生成元**（基为 `[1..n]`）。内置构造器满足这一点；任意生成元请先 `ConstructStrongGenSet`。乘积、`ISortedProduct`、`HeldTimes`、`TDagger`、`MetricBarrier` 等有内部规则。
""",
    "ITensorReduce": """
**签名**：`ITensorReduce[expr]`、`ITensorReduce[expr, opts]`

对张量表达式做规范化：展开 `Plus`，排序乘积因子，按槽位对称性与哑指标重命名把等价单项化成同一形式；因反对称而相消的项会变成 `0`。

**选项**：

| 选项 | 默认 | 说明 |
|---|---|---|
| `FreeIndexNames` | `Automatic` | 指定自由指标；`Automatic` 按出现次数推断，`{}` 表示全部按哑指标处理 |
| `FreeIndicesSymmetry` | `{}` | 给自由指标附加对称性（视作与一个对称张量缩并） |
| `RenameDummies` | `True` | 是否把哑指标改成规范名字 |
| `UseMetricOnSlots` | `All` | 允许用度规对称性交换上下指标对的槽位类型；`{}` 表示全部禁用 |
| `FixedPoint` | `False` | 反复规范化直到结果稳定 |

**示例**：

~~~text
ITensorReduce[T[b, a]]                          -> T[a, b]
ITensorReduce[T[a, b] + T[b, a]]                -> 2 T[a, b]
ITensorReduce[S[a, a]]                          -> 0   (* S 反对称 *)
ITensorReduce[T[a, b] A[a]]                     -> A[a] T[b, a]
~~~

**注意**：只使用单项（mono-term）对称性。Bianchi 等多重关系请用 `AlgSimp`；度规缩并请用 `ContractMetric`。`PostprocessCanonicalIndicesList` 选项目前被接受但未使用。
""",
    "ITensorReduceOneTerm": """
**签名**：`ITensorReduceOneTerm[expr, opts]`

与 `ITensorReduce` 相同的规范化，但不展开 `Plus`（`Plus` 会逐项映射）。当你确定输入是单项时更快。

~~~text
ITensorReduceOneTerm[T[b, a]] -> T[a, b]
~~~
""",
    "DefSimpleSlotType": """
**签名**：`DefSimpleSlotType[type, dim, {names, fallback}]`

定义一个槽位类型：维数、可用指标名列表、候选名用尽时的兜底生成器。等价于设置 `DimensionOfSlotType`、`IndicesCandidateOfSlotType`。

**选项**：`DisplayName -> name` 设置类型的显示名。

~~~text
DefSimpleSlotType[mf, 4, {Symbol /@ CharacterRange["a", "z"], DefaultIndex}];
~~~
""",
    "DefSimpleTensor": """
**签名**：`DefSimpleTensor[name, slots, symmetry]`

按槽位类型列表定义张量，并安装 `FindIndicesSlots`、`SymmetryOfExpression` 和排版规则。

- `slots` 是槽位类型列表；`TFLabelSlot`、`TFFunctionSlot` 这类非指标槽位会被 `FindIndicesSlots` 跳过；
- `symmetry` 是带符号置换生成元列表；`{}` 表示没有对称性。

**选项**：`DisplayName`、`IndexStyle`、`AllowSubsuperscriptBox`（见 `TensorGridBox`）。

~~~text
DefSimpleTensor[T, {mf, mf}, {SCycles@{1, 2}}];
DefSimpleTensor[R, {mf, mf, mf, mf}, RiemannSymmetricGenSet[1]];
~~~
""",
    "DefSimpleMetric": """
**签名**：`DefSimpleMetric[symbol, type, symSign]`

定义度规张量并安装缩并规则。

- `symSign = 1`：对称；`-1`：反对称；`0`：无对称性；
- 默认 `ContractionRuleMode -> "Normal"`：`g[DI@a, DI@b]` 降指标、`g[a, b]` 升指标、`g[a, DI@b]` 作为 δ；
- `"AllUp"`：不区分上下号，乘法直接替换指标；
- 反对称度规的缩并规则目前仍按对称情形处理（代码中有 TODO）。

**选项**：`ContractionRuleMode`、`DisplayName` 等。
""",
    "ContractMetric": """
**签名**：`ContractMetric[expr]`、`ContractMetric[expr, metrics]`

消去表达式中的度规因子：找到度规指标在其它因子中的"相反符号"同名指标，用度规的另一个指标替换它；必要时乘上度规对称性带来的符号。

- `ContractMetric[expr]` / `ContractMetric[expr, All]`：所有 `ContractableMetricQ` 为真的度规；
- `ContractMetric[expr, g]`：只处理匹配 `g` 的度规；
- 会检查 `ExpressionPassThroughQ` 和 `SumPassThroughQ`，不会缩并求和号内部或不能穿过的位置。

~~~text
ContractMetric[T[a, b] g[DI@b, DI@c]]   -> T[a, DI[c]]
ContractMetric[g[a, DI@b] T[b, c]]      -> T[a, c]
ContractMetric[g[DI@a, a] T[b, c]]      -> 4 T[b, c]
~~~
""",
    "MetricOfSlotType": """
**签名**：`MetricOfSlotType[type]`

槽位类型 `type` 的默认度规。可以像普通张量一样使用；也可以用 `MetricOfSlotType[type] ^= metric` 指定具体度规。

默认行为假设上下指标成对：

~~~text
MetricOfSlotType[mf][a, DI@b] T[b, c]  -> T[a, c]
MetricOfSlotType[mf][DI@a, a]          -> 4
~~~
""",
    "IndexedSum": """
**签名**：`IndexedSum[expr, {i, type}, ...]`

显式的指标求和（不是张量缩并）。如果求和指标在 `expr` 中完全不出现，直接乘上 `DimensionOfSlotType[type]`；如果是 `Times`，不含该指标的因子会被提到求和号外。

~~~text
IndexedSum[5, {a, mf}]                          -> 20
FindIndicesSlots[IndexedSum[T[a, b], {a, mf}]]  -> {{1, 2} -> mf}
~~~

相关：`ScalarIndexedSumQ`、`MoveIndexedSumToOutermost`、`MoveIndexedSumToInnermost`、`IndexedSumTimes`。
""",
    "IndexScope": """
**签名**：`IndexScope[expr]`

把 `expr` 当作标量：内部指标都是局部哑指标，不与外层缩并，也不影响外层命名。`FindIndicesSlots[IndexScope[expr]] = {}`；在笔记本里显示为红色圆括号。

~~~text
FindIndicesSlots[IndexScope[T[a, a]]] -> {}
~~~
""",
    "ETensor": """
**签名**：`ETensor[expr, frees]`、`ETensor[expr, frees][inds]`

"表达式 + 自由指标结构"的表示，用于分量计算和分块张量。

- `frees` 必须**扁平**，长度等于槽位数；每一项是指标名或 `Null`（该槽位没有自由指标）；
- 对 Core 来说 `ETensor` 没有指标槽位（`FindIndicesSlots` 返回 `{}`）；
- 支持 `+`、`*`、`ITensorTranspose`、`ITensorOuter`、`ITensorContractTwo`、`ITensorFixedContract`、`ITensorSum`；
- `ETensor[expr, frees][inds]` 按位置替换自由指标。

~~~text
ETensor[T[a, b], {a, b}][m, n]                -> T[m, n]
ETensor[A[a], {a, Null}] + ETensor[A[b], {Null, b}]
                                              -> ETensor[A[a] + A[b], {a, b}]
~~~

**注意**：`ITensorContract` 只接受普通数组；`ETensor` 用 `ITensorContractTwo`/`ITensorFixedContract`。数组的每个非零分量应是 `ETensor`（或 `0`）。
""",
    "NITensorReduce": """
**签名**：`NITensorReduce[expr, frees]`、`NITensorReduce[expr, frees, reducer]`

对 `NITensor` 表达式做缩并：合并重复指标、挑选取缩并对、求和。第三个参数可以是一个"reducer 名字"，配合 `PopulateCurvatureTReducerRules` 使用。

~~~text
NITensorReduce[NITensor[A, {a}] NITensor[A, {b}], {}]
~~~

**注意**：两个全等的 `NITensor` 相乘会被合并成 `Power`，当前不处理；请避免让两个因子在词法上完全相同，或改用不同的哑指标名。
""",
    "MakeIndexRule": """
**签名**：`MakeIndexRule[lhs -> rhs, frees]`、`MakeIndexRule[lhs :> rhs, frees, others]`

把"含自由指标的表达式"变成规则：`frees` 里的指标变成模式变量，`rhs` 里的哑指标自动唯一化。

~~~text
g[DI@a, DI@b] /. MakeIndexRule[g[a, b] -> p[a] p[b] p[c] p[DI@c], {a, b}]
    -> p[c] p[DI[a]] p[DI[b]] p[DI[c]]
~~~
""",
    "MakeIndexFunction": """
**签名**：`MakeIndexFunction[expr, frees]`

把含自由指标的表达式变成函数，`frees` 按位置变成参数，哑指标自动唯一化。

~~~text
MakeIndexFunction[p[a] p[b] p[c] p[DI@c], {a, b}][DI@a, a]
    -> p[a] p[b] p[DI[a]] p[DI[b]]
~~~
""",
    "ISort": """
**签名**：`ISort[expr]`、`ISort[expr, sorter]`

只对无序乘积（`Times`、`Wedge` 等）的因子排序，不碰指标。排序键依次是：对称群阶数降序、槽位数降序、去指标表达式升序；排序后记录相邻全等因子（用于块对称性）。

结果形如 `ISortedProduct[head, args, symList]`；`ReleaseISort` 可以还原成普通表达式。`ITensorReduce` 内部会调用它。
""",
    "DoubleCosetRepresentative": """
**签名**：`DoubleCosetRepresentative[S, g, D]`

返回双陪集 `⟨S⟩ · g · ⟨D⟩` 中按像的字典序最小的代表元；如果双陪集中同时存在符号相反的两个相同置换，返回 `0`。

- `S`、`D` 必须是相对于基 `[1, 2, ..., n]` 的**强生成元**；
- 置换用 `Images` 或 `SCycles` 表示，负号表示奇置换；
- 选项：`UseTwoStep -> True`（Portugal 两步法，默认 `False`）、`PPermVerbose -> True`（写日志，需 `PPERM_DEBUG` 构建）。

~~~text
DoubleCosetRepresentative[{-SCycles@{1, 2}, -SCycles@{3, 4}, SCycles[{1, 3}, {2, 4}]},
                          Images@{2, 1, 3, 4},
                          DummiesGenSet[1, {{1, 2}}]]
~~~
""",
    "ConstructStrongGenSet": """
**签名**：`ConstructStrongGenSet[genset]`

把任意生成元转换成相对于基 `[1..n]` 的强生成元（Jerrum 的 Schreier–Sims 变体）。`SymmetryOfExpression` 的返回值必须是强生成元；内置构造器已经满足，只有自定义生成元需要这一步。

~~~text
ConstructStrongGenSet[{SCycles@{1, 2, 3}}]
    -> {SCycles[{1, 2, 3}], SCycles[{1, 3, 2}]}
~~~
""",
    "GroupOrderFromStrongGenSet": """
**签名**：`GroupOrderFromStrongGenSet[genset]`

返回强生成元张成的群的阶。`{}` 返回 1；单个带符号二元轮换返回 2。
""",
    "DefTensorDerivativeOperator": """
**签名**：`DefTensorDerivativeOperator[sym, slots, symmetry]`

定义一个协变导数算子：

- `sym[expr, inds...]` 表示对 `expr` 求导；也支持 `sym[Null, inds...][expr]`；
- 会自动安装 Leibniz 法则、链式法则、常数/标量函数规则；
- `sym` 的槽位 = 被作用表达式的槽位 + `slots` 指定的导数槽位；
- `SumPassThroughQ`、`ExpressionPassThroughQ` 有对应规则。

**选项**：`DisplayName`（默认 `∂`）、`SymmetriedDer`（对称化导数的对称性）、`AllowPassThrough`、`DerConstants`、`DerFunctions`。`DerConstants` 的默认值是 `{_?DerConstantQ}`；声明常量符号时优先 `SetAttributes[foo, Constant]`，不要为每个符号手动写 `DerConstantQ[foo] ^= True`。

~~~text
DefTensorDerivativeOperator[cd, {mf}, {}];
cd[g[_, _], _] = 0;                 (* 度规相容 *)
cd[A[a] A[b], c]                     (* Leibniz *)
    -> A[b] cd[A[a], c] + A[a] cd[A[b], c]
~~~
""",
    "DefParametreDerivativeOperator": """
**签名**：`DefParametreDerivativeOperator[sym, opts]`

定义对参数的导数（坐标、扰动变量等）。

**选项**：`DerParameters`（参数列表，用于生成 `sym[p1, p2] = 0`）、`DisplayName`、`AllowPassThrough`、`DerConstants`、`DerFunctions`。

~~~text
DefParametreDerivativeOperator[paramd, DerParameters -> {t, r}];
paramd[Null, t][f[r]]  -> f'[r]
paramd[r, r]           -> 1
paramd[r, t]           -> 0
~~~
""",
    "LeviCivitaChristoffel": """
**签名**：`LeviCivitaChristoffel[cd, metric]`、`LeviCivitaChristoffel[cd, metric, metricInv]`

返回联系给定导数算符 `cd` 与度规的 Levi-Civita 联络的 Christoffel（差）张量 $\\Gamma(\\nabla_{LC}, \\nabla)$，其中 $\\nabla = cd$，$\\nabla_{LC} = \\nabla + \\Gamma$：

~~~text
LeviCivitaChristoffel[cd, metric][a, DI@b, DI@c]
    = 1/2 g^{ad}(∇_b g_{dc} + ∇_c g_{bd} - ∇_d g_{bc})   (∇ = cd)
    = ˚Γ_{bc}{}^a - Γ_{bc}{}^a
~~~

其中 `˚Γ` 是 Levi-Civita 联络的 Christoffel 符号，`Γ` 是 `cd` 的 Christoffel 符号。**该公式只对无挠的 `cd` 成立**；`cd` 有挠时它不再等于完整的联络差张量 $\\Gamma(\\nabla_{LC}, \\nabla_{cd})$（还差与挠率有关的项）。`LeviCivitaChristoffelDer[cd, der, metric, metricInv]` 允许把普通导数 `der` 换成参数导数；`LeviCivitaChristoffelValue` 返回分量数组。
""",
    "RiemannDifference": """
**签名**：`RiemannDifference[cd, chris]`

两个导数算符的黎曼张量之差。设 `cd` 表示参考导数算符 $\\nabla$，`chris` 表示联络差 $\\Gamma(\\nabla', \\nabla)$（即 $\\nabla' = \\nabla + \\Gamma$），则返回 **$R(\\nabla') - R(\\nabla)$**：

~~~text
RiemannDifference[cd, chris][a, b, c, d]
    = -∇_a chris[d, b, c] + ∇_b chris[d, a, c]
      + chris[e, a, c] chris[d, b, DI@e]
      - chris[e, b, c] chris[d, a, DI@e]
~~~

**该函数要求参考导数 `cd` 无挠**；`chris` 通常来自与度规适配的无挠联络，例如 `LeviCivitaChristoffel`。`RiemannDifferenceValue[slot, cd, chris]` 返回分量数组。
""",
    "SortCovD": """
**签名**：`SortCovD[expr, cd, commutator]`、`SortCovD[expr, cd, commutator, orderedQ]`

把嵌套的协变导数按指标顺序整理；交换顺序时用 `commutator` 替换。

~~~text
SortCovD[cd[cd[T[a, b], d], c], cd, CovDCommutatorOfRiemann[R]]
    -> cd[cd[T[a, b], c], d] - R[c, d, DI[i], b] T[a, i] - R[c, d, DI[i], a] T[i, b]
~~~

`CovDCommutatorOfRiemann[R]`、`CovDCommutatorNoTorsion[expr, a, b, R]` 提供常用的交换子；**这两个交换子只适用于无挠导数算符**（如 `cd` 有挠，需要自己提供包含挠率项的 `commutator`）。`CovDDefaultIndexOrderedQ` 是默认排序判据。
""",
    "DefRiemannCurvature": """
**签名**：`DefRiemannCurvature[slot, riemann, ricci, ricciScalar]`

一次性定义黎曼张量（`RiemannSymmetricGenSet[1]`）、Ricci 张量（对称）和 Ricci 标量，并安装 8 条 Ricci 缩并规则。

~~~text
DefRiemannCurvature[mf, R, Ric, Rs];
ITensorReduce[R[a, b, c, DI@b]]  -> Ric[a, c]
ITensorReduce[Ric[DI@a, a]]      -> Rs
~~~
""",
    "RiemannBianchiRelations": """
**签名**：`RiemannBianchiRelations[expr, covd, riemann]`

生成 Bianchi 关系（返回等式列表）：

~~~text
RiemannBianchiRelations[R[a, b, c, d], cd, R]
    -> {R[a, b, c, d] + R[a, c, d, b] + R[a, d, b, c]}
~~~

对 `Plus`/`Times` 递归；对 `cd[R[...], e]` 生成第一 Bianchi 和导数 Bianchi。**这些关系是无挠情形的 Bianchi 恒等式**。
""",
    "AlgTensorSimplifyData": """
**签名**：`AlgTensorSimplifyData[tensor, frees]`

返回 `{basis, rules}`：

- `basis`：所有不等价的单项（`AlgTensorTerm[...]`）；
- `rules`：用基表示其余单项的重写规则。

当前主要在 `AlgDRiemann[n]`/`AlgDRicci[n]`/`AlgEpsilon[n]` 及其 `TensorProduct` 上工作。

~~~text
data = AlgTensorSimplifyData[AlgDRiemannList[{0, 0}], 0];
Length[data[[1]]]   -> 4
Length[data[[2]]]   -> 1
data[[1, 4]] /. data[[2]]
~~~

**实验性**：与真实黎曼张量互转的 `ToAlgTensor`、`RiemannToAlgTensor`、`AlgTensorToRiemann` 尚未实现。
""",
    "AlgDRiemann": """
**签名**：`AlgDRiemann[n]`

表示 n 阶协变导数的黎曼张量，槽位数为 `n + 4`，对称性为 `RiemannSymmetricGenSet[n + 1]`。`AlgDRiemannList[{n1, n2, ...}]` 表示多个因子的乘积。

~~~text
AlgDRiemann[0]      (* R *)
AlgDRiemann[1]      (* ∇R *)
AlgDRiemannList[{0, 0}]  (* R ⊗ R *)
~~~
""",
    "SetupRiemannManifold": """
**签名**：`SetupRiemannManifold[slot, dim, metric, cd, {riemann, ricci, ricciScalar}]`

一次性定义黎曼流形所需的槽位类型、度规、协变导数、黎曼/Ricci/Ricci 标量，并返回一个 Association：

~~~text
<|"Slot" -> slot, "Metric" -> metric, "CovD" -> cd,
  "Riemann" -> riemann, "Ricci" -> ricci, "RicciScalar" -> ricciScalar|>
~~~

**选项**：`ClearAll -> True`（清除同名符号）、`PPRiemannDisplayName -> "R"`。
""",
    "ComputeCurvature": """
**签名**：`ComputeCurvature[input]`

从度规和坐标导数计算曲率。`input` 是一个 Association，常用键：

| 键 | 说明 |
|---|---|
| `"Metric"`, `"InverseMetric"` | 度规和逆度规（数组，分量通常为 `ETensor`） |
| `"Pd"` | 坐标导数算子数组；每个分量是 `ETensor[pd, {Null}]` 或 `ETensor[cd[Null, DI@a], {a}]` |
| `"PdSpec"` | 混合导数规范：`{"Pd", pd}` 或 `{"CovD", cd, data}` 的列表；与 `"Pd"` 二选一。`data` 可以是一个带 `"Metric"`/`"InverseMetric"`/`"Riemann"` 的 Association，也可以直接是该协变导数的曲率张量（如 `k*SymmetricRiemann[g, #] &`）；未给度规时回退到 `MetricOfSlotType` |
| `"RiemannPd"` | 块导数算符的曲率张量（例如角度块用 `SymmetricRiemann` 表示的常曲率），与 `"PdSpec"` 二选一 |

返回 `Join[input, <|"Pd"->..., "RiemannPd"->..., "Christoffel"->..., "Riemann"->..., "Ricci"->..., "RicciScalar"->...|>]`。

**选项**：`PostCurvatureCompute`（默认 `Simplify @* ITensorReduce @* ContractMetric`）。
""",
    "MakeCurvatureValueRules": """
**签名**：`MakeCurvatureValueRules[syms, values]`

把 `SetupRiemannManifold` 定义的抽象符号与 `ComputeCurvature` 得到的具体分量连接起来，生成替换规则。之后用 `NITensorReduce` 计算、`ExtractNITensor` 取回数组：

~~~text
rules = MakeCurvatureValueRules[sphSyms, sphVals];
arr = NITensorReduce[Ricsph[DI@a, DI@b] /. rules, {a, b}] // ExtractNITensor[{a, b}];
~~~

相关：`MakeTensorValueRules`、`MakeMetricAdapter`。
""",
    "PopulateCurvatureTReducerRules": """
**签名**：`PopulateCurvatureTReducerRules[name, syms, values]`

把 `SetupRiemannManifold`/`ComputeCurvature` 的结果挂到名字 `name` 上，之后可以用 `NITensorReduce[expr, frees, name]` 直接求值（第三个参数是 reducer 名字）。

内部通过 `StoredTensorData[name, tag]` 和 `name` 的 `UpValue` 实现。**实验性**：参数约定可能变化。
""",
    "DefTensorVariationOperator": """
**签名**：`DefTensorVariationOperator[name]`

定义变分算子 `name[expr, field]`。

~~~text
DefTensorVariationOperator[var];
var[T[a, b], T[c, d]]
    -> (g[a, d] g[b, c] + g[a, c] g[b, d]) / 2
~~~

**选项**：`VarCovDs`、`VarParamDs`：把协变导数/参数导数的分部积分纳入变分。目前这两个选项仍是实验性的（协变导数的规则在 `rest = 1` 时会把常数导数化为 0）。
""",
    "DefLabelledTensor": """
**签名**：`DefLabelledTensor[name, slots, symmetry]`

在槽位列表前加一个标签槽位（`TFLabelSlot["(", ")"]`），得到 `name[label, i1, ...]`。标签指标 `LabelI[...]` 不参与缩并、不参与重命名。

~~~text
DefLabelledTensor[ricL, {mf, mf}, {SCycles@{1, 2}}];
FindIndicesSlots[ricL[LabelI[1], a, b]]  -> {{2} -> mf, {3} -> mf}
~~~
""",
    "DefScalarFunction": """
**签名**：`DefScalarFunction[name, displayName]`

把 `name` 标记为标量函数（`DerFunctionQ[name] ^= True`），导数按链式法则展开；`displayName` 用于排版（传 `None` 跳过）。
""",
    "PerturbationLeviCivitaRiemann": """
**签名**：`PerturbationLeviCivitaRiemann[covd, metric, metricPert, n]`

返回 n 阶线性化黎曼张量，结果是一个 `IndexedExprFunction`；用 `[a, b, c, d]` 取值：

~~~text
PerturbationLeviCivitaRiemann[cd, g, h, 1][a, b, c, d]
~~~

`PerturbationLeviCivitaChristoffel[cd, metric, metricPert, n][a, b, c]` 是联络的对应版本；`VarInverseMatrix` 展开逆度规。
""",
    "ExpandMetricPerturbation": """
**签名**：`ExpandMetricPerturbation[expr, {pert, metric, metricPert}]`

把 `pert` 作用在度规/逆度规上的项展开成度量扰动：

~~~text
ExpandMetricPerturbation[pert[g[DI@a, DI@b], 1], {pert, g, h}]
    -> h[1, DI[a], DI[b]]
ExpandMetricPerturbation[pert[g[a, b], 1], {pert, g, h}]
    -> -h[1, a, b]
~~~

同类函数：`ExpandTensorDetPerturbation`、`ExpandRiemannPerturbation`、`ExpandCovDPerturbation`；`Presets` 的 `ExpandAllPerturbations` 会把它们串起来。
""",
    "xToolsDebugPrint": """
**签名**：`xToolsDebugPrint[tag, msg...]`

调试打印。默认输出到 `Print`；`$xToolsDebugFilter` 控制哪些标签被打印（`All`、`None`、标签列表或谓词）。`ITensorReduceOneTermSorted` 里有多个调试点，标签就是 `ITensorReduceOneTermSorted`。

~~~text
$xToolsDebugFilter = All;
ITensorReduce[T[b, a]]
~~~
""",
    "NoIndicesQ": """
**已知问题**：实现里 `NoIndicesQ[expr_] := Length@FindIndicesSlots === 0`，漏了参数，当前对所有输入都返回 `True`。不要依赖它。
""",
    "RiemannScalars": """
**已知问题**：`::usage` 写的是 `RiemannScalars[riemann, slot, n]`，但实际实现是 `RiemannScalars[riemann, n]`（n ≤ 4，预计算了 `RiemannScalarIndexPermutations`）。
""",
    "SetupPerturbation": """
**已知问题**：`::usage` 写的是两个参数，实际定义需要三个参数（`pert, vard, info`）；`vard` 没有被使用，返回值也不稳定。建议直接用 `DefPerturbationOperator` 和 `Expand*Perturbation`。
""",
    "DI": """
**签名**：`DI[a]`

下指标。**下指标一律写成 `DI[a]`**；`-a` 只是代码里把旧写法规范化成 `DI` 的兼容残余，不要在新代码中使用。

~~~text
ITensorReduce[g[DI@a, DI@b] T[a, b]]
~~~
""",
    "DerConstantQ": """
**签名**：`DerConstantQ[expr]`

判断表达式是否应被视为"导数为 0 的常数"。对数值、带 `Constant` 属性的符号，以及所有参数都是常数的标量函数返回 `True`。

**推荐做法**：用 `SetAttributes[foo, Constant]` 声明常量符号：

~~~text
SetAttributes[dim, Constant];
DerConstantQ[dim]   -> True
~~~

只有不能设置属性的表达式（复合表达式、受保护的符号等）才用 `DerConstantQ[expr] ^= True`。
""",
    "SCycles": """
**签名**：`SCycles[{...}, ...]`

置换的轮换记号；前面加 `-` 表示奇置换。例如 `SCycles@{1, 2}` 是交换 1 和 2，`-SCycles@{1, 2}` 在对称性里表示"交换后变号"（反对称）。

与 `Images` 可以用 `ToImagesNotation`/`ToSCyclesNotation` 互转。
""",
    "Images": """
**签名**：`Images[p1, p2, ...]`

置换的单行像记号；可以比实际度数短，未写出的点视为不动点。前面加 `-` 表示奇置换。
""",
}

ORDER_HINT = [
    # 每个模块里，详细条目优先展示的顺序（其余按源码顺序）
]


@dataclass
class Symbol:
    module: str
    file: str
    name: str
    usage: str
    line: int

    @property
    def status(self) -> str:
        return STATUS.get(self.name, "公开")


def parse_module(module: str, filename: str) -> list[Symbol]:
    path = REPO_ROOT / filename
    symbols: list[Symbol] = []
    for lineno, line in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        match = USAGE_RE.match(line)
        if match:
            symbols.append(Symbol(module, filename, match.group(1), match.group(2), lineno))
    for name, description in EXTRA.get(filename, []):
        if not any(s.name == name for s in symbols):
            symbols.append(Symbol(module, filename, name, description, 0))
    return symbols


def entry_anchor(name: str) -> str:
    return re.sub(r"[^a-z0-9-]", "", name.lower())


def render_symbol(symbol: Symbol, detailed: bool) -> str:
    status = symbol.status
    source = f"[{symbol.file}](../{symbol.file})"
    if symbol.line:
        source = f"[{symbol.file}:{symbol.line}](../{symbol.file}#L{symbol.line})"
    lines = [f"#### `{symbol.name}`", ""]
    tags = [status]
    if detailed:
        tags.append("重点")
    lines.append(" · ".join(tags) + f" · 源码：{source}")
    lines.append("")
    if detailed and symbol.name in OVERRIDES:
        lines.append(OVERRIDES[symbol.name].strip())
        lines.append("")
    else:
        usage = symbol.usage.strip() if symbol.usage.strip() else "（无说明）"
        lines.append(f"**用法**：{usage}")
        lines.append("")
        if symbol.name in OVERRIDES:
            lines.append(OVERRIDES[symbol.name].strip())
            lines.append("")
    return "\n".join(lines)


def main() -> None:
    modules: dict[str, list[Symbol]] = {}
    for module, filename, _ in MODULES:
        modules[module] = parse_module(module, filename)

    out: list[str] = []
    out.append("# Peanotica API 参考")
    out.append("")
    out.append(
        "本文件由 [`docs/gen_api.py`](gen_api.py) 根据各模块源码中的 `::usage` 生成；"
        "人工说明、选项表和示例来自脚本里的 `OVERRIDES` 表。要修改说明，请改脚本后重新生成。"
    )
    out.append("")
    out.append("用法教程见 [features.md](features.md)，实现细节见 [implementation.md](implementation.md)。")
    out.append("")
    out.append("## 约定")
    out.append("")
    out.append("- **签名** 以 `::usage` 或人工说明为准；Mathematica 的 `Options[...]` 可能还有额外选项。")
    out.append("- **状态**：`公开`（稳定可用）、`高级`（公开但偏底层）、`实验性`（接口可能变）、"
               "`未实现`（只有声明）、`已知问题`（见说明）。")
    out.append("- **源码** 链接指向仓库内的 `.wl` 文件与行号，方便对照实现。")
    out.append("- 所有符号都在对应的 <code>Peanotica`XXX`</code> 上下文里；用 `Needs[\"Peanotica`\"]` 加载后直接使用短名。")
    out.append("")
    out.append("## 模块概览")
    out.append("")
    out.append("| 模块 | 符号数 | 源文件 | 内容 |")
    out.append("|---|---:|---|---|")
    for module, filename, description in MODULES:
        out.append(f"| <code>{module}</code> | {len(modules[module])} | [{filename}](../{filename}) | {description} |")
    out.append("")
    out.append("## 符号状态统计")
    out.append("")
    out.append("| 状态 | 数量 |")
    out.append("|---|---:|")
    for status in ["公开", "高级", "实验性", "未实现", "已知问题", "内部"]:
        count = sum(1 for syms in modules.values() for s in syms if s.status == status)
        if count:
            out.append(f"| {status} | {count} |")
    out.append("")

    for module, filename, description in MODULES:
        syms = modules[module]
        out.append(f"## <code>{module}</code>")
        out.append("")
        out.append(description)
        out.append("")
        detailed = [s for s in syms if s.name in OVERRIDES]
        rest = [s for s in syms if s.name not in OVERRIDES]
        if detailed:
            out.append("### 重点符号")
            out.append("")
            for symbol in detailed:
                out.append(render_symbol(symbol, True))
        if rest:
            out.append("### 完整列表")
            out.append("")
            for symbol in rest:
                out.append(render_symbol(symbol, False))
        out.append("")

    out.append("## 未实现与已知问题汇总")
    out.append("")
    out.append("以下符号已经声明了 `::usage`，但没有实现或存在已知问题：")
    out.append("")
    for status, title in [("未实现", "未实现"), ("已知问题", "已知问题"), ("实验性", "实验性")]:
        names = sorted(s.name for syms in modules.values() for s in syms if s.status == status)
        if names:
            out.append(f"### {title}")
            out.append("")
            out.append("、".join(f"`{name}`" for name in names))
            out.append("")

    OUTPUT.write_text("\n".join(out) + "\n", encoding="utf-8")
    total = sum(len(syms) for syms in modules.values())
    print(f"wrote {OUTPUT.relative_to(REPO_ROOT)} with {total} symbols")


if __name__ == "__main__":
    main()
