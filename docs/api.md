# Peanotica API 参考

本文件由 [`docs/gen_api.py`](gen_api.py) 根据各模块源码中的 `::usage` 生成；人工说明、选项表和示例来自脚本里的 `OVERRIDES` 表。要修改说明，请改脚本后重新生成。

用法教程见 [features.md](features.md)，实现细节见 [implementation.md](implementation.md)。

## 约定

- **签名** 以 `::usage` 或人工说明为准；Mathematica 的 `Options[...]` 可能还有额外选项。
- **状态**：`公开`（稳定可用）、`高级`（公开但偏底层）、`实验性`（接口可能变）、`未实现`（只有声明）、`已知问题`（见说明）。
- **源码** 链接指向仓库内的 `.wl` 文件与行号，方便对照实现。
- 所有符号都在对应的 <code>Peanotica`XXX`</code> 上下文里；用 `Needs["Peanotica`"]` 加载后直接使用短名。

## 模块概览

| 模块 | 符号数 | 源文件 | 内容 |
|---|---:|---|---|
| <code>Peanotica`Internal`</code> | 7 | [Internal.wl](../Internal.wl) | 内部工具：调试打印、排序辅助、数组化简。 |
| <code>Peanotica`Perm`</code> | 35 | [Perm.wl](../Perm.wl) | 置换代数、预定义对称群、WSTP 后端封装。 |
| <code>Peanotica`Core`</code> | 160 | [Core.wl](../Core.wl) | 指标/槽位协议、规范化、度规、ETensor/NITensor、显示。 |
| <code>Peanotica`DiffGeo`</code> | 65 | [DiffGeo.wl](../DiffGeo.wl) | 协变导数、曲率、指标适配、扰动。 |
| <code>Peanotica`AlgSimp`</code> | 22 | [AlgSimp.wl](../AlgSimp.wl) | Bianchi 等多重线性关系的枚举与化简。 |
| <code>Peanotica`Presets`</code> | 21 | [Presets.wl](../Presets.wl) | 流形设置、曲率计算、分量代入、变分。 |

## 符号状态统计

| 状态 | 数量 |
|---|---:|
| 公开 | 195 |
| 高级 | 60 |
| 实验性 | 23 |
| 未实现 | 21 |
| 已知问题 | 3 |
| 内部 | 8 |

## <code>Peanotica`Internal`</code>

内部工具：调试打印、排序辅助、数组化简。

### 重点符号

#### `xToolsDebugPrint`

内部 · 重点 · 源码：[Internal.wl:5](../Internal.wl#L5)

**签名**：`xToolsDebugPrint[tag, msg...]`

调试打印。默认输出到 `Print`；`$xToolsDebugFilter` 控制哪些标签被打印（`All`、`None`、标签列表或谓词）。`ITensorReduceOneTermSorted` 里有多个调试点，标签就是 `ITensorReduceOneTermSorted`。

~~~text
$xToolsDebugFilter = All;
ITensorReduce[T[b, a]]
~~~

### 完整列表

#### `FilterExprList`

内部 · 源码：[Internal.wl:6](../Internal.wl#L6)

**用法**：FilterExprList[filter, expr] returns true if expr is not filtered by filter.

#### `MakeThreadableRules`

内部 · 源码：[Internal.wl:8](../Internal.wl#L8)

**用法**：MakeThreadableRules[lhs :> rhs]

#### `OrderOr`

内部 · 源码：[Internal.wl:10](../Internal.wl#L10)

**用法**：OrderOr[terms, ...]

#### `SymmetryList`

内部 · 源码：[Internal.wl:11](../Internal.wl#L11)

**用法**：SymmetryList[elems, comp]

#### `DefDisplayAs`

内部 · 源码：[Internal.wl:12](../Internal.wl#L12)

**用法**：DefDisplayAs[symbol, displayName]

#### `RelationClosureTableToArray`

未实现 · 源码：[Internal.wl:19](../Internal.wl#L19)

**用法**：（无说明）


## <code>Peanotica`Perm`</code>

置换代数、预定义对称群、WSTP 后端封装。

### 重点符号

#### `ConstructStrongGenSet`

公开 · 重点 · 源码：[Perm.wl:33](../Perm.wl#L33)

**签名**：`ConstructStrongGenSet[genset]`

把任意生成元转换成相对于基 `[1..n]` 的强生成元（Jerrum 的 Schreier–Sims 变体）。`SymmetryOfExpression` 的返回值必须是强生成元；内置构造器已经满足，只有自定义生成元需要这一步。

~~~text
ConstructStrongGenSet[{SCycles@{1, 2, 3}}]
    -> {SCycles[{1, 2, 3}], SCycles[{1, 3, 2}]}
~~~

#### `DoubleCosetRepresentative`

公开 · 重点 · 源码：[Perm.wl:34](../Perm.wl#L34)

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

#### `GroupOrderFromStrongGenSet`

公开 · 重点 · 源码：[Perm.wl:35](../Perm.wl#L35)

**签名**：`GroupOrderFromStrongGenSet[genset]`

返回强生成元张成的群的阶。`{}` 返回 1；单个带符号二元轮换返回 2。

#### `Images`

公开 · 重点 · 源码：[Perm.wl](../Perm.wl)

**签名**：`Images[p1, p2, ...]`

置换的单行像记号；可以比实际度数短，未写出的点视为不动点。前面加 `-` 表示奇置换。

#### `SCycles`

公开 · 重点 · 源码：[Perm.wl](../Perm.wl)

**签名**：`SCycles[{...}, ...]`

置换的轮换记号；前面加 `-` 表示奇置换。例如 `SCycles@{1, 2}` 是交换 1 和 2，`-SCycles@{1, 2}` 在对称性里表示"交换后变号"（反对称）。

与 `Images` 可以用 `ToImagesNotation`/`ToSCyclesNotation` 互转。

### 完整列表

#### `NotationOfPermutation`

公开 · 源码：[Perm.wl:8](../Perm.wl#L8)

**用法**：NotationOfPermutation[perm]

#### `ConvertPermutationNotation`

未实现 · 源码：[Perm.wl:9](../Perm.wl#L9)

**用法**：ConvertPermutationNotation[perm, notation]

#### `ToImagesNotation`

公开 · 源码：[Perm.wl:12](../Perm.wl#L12)

**用法**：ToImagesNotation[perm]

#### `ToSCyclesNotation`

公开 · 源码：[Perm.wl:13](../Perm.wl#L13)

**用法**：ToSCyclesNotation[perm]

#### `MinPermutationLength`

公开 · 源码：[Perm.wl:14](../Perm.wl#L14)

**用法**：MinPermutationLength[perm]

#### `ShiftPermutation`

公开 · 源码：[Perm.wl:15](../Perm.wl#L15)

**用法**：ShiftPermutation[perm, length]

#### `ShiftAndJoinGenSets`

公开 · 源码：[Perm.wl:16](../Perm.wl#L16)

**用法**：ShiftAndJoinGenSets[gensets, lengths]

#### `SignedInversePermutation`

公开 · 源码：[Perm.wl:17](../Perm.wl#L17)

**用法**：SignedInversePermutation[perm]

#### `SeparatePermSign`

公开 · 源码：[Perm.wl:18](../Perm.wl#L18)

**用法**：SeparatePermSign[perm]

#### `PPermPermutationProduct`

公开 · 源码：[Perm.wl:19](../Perm.wl#L19)

**用法**：PPermPermutationProduct[perm1, perm2, ...]

#### `SymmetricGenSet`

公开 · 源码：[Perm.wl:22](../Perm.wl#L22)

**用法**：SymmetricGenSet[n1, n2, ...]

#### `RiemannSymmetricGenSet`

公开 · 源码：[Perm.wl:23](../Perm.wl#L23)

**用法**：RiemannSymmetricGenSet[n1, n2, n3, n4]

#### `BlockSymmetricGenSet`

公开 · 源码：[Perm.wl:24](../Perm.wl#L24)

**用法**：BlockSymmetricGenSet[list1, list2, ...]

#### `RiemannMonomialGenSet`

公开 · 源码：[Perm.wl:25](../Perm.wl#L25)

**用法**：RiemannMonomialGenSet[n]

#### `ScalarMonomialDummiesGenSet`

公开 · 源码：[Perm.wl:26](../Perm.wl#L26)

**用法**：ScalarMonomialDummiesGenSet[n]

#### `DummiesGenSet`

公开 · 源码：[Perm.wl:27](../Perm.wl#L27)

**用法**：DummiesGenSet[sign, {{u1, d1}, ...}]

#### `SymmetryOfSortedList`

公开 · 源码：[Perm.wl:29](../Perm.wl#L29)

**用法**：SymmetryOfSortedList[list]

#### `SortedPartitions`

公开 · 源码：[Perm.wl:30](../Perm.wl#L30)

**用法**：SortedPartitions[n]

#### `PPermGroupElements`

公开 · 源码：[Perm.wl:37](../Perm.wl#L37)

**用法**：PPermGroupElements[g] gives all the group elements generated by g.

#### `PPermStabilizer`

公开 · 源码：[Perm.wl:38](../Perm.wl#L38)

**用法**：PPermStabilizer[genset, p] gives a list of generators of the stabilizer.

#### `PPermOpenLogFile`

公开 · 源码：[Perm.wl:39](../Perm.wl#L39)

**用法**：PPermOpenLogFile[path] opens a log file for the logs to be printed.

#### `PPermCloseLogFile`

公开 · 源码：[Perm.wl:40](../Perm.wl#L40)

**用法**：PPermCloseLogFile[] closes the current log file.

#### `DoubleTransversalInSymmetricGroup`

公开 · 源码：[Perm.wl:41](../Perm.wl#L41)

**用法**：DoubleTransversalInSymmetricGroup[S, D]

#### `UseTwoStep`

公开 · 源码：[Perm.wl:43](../Perm.wl#L43)

**用法**：UseTwoStep is a boolean option for DoubleCosetRepresentative specifying whether to use the two-step method described by Portugal: apply the right coset representative algorithm on the stable points of \[LeftAngleBracket]D\[RightAngleBracket] first, and then apply Butler's algorithm on the result. It's claimed that such method is more efficient, while profiling result shows otherwise. The default is False.

#### `PPermVerbose`

公开 · 源码：[Perm.wl:44](../Perm.wl#L44)

**用法**：PPermVerbose is a boolean option for various PPerm functions specifying whether to print messages to log file. This option is maining for debug purpose, and the messages are only enabled in debug builds. The default is False.

#### `$PPermLink`

公开 · 源码：[Perm.wl:47](../Perm.wl#L47)

**用法**：$PPermLink is a global variable holding the LinkObject of the external executable of PPerm.

#### `PPermConnect`

公开 · 源码：[Perm.wl:48](../Perm.wl#L48)

**用法**：PPermConnect[] creates the connection to the external executable of PPerm.

#### `PPermDisconnect`

公开 · 源码：[Perm.wl:49](../Perm.wl#L49)

**用法**：PPermDisconnect[] disconnects the external executable.

#### `PPermEnsureLink`

公开 · 源码：[Perm.wl:50](../Perm.wl#L50)

**用法**：PPermEnsureLink[] checks if $PPermLink is valid, and calls PPermConnect[] if it's not.

#### `RubiksCubeGenSet`

未实现 · 源码：[Perm.wl](../Perm.wl)

**用法**：（未实现）占位符号。


## <code>Peanotica`Core`</code>

指标/槽位协议、规范化、度规、ETensor/NITensor、显示。

### 重点符号

#### `FindIndicesSlots`

公开 · 重点 · 源码：[Core.wl:7](../Core.wl#L7)

**签名**：`FindIndicesSlots[expr]`、`FindIndicesSlots[expr, prefix]`

返回 `{{pos...} -> slotType, ...}`：每个指标槽位在表达式中的位置，以及槽位类型。`prefix` 会把给定位置前缀加到每个结果上，便于在自定义对象里复用子表达式的槽位。

**示例**：

~~~text
FindIndicesSlots[T[a, b]]        -> {{1} -> mf, {2} -> mf}
FindIndicesSlots[T[a, b], {9}]   -> {{9, 1} -> mf, {9, 2} -> mf}
~~~

**注意**：有 `HoldFirst` 属性，可用模式定义；自定义对象请用 `TagSet`/`UpValue`。`Plus` 返回的槽位会带 `SummedIndices` 标记；`IndexedSum` 会删掉被求和的槽位。

#### `ISort`

公开 · 重点 · 源码：[Core.wl:14](../Core.wl#L14)

**签名**：`ISort[expr]`、`ISort[expr, sorter]`

只对无序乘积（`Times`、`Wedge` 等）的因子排序，不碰指标。排序键依次是：对称群阶数降序、槽位数降序、去指标表达式升序；排序后记录相邻全等因子（用于块对称性）。

结果形如 `ISortedProduct[head, args, symList]`；`ReleaseISort` 可以还原成普通表达式。`ITensorReduce` 内部会调用它。

#### `DI`

公开 · 重点 · 源码：[Core.wl:28](../Core.wl#L28)

**签名**：`DI[a]`

下指标。**下指标一律写成 `DI[a]`**；`-a` 只是代码里把旧写法规范化成 `DI` 的兼容残余，不要在新代码中使用。

~~~text
ITensorReduce[g[DI@a, DI@b] T[a, b]]
~~~

#### `IndexScope`

公开 · 重点 · 源码：[Core.wl:80](../Core.wl#L80)

**签名**：`IndexScope[expr]`

把 `expr` 当作标量：内部指标都是局部哑指标，不与外层缩并，也不影响外层命名。`FindIndicesSlots[IndexScope[expr]] = {}`；在笔记本里显示为红色圆括号。

~~~text
FindIndicesSlots[IndexScope[T[a, a]]] -> {}
~~~

#### `MakeIndexRule`

公开 · 重点 · 源码：[Core.wl:92](../Core.wl#L92)

**签名**：`MakeIndexRule[lhs -> rhs, frees]`、`MakeIndexRule[lhs :> rhs, frees, others]`

把"含自由指标的表达式"变成规则：`frees` 里的指标变成模式变量，`rhs` 里的哑指标自动唯一化。

~~~text
g[DI@a, DI@b] /. MakeIndexRule[g[a, b] -> p[a] p[b] p[c] p[DI@c], {a, b}]
    -> p[c] p[DI[a]] p[DI[b]] p[DI[c]]
~~~

#### `MakeIndexFunction`

公开 · 重点 · 源码：[Core.wl:93](../Core.wl#L93)

**签名**：`MakeIndexFunction[expr, frees]`

把含自由指标的表达式变成函数，`frees` 按位置变成参数，哑指标自动唯一化。

~~~text
MakeIndexFunction[p[a] p[b] p[c] p[DI@c], {a, b}][DI@a, a]
    -> p[a] p[b] p[DI[a]] p[DI[b]]
~~~

#### `IndexedSum`

公开 · 重点 · 源码：[Core.wl:96](../Core.wl#L96)

**签名**：`IndexedSum[expr, {i, type}, ...]`

显式的指标求和（不是张量缩并）。如果求和指标在 `expr` 中完全不出现，直接乘上 `DimensionOfSlotType[type]`；如果是 `Times`，不含该指标的因子会被提到求和号外。

~~~text
IndexedSum[5, {a, mf}]                          -> 20
FindIndicesSlots[IndexedSum[T[a, b], {a, mf}]]  -> {{1, 2} -> mf}
~~~

相关：`ScalarIndexedSumQ`、`MoveIndexedSumToOutermost`、`MoveIndexedSumToInnermost`、`IndexedSumTimes`。

#### `MetricOfSlotType`

公开 · 重点 · 源码：[Core.wl:107](../Core.wl#L107)

**签名**：`MetricOfSlotType[type]`

槽位类型 `type` 的默认度规。可以像普通张量一样使用；也可以用 `MetricOfSlotType[type] ^= metric` 指定具体度规。

默认行为假设上下指标成对：

~~~text
MetricOfSlotType[mf][a, DI@b] T[b, c]  -> T[a, c]
MetricOfSlotType[mf][DI@a, a]          -> 4
~~~

#### `DefSimpleMetric`

公开 · 重点 · 源码：[Core.wl:108](../Core.wl#L108)

**签名**：`DefSimpleMetric[symbol, type, symSign]`

定义度规张量并安装缩并规则。

- `symSign = 1`：对称；`-1`：反对称；`0`：无对称性；
- 默认 `ContractionRuleMode -> "Normal"`：`g[DI@a, DI@b]` 降指标、`g[a, b]` 升指标、`g[a, DI@b]` 作为 δ；
- `"AllUp"`：不区分上下号，乘法直接替换指标；
- 反对称度规的缩并规则目前仍按对称情形处理（代码中有 TODO）。

**选项**：`ContractionRuleMode`、`DisplayName` 等。

#### `ContractMetric`

公开 · 重点 · 源码：[Core.wl:113](../Core.wl#L113)

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

#### `DefSimpleTensor`

公开 · 重点 · 源码：[Core.wl:117](../Core.wl#L117)

**签名**：`DefSimpleTensor[name, slots, symmetry]`

按槽位类型列表定义张量，并安装 `FindIndicesSlots`、`SymmetryOfExpression` 和排版规则。

- `slots` 是槽位类型列表；`TFLabelSlot`、`TFFunctionSlot` 这类非指标槽位会被 `FindIndicesSlots` 跳过；
- `symmetry` 是带符号置换生成元列表；`{}` 表示没有对称性。

**选项**：`DisplayName`、`IndexStyle`、`AllowSubsuperscriptBox`（见 `TensorGridBox`）。

~~~text
DefSimpleTensor[T, {mf, mf}, {SCycles@{1, 2}}];
DefSimpleTensor[R, {mf, mf, mf, mf}, RiemannSymmetricGenSet[1]];
~~~

#### `DefSimpleSlotType`

公开 · 重点 · 源码：[Core.wl:118](../Core.wl#L118)

**签名**：`DefSimpleSlotType[type, dim, {names, fallback}]`

定义一个槽位类型：维数、可用指标名列表、候选名用尽时的兜底生成器。等价于设置 `DimensionOfSlotType`、`IndicesCandidateOfSlotType`。

**选项**：`DisplayName -> name` 设置类型的显示名。

~~~text
DefSimpleSlotType[mf, 4, {Symbol /@ CharacterRange["a", "z"], DefaultIndex}];
~~~

#### `NoIndicesQ`

已知问题 · 重点 · 源码：[Core.wl:119](../Core.wl#L119)

**已知问题**：实现里 `NoIndicesQ[expr_] := Length@FindIndicesSlots === 0`，漏了参数，当前对所有输入都返回 `True`。不要依赖它。

#### `ITensorReduceOneTerm`

公开 · 重点 · 源码：[Core.wl:147](../Core.wl#L147)

**签名**：`ITensorReduceOneTerm[expr, opts]`

与 `ITensorReduce` 相同的规范化，但不展开 `Plus`（`Plus` 会逐项映射）。当你确定输入是单项时更快。

~~~text
ITensorReduceOneTerm[T[b, a]] -> T[a, b]
~~~

#### `ITensorReduce`

公开 · 重点 · 源码：[Core.wl:156](../Core.wl#L156)

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

#### `ETensor`

公开 · 重点 · 源码：[Core.wl:172](../Core.wl#L172)

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

#### `NITensorReduce`

公开 · 重点 · 源码：[Core.wl:185](../Core.wl#L185)

**签名**：`NITensorReduce[expr, frees]`、`NITensorReduce[expr, frees, reducer]`

对 `NITensor` 表达式做缩并：合并重复指标、挑选取缩并对、求和。第三个参数可以是一个"reducer 名字"，配合 `PopulateCurvatureTReducerRules` 使用。

~~~text
NITensorReduce[NITensor[A, {a}] NITensor[A, {b}], {}]
~~~

**注意**：两个全等的 `NITensor` 相乘会被合并成 `Power`，当前不处理；请避免让两个因子在词法上完全相同，或改用不同的哑指标名。

### 完整列表

#### `RenamingGroupOfIndexName`

公开 · 源码：[Core.wl:15](../Core.wl#L15)

**用法**：RenamingGroupOfIndexName[a] gives the renaming group of the index name a. Index names with the same renaming group can be interchanged, while the renaming group None cannot be renamed.

#### `RenamableIndexNameQ`

公开 · 源码：[Core.wl:16](../Core.wl#L16)

**用法**：RenamableIndexNameQ[a]

#### `ExpressionPassThroughQ`

公开 · 源码：[Core.wl:17](../Core.wl#L17)

**用法**：ExpressionPassThroughQ[T[..., i, ...], expr, pos] gives True if T[..., i, ...] expr = T[..., i * expr, ...], where the position of i is given by pos.

#### `SumPassThroughQ`

公开 · 源码：[Core.wl:18](../Core.wl#L18)

**用法**：SumPassThroughQ[expr, pos]

#### `SignOfSymmetricPair`

公开 · 源码：[Core.wl:19](../Core.wl#L19)

**用法**：SignOfSymmetricPair[type1, type2, ind] returns 1 or -1.

#### `InterchangableIndexPairQ`

公开 · 源码：[Core.wl:20](../Core.wl#L20)

**用法**：InterchangableIndexPairQ[type, a] gives true if the index name a and DI[a] can be interchanged.

#### `ITensor`

公开 · 源码：[Core.wl:21](../Core.wl#L21)

**用法**：ITensor[expr, indices] represents a general tensor object.

#### `AbsIndexNameQ`

公开 · 源码：[Core.wl:22](../Core.wl#L22)

**用法**：（无说明）

#### `DisplayName`

公开 · 源码：[Core.wl:23](../Core.wl#L23)

**用法**：（无说明）

#### `DisplayNameOf`

公开 · 源码：[Core.wl:24](../Core.wl#L24)

**用法**：DisplayNameOf[symbol]

#### `FindTermIndicesList`

公开 · 源码：[Core.wl:25](../Core.wl#L25)

**用法**：FindTermIndicesList[expr]

#### `LabelI`

公开 · 源码：[Core.wl:29](../Core.wl#L29)

**用法**：LabelI[a] represents a label index, with no name.

#### `DefaultIndex`

公开 · 源码：[Core.wl:30](../Core.wl#L30)

**用法**：DefaultIndex[n] is returned by the default definition of GetIndexOfSlotType[type, indices].

#### `TempIndex`

公开 · 源码：[Core.wl:31](../Core.wl#L31)

**用法**：TempIndex[n] is a temporary index generated during internal

#### `$TempIndexNumber`

公开 · 源码：[Core.wl:32](../Core.wl#L32)

**用法**：$TempIndexNumber is a global variable giving the next global unique id of TempIndex to be used.

#### `ToUpIndex`

公开 · 源码：[Core.wl:34](../Core.wl#L34)

**用法**：ToUpIndex[ind]

#### `SeparateIndexName`

公开 · 源码：[Core.wl:35](../Core.wl#L35)

**用法**：SeparateIndexName[index] returns {name, pattern} where name is the name of the index, pattern is it's pattern. An index is determined by its name and pattern.

#### `IndexBoxForm`

公开 · 源码：[Core.wl:36](../Core.wl#L36)

**用法**：IndexBoxForm[index]

#### `IndexName`

公开 · 源码：[Core.wl:37](../Core.wl#L37)

**用法**：（无说明）

#### `GetIndexOfSlotType`

公开 · 源码：[Core.wl:38](../Core.wl#L38)

**用法**：GetIndexOfSlotType[type, inds] returns a new index that is not in inds.

#### `GetIndicesOfSlotType`

公开 · 源码：[Core.wl:39](../Core.wl#L39)

**用法**：GetIndicesOfSlotType[types, inds] returns a list of index for each type.

#### `IndicesCandidateOfSlotType`

公开 · 源码：[Core.wl:40](../Core.wl#L40)

**用法**：IndicesCandidateOfSlotType[type] returns a list, .

#### `GetUniqueIndexOfSlotType`

公开 · 源码：[Core.wl:41](../Core.wl#L41)

**用法**：GetUniqueIndexOfSlotType[type] returns a globally unique index using the given slot type as a hint. the default definition returns TempIndex[++$TempIndexNumber].

#### `UInd`

公开 · 源码：[Core.wl:42](../Core.wl#L42)

**用法**：UInd[ind]

#### `GetUniqueIndicesOfSlotType`

公开 · 源码：[Core.wl:43](../Core.wl#L43)

**用法**：GetUniqueIndicesOfSlotType[type, n]

#### `IndexList`

公开 · 源码：[Core.wl:44](../Core.wl#L44)

**用法**：IndexList[i1, i2, ...] represents a list of indices, similar to List. The purpose of this head is that it may have an overall negative sign.

#### `DefaultRenamingGroup`

公开 · 源码：[Core.wl:45](../Core.wl#L45)

**用法**：DefaultRenamingGroup is the default renaming group of index name.

#### `RenamableIndicesQ`

公开 · 源码：[Core.wl:46](../Core.wl#L46)

**用法**：RenamableIndicesQ[a, b] returns true if index names a and b can be interchanged.

#### `GroupedIndexList`

高级 · 源码：[Core.wl:47](../Core.wl#L47)

**用法**：GroupedIndexList[type1 -> {i1 -> {v1, v2, ...}, ...}] represents a grouped list of indices.

#### `SymmetricIndexGroup`

未实现 · 源码：[Core.wl:48](../Core.wl#L48)

**用法**：SymmetricIndexGroup[pattern, symmetrySpec, namingGroups]

#### `IndexNameGroup`

未实现 · 源码：[Core.wl:49](../Core.wl#L49)

**用法**：IndexNameGroup[namingGroup, indices]

#### `GroupIndexList`

高级 · 源码：[Core.wl:50](../Core.wl#L50)

**用法**：GroupIndexList[{i1 -> v1, i2 -> v2, ...}] groups the indices list into blocks according to their types.

#### `GroupIndexListWithSlotTypes`

高级 · 源码：[Core.wl:51](../Core.wl#L51)

**用法**：GroupIndexListWithSlotTypes[{i1, slot1}, ...]

#### `SortGroupedIndexList`

高级 · 源码：[Core.wl:54](../Core.wl#L54)

**用法**：SortGroupedIndexList[GroupedIndexList[...]] returns a sorted version of GroupedIndexList[...].

#### `FindAndDropFrees`

高级 · 源码：[Core.wl:55](../Core.wl#L55)

**用法**：FindAndDropFrees[GroupedIndexList[...], frees] returns a list containing two GroupedIndexList with the first containing only free indices as specified by frees, the other contains no free indices. FindAndDropFrees[GroupedIndexList[...], Automatic] select frees as those only has one occurance.

#### `GroupedIndexListMapFold`

高级 · 源码：[Core.wl:56](../Core.wl#L56)

**用法**：GroupedIndexListMapFold[f, x, GroupedIndexList[...]] returns a new GroupedIndexList with the entries being replaced by f[x, type1, name1, poses1], where each time f returns {name -> val, x}.

#### `MapGroupedIndexList`

高级 · 源码：[Core.wl:57](../Core.wl#L57)

**用法**：MapGroupedIndexList[f, GroupedIndexList[...]] returns a list by apply f to every entries of the GroupedIndexList with f[type, name, val].

#### `MapIndexedGroupedIndexList`

未实现 · 源码：[Core.wl:58](../Core.wl#L58)

**用法**：MapIndexedGroupedIndexList[f, GroupedIndexList[...]] returns a list by apply f to every entries of the GroupedIndexList with f[type, name, val, index].

#### `FoldListGroupedIndexList`

未实现 · 源码：[Core.wl:59](../Core.wl#L59)

**用法**：FoldGroupedIndexList[f, x, GroupedIndexList[...]] returns

#### `CollectGroupedIndices`

高级 · 源码：[Core.wl:60](../Core.wl#L60)

**用法**：CollectGroupedIndices[GroupedIndexList[...]] returns a list of indices by collecting all the indices in the index list in order.

#### `CollectGroupedIndicesNames`

高级 · 源码：[Core.wl:61](../Core.wl#L61)

**用法**：CollectGroupedIndices[GroupedIndexList[...]] returns a list containing all the index names in the list.

#### `RenameGroupedIndexList`

高级 · 源码：[Core.wl:62](../Core.wl#L62)

**用法**：RenameGroupedIndexList[GroupedIndexList[...], indices, idToType] renames the indices names.

#### `PrependPosToSlotSpec`

高级 · 源码：[Core.wl:63](../Core.wl#L63)

**用法**：PrependPosToSlotSpec[]

#### `$IndexPairPattern`

公开 · 源码：[Core.wl:64](../Core.wl#L64)

**用法**：$IndexPairPattern is global constant with value given by Sort@{IndexNameSlot, DI[IndexNameSlot]}.

#### `ToIndexFunction`

公开 · 源码：[Core.wl:65](../Core.wl#L65)

**用法**：ToIndexFunction[expr, frees] converts expr into a function that takes free indices as arguments.

#### `ReplaceDummiesToUnique`

公开 · 源码：[Core.wl:66](../Core.wl#L66)

**用法**：ReplaceDummiesToUnique[expr, frees] or ReplaceDummiesToUnique[expr]

#### `IndexSlot`

高级 · 源码：[Core.wl:69](../Core.wl#L69)

**用法**：IndexSlot[n] represents an index slot used in IndexedObject.

#### `IndexedObject`

高级 · 源码：[Core.wl:70](../Core.wl#L70)

**用法**：IndexedObject[expr, {name -> type, ...}] represents an indexed object with IndexSlot's and indices.

#### `ToIndexedObject`

高级 · 源码：[Core.wl:71](../Core.wl#L71)

**用法**：ToIndexedObject[expr, inds] returns an IndexedObject.

#### `MapIndexSlots`

高级 · 源码：[Core.wl:72](../Core.wl#L72)

**用法**：MapIndexSlots[fn, expr] replaces all IndexSlot[n] in expr as fn[n], skipping IndexedObject's and ETensor's.

#### `NonDIQ`

公开 · 源码：[Core.wl:73](../Core.wl#L73)

**用法**：NonDIQ[i] returns false on DI[a] or -a, otherwise true.

#### `NoDI`

公开 · 源码：[Core.wl:74](../Core.wl#L74)

**用法**：NoDI[DI[a]] gives a, NoDI[a] gives a if Head[a] =!= DI.

#### `CopyDI`

公开 · 源码：[Core.wl:75](../Core.wl#L75)

**用法**：CopyDI[a, b]

#### `ReplaceFreesRules`

公开 · 源码：[Core.wl:76](../Core.wl#L76)

**用法**：ReplaceFreesRules[indsAndPos, oldFrees, newFrees]

#### `ReplaceFrees`

公开 · 源码：[Core.wl:77](../Core.wl#L77)

**用法**：ReplaceFrees[expr, oldFrees, newFrees]

#### `DummyIndex`

公开 · 源码：[Core.wl:78](../Core.wl#L78)

**用法**：DummyIndex[name, repeats] represents an index with hint that it's a dummy index with specified repeats.

#### `PopulateDummyIndexHint`

公开 · 源码：[Core.wl:79](../Core.wl#L79)

**用法**：PopulateDummyIndexHint[expr, frees]

#### `PutIndexScope`

公开 · 源码：[Core.wl:81](../Core.wl#L81)

**用法**：PutIndexScope[expr, frees]

#### `PermuteIndexList`

公开 · 源码：[Core.wl:83](../Core.wl#L83)

**用法**：PermuteIndexList[list, perm]

#### `ApplyIndices`

公开 · 源码：[Core.wl:84](../Core.wl#L84)

**用法**：ApplyIndices[expr, indPos, indList]

#### `ReplaceIndexNames`

公开 · 源码：[Core.wl:85](../Core.wl#L85)

**用法**：ReplaceIndexNames[expr, reps]

#### `TryReplaceIndexNames`

公开 · 源码：[Core.wl:86](../Core.wl#L86)

**用法**：TryReplaceIndexNames[expr, reps]

#### `MapIndicesSlots`

公开 · 源码：[Core.wl:87](../Core.wl#L87)

**用法**：MapIndicesSlots[fn, inds]

#### `ReplaceIndicesToSame`

公开 · 源码：[Core.wl:88](../Core.wl#L88)

**用法**：ReplaceIndicesToSame[expr, ind]

#### `IndexOrder`

公开 · 源码：[Core.wl:89](../Core.wl#L89)

**用法**：IndexOrder[a, b]

#### `IndexNamedOrder`

公开 · 源码：[Core.wl:90](../Core.wl#L90)

**用法**：IndexNamedOrder[a, b]

#### `IndexedToFunction`

未实现 · 源码：[Core.wl:91](../Core.wl#L91)

**用法**：IndexedToFunction[expr, frees]

#### `ScalarIndexedSumQ`

公开 · 源码：[Core.wl:97](../Core.wl#L97)

**用法**：ScalarIndexedSumQ[expr]

#### `MoveIndexedSumToOutermost`

公开 · 源码：[Core.wl:98](../Core.wl#L98)

**用法**：MoveIndexedSumToOutermost[expr]

#### `MoveIndexedSumToInnermost`

公开 · 源码：[Core.wl:99](../Core.wl#L99)

**用法**：MoveIndexedSumToInnermost[expr]

#### `IndexedSumTimes`

公开 · 源码：[Core.wl:100](../Core.wl#L100)

**用法**：IndexedSumTimes[{inds...}, factor1, factor2, ...]

#### `PopulateIndexedSumTimes`

公开 · 源码：[Core.wl:101](../Core.wl#L101)

**用法**：PopulateIndexedSumTimes[expr]

#### `ReplaceIndexedSumIndsToUnique`

公开 · 源码：[Core.wl:102](../Core.wl#L102)

**用法**：ReplaceIndexedSumIndsToUnique[expr]

#### `ReplaceAllIndexedSumIndsToUnique`

公开 · 源码：[Core.wl:103](../Core.wl#L103)

**用法**：ReplaceAllIndexedSumIndsToUnique[expr]

#### `DimensionOfSlotType`

公开 · 源码：[Core.wl:106](../Core.wl#L106)

**用法**：DimensionOfSlotType[type] represents the dimension of the slot type.

#### `DefSimpleDeltaTensor`

公开 · 源码：[Core.wl:109](../Core.wl#L109)

**用法**：DefSimpleDeltaTensor[symbol, type]

#### `ContractionRuleMode`

公开 · 源码：[Core.wl:110](../Core.wl#L110)

**用法**：ContractionRuleMode is an option for DefSimpleMetric, specifying whether the indices are assumed to be paired.

#### `ContractableMetricQ`

公开 · 源码：[Core.wl:111](../Core.wl#L111)

**用法**：ContractableMetricQ[expr] returns true of expr is a metric that can be used in ContractMetric.

#### `ContractionSlotOfMetric`

公开 · 源码：[Core.wl:112](../Core.wl#L112)

**用法**：ContractionSlotOfMetric[metric] returns 1 or 2, specifiying which slot of the metric to be contracted with. Specifically, for 1 we have p[DI@a] = g[DI@b, DI@a]p[b], while for 2 we have p[DI@a] = g[DI@a, DI@b]p[b]. The default is north-west convention: p[a] = g[a, b]p[DI@b], p[DI@a] = p[b]g[DI@b, DI@a]. Note that the value is only relevant for non-symmetric metrics.

#### `SeparateMetricOne`

公开 · 源码：[Core.wl:114](../Core.wl#L114)

**用法**：SeparateMetricOne[expr, indPattern, metricProvider]

#### `NonTensorTermQ`

公开 · 源码：[Core.wl:120](../Core.wl#L120)

**用法**：NonTensorTermQ[expr]

#### `GroupByTensors`

未实现 · 源码：[Core.wl:121](../Core.wl#L121)

**用法**：GroupByTensors[expr] returns an association with key being tensors and values their coefficients.

#### `UnionClosures`

公开 · 源码：[Core.wl:122](../Core.wl#L122)

**用法**：UnionClosures[{tag1 -> list1, tag2 -> list2, ...}]

#### `UnionClosureValues`

公开 · 源码：[Core.wl:123](../Core.wl#L123)

**用法**：UnionClosureValues[{list1, list2, ...}}

#### `ContractList`

公开 · 源码：[Core.wl:124](../Core.wl#L124)

**用法**：ContractList[list, a, b, slot]

#### `IndexedExprFunction`

高级 · 源码：[Core.wl:125](../Core.wl#L125)

**用法**：IndexedExprFunction[expr, frees, dummies, dummyTypes]

#### `ToIndexedExprFunction`

高级 · 源码：[Core.wl:126](../Core.wl#L126)

**用法**：ToIndexedExprFunction[expr, frees]

#### `RaiseFrees`

高级 · 源码：[Core.wl:127](../Core.wl#L127)

**用法**：RaiseFrees[expr, frees]

#### `WithTempIndex1`

高级 · 源码：[Core.wl:128](../Core.wl#L128)

**用法**：WithTempIndex1[expr]

#### `PermuteIndices`

高级 · 源码：[Core.wl:129](../Core.wl#L129)

**用法**：PermuteIndices[expr, inds, perm]

#### `ImposeSymmetry`

高级 · 源码：[Core.wl:130](../Core.wl#L130)

**用法**：ImposeSymmetry[expr, inds, genset]

#### `ExpandScalarIndexScope`

高级 · 源码：[Core.wl:131](../Core.wl#L131)

**用法**：ExpandScalarIndexScope[expr]

#### `MapScalarIndexScopes`

高级 · 源码：[Core.wl:132](../Core.wl#L132)

**用法**：MapScalarIndexScopes[fn, expr] or MapScalarIndexScopes[fn][expr]

#### `DeepContractMetric`

公开 · 源码：[Core.wl:133](../Core.wl#L133)

**用法**：DeepContractMetric[expr, metrics]

#### `MetricBarrier`

高级 · 源码：[Core.wl:134](../Core.wl#L134)

**用法**：MetricBarrier[expr]

#### `ReleaseMetricBarrier`

高级 · 源码：[Core.wl:135](../Core.wl#L135)

**用法**：ReleaseMetricBarrier[expr]

#### `TDagger`

高级 · 源码：[Core.wl:136](../Core.wl#L136)

**用法**：TDagger[expr]

#### `SymmetryListGenSet`

高级 · 源码：[Core.wl:137](../Core.wl#L137)

**用法**：SymmetryListGenSet[slotCounts, predicate]

#### `HeldTimes`

高级 · 源码：[Core.wl:138](../Core.wl#L138)

**用法**：HeldTimes[terms...]

#### `ISortedProduct`

高级 · 源码：[Core.wl:141](../Core.wl#L141)

**用法**：ISortedProduct[head, args, symList]

#### `ISortedGeneral`

高级 · 源码：[Core.wl:142](../Core.wl#L142)

**用法**：ISortedGeneral[expr]

#### `ISortUnorderedArgs`

高级 · 源码：[Core.wl:143](../Core.wl#L143)

**用法**：ISortUnorderedArgs[expr, args]

#### `ITensorReduceOneTermSorted`

公开 · 源码：[Core.wl:148](../Core.wl#L148)

**用法**：ITensorReduceOneTermSorted[expr, frees, options...]

#### `FreeIndicesSymmetry`

公开 · 源码：[Core.wl:152](../Core.wl#L152)

**用法**：FreeIndicesSymmetry is an option for ITensorReduceOneTerm and related functions, specifying the symmetry of free indices, as if they are contracted with a tensor with certain symmetry.

#### `IndexWrapper`

高级 · 源码：[Core.wl:157](../Core.wl#L157)

**用法**：IndexWrapper[expr, innerInds, outerInds]

#### `PutIndexWrapper`

高级 · 源码：[Core.wl:158](../Core.wl#L158)

**用法**：PutIndexWrapper[expr]

#### `ReleaseIndexWrapper`

高级 · 源码：[Core.wl:159](../Core.wl#L159)

**用法**：ReleaseIndexWrapper[expr]

#### `TensorGridBox`

高级 · 源码：[Core.wl:162](../Core.wl#L162)

**用法**：TensorGridBox[head, indices]

#### `TFIndexSlot`

高级 · 源码：[Core.wl:166](../Core.wl#L166)

**用法**：（无说明）

#### `TFLabelSlot`

高级 · 源码：[Core.wl:167](../Core.wl#L167)

**用法**：LabelSlot can be used

#### `TFFunctionSlot`

高级 · 源码：[Core.wl:168](../Core.wl#L168)

**用法**：（无说明）

#### `DefTensorFormatings`

高级 · 源码：[Core.wl:169](../Core.wl#L169)

**用法**：DefTensorFormatings[name, slots, options...]

#### `ETensorIndices`

高级 · 源码：[Core.wl:173](../Core.wl#L173)

**用法**：ETensorIndices[inds, nullPos, lengths]

#### `CETensor`

未实现 · 源码：[Core.wl:174](../Core.wl#L174)

**用法**：CETensor[array] represents a concrete tensor whose components are ETensor's.

#### `NITensor`

公开 · 源码：[Core.wl:177](../Core.wl#L177)

**用法**：NITensor[expr, indices]

#### `ITensorTranspose`

公开 · 源码：[Core.wl:178](../Core.wl#L178)

**用法**：ITensorTranspose[t, permutation] transposes the tensor according to the given permutation. Similar to Transpose, permutation may contain repeated elements, in which case the resulting tensor has lower rank.

#### `ITensorOuter`

公开 · 源码：[Core.wl:179](../Core.wl#L179)

**用法**：ITensorOutter[prod, t1, t2, {{s11, s12}, ...}]

#### `ITensorSum`

公开 · 源码：[Core.wl:180](../Core.wl#L180)

**用法**：ITensorSum[t, {a1, a2, ...}]

#### `ITensorScalarMultiply`

公开 · 源码：[Core.wl:181](../Core.wl#L181)

**用法**：ITensorScalarMultiply[prod, tensor, scalar]

#### `ITensorFixedContract`

公开 · 源码：[Core.wl:182](../Core.wl#L182)

**用法**：ITensorFixedContract[t1, t2, n1, n2]

#### `ITensorContract`

公开 · 源码：[Core.wl:183](../Core.wl#L183)

**用法**：ITensorContract[t1, t2, {{s11, s12}, ...}]

#### `ITensorContractTwo`

公开 · 源码：[Core.wl:184](../Core.wl#L184)

**用法**：ITensorContractTwo[prod, t1, t2, {{s11, s12}, ...}]

#### `ExtractNITensor`

公开 · 源码：[Core.wl:186](../Core.wl#L186)

**用法**：ExtractNITensor[expr, inds]

#### `NITensorReduceExtract`

公开 · 源码：[Core.wl:187](../Core.wl#L187)

**用法**：NITensorReduceExtract[expr, frees]

#### `ReduceNITensorContractions`

公开 · 源码：[Core.wl:188](../Core.wl#L188)

**用法**：ReduceNITensorContractions[prod, factors, frees]

#### `ScalarValue`

未实现 · 源码：[Core.wl:189](../Core.wl#L189)

**用法**：ScalarValue[expr] represents a scalar value expr.

#### `ToNITensorIndex`

高级 · 源码：[Core.wl:190](../Core.wl#L190)

**用法**：ToNITensorIndex[ind, slot]

#### `FromNITensorIndex`

高级 · 源码：[Core.wl:191](../Core.wl#L191)

**用法**：FromNITensorIndex[ind]

#### `SumIndexOfSlot`

高级 · 源码：[Core.wl:192](../Core.wl#L192)

**用法**：SumIndexOfSlot[slot, ind, expr]

#### `ETensorArray`

高级 · 源码：[Core.wl:193](../Core.wl#L193)

**用法**：ETensorArray[fn, {inds1, inds2, ...}]

#### `WrapETensor`

未实现 · 源码：[Core.wl:194](../Core.wl#L194)

**用法**：WrapETensor[ETensor[...], slots]

#### `ExpandedGDelta`

高级 · 源码：[Core.wl:195](../Core.wl#L195)

**用法**：ExpandedGDelta[metric, inds]

#### `ITensorRowReduce`

未实现 · 源码：[Core.wl:197](../Core.wl#L197)

**用法**：ITensorRowReduce[tensor]

#### `SummedIndices`

高级 · 源码：[Core.wl](../Core.wl)

**用法**：`FindIndicesSlots` 对 `Plus` 返回的内部标记，表示指标是跨项求和的。

#### `IndexNameSlot`

高级 · 源码：[Core.wl](../Core.wl)

**用法**：上指标的模板（`DI[IndexNameSlot]` 是下指标模板）。

#### `FindIndicesSlotsAndNames`

高级 · 源码：[Core.wl](../Core.wl)

**用法**：`FindIndicesSlots` 的变体，同时返回槽位上的指标表达式。

#### `AddSymmetryToGroupedIndexList`

高级 · 源码：[Core.wl](../Core.wl)

**用法**：给分组指标列表附加对称性，供规范化内部使用。

#### `ReleaseISort`

公开 · 源码：[Core.wl](../Core.wl)

**用法**：把 `ISortedProduct`/`ISortedGeneral` 还原成普通表达式。

#### `PreITensorReduce`

高级 · 源码：[Core.wl](../Core.wl)

**用法**：`ITensorReduce` 进入规范化前的扩展点（可写 UpValue）。

#### `PostITensorReduce`

高级 · 源码：[Core.wl](../Core.wl)

**用法**：`ITensorReduce` 离开规范化时的扩展点（可写 UpValue）。

#### `ExpandToTensorPolynomial`

高级 · 源码：[Core.wl](../Core.wl)

**用法**：把非张量因子乘 `Plus`、正整数幂展开成单项之和。

#### `SignOfUpSlot`

高级 · 源码：[Core.wl](../Core.wl)

**用法**：上指标返回 1，下指标返回 -1。

#### `CanonicalizationUnitQ`

内部 · 源码：[Core.wl](../Core.wl)

**用法**：判断一个表达式是否适合作为规范化的最小单位。


## <code>Peanotica`DiffGeo`</code>

协变导数、曲率、指标适配、扰动。

### 重点符号

#### `LeviCivitaChristoffel`

公开 · 重点 · 源码：[DiffGeo.wl:6](../DiffGeo.wl#L6)

**签名**：`LeviCivitaChristoffel[cd, metric]`、`LeviCivitaChristoffel[cd, metric, metricInv]`

由度规和（可能非度规相容的）协变导数构造 Levi-Civita 联络：

~~~text
LeviCivitaChristoffel[cd, metric][a, DI@b, DI@c]
    = 1/2 g^{ad}(∇_b g_{dc} + ∇_c g_{bd} - ∇_d g_{bc})
~~~

`LeviCivitaChristoffelDer[cd, der, metric, metricInv]` 允许把普通导数 `der` 换成参数导数；`LeviCivitaChristoffelValue` 返回分量数组。

#### `RiemannDifference`

公开 · 重点 · 源码：[DiffGeo.wl:9](../DiffGeo.wl#L9)

**签名**：`RiemannDifference[cd, chris]`

两个协变导数的黎曼张量之差：`RiemannDifference[cd, chris][a, b, c, d]` = 坐标导数项 + Christoffel 二次项。`chris` 是联络 `cd` 相对于 Levi-Civita 联络的差。

`RiemannDifferenceValue[slot, cd, chris]` 返回分量数组。

#### `SortCovD`

公开 · 重点 · 源码：[DiffGeo.wl:18](../DiffGeo.wl#L18)

**签名**：`SortCovD[expr, cd, commutator]`、`SortCovD[expr, cd, commutator, orderedQ]`

把嵌套的协变导数按指标顺序整理；交换顺序时用 `commutator` 替换。

~~~text
SortCovD[cd[cd[T[a, b], d], c], cd, CovDCommutatorOfRiemann[R]]
    -> cd[cd[T[a, b], c], d] - R[c, d, DI[i], b] T[a, i] - R[c, d, DI[i], a] T[i, b]
~~~

`CovDCommutatorOfRiemann[R]`、`CovDCommutatorNoTorsion[expr, a, b, R]` 提供常用的交换子；`CovDDefaultIndexOrderedQ` 是默认排序判据。

#### `DerConstantQ`

公开 · 重点 · 源码：[DiffGeo.wl:23](../DiffGeo.wl#L23)

**签名**：`DerConstantQ[expr]`

判断表达式是否应被视为"导数为 0 的常数"。对数值、带 `Constant` 属性的符号，以及所有参数都是常数的标量函数返回 `True`。

**推荐做法**：用 `SetAttributes[foo, Constant]` 声明常量符号：

~~~text
SetAttributes[dim, Constant];
DerConstantQ[dim]   -> True
~~~

只有不能设置属性的表达式（复合表达式、受保护的符号等）才用 `DerConstantQ[expr] ^= True`。

#### `DefParametreDerivativeOperator`

公开 · 重点 · 源码：[DiffGeo.wl:34](../DiffGeo.wl#L34)

**签名**：`DefParametreDerivativeOperator[sym, opts]`

定义对参数的导数（坐标、扰动变量等）。

**选项**：`DerParameters`（参数列表，用于生成 `sym[p1, p2] = 0`）、`DisplayName`、`AllowPassThrough`、`DerConstants`、`DerFunctions`。

~~~text
DefParametreDerivativeOperator[paramd, DerParameters -> {t, r}];
paramd[Null, t][f[r]]  -> f'[r]
paramd[r, r]           -> 1
paramd[r, t]           -> 0
~~~

#### `DefTensorDerivativeOperator`

公开 · 重点 · 源码：[DiffGeo.wl:36](../DiffGeo.wl#L36)

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

#### `DefRiemannCurvature`

公开 · 重点 · 源码：[DiffGeo.wl:57](../DiffGeo.wl#L57)

**签名**：`DefRiemannCurvature[slot, riemann, ricci, ricciScalar]`

一次性定义黎曼张量（`RiemannSymmetricGenSet[1]`）、Ricci 张量（对称）和 Ricci 标量，并安装 8 条 Ricci 缩并规则。

~~~text
DefRiemannCurvature[mf, R, Ric, Rs];
ITensorReduce[R[a, b, c, DI@b]]  -> Ric[a, c]
ITensorReduce[Ric[DI@a, a]]      -> Rs
~~~

#### `RiemannScalars`

已知问题 · 重点 · 源码：[DiffGeo.wl:59](../DiffGeo.wl#L59)

**已知问题**：`::usage` 写的是 `RiemannScalars[riemann, slot, n]`，但实际实现是 `RiemannScalars[riemann, n]`（n ≤ 4，预计算了 `RiemannScalarIndexPermutations`）。

#### `RiemannBianchiRelations`

公开 · 重点 · 源码：[DiffGeo.wl:65](../DiffGeo.wl#L65)

**签名**：`RiemannBianchiRelations[expr, covd, riemann]`

生成 Bianchi 关系（返回等式列表）：

~~~text
RiemannBianchiRelations[R[a, b, c, d], cd, R]
    -> {R[a, b, c, d] + R[a, c, d, b] + R[a, d, b, c]}
~~~

对 `Plus`/`Times` 递归；对 `cd[R[...], e]` 生成第一 Bianchi 和导数 Bianchi。

#### `PerturbationLeviCivitaRiemann`

公开 · 重点 · 源码：[DiffGeo.wl:79](../DiffGeo.wl#L79)

**签名**：`PerturbationLeviCivitaRiemann[covd, metric, metricPert, n]`

返回 n 阶线性化黎曼张量，结果是一个 `IndexedExprFunction`；用 `[a, b, c, d]` 取值：

~~~text
PerturbationLeviCivitaRiemann[cd, g, h, 1][a, b, c, d]
~~~

`PerturbationLeviCivitaChristoffel[cd, metric, metricPert, n][a, b, c]` 是联络的对应版本；`VarInverseMatrix` 展开逆度规。

#### `ExpandMetricPerturbation`

公开 · 重点 · 源码：[DiffGeo.wl:81](../DiffGeo.wl#L81)

**签名**：`ExpandMetricPerturbation[expr, {pert, metric, metricPert}]`

把 `pert` 作用在度规/逆度规上的项展开成度量扰动：

~~~text
ExpandMetricPerturbation[pert[g[DI@a, DI@b], 1], {pert, g, h}]
    -> h[1, DI[a], DI[b]]
ExpandMetricPerturbation[pert[g[a, b], 1], {pert, g, h}]
    -> -h[1, a, b]
~~~

同类函数：`ExpandTensorDetPerturbation`、`ExpandRiemannPerturbation`、`ExpandCovDPerturbation`；`Presets` 的 `ExpandAllPerturbations` 会把它们串起来。

### 完整列表

#### `LeviCivitaChristoffelDer`

公开 · 源码：[DiffGeo.wl:5](../DiffGeo.wl#L5)

**用法**：LeviCivitaChristoffel[cd, der, metric]

#### `CovDDifference`

公开 · 源码：[DiffGeo.wl:10](../DiffGeo.wl#L10)

**用法**：CovDDifference[expr, inds, chrisProvider, a] gives the difference

#### `CovDDifferenceDirect`

公开 · 源码：[DiffGeo.wl:11](../DiffGeo.wl#L11)

**用法**：CovDDifferenceDirect[expr, chrisProvider, a]

#### `CovDCommutatorNoTorsion`

公开 · 源码：[DiffGeo.wl:12](../DiffGeo.wl#L12)

**用法**：CovDCommutatorNoTorsion[expr, a, b, riemann]

#### `CovDCommutatorOfRiemann`

公开 · 源码：[DiffGeo.wl:13](../DiffGeo.wl#L13)

**用法**：CovDCommutator[riemann]

#### `ParamDLCCovDCommutator`

公开 · 源码：[DiffGeo.wl:14](../DiffGeo.wl#L14)

**用法**：ParamDLCCovDCommutator[expr, paramd, cd, metric]

#### `SortParamDLCCovDRule`

公开 · 源码：[DiffGeo.wl:15](../DiffGeo.wl#L15)

**用法**：SortParamDLCCovDRule[paramd, covd, metric]

#### `CovDDefaultIndexOrderedQ`

公开 · 源码：[DiffGeo.wl:16](../DiffGeo.wl#L16)

**用法**：CovDDefaultIndexOrderedQ[a, b]

#### `SortCovDRules`

公开 · 源码：[DiffGeo.wl:17](../DiffGeo.wl#L17)

**用法**：SortCovDRules[cd, commutator, orderedQ]

#### `Cocurvature`

未实现 · 源码：[DiffGeo.wl:20](../DiffGeo.wl#L20)

**用法**：（无说明）

#### `Cotorsion`

未实现 · 源码：[DiffGeo.wl:21](../DiffGeo.wl#L21)

**用法**：（无说明）

#### `DerFunctionQ`

公开 · 源码：[DiffGeo.wl:24](../DiffGeo.wl#L24)

**用法**：DerFunctionQ[fn] gives true if fn should be treated as a scalar function by the derivative operators. Derivative[...][fn] is always treated so.

#### `ExpandDerivativeRules`

公开 · 源码：[DiffGeo.wl:30](../DiffGeo.wl#L30)

**用法**：ExpandDerivativeRules[lhs :> rhs]

#### `DerParameters`

公开 · 源码：[DiffGeo.wl:35](../DiffGeo.wl#L35)

**用法**：DerParameters is an option for DefParametreDerivativeOperator

#### `NICovD`

公开 · 源码：[DiffGeo.wl:38](../DiffGeo.wl#L38)

**用法**：NICovD[expr, a, value]

#### `NIChristoffel`

公开 · 源码：[DiffGeo.wl:39](../DiffGeo.wl#L39)

**用法**：NIChristoffel[value, slot1, slot2]

#### `ITensorCovD`

公开 · 源码：[DiffGeo.wl:40](../DiffGeo.wl#L40)

**用法**：ITensorCovD[expr, value, inds, ind]

#### `EnsureUpDownIndices`

公开 · 源码：[DiffGeo.wl:41](../DiffGeo.wl#L41)

**用法**：EnsureUpDownIndices[tensor, slots, newSlotSigns, slotToMetric]

#### `AdaptIndices`

公开 · 源码：[DiffGeo.wl:42](../DiffGeo.wl#L42)

**用法**：AdaptIndices[tensor, slots, indPats, adapter]

#### `AdaptOneIndex`

公开 · 源码：[DiffGeo.wl:43](../DiffGeo.wl#L43)

**用法**：AdaptOneIndex[adapter, tensor, i, slot, indPat]

#### `MetricAdapter`

公开 · 源码：[DiffGeo.wl:44](../DiffGeo.wl#L44)

**用法**：MetricAdapter[{slot1 -> {metric, invMetric}, ...}]

#### `AdaptNITensor`

公开 · 源码：[DiffGeo.wl:45](../DiffGeo.wl#L45)

**用法**：AdaptNITensor[value, slots, inds, metricProvider]

#### `AdaptNITensorCovD`

公开 · 源码：[DiffGeo.wl:46](../DiffGeo.wl#L46)

**用法**：AdaptNITensorCovD[NITensor[...], ind, slot, covdValue, metricProvider]

#### `LeviCivitaChristoffelValue`

公开 · 源码：[DiffGeo.wl:48](../DiffGeo.wl#L48)

**用法**：LeviCivitaChristoffelValue[slot, cd, metric, metricInv]

#### `RiemannDifferenceValue`

公开 · 源码：[DiffGeo.wl:49](../DiffGeo.wl#L49)

**用法**：RiemannDifferenceValue[slot, cd, chris]

#### `CoRiemannValue`

未实现 · 源码：[DiffGeo.wl:50](../DiffGeo.wl#L50)

**用法**：CoRiemannValue[]

#### `SymmetricRiemann`

公开 · 源码：[DiffGeo.wl:51](../DiffGeo.wl#L51)

**用法**：SymmetricRiemann[metric, a, b, c, d]

#### `SymmetricRiemannETensor`

公开 · 源码：[DiffGeo.wl:52](../DiffGeo.wl#L52)

**用法**：SymmetricRiemannETensor[metric]

#### `RiemannToRicciRules`

公开 · 源码：[DiffGeo.wl:54](../DiffGeo.wl#L54)

**用法**：RiemannToRicciRules[riemann, ricci]

#### `RicciToRicciScalarRules`

公开 · 源码：[DiffGeo.wl:55](../DiffGeo.wl#L55)

**用法**：RicciToRicciScalarRules[ricci, ricciScalar]

#### `RiemannRicciRules`

公开 · 源码：[DiffGeo.wl:56](../DiffGeo.wl#L56)

**用法**：RiemannRicciRules[riemann, ricci, ricciScalar]

#### `RiemannScalarIndexPermutations`

公开 · 源码：[DiffGeo.wl:60](../DiffGeo.wl#L60)

**用法**：RiemannScalarIndexPermutations[n]

#### `LovelockDensity`

公开 · 源码：[DiffGeo.wl:62](../DiffGeo.wl#L62)

**用法**：LovelockDensity[riemann, n]

#### `WeylToRiemann`

公开 · 源码：[DiffGeo.wl:63](../DiffGeo.wl#L63)

**用法**：WeylToRiemann[riemann, metric, {a, b, c, d}]

#### `WeylToRiemannSchouten`

公开 · 源码：[DiffGeo.wl:64](../DiffGeo.wl#L64)

**用法**：WeylToRiemann[riemann, metric, schouten, {a, b, c, d}]

#### `CovDCommutatorRelations`

公开 · 源码：[DiffGeo.wl:66](../DiffGeo.wl#L66)

**用法**：CovDCommutatorRelations[expr, covd, commu]

#### `DefPerturbationOperator`

公开 · 源码：[DiffGeo.wl:69](../DiffGeo.wl#L69)

**用法**：DefPerturbationOperator[symbol]

#### `DefTensorPerturbation`

未实现 · 源码：[DiffGeo.wl:70](../DiffGeo.wl#L70)

**用法**：DefTensorPerturbation[pert, name, slots]

#### `VarInverseMatrix`

公开 · 源码：[DiffGeo.wl:71](../DiffGeo.wl#L71)

**用法**：VarInverseMatrix[invMat[a, b], varmat, order]

#### `PerturbShiftedMetric`

公开 · 源码：[DiffGeo.wl:77](../DiffGeo.wl#L77)

**用法**：PerturbShiftedMetric[expr, n]

#### `PerturbationLeviCivitaChristoffel`

公开 · 源码：[DiffGeo.wl:78](../DiffGeo.wl#L78)

**用法**：PerturbationLeviCivitaChristoffel[cd, metric, n]

#### `PerturbationCovD`

未实现 · 源码：[DiffGeo.wl:80](../DiffGeo.wl#L80)

**用法**：PerturbationCovD[covd[expr, a], covdPert, metric, n]

#### `ExpandTensorDetPerturbation`

公开 · 源码：[DiffGeo.wl:82](../DiffGeo.wl#L82)

**用法**：ExpandTensorDetPerturbation[expr, {pert, tensor, det}]

#### `ExpandRiemannPerturbation`

公开 · 源码：[DiffGeo.wl:83](../DiffGeo.wl#L83)

**用法**：ExpandRiemannPerturbation[expr, {pert, metric, metricPert}, {cd, riemann, ricci, ricciScalar}]

#### `ExpandCovDPerturbation`

公开 · 源码：[DiffGeo.wl:84](../DiffGeo.wl#L84)

**用法**：ExpandCovDPerturbation[expr, {cd, cdPert, metric, metricPert}]

#### `VectorBasisCommutator`

公开 · 源码：[DiffGeo.wl:85](../DiffGeo.wl#L85)

**用法**：VectorBasisCommutator[mat, ders]

#### `DerivativeExpandableQ`

内部 · 源码：[DiffGeo.wl](../DiffGeo.wl)

**用法**：只是 `ExpandDerivativeRules` 用来定位被求导表达式的语法标记，本身没有定义。

#### `SymmetriedDer`

高级 · 源码：[DiffGeo.wl](../DiffGeo.wl)

**用法**：`DefTensorDerivativeOperator` 的选项：构造对称化导数的对称性。

#### `AllowPassThrough`

高级 · 源码：[DiffGeo.wl](../DiffGeo.wl)

**用法**：导数/扰动算子是否允许把被作用表达式看作可通过的因子。

#### `PredefinedSlotType`

高级 · 源码：[DiffGeo.wl](../DiffGeo.wl)

**用法**：扰动公式里使用的占位槽位类型。

#### `PredefinedMetric`

高级 · 源码：[DiffGeo.wl](../DiffGeo.wl)

**用法**：扰动公式里使用的占位度规。

#### `PredefinedCovD`

高级 · 源码：[DiffGeo.wl](../DiffGeo.wl)

**用法**：扰动公式里使用的占位协变导数。

#### `ShiftedMetric`

高级 · 源码：[DiffGeo.wl](../DiffGeo.wl)

**用法**：扰动展开中的 `g + δg + ...` 记号。

#### `ShiftedInverseMetric`

高级 · 源码：[DiffGeo.wl](../DiffGeo.wl)

**用法**：扰动展开中的逆度规记号。


## <code>Peanotica`AlgSimp`</code>

Bianchi 等多重线性关系的枚举与化简。

### 重点符号

#### `AlgTensorSimplifyData`

实验性 · 重点 · 源码：[AlgSimp.wl:14](../AlgSimp.wl#L14)

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

#### `AlgDRiemann`

实验性 · 重点 · 源码：[AlgSimp.wl:16](../AlgSimp.wl#L16)

**签名**：`AlgDRiemann[n]`

表示 n 阶协变导数的黎曼张量，槽位数为 `n + 4`，对称性为 `RiemannSymmetricGenSet[n + 1]`。`AlgDRiemannList[{n1, n2, ...}]` 表示多个因子的乘积。

~~~text
AlgDRiemann[0]      (* R *)
AlgDRiemann[1]      (* ∇R *)
AlgDRiemannList[{0, 0}]  (* R ⊗ R *)
~~~

### 完整列表

#### `SlotsOfAlgTensor`

公开 · 源码：[AlgSimp.wl:5](../AlgSimp.wl#L5)

**用法**：SlotsOfAlgTensor[tensor]

#### `SymmetryOfAlgTensor`

公开 · 源码：[AlgSimp.wl:6](../AlgSimp.wl#L6)

**用法**：SymmetryOfAlgTensor[tensor]

#### `AlgTensorOrder`

实验性 · 源码：[AlgSimp.wl:8](../AlgSimp.wl#L8)

**用法**：AlgTensorOrder[t1, t2]

#### `AlgTensorTerm`

实验性 · 源码：[AlgSimp.wl:9](../AlgSimp.wl#L9)

**用法**：AlgTensorTerm[type, frees, perm]

#### `AlgTensorTermReduce`

实验性 · 源码：[AlgSimp.wl:10](../AlgSimp.wl#L10)

**用法**：AlgTensorTermReduce[expr]

#### `AlgRelations`

实验性 · 源码：[AlgSimp.wl:11](../AlgSimp.wl#L11)

**用法**：AlgRelations[tensor, frees, perm]

#### `EnumerateAlgTensorTerms`

实验性 · 源码：[AlgSimp.wl:12](../AlgSimp.wl#L12)

**用法**：EnumerateAlgTensorTerms[tensor, frees]

#### `AlgTensorReduceRules`

实验性 · 源码：[AlgSimp.wl:13](../AlgSimp.wl#L13)

**用法**：AlgTensorReduceRules[allTerms, relations]

#### `AlgDRiemannList`

实验性 · 源码：[AlgSimp.wl:17](../AlgSimp.wl#L17)

**用法**：AlgDRiemann[list]

#### `AlgDRicci`

实验性 · 源码：[AlgSimp.wl:18](../AlgSimp.wl#L18)

**用法**：AlgDRicci[n]

#### `AlgEpsilon`

实验性 · 源码：[AlgSimp.wl:19](../AlgSimp.wl#L19)

**用法**：AlgEpsilon[n]

#### `ToAlgTensor`

未实现 · 源码：[AlgSimp.wl:21](../AlgSimp.wl#L21)

**用法**：ToAlgTensor[expr, frees]

#### `RiemannToAlgTensor`

未实现 · 源码：[AlgSimp.wl:22](../AlgSimp.wl#L22)

**用法**：RiemannToAlgTensor[expr, frees, info]

#### `AlgTensorToRiemann`

未实现 · 源码：[AlgSimp.wl:23](../AlgSimp.wl#L23)

**用法**：AlgTensorToRiemann[tensor, frees, covd, riemann]

#### `AlgDRiemannETensor`

实验性 · 源码：[AlgSimp.wl:24](../AlgSimp.wl#L24)

**用法**：AlgDRiemannETensor[n, covd, riemann]

#### `IndexedAlgTensor`

实验性 · 源码：[AlgSimp.wl:25](../AlgSimp.wl#L25)

**用法**：IndexedAlgTensor[expr, inds]

#### `IndexedAlgTensorCovD`

实验性 · 源码：[AlgSimp.wl:26](../AlgSimp.wl#L26)

**用法**：IndexedAlgTensorCovD[expr, ind]

#### `ExtractAlgTensor`

实验性 · 源码：[AlgSimp.wl:27](../AlgSimp.wl#L27)

**用法**：ExtractAlgTensor[expr, frees]

#### `ExtractAllAlgTensors`

实验性 · 源码：[AlgSimp.wl:28](../AlgSimp.wl#L28)

**用法**：ExtractAllAlgTensors[expr, frees]

#### `RiemannTensorOrder`

实验性 · 源码：[AlgSimp.wl:29](../AlgSimp.wl#L29)

**用法**：RiemannTensorOrder[{cd, riemann, ricci, ricciScalar}, a, b]


## <code>Peanotica`Presets`</code>

流形设置、曲率计算、分量代入、变分。

### 重点符号

#### `SetupRiemannManifold`

公开 · 重点 · 源码：[Presets.wl:5](../Presets.wl#L5)

**签名**：`SetupRiemannManifold[slot, dim, metric, cd, {riemann, ricci, ricciScalar}]`

一次性定义黎曼流形所需的槽位类型、度规、协变导数、黎曼/Ricci/Ricci 标量，并返回一个 Association：

~~~text
<|"Slot" -> slot, "Metric" -> metric, "CovD" -> cd,
  "Riemann" -> riemann, "Ricci" -> ricci, "RicciScalar" -> ricciScalar|>
~~~

**选项**：`ClearAll -> True`（清除同名符号）、`PPRiemannDisplayName -> "R"`。

#### `SetupPerturbation`

已知问题 · 重点 · 源码：[Presets.wl:8](../Presets.wl#L8)

**已知问题**：`::usage` 写的是两个参数，实际定义需要三个参数（`pert, vard, info`）；`vard` 没有被使用，返回值也不稳定。建议直接用 `DefPerturbationOperator` 和 `Expand*Perturbation`。

#### `ComputeCurvature`

公开 · 重点 · 源码：[Presets.wl:16](../Presets.wl#L16)

**签名**：`ComputeCurvature[input]`

从度规和坐标导数计算曲率。`input` 是一个 Association，常用键：

| 键 | 说明 |
|---|---|
| `"Metric"`, `"InverseMetric"` | 度规和逆度规（数组，分量通常为 `ETensor`） |
| `"Pd"` | 坐标导数算子数组；每个分量是 `ETensor[pd, {Null}]` 或 `ETensor[cd[Null, DI@a], {a}]` |
| `"PdSpec"` | 混合导数规范：`{"Pd", pd}` 或 `{"CovD", cd, data}` 的列表；与 `"Pd"` 二选一。`data` 可以是一个带 `"Metric"`/`"InverseMetric"`/`"Riemann"` 的 Association，也可以直接是该协变导数的黎曼张量（如 `k*SymmetricRiemann[g, #] &`）；未给度规时回退到 `MetricOfSlotType` |
| `"RiemannPd"` | 坐标导数部分的子黎曼张量（例如球面块的内禀曲率） |

返回 `Join[input, <|"Pd"->..., "RiemannPd"->..., "Christoffel"->..., "Riemann"->..., "Ricci"->..., "RicciScalar"->...|>]`。

**选项**：`PostCurvatureCompute`（默认 `Simplify @* ITensorReduce @* ContractMetric`）。

#### `MakeCurvatureValueRules`

公开 · 重点 · 源码：[Presets.wl:18](../Presets.wl#L18)

**签名**：`MakeCurvatureValueRules[syms, values]`

把 `SetupRiemannManifold` 定义的抽象符号与 `ComputeCurvature` 得到的具体分量连接起来，生成替换规则。之后用 `NITensorReduce` 计算、`ExtractNITensor` 取回数组：

~~~text
rules = MakeCurvatureValueRules[sphSyms, sphVals];
arr = NITensorReduce[Ricsph[DI@a, DI@b] /. rules, {a, b}] // ExtractNITensor[{a, b}];
~~~

相关：`MakeTensorValueRules`、`MakeMetricAdapter`。

#### `DefLabelledTensor`

公开 · 重点 · 源码：[Presets.wl:20](../Presets.wl#L20)

**签名**：`DefLabelledTensor[name, slots, symmetry]`

在槽位列表前加一个标签槽位（`TFLabelSlot["(", ")"]`），得到 `name[label, i1, ...]`。标签指标 `LabelI[...]` 不参与缩并、不参与重命名。

~~~text
DefLabelledTensor[ricL, {mf, mf}, {SCycles@{1, 2}}];
FindIndicesSlots[ricL[LabelI[1], a, b]]  -> {{2} -> mf, {3} -> mf}
~~~

#### `DefTensorVariationOperator`

实验性 · 重点 · 源码：[Presets.wl:23](../Presets.wl#L23)

**签名**：`DefTensorVariationOperator[name]`

定义变分算子 `name[expr, field]`。

~~~text
DefTensorVariationOperator[var];
var[T[a, b], T[c, d]]
    -> (g[a, d] g[b, c] + g[a, c] g[b, d]) / 2
~~~

**选项**：`VarCovDs`、`VarParamDs`：把协变导数/参数导数的分部积分纳入变分。目前这两个选项仍是实验性的（协变导数的规则在 `rest = 1` 时会把常数导数化为 0）。

#### `DefScalarFunction`

公开 · 重点 · 源码：[Presets.wl:24](../Presets.wl#L24)

**签名**：`DefScalarFunction[name, displayName]`

把 `name` 标记为标量函数（`DerFunctionQ[name] ^= True`），导数按链式法则展开；`displayName` 用于排版（传 `None` 跳过）。

#### `PopulateCurvatureTReducerRules`

实验性 · 重点 · 源码：[Presets.wl:27](../Presets.wl#L27)

**签名**：`PopulateCurvatureTReducerRules[name, syms, values]`

把 `SetupRiemannManifold`/`ComputeCurvature` 的结果挂到名字 `name` 上，之后可以用 `NITensorReduce[expr, frees, name]` 直接求值（第三个参数是 reducer 名字）。

内部通过 `StoredTensorData[name, tag]` 和 `name` 的 `UpValue` 实现。**实验性**：参数约定可能变化。

### 完整列表

#### `PPRiemannDisplayName`

公开 · 源码：[Presets.wl:6](../Presets.wl#L6)

**用法**：PPRiemannDisplayName is an option of SetupRiemannManifold.

#### `PPMetricPert`

公开 · 源码：[Presets.wl:9](../Presets.wl#L9)

**用法**：PPMetricPert is an option of SetupPerturbation.

#### `ExpandAllPerturbations`

公开 · 源码：[Presets.wl:10](../Presets.wl#L10)

**用法**：ExpandAllPerturbations[expr, pert, info]

#### `PostCurvatureCompute`

公开 · 源码：[Presets.wl:12](../Presets.wl#L12)

**用法**：PostCurvatureCompute is an option for ComputeCurvature

#### `RiemannPdFromPdSpec`

公开 · 源码：[Presets.wl:13](../Presets.wl#L13)

**用法**：RiemannPdFromPdSpec[pds]

#### `PdArrayFromPdSpec`

公开 · 源码：[Presets.wl:14](../Presets.wl#L14)

**用法**：PdArrayFromPdSpec[pds]

#### `DeltaTensorFromPdSpec`

公开 · 源码：[Presets.wl:15](../Presets.wl#L15)

**用法**：DeltaTensorFromPdSpec[pds]

#### `MakeMetricAdapter`

公开 · 源码：[Presets.wl:17](../Presets.wl#L17)

**用法**：MakeMetricAdapter[symbols, values]

#### `MakeTensorValueRules`

公开 · 源码：[Presets.wl:19](../Presets.wl#L19)

**用法**：MakeTensorValueRules[symbols, values]

#### `VarCovDs`

实验性 · 源码：[Presets.wl:21](../Presets.wl#L21)

**用法**：VarCovDs is an option for DefTensorVariationOperator.

#### `VarParamDs`

实验性 · 源码：[Presets.wl:22](../Presets.wl#L22)

**用法**：VarParamDs is an option for DefTensorVariationOperator.

#### `StoredTensorData`

实验性 · 源码：[Presets.wl:26](../Presets.wl#L26)

**用法**：StoredTensorData[name, tags...]

#### `MetricAdapterOfTReducer`

实验性 · 源码：[Presets.wl:28](../Presets.wl#L28)

**用法**：MetricAdapterOfTReducer[name]


## 未实现与已知问题汇总

以下符号已经声明了 `::usage`，但没有实现或存在已知问题：

### 未实现

`AlgTensorToRiemann`、`CETensor`、`CoRiemannValue`、`Cocurvature`、`ConvertPermutationNotation`、`Cotorsion`、`DefTensorPerturbation`、`FoldListGroupedIndexList`、`GroupByTensors`、`ITensorRowReduce`、`IndexNameGroup`、`IndexedToFunction`、`MapIndexedGroupedIndexList`、`PerturbationCovD`、`RelationClosureTableToArray`、`RiemannToAlgTensor`、`RubiksCubeGenSet`、`ScalarValue`、`SymmetricIndexGroup`、`ToAlgTensor`、`WrapETensor`

### 已知问题

`NoIndicesQ`、`RiemannScalars`、`SetupPerturbation`

### 实验性

`AlgDRicci`、`AlgDRiemann`、`AlgDRiemannETensor`、`AlgDRiemannList`、`AlgEpsilon`、`AlgRelations`、`AlgTensorOrder`、`AlgTensorReduceRules`、`AlgTensorSimplifyData`、`AlgTensorTerm`、`AlgTensorTermReduce`、`DefTensorVariationOperator`、`EnumerateAlgTensorTerms`、`ExtractAlgTensor`、`ExtractAllAlgTensors`、`IndexedAlgTensor`、`IndexedAlgTensorCovD`、`MetricAdapterOfTReducer`、`PopulateCurvatureTReducerRules`、`RiemannTensorOrder`、`StoredTensorData`、`VarCovDs`、`VarParamDs`

