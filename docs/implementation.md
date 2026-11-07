# Peanotica 实现细节

本文面向想理解或修改 Peanotica 的读者，说明各模块的内部结构、算法和数据结构。只关心用法的读者请读 [features.md](features.md)；符号级索引见 [api.md](api.md)。

文中出现的函数名都可以在对应的 `.wl` 文件里找到；代码按 `Kernel/init.m` 的顺序加载：

```text
Peanotica`Internal`    Internal.wl
Peanotica`Perm`        Perm.wl          + mathlink/pperm-ml（WSTP 程序）
Peanotica`Core`        Core.wl
Peanotica`DiffGeo`     DiffGeo.wl
Peanotica`AlgSimp`     AlgSimp.wl
Peanotica`Presets`     Presets.wl
```

每个包文件的结构都是 `BeginPackage` → 公开符号与 `::usage` → `Begin["`Private`"]` → 实现 → `End[]` → `Protect`（`Presets` 除外）→ `EndPackage`。私有实现全部在 ``Peanotica`XXX`Private`` 上下文里；公开符号大多被 `Protect`，因此用户扩展应该通过 `UpValues`/`TagSet`，而不是直接改 `DownValues`。

## 1. 总体架构

Peanotica 的核心可以概括为一句话：**把"张量表达式"看成"带指标槽位的普通表达式"，用槽位置换群和哑指标重命名群把等价的表达式化成同一个规范形式**。

规范化的一次调用大致经过：

```text
ITensorReduce[expr]
 ├─ ExpandToTensorPolynomial        展开非张量因子 × Plus / 正整数幂
 ├─ ITensorReduceOneTerm             逐项处理（Plus → Map；List/Rule/ETensor/NITensor 分发）
 │   ├─ CollectIndexedSumTermAndPopulateIndexedSumTimes   整理 IndexedSum 因子
 │   ├─ PreITensorReduce             递归处理子表达式；IndexScope、ETensor 有专门规则
 │   ├─ PutIndexWrapperIfNeeded      给跨项求和指标加作用域
 │   ├─ ISort                        乘积因子排序；记录全等因子
 │   ├─ ITensorReduceOneTermSorted
 │   │   ├─ FindIndicesSlots / GroupIndexListWithSlotTypes   指标分组
 │   │   ├─ FindAndDropFrees                                 自由指标 / 哑指标
 │   │   ├─ AddSymmetryToGroupedIndexList                    哑指标重命名群 + 度规配对符号
 │   │   ├─ SymmetryGroupOfSymmetricIndexList                槽位对称群（平移）
 │   │   ├─ DoubleCosetRepresentative[S, g, D]               双陪集代表元（WSTP）
 │   │   └─ ApplyIndices / RenameGroupedIndexList             写回规范指标
 │   └─ ReleaseISort / PostITensorReduce / ReleaseIndexWrapper
 └─ IndexedSum 重新包装
```

数学上，设

- `S` 是表达式槽位的对称群（由 `SymmetryOfExpression` 给出的生成元）；
- `D` 是哑指标的重命名群（由指标类型、出现位置、度规配对符号、重命名组共同决定）；
- `g` 是"当前指标顺序相对于规范顺序"的置换。

那么 `ITensorReduce` 要求的是双陪集 `⟨S⟩ · g · ⟨D⟩` 中字典序最小的代表元。`S` 作用在槽位上（左作用），`D` 作用在哑指标名上（右作用）；符号（反对称/费米子）也编码在置换里，因此当双陪集中同时出现符号相反的两个相同置换时，代表元为 `0`，这一项就消失了。具体的置换群运算在 WSTP 程序里完成（第 5 节）。

区分两层很重要：

- **Core 层**只认识"槽位 + 指标名"，一个表达式是不是张量、有哪些槽位，完全由 `FindIndicesSlots`/`SymmetryOfExpression` 决定；
- **ETensor/NITensor 层**是"分量/中间表示"，对 Core 来说它们是**没有指标槽位的原子**（`FindIndicesSlots[_ETensor] = {}`），它们自己管理自由指标结构，并在 `PreITensorReduce` 里把内部表达式交给 Core 化简。

## 2. 指标与槽位的数据表示

### 2.1 指标

指标由"名字 + 类型模板"两部分组成，`SeparateIndexName` 负责拆分：

```text
SeparateIndexName[a]      -> {a, IndexNameSlot}
SeparateIndexName[DI[a]]  -> {a, DI[IndexNameSlot]}
```

- `IndexNameSlot` 表示上指标模板，`DI[IndexNameSlot]` 表示下指标模板；
- `DI` 有化简规则：`DI[DI[a]] = a`；`DI[-a] = a` 和 `SeparateIndexName[-a]` 只是把旧写法规范化成 `DI` 的兼容代码，新代码请一律写 `DI[a]`；
- `IndexName[ind]` 返回名字；`IndexBoxForm` 用于排版。

`RenamingGroupOfIndexName` 决定哪些指标名可以互相改名：默认所有符号属于 `DefaultRenamingGroup`；整数和 `LabelI` 返回 `Null`（不可改名）；用户也可以给某个符号指定另一个组。`RenamableIndicesQ` 判断两个名字是否同组。

`GetIndexOfSlotType[type, inds]` 从 `IndicesCandidateOfSlotType[type]` 的候选名里挑一个不在 `inds` 中的；候选名用尽时调用兜底生成器（默认 `DefaultIndex`）。`GetIndicesOfSlotType` 是批量版本；`GetUniqueIndexOfSlotType` 直接生成全局唯一的 `TempIndex`（`$TempIndexNumber++`）。

`IndexList[...]` 是带整体符号的指标列表（可以理解为 `Times[sign, List[...]]`），`PermuteIndexList`、`ApplyIndices` 处理它。

### 2.2 槽位

`FindIndicesSlots` 的返回值是 `pos -> type` 的列表，`pos` 是表达式树中的位置列表。它的实现是一组模式规则：

| 输入 | 行为 |
|---|---|
| `fn_[args___]` | 先查 `fn` 自身（位置 `{0}`），再对每个参数递归并加位置前缀 |
| `Plus[...]` | 返回 `SummedIndices[...]`，标记"这些指标是跨项求和的" |
| `Hold` / `HoldForm` | 去掉外壳 |
| `IndexedSum[expr, inds__]` | 递归后删掉被求和指标对应的槽位 |
| `ITensor` / `NITensor` / `ETensor` | 各自的 `UpValue` |
| 其它 | `{}` |

只有通过 `TagSet`/`UpValue` 给 `FindIndicesSlots` 下定义，才能让自定义对象参与运算；`FindIndicesSlots` 有 `HoldFirst`，所以模式可以保持未求值。

`FindIndicesSlots[expr, prefix]` 只是把前缀拼到每个位置上；`FindIndicesSlotsAndNames` 同时返回每个槽位上的指标表达式；`FindAllIndicesNames` 进一步给出 `name -> {{pattern, slotType, pos}, ...}` 的关联，供内部按名字分组。

`FindTermIndicesList` 是"只取指标表达式、不要位置"的便捷函数。

### 2.3 求和与作用域

- `IndexedSum[expr, {i, type}, ...]`：显式的指标求和。构造时会：
  - 对求和变量排序；
  - 如果某个求和指标在 `expr` 中完全不出现，直接乘上 `DimensionOfSlotType[type]` 并删掉该求和；
  - 如果是 `Times`，用 `TrySplitIndexScopeFactors` 把不含求和指标的因子提出去；
  - 如果指标在 `expr` 中自由出现，就保留符号形式。
- `IndexScope[expr]`：把 `expr` 当作标量。`FindIndicesSlots[IndexScope[expr]] = {}`；`PreITensorReduce` 对它的专门规则是把内部表达式用 `FreeIndexNames -> {}` 化简（即内部指标都按哑指标处理），再包回去。显示时加红色括号。
- `PutIndexScope[expr, frees]`：把 `Times` 分解成"含自由指标的部分"和"可以独立成 scope 的部分"。
- `IndexWrapper[expr, innerInds, outerInds]`：用于 `SummedIndices` 跨项求和时保持外层指标名。`PutIndexWrapperIfNeeded` 只在 `FindIndicesSlots` 结果里出现 `SummedIndices` 时包装；`ReleaseIndexWrapper` 在规范化后把内层名字换回外层名字。

## 3. 分组与对称群构造

### 3.1 GroupedIndexList

规范化前，先把"每个指标名出现在哪些位置"分组。`GroupIndexListWithSlotTypes` 的输入是 `{index, slot}` 对，输出形如

```text
{{slotType, indexTemplate} -> {name -> {pos1, pos2, ...}, ...}, ...}
```

其中 `indexTemplate` 是 `IndexNameSlot` 或 `DI[IndexNameSlot]`（上/下）。`GroupIndexList` 是不带槽位类型的简化版本，按模板分组。

`FindAndDropFrees[grouped, frees]` 把每个组拆成"自由"和"哑"两部分：

- `frees` 是列表时，按名字判断；
- `frees = Automatic` 时，出现一次的指标算自由指标；
- `frees` 是谓词时，用 `frees[indPat, name]` 判断。

返回 `{freeGrouped, dummyGrouped}`，两边都保持分组结构。

### 3.2 哑指标对称群 D

对每个哑指标组 `{slot, pat} -> (name -> positions)`：

1. **配对模板**：如果 `pat` 是 `{a, DI[a]}` 或 `{DI[a], a}`（一上一下），这是一个度规配对。用 `ExpressionPairSymProvider` 决定它能否与同组另一个配对交换：
   - 两个位置的槽位类型一致；
   - `InterchangableIndexPairQ` 允许该指标名与它的 `DI` 互换（整数/`LabelI` 不允许）；
   - `SumPassThroughQ` 和 `ExpressionPassThroughQ` 保证这个位置不在求和号内、且度规因子可以"穿过"其它因子；
   - 满足时返回 `SignOfSymmetricPair[type1, type2, name]`（默认 `1`；反对称度规可以覆盖为 `-1`），否则返回 `0`（不能交换）。
2. **普通模板**：对 `pat` 里相邻的槽位做 `MapShiftedIndexed`，得到相邻交换生成元。
3. 每个组记录 `{slot, pat, symList, renamingGroup}`，并把 `name -> positions` 附上。

`AddSymmetryToGroupedIndexList` 把这些组按 `{slot, pat}` 排序、分组，得到"对称化后的分组指标列表"。

### 3.3 槽位对称群 S

`SymmetryGroupOfSymmetricIndexList` 把每个组的对称性平移到全局槽位编号：

- 组 `i` 的长度为 `blockLen`；
- 对组内第 `j` 次出现，把 `symList` 平移 `blockLen * (j - 1) + i`；
- 如果该指标名的重命名组不是 `Null`，再加一组"第 j 次出现与第 j+1 次出现之间的块交换"（`BlockSymmetricGenSet`），这对应"同名指标出现在多个位置时，这些位置可以整体互换"。

最后返回生成元列表和总槽位数。注意这一步只构造生成元，不构造整个群；群运算交给 WSTP。

### 3.4 自由指标对称性

`FreeIndicesSymmetry` 选项允许用户给自由指标附加一个对称群，含义是"把自由指标当成与某个对称张量缩并"。实现是 `ConvertFreeIndicesSym`：

```text
perm = FillNoneSlots[Images @@ (FirstPosition[freeInds, #] & /@ frees), Length@freeInds + 1]
sym' = perm · sym · perm^{-1}
```

也就是把作用在自由指标小列表上的置换，共轭到"自由指标在前、哑指标在后"的完整指标顺序上。`FillNoneSlots` 用 `Null` 占位，保证没有自由指标的位置不动。

## 4. 规范化管线

### 4.1 ExpandToTensorPolynomial

`ITensorReduce` 先做一次浅展开：

- 非张量因子（`NonTensorTermQ` 为真，即不含 `SummedIndices`）乘以 `Plus` 时，分配到每一项；
- 非张量因子的正整数幂展开成 `Outer[Times, ...]`；
- `IndexScope` 原样保留（它自己要单独处理）。

目的是让 `ITensorReduceOneTerm` 看到的是"单项的乘积"，而不是"乘积里套着和"。

### 4.2 ITensorReduceOneTerm

`ITensorReduceOneTerm` 对 `Plus`、数组、`List` 逐项映射；对单项：

1. `CollectIndexedSumTermAndPopulateIndexedSumTimes` 把 `IndexedSum` 因子整理成 `IndexedSumTimes`，收集求和指标；
2. `PreITensorReduce` 递归处理子表达式。默认规则会对每个子表达式调用 `FindAllIndicesNames`，如果它含指标就包一层 `IndexScope`——也就是说，**子表达式默认被看作独立的标量因子**。`IndexScope` 和 `ETensor` 有专门的 `PreITensorReduce` `UpValue`；
3. 如果结果里还有 `SummedIndices`（说明是跨项求和），用 `IndexWrapper` 包住；
4. `ISort` 排序；
5. `ITensorReduceOneTermSorted` 做指标规范化；
6. 依次 `ReleaseISort`、`PostITensorReduce`、`ReleaseIndexWrapper`；
7. 如果有 `IndexedSum` 指标，重新包成 `IndexedSum`。

`FixedPoint -> True` 会把第 1–6 步重复到结果不再变化。

`PreITensorReduce`/`PostITensorReduce` 是公开的扩展点：给它们写 `UpValue` 可以改变"进入/离开规范化"时的行为。目前 `PostITensorReduce` 没有内部使用者。

### 4.3 ISort

`ISort` 只做乘积因子的排序，不碰指标。对每个因子计算排序键：

```text
tag = {arg, GroupOrderFromStrongGenSet[SymmetryOfExpression@arg],
       Length@FindIndicesSlots@arg,
       ReplacePart[arg, 指标位置 -> IndexSlot[1]]}
```

排序规则 `ISortDefaultOrder` 依次比较：

1. 对称群阶数**降序**（对称性越强的因子越靠前）；
2. 槽位数量**降序**；
3. 去掉指标后的表达式**升序**（`Order`）。

排序后，用 `SymmetryList[ReplaceIndicesToSame[#, IndexSlot[1]] & /@ args, SameQ]` 检查**相邻因子是否全等**（把所有指标都替换成 `IndexSlot[1]` 后比较），得到 `symList`；`ISortedProduct[head, args, symList]` 保存这个结构。`SymmetryOfExpression` 对 `ISortedProduct` 的规则会把各因子的对称性平移拼接，并用 `SymmetryListGenSet` 给相邻全等因子加上块对称性——这就是"两个相同的 T 因子可以互换"的来源。

`ReleaseISort` 把 `ISortedProduct`/`ISortedGeneral` 还原成普通表达式。

`ISortUnorderedArgs` 是早期版本的另一种实现，目前没有被调用。

### 4.4 ITensorReduceOneTermSorted

这是规范化的核心。输入是排序后的单项（可能含 `ISortedProduct`、`IndexedSum` 等），步骤：

```text
indPos   = FindIndicesSlots[expr]                 -- 位置 -> 槽位类型
slotsSym = SymmetryOfExpression[expr]             -- 槽位对称群（SGS）

actualIndsAndSlot = {Extract[expr, pos], slotType}  -- 每个槽位的实际指标
grouped = GroupIndexListWithSlotTypes[actualIndsAndSlot]
{freesGrouped, dummiesGrouped} = FindAndDropFrees[grouped, freesPredicate]

dummySymList = AddSymmetryToGroupedIndexList[dummiesGrouped,
                  ExpressionPairSymProvider[expr, indPos, UseMetricOnSlots]]
freeInds = CollectGroupedIndices[freesGrouped]

D = ShiftPermutation[SymmetryGroupOfSymmetricIndexList[dummySymList], Length@freeInds]
  ⋈ ConvertFreeIndicesSym[FreeIndicesSymmetry, freeIndices]

g = Image permutation of the current index order
canonPerm = DoubleCosetRepresentative[slotsSym, g, D]
if canonPerm == 0: return 0

renamedDummies = RenameGroupedIndexList[dummiesGrouped, ...]
newInds = freeInds ⋈ CollectGroupedIndices[renamedDummies]
result = ApplyIndices[expr, indPos[[All, 1]],
                      PermuteIndexList[newInds, SignedInversePermutation[canonPerm]]]
```

几个细节：

- **自由指标在前**：构造 `g` 时先把自由指标排在前面，再排哑指标，所以 `D` 只需要作用在哑指标部分（`ShiftPermutation` 平移自由指标个数）。
- **哑指标改名**：`RenameGroupedIndexList` 按"重命名组"逐个分配新指标，类型取该组第一次出现位置的槽位类型；`RenameDummies -> False` 时跳过这一步，直接用 `canonInds`。
- **写回**：`ApplyIndices` 用 `ReplacePart` 把新指标写到原来的槽位位置；`PermuteIndexList` 支持带符号的置换，负号会变成整体系数。
- **返回值**：`{结果, Association[旧哑指标名 -> 新哑指标名]}`，第二个元素供 `IndexedSum` 重新包装时使用。
- **零判定**：`DoubleCosetRepresentative` 返回 `0` 时，`ApplyIndices[_, _, 0] = 0`，整项为 0。

`FindIndicesSlots` 的 `SummedIndices`、`IndexedSum`、`IndexScope` 已经在这一步之前处理过，所以这里假设槽位是平铺的。

### 4.5 规范化之后

- `PostITensorReduce` 默认是恒等；可以给 `ETensor`、`IndexScope` 等写 `UpValue`。
- `ReleaseIndexWrapper` 把 `IndexWrapper` 里的内层指标名换回外层。
- 如果原来有 `IndexedSum` 因子，按返回值里的改名关系重新写出 `IndexedSum`。

## 5. 双陪集与 WSTP 后端

### 5.1 置换的表示

<code>Peanotica`Perm`</code> 用两种记号：

- `Images[p1, p2, ...]`：单行像，表示点 `i` 映到 `pi`；可以比实际度数短，未写出的点视为不动点；
- `SCycles[{...}, {...}, ...]`：轮换记号；
- 负号表示奇置换：`-Images[...]`、`-SCycles[...]`。

`ToImagesNotation`/`ToSCyclesNotation` 互相转换；`MinPermutationLength` 求出让所有点都有定义的最小度数；`ShiftPermutation` 把置换平移到另一个槽位区间；`PPermPermutationProduct` 做带符号的乘法（先转成 `Images` 或 `SCycles`，用 Mathematica 的 `PermutationProduct` 计算，再转回）。

`SeparatePermSign[perm]` 返回 `{sign, perm}`，`SignedInversePermutation` 保留符号地求逆。

### 5.2 预先定义的生成元

| 函数 | 作用 |
|---|---|
| `SymmetricGenSet[n1, n2, ...]` | 相邻交换 `(n1 n2), (n2 n3), ...`，生成对称群 |
| `RiemannSymmetricGenSet[n]` | `-SCycles@{n,n+1}`、`-SCycles@{n+2,n+3}`、`SCycles[{n,n+2},{n+1,n+3}]` |
| `BlockSymmetricGenSet[b1, b2, ...]` | 块之间的逐点交换 |
| `RiemannMonomialGenSet[n]` | n 个黎曼张量的整体对称性（块对称 + 每个黎曼因子的对称） |
| `ScalarMonomialDummiesGenSet[n]` | n 个哑指标对的对称性 |
| `DummiesGenSet[sign, {{u1,d1},...}]` | 哑指标对：`sign` 控制上下交换的符号，`BlockSymmetricGenSet` 控制哑指标之间的交换 |
| `SymmetryOfSortedList[list, eq]` | 已排序列表中相邻相等元素的交换生成元 |

`BlockSymmetricGenSet` 用 `MapThread` 生成 `SCycles @@ Thread[{block1, block2}]`，即两块逐点交换；块长度不等时会得到非法的轮换，调用者要保证等长。

`AlgSimp` 里的 `DoubleTransversalInSymmetricGroup` 用 `DummiesGenSet` 作为右侧群，枚举双陪集代表元（见第 10 节）。

### 5.3 强生成元

`SymmetryOfExpression` 返回的必须是**相对于基 `[1, 2, ..., n]` 的强生成元**。上面的内置构造器都满足这个条件；用户如果只给出任意生成元，要先调用 `ConstructStrongGenSet`：

```text
ConstructStrongGenSet[gs] -> 强生成元列表
```

C++ 侧用 Jerrum 的 Schreier–Sims 变体：`JerrumBranchingBuilder::build` 把生成元逐个 sift 进 `JerrumBranching`，收集 Schreier 生成元，得到强生成元。`PermutationStack` 提供可复用的置换缓冲区。

`GroupOrderFromStrongGenSet` 用 `GroupOrderCalculator` 逐层计算轨道大小并相乘。

### 5.4 双陪集代表元

`DoubleCosetRepresentative[S, g, D, opts]`：

1. 把 `S`、`D` 读成强生成元，构造 `BaseChangingStrongGenSetProvider`；
2. `DoubleCosetRepresentativeSolver::solve` 对基点 `i = 1..n` 迭代：
   - 计算 `S` 在 `i` 上的轨道 `b_i^S`；
   - 对当前候选集合 `sgd` 的每个元素，计算 `(b_i^S)^sgd` 与 `D` 的轨道的关系，选出最小点 `p_i`；
   - 收集所有满足 `b_i^(s1·sgd) = p_i^(d2)` 的新代表元，加入 `sgd`；
   - 如果某个新代表元与已有代表元符号相反（同一置换、相反符号），返回 `0`；
   - 用 `stabilizeOnePoint` 同时把 `S` 稳定到 `i`、`D` 稳定到 `p_i`（`BaseChanger` 负责换基）；
3. 返回 `sgd` 中字典序最小的元素（`PermutationView::compare` 比较像）。

选项：

- `UseTwoStep -> True`：按 Portugal 的两步法，先对 `D` 的稳定点做右陪集代表元，再对结果做 Butler；`::usage` 里注明 profile 结果显示单步更快，默认 `False`；
- `PPermVerbose -> True`：把过程写入日志（只对 `PPERM_DEBUG` 构建有效）。

`DoubleTransversalInSymmetricGroup[S, D]` 枚举所有双陪集代表元，用于 `AlgSimp` 和 `LovelockDensity`。

### 5.5 WSTP 协议

`Perm.wl` 把置换编码成 `{sign, images...}`：

```text
PermToMLPerm[Images[i1, i2, ...]]  -> {1, i1, i2, ...}
PermToMLPerm[-Images[...]]         -> {-1, ...}
MLPermToPerm[{n, i1, ...}]         -> n * Images[i1, ...]
```

`mathlink.cc` 注册的函数（都在 ``Peanotica`Perm`Private`` 上下文）：

| Mathematica 包装 | C++ 入口 |
|---|---|
| `ConstructStrongGenSet` | `MathLinkConstructStrongGenSet` |
| `DoubleCosetRepresentative` | `MathLinkDoubleCosetRepresentative` |
| `GroupOrderFromStrongGenSet` | `MathLinkGroupOrderFromStrongGenSet` |
| `PPermGroupElements` | `MathLinkPPermGroupElements` |
| `PPermStabilizer` | `MathLinkPPermStabilizer` |
| `MoveBasePoint` | `MathLinkMoveBasePoint` |
| `DoubleTransversalInSymmetricGroup` | `MathLinkDoubleTransversalInSymmetricGroup` |
| `PPermOpenLogFile` | `MathLinkOpenLogFile` |

`mathlink/perm.h` 里的主要数据结构：

- `PermutationView`/`PermutationList`：符号 + 像数组；`compare` 做字典序比较；
- `PermutationSet`：去重集合，支持带符号；
- `SchreierOrbit`：轨道 + Schreier 向量；
- `JerrumBranching`/`JerrumBranchingBuilder`：Schreier–Sims 的 sifting；
- `SimsFilter`、`Stabilizer`、`BaseChanger`：稳定子链和换基；
- `StrongGenSetProvider`/`BaseChangingStrongGenSetProvider`：给双陪集算法提供"当前基下的强生成元"；
- `DoubleCosetRepresentativeSolver`：Butler 算法；
- `GroupOrderCalculator`、`GroupEnumerator`、`DoubleCosetEnumerator`：群阶、群元素、双陪集枚举。

`PPermEnsureLink` 在每次调用前用 `CheckLink[]` 检查 `$PPermLink`；失效时重新 `Install`。`PPermConnect` 的路径是

```text
FileNameJoin[{DirectoryName@FindFile["Peanotica`Perm`"], "mathlink", "pperm-ml"}]
```

`PPermDisconnect` 在 `Perm.wl` 加载时先调用一次，防止旧链接残留。

## 6. 乘积、穿透性与符号

### 6.1 ProductQ / UnorderedProductQ

- `UnorderedProductQ[Times] = True`，`UnorderedProductQ[Wedge] = True`，`Inactive[head]` 递归；
- `ProductQ[NonCommutativeMultiply] = True`，否则等于 `UnorderedProductQ`；
- `HeldTimes` 是保持参数的乘积变体（`OneIdentity`），用于在 `SymmetryOfExpression` 里构造"可以整体交换的因子"。

`ISort` 只对 `UnorderedProductQ` 的头部排序；`Wedge` 目前没有处理交换符号（代码里有 `TODO` 注释），因此 `Wedge` 的规范形不保证符号正确。

### 6.2 ExpressionPassThroughQ / SumPassThroughQ

这两个谓词回答"某个张量因子能不能从表达式里的某个位置'穿过'到外面"：

- `ExpressionPassThroughQ[expr, tensor, pos]`：把 `tensor` 放到 `pos` 位置上，是否满足 `T[...] expr = T[... * expr, ...]`；
- `SumPassThroughQ[expr, pos]`：该位置是否在所有求和号（`IndexedSum`）之外。

实现是按表达式结构递归的：`Times` 乘积取对应参数继续查；`ISortedProduct` 取内部参数；`ISortedGeneral` 去壳；`HeldTimes`、`TDagger`、`MetricBarrier` 等有专门规则；默认返回 `False`。

度规缩并（`ContractMetric`）和哑指标配对（`ExpressionPairSymProvider`）都用这两个谓词保证"换指标不会改变表达式的值"。这是 Peanotica 能安全处理非平凡乘积结构的关键。

## 7. 度规与指标升降

### 7.1 MetricOfSlotType 的默认度规

`MetricOfSlotType[type]` 是一个"虚拟度规"，默认假设上下指标成对：

- `MetricOfSlotType[type][a, DI@b]` 会重排成 `[DI@b, a]`（西北约定）；
- `MetricOfSlotType[type][a_?AbsIndexNameQ, DI@a_] = DimensionOfSlotType[type]`（迹）；
- 四个乘积规则把 `g[.., b] g[DI@b, ..]` 合并成 `g[.., ..]`；
- `Times` 的 `UpValue` 用 `TryReplaceIndexNames` 把 `g[DI@b, a] * rest` 里的 `a, b` 互换（即用 δ 替换指标）；
- `ContractionSlotOfMetric` 默认：两个下指标 → 1，两个上指标 → 2，混合 → 1。

用户可以用 `MetricOfSlotType[type] ^= metric` 把默认度规换成具体张量。

### 7.2 DefSimpleMetric

`DefSimpleMetric[sym, type, sign]` 先调用 `DefSimpleTensor` 定义槽位和对称性，再按模式安装缩并规则：

- `"Normal"` 模式：
  - `sym[a_?AbsIndexNameQ, DI@a_] = DimensionOfSlotType[type]`（以及反向）；
  - `ContractableMetricQ[sym[_, _]] = True`；
  - 四个 `sym[.., b] sym[DI@b, ..] := sym[.., ..]` 规则；
  - `Times` 的 `UpValue`：`sym[DI@b, a] * rest` 和 `sym[a, DI@b] * rest` 用 `TryReplaceIndexNames[rest, {a -> b, b -> a}]` 交换指标（δ 行为）；
- `"AllUp"` 模式：`sym[a_, a_] = dim`，乘法规则不问上下号；
- 反对称度规（`symSign = -1`）只影响槽位对称性，缩并规则仍是 `"Normal"`；代码里有 `TODO: anti-symmetric metric`。

`TryReplaceIndexNames` 只替换**可重命名**的指标名（`AbsIndexNameQ`），整数/`LabelI` 不动。

### 7.3 ContractMetric

`ContractMetric[expr, metrics]` 不是"把度规乘进去"，而是"消去度规因子"：

1. `ContractMetricExpanded` 展开 `Plus`；
2. 对 `Times` 的每个因子，用 `SelectMetricForContraction[metrics]` 选出度规因子；
3. `Fold[ContractOneMetric, 非度规因子, 度规因子]`。

`ContractOneMetric[expr, metric]`：

```text
exprIndToPos = 指标名 -> {槽位类型, 位置}      -- 只在表达式中出现一次的名字
metricInds   = metric 的两个指标
pos1         = Lookup[exprIndToPos, DI /@ metricInds]
```

- 如果度规的第一个指标能在 `expr` 里找到**相反符号**的同一个名字，并且 `ExpressionPassThroughQ`、`SumPassThroughQ` 都允许，就把 `expr` 中那个位置替换成度规的第二个指标；
- 第二个指标同理；
- 符号：`metricSym = SignOf2SymGroup[SymmetryOfExpression@metric]`（`{}`→0，`{SCycles@{1,2}}`→1，`{-SCycles@{1,2}}`→-1）；`ContractionSlotOfMetric` 与实际收缩的槽位不一致时乘上 `metricSym`；
- 都不满足时保留 `expr * metric`。

`ContractMetric[expr]`/`ContractMetric[expr, All]` 选择所有 `ContractableMetricQ` 为真的度规；`DeepContractMetric` 递归进入 `IndexScope`。

`SeparateMetricOne[expr, indsPat, metricProvider]` 把一个度规拆成"一个指标 + 一个度规"的乘积，供扰动展开使用。

### 7.4 指标适配

- `EnsureUpDownIndices[value, slots, signs, slotToMetric]`：对每个槽位，如果目标符号与数组的槽位符号不同，用 `ITensorFixedContract` 乘上度规/逆度规升降；
- `AdaptIndices[tensor, slots, indPats, adapter]`：逐个槽位调用 `AdaptOneIndex`；
- `MetricAdapter[{slot -> {metric, invMetric}, ...}]` 是 `adapter` 的常用实现：`AdaptOneIndex[MetricAdapter[...], tensor, i, slot, DI@IndexNameSlot]` 降指标，`IndexNameSlot` 升指标；
- `AdaptNITensor[value, slots, inds, adapter]`：把 `NITensor` 的每个指标按 `inds` 的上下号适配，返回新的 `NITensor`。

注意 `DiffGeo.wl` 里 `AdaptNITensor` 有两个同签名的定义：第一个用 `EnsureUpDownIndices`，第二个用 `AdaptIndices`；Mathematica 会保留后定义的规则，所以实际生效的是第二个。

## 8. ETensor / NITensor 表示层

### 8.1 ETensor 的不变式

`ETensor[expr, frees]` 里：

- `expr` 是普通表达式（可以含指标名）；
- `frees` 是**扁平的**列表，长度等于张量的槽位数；每一项是
  - 一个指标名：该槽位有自由指标；
  - `Null`：该槽位没有自由指标（标量槽，或指标被外层数组的块坐标吸收）。

对 Core 来说，`ETensor` 是**没有指标槽位的原子**：`FindIndicesSlots[_ETensor] = {}`。它只在 `PreITensorReduce` 里把 `expr` 展开、化简，再按 `frees` 重组。

`ETensor[expr, {}]` 会变成 `IndexScope[expr]`（标量因子）；`ETensor[0, _] = 0`；`ETensor[expr, frees][inds]` 通过 `ToIndexedExprFunction` 把自由指标按位置替换。

### 8.2 结构表示

- `StructureOfETensorIndices[frees]` → `ETensorIndices[非Null指标, Null的位置, 每段的长度]`；
- `ReconstructETensorIndices` 反向重建；
- `CompatibleETensorIndicesQ` 比较两个结构；
- `MultiILength` 处理"一个槽位是列表"的情况（供内部结构表示使用；公开用法要求 `frees` 扁平）。

当前版本中 `frees` 的每个元素都是单个指标名或 `Null`，所以 `lens` 全是 1；保留这套结构是为了让内部代码可以统一处理。

### 8.3 加法与块结构合并

`ETensor /: _?PlusQ[e1__ETensor]` 捕获 `ETensor + ETensor`：

- 如果所有项都是标量（`frees` 全 `Null`）且长度相同，直接把 `expr` 相加；
- 否则 `UncatchedETensorPlusNonNullInds`：
  1. 对每项求 `FindAllIndicesNames[expr]`，得到自由指标名到槽位类型的映射；
  2. `CombineETensorFrees` 逐槽位合并结构：
     - 所有项在该槽位都是 `Null` → 结果 `Null`；
     - 所有项在该槽位都是列表 → 递归合并（当前扁平用法不会出现）；
     - 否则用 `GetIndexOfSlotType[第一项的类型, 已有指标]` 生成一个新指标；
  3. `ReplaceETensorFrees` 把每项的指标重命名到合并后的结构；
  4. 返回 `ETensor[Plus @@ 重命名后的 expr, 合并后的 frees]`。

`CombineETensorFrees` 依赖 `LookupOneFreeTypeOrNone[Null, _] = NoneSlot` 这个内部哨兵来区分"没有指标"和"某个指标"。

### 8.4 乘法、外积、缩并

- `ETensor /: prod_?ProductQ[l___, ETensor[expr_, frees_], r___] := ETensor[prod[l, expr, r], frees]`：`ETensor` 在普通乘积里像一个标量因子，被提到外层；
- `ETensorProduct[prod, e1, e2]`：先把 `e2` 中与 `e1` 冲突的可重命名指标改名，再 `Join` 两个 `frees`（用 `StructureOfETensorIndices` 重建）；
- `ITensorOuter[prod, e1_ETensor, e2_ETensor, cont]`：`ETensorProduct` 之后用 `ITensorTranspose` 把 `cont` 指定的槽位相邻化（重复槽位表示"识别/缩并"）；
- `ITensorTranspose[e_ETensor, perm]`：
  - `PadPermutation` 把短置换补成恒等；
  - `indsBySlots` 把 `frees` 按 `perm` 分组；
  - 如果某个目标槽位收到多个 `frees`，用 `ReplaceIndicesRules` 把这些指标识别成同一个；
  - 新 `frees = First /@ indsBySlots`；
- `ITensorSum[ETensor[expr_, frees_], dims]`：删掉对应 `frees` 槽位（`SumIndexOfSlot` 目前是恒等，实际求和由外层数组/`NITensor` 完成）；
- `ITensorContractTwo[prod, t1, t2, slots]` = `Fold[ITensorSum[#1, {#2[[1]]}] &, ITensorOuter[...], ReverseSortBy[slots, First]]`；
- `ITensorFixedContract[prod, t1, t2, n1, n2]` = 外积 `{{n1,n2}}` → 对 `n1` 求和 → 把结果转置到 `n2` 的位置；
- `ITensorContract[t1_?ArrayQ, slots]` 只对普通数组有效，`ETensor` 请用 `ITensorContractTwo`/`ITensorFixedContract`。

### 8.5 NITensor

`NITensor[expr, inds]` 是"带具体指标名的中间表示"：

- `FindIndicesSlots[NITensor[_, inds]]` 为每个 `ind` 生成槽位；`ExpressionPassThroughQ` 对 `{2, __}` 恒为真（指标部分不阻挡）；
- 数值因子会被吸收进 `expr`；
- `NITensorReduce` 的分发：
  - 乘积 → `ReduceNITensorContractions`；
  - 单个 `NITensor` → `ContractOneNITensor`；
  - `Plus` → 先分别化简，再用 `NITensorReduce$CombineSummedNITensor` 把各项的指标顺序对齐后相加；
  - `IndexScope` → 递归；
  - 原子 → `NITensor[atom, {}]`；
  - 其它含指标的表达式 → `Message[NITensorReduce::unknown]` 并保持未求值。

`ContractOneNITensor`：

1. 按指标名分组，出现两次以上的指标需要合并（`PermutationOfMergeIndices` + `ITensorTranspose`）；
2. 剩下的重复指标中，不在 `frees` 里的用 `ITensorSum` 求和；
3. 返回新的 `NITensor`。

`ReduceNITensorContractions`：

1. 对每个因子，用 `FreesByArgs` 计算"如果只考虑其它因子，它应该保留哪些自由指标"；
2. `NITensorReduce` 每个因子，`AbsorbNonNITensor` 把标量因子吸收到相邻因子；
3. 反复选择"缩并数最多"的一对因子（`CombinationPairs` + `CountContractions`），用 `ContractTwoNITensors` 合并，直到只剩一个因子。

`ContractTwoNITensors` 找出两个因子的公共指标名，用 `ITensorOuter` 对齐槽位，非自由指标求和，最后拼接自由指标。

`ExtractNITensor[expr, inds]`：把 `NITensor` 的指标顺序转置成 `inds` 指定的顺序；指标不匹配时报 `ExtractNITensor::incompat`；`NITensor[0, _]` 直接返回 0。

已知限制：两个**全等**的 `NITensor` 相乘会被 Mathematica 自动合并成 `Power`，而 `NITensorReduce` 没有 `Power` 规则（会报 `NITensorReduce::unknown`）。写表达式时避免让两个因子在词法上完全相同。

## 9. DiffGeo 的实现

### 9.1 导数算子的展开规则

`ExpandDerivativeRules[lhs :> rhs, opts]` 是"给任意算子生成展开规则"的工厂。它的输入 `lhs` 形如

```text
sym[expr_?DerivativeExpandableQ, inds___]
```

其中 `?DerivativeExpandableQ` **只是用来定位"被求导的表达式"这个模式变量**，并不是运行时的判断条件（`DerivativeExpandableQ` 本身没有定义）。函数用 `Cases` 找出这个模式变量名，构造 `lhsFn`，然后为以下情形各生成一条规则：

| 情形 | 行为 |
|---|---|
| `Plus` | 逐项求导 |
| `List` | 逐元素求导 |
| `Derivative[ders][fn][args]` | 链式法则（`MapDerivativeOnFnDerivative`） |
| `IndexScope[expr]` | 先把内部哑指标唯一化再求导 |
| `IndexedSum[...]` | 把求和指标唯一化后求导 |
| `_?NumberQ` | 0 |
| `_?DerConstantQ` | 0 |
| `(fn : _?DerFunctionQ)[args__]` | 对标量函数用链式法则 |

其中 `rhs` 里的 `ExpandDerivative[fn, expr]` / `ExpandDerivativeWithRest[fn, expr]` 是占位符，每条规则会把它们替换成对应情形下的具体写法（例如 `fn2 /@ expr`、`fn2[#, 1] & /@ expr`）。

关键点：**乘积和幂的 Leibniz 法则不是单独写的**，而是靠

```text
DerFunctionQ[Times] = True;
DerFunctionQ[Power] = True;
```

让 `Times[...]` / `Power[...]` 匹配"标量函数"规则，再用 `MapDerivativeOnFnDerivative` 对每个参数求导并求和。这等价于 Leibniz 法则，但复用了链式法则的代码。

`DerConstantQ`：数值、带 `Constant` 属性的符号、以及所有参数都是常数的标量函数。声明常量符号时优先 `SetAttributes[foo, Constant]`；只有不能设置属性的表达式（复合表达式、受保护的符号等）才用 `DerConstantQ[expr] ^= True`；`DerFunctionQ`：`Times`、`Power`、`NumericFunction` 符号，以及用户用 `DefScalarFunction` 标记的函数。

### 9.2 DefTensorDerivativeOperator

```text
DefTensorDerivativeOperator[sym, slots, symmetry, opts]
```

- 用 `ExpandDerivativeRules` 生成 `sym[expr, inds]` 的展开规则（`SetDelayed @@@ ...`）；
- `sym[Null, a__][expr] := sym[expr, a]`：允许"先写导数、再写被作用表达式"的柯里化写法；
- `FindIndicesSlots[sym[expr_, ...]]`：被作用表达式的槽位 + 导数槽位；
- `SymmetryOfExpression`：被作用表达式的对称性 + 导数槽位的对称性（`SymmetriedDer` 为 `True` 时用 `SymmetryOfSymmetrizedDer` 构造对称化导数的对称性）；
- `SumPassThroughQ`：导数槽位（位置 ≥ 2）总是"可通过"；`ExpressionPassThroughQ`：第一个导数槽位（位置 2）可通过，被作用表达式部分要看张量是否与导数交换（度规相容等）；
- `DisplayName` 用于排版；
- `NITensorReduce[sym[expr_, inds__], frees]` 用 `ReduceNITensorContractions` 把导数当成一个 `NITensor` 因子。

`SymmetriedDer` 的对称性构造：数出嵌套层数，把 `symmetry` 平移到每一层，并加上"不同层的导数指标之间可以整体交换"的块对称性。

### 9.3 DefParametreDerivativeOperator

用于对参数（坐标、扰动变量）求导：

- `sym[expr, v, rest]` 只对第一个参数展开，其余参数保留（支持高阶/混合偏导）；
- `sym[Null, vs__][expr] := sym[expr, vs]`；
- `sym[expr_] := expr`；`sym[r_, r_] = 1`；不同参数之间 `sym[p1, p2] = 0`（由 `DerParameters` 列表生成）；
- 嵌套 `sym[sym[expr, v1], v2]` 会展平；
- 对 `ETensor`/`NITensor` 直接穿过；
- `FindIndicesSlots`/`SymmetryOfExpression` 与 `DefTensorDerivativeOperator` 类似，但参数本身不是指标；
- 排版为下标。

### 9.4 曲率

- `LeviCivitaChristoffelDer[cd, der, metric, metricInv][a, b, c]`：
  ```text
  1/2 metricInv[a, d] (
      cd[der@metric[DI@d, c], b]
    + cd[der@metric[b, DI@d], c]
    - cd[der@metric[b, c], DI@d])
  ```
  `der` 通常是 `Identity`；用于参数导数时需要把 `der` 传进去。
- `LeviCivitaChristoffel[cd, metric]` = `LeviCivitaChristoffelDer[cd, Identity, metric]`。
- `RiemannDifferencePart1[cd, chris][a,b,c,d] = -cd[chris[d,b,c],a] + cd[chris[d,a,c],b]`；
  `RiemannDifferencePart2[chris][a,b,c,d] = chris[e,a,c] chris[d,b,DI@e] - chris[e,b,c] chris[d,a,DI@e]`；
  `RiemannDifference = Part1 + Part2`。
- 上述 `LeviCivitaChristoffel` 公式和 `RiemannDifference` 的恒等式都要求作为参考的 `cd` 无挠；`cd` 有挠时 `LeviCivitaChristoffel` 不再等于联络差张量，`RiemannDifference` 也不再等于两个黎曼张量之差。本包不处理有挠的参考导数。
- `CovDDifference[expr, inds, chrisProvider, a]`：对每个槽位，如果指标是下指标，减 `chris[d, a, ind]`；如果是上指标，加 `chris[ind, a, d]`；`CovDDifferenceDirect` 从表达式直接取槽位。
- `NICovD[NITensor[t, inds], a -> type, value]`：在 `NITensor` 末尾追加一个槽位，并调用 `ITensorCovD`；
- `ITensorCovD[expr, value, slots, ind]`：把协变导数拆成"普通导数"和"Christoffel 修正"两部分，Christoffel 部分用 `CovDDifference` 在 `NITensor` 层计算，再用 `NITensorReduce` 化简、`ExtractNITensor` 取回。

### 9.5 协变导数的排序

`SortCovDRules[cd, commutator, orderedQ]` 生成一条规则：

```text
cd[cd[expr, a], b] :> cd[cd[expr, b], a] + commutator[expr, b, a]
```

当 `!orderedQ[a, b, expr]` 时触发。`CovDDefaultIndexOrderedQ` 先比较指标是否出现在 `expr` 中（自由指标优先），再按 `SeparateIndexName` 的字典序。`SortCovD` 就是反复应用这条规则。

`CovDCommutatorNoTorsion[expr, a, b, riemann]` 对 `expr` 的每个槽位生成 `±riemann[a,b,ind,d] * expr[..., d, ...]`；`CovDCommutatorOfRiemann[riemann]` 是它的算子形式。两者都只适用于无挠导数算符。

### 9.6 曲率定义

`DefRiemannCurvature[slot, R, Ric, Rs]`：

- `DefSimpleTensor[R, ConstantArray[slot,4], RiemannSymmetricGenSet[1]]`；
- `DefSimpleTensor[Ric, {slot,slot}, {SCycles@{1,2}}]`；
- `Rs` 的 `MakeBoxes` 设为 `"R"`；
- `SetDelayed @@@ RiemannRicciRules[R, Ric, Rs]`。

`RiemannToRicciRules` 列出 8 种 `R[a,b,c,DI@c]` 的缩并模式（含符号），`RicciToRicciScalarRules` 是 `Ric[DI@a,a] -> Rs`。

### 9.7 扰动

- `DefPerturbationOperator[pert]`：
  - 用 `ExpandDerivativeRules` 生成 `pert[expr, n] :> pert[ExpandDerivative[pert[#,1] &, expr], n-1]`（`n >= 1`）；
  - `pert[expr, 0] := expr`；
  - `pert[pert[expr, n1], n2] := pert[expr, n1+n2]`；
  - 对 `pert[head_][_, args___]` 有专门的槽位平移规则；
  - 排版为 `δ`/`δ^n`。
- `DefCovdPerturbationRules[pert, cd, metric]` 定义 `pert[cd[expr,a], n]` 的展开（协变导数的扰动）；
- `VarInverseMatrix[invMat[a,b], var, n]`：用 `SortedPartitions[n]` 枚举逆矩阵的 n 阶展开（组合系数 + `ContractList`）；
- `PerturbationLeviCivitaChristoffel[cd, metric, metricPert, n]`：把 `ShiftedMetric` 代入 Christoffel 公式，再用 `ExpandPerturbShiftedMetric` 把扰动算子作用到 `ShiftedMetric`/`ShiftedInverseMetric` 上；
- `PerturbationLeviCivitaRiemann[n]`：用 `PredefinedMetric`/`PredefinedCovD` 计算 `RiemannDifference`，展开到 n 阶，`Block[{$TempIndexNumber = 1}, ...]` 保证哑指标从 1 开始，最后 `ToIndexedExprFunction[..., {a,b,c,d}]` 生成函数；结果缓存；
- `ExpandMetricPerturbation`、`ExpandTensorDetPerturbation`、`ExpandCovDPerturbation`、`ExpandRiemannPerturbation`：把 `pert[...]` 逐条重写成度规扰动、行列式、协变导数、黎曼张量的组合；
- `Presets` 的 `ExpandAllPerturbations` 根据 Association 里有哪些键决定按什么顺序调用上面几个展开。

`PredefinedSlotType`/`PredefinedMetric`/`PredefinedCovD` 是"占位"符号：扰动公式先在它们上面推导，最后用 `/. {PredefinedCovD -> cd, PredefinedMetric -> metric, ShiftedMetric -> metricPert}` 替换成用户的对象。

### 9.8 其他

- `RiemannBianchiRelations[expr, cd, R]`：对 `Plus`/`Times` 递归；对 `cd[R[a,b,c,d], e]` 生成两条（第一 Bianchi 和导数 Bianchi）；对裸 `R[a,b,c,d]` 生成第一 Bianchi；这些关系假设 `cd` 无挠；
- `CovDCommutatorRelations`：对 `cd[cd[expr,a],b]` 生成 `expr0 - cd[cd[expr,b],a] - commu[expr,b,a]`；
- `WeylToRiemann`：用 `dim = IndexScope[metric[a, DI@a]]` 和 Ricci 缩并表示外尔张量；
- `LovelockDensity[riemann, n]`：用 `DoubleTransversalInSymmetricGroup` 枚举 `2n` 个指标的配对，`Signature` 给符号；
- `RiemannScalars[riemann, n]`：预计算的 `RiemannScalarIndexPermutations[n]`（n ≤ 4），生成 n 阶黎曼标量单项式；
- `VectorBasisCommutator[mat, ders]`：`mat2 - Transpose[mat2, {2,1,3}]`。

## 10. AlgSimp 的实现

### 10.1 数据表示

`AlgTensorTerm[type, frees, perm]` 是一个**惰性数据容器**，不是可求值的张量：

- `type`：`AlgDRiemann[n]`、`AlgDRicci[n]`、`AlgEpsilon[n]` 或它们的 `TensorProduct`；
- `frees`：自由指标个数；
- `perm`：槽位到指标位置的置换（`Images` 或列表）。

对 `AlgDRiemann` 有 `UpValue`：

```text
SlotsOfAlgTensor[AlgDRiemann[n]] = ConstantArray[1, n+4]
SymmetryOfAlgTensor[AlgDRiemann[n]] = RiemannSymmetricGenSet[n+1]
AlgTensorOrder[AlgDRiemann[n1], AlgDRiemann[n2]] = Order[n1, n2]
```

`AlgDRicci[n]` 的对称性是 `{SCycles@{n+1,n+2}}`；`AlgEpsilon[n]` 的对称性是 `-SymmetricGenSet[1..n]`。

`SortAlgTensor` 按 `AlgTensorOrder` 排序；`SplitAlgTensor` 把 `TensorProduct` 的 `perm` 按各因子的槽位数切开。

### 10.2 枚举与关系

`EnumerateAlgTensorTerms[tensor, frees]`：

```text
S = SymmetryOfAlgTensor[tensor]
D = DummiesGenSet[1, Partition[Range[frees+1, Length@SlotsOfAlgTensor[tensor]], 2]]
reps = DoubleTransversalInSymmetricGroup[S, D]
terms = AlgTensorTerm[tensor, frees, #] & /@ reps
```

`DummiesGenSet[1, pairs]` 生成"哑指标对之间的交换"（符号 +1，因为黎曼缩并的上下交换是对称的）；`DoubleTransversalInSymmetricGroup` 枚举双陪集代表元，保证每个不等价单项恰好出现一次。

`AlgRelations[tensor, frees, perm]`：

- 对 `AlgDRiemann[n]`：返回第一 Bianchi（`{l,a,b,c,d} + {l,a,c,d,b} + {l,a,d,b,c}`）和 `AlgRelationBianchi2`（`{l,e,a,b,c,d} + {l,a,b,e,c,d} + {l,b,e,a,c,d}`）；
- 对 `TensorProduct`：把关系按因子分配，`TensorProduct @@ ReplacePart[subTerms, i -> relation]`；
- 其他：空。

### 10.3 化简数据

`AlgTensorSimplifyData[tensor, frees]`：

1. `allTerms = EnumerateAlgTensorTerms[tensor, frees]`；
2. `relations = AlgRelations[allTerms] // TensorExpand // AlgTensorTermReduce`；
3. `rules = AlgTensorReduceRules[allTerms, relations]`；
4. 返回 `{allTerms, rules}`。

`AlgTensorReduceRules` 把每个关系写成"所有单项的线性组合 = 0"，用 `RowReduce` 求出以基表示其他单项的规则：

```text
matrix = (relations /. Thread[allTerms -> IdentityMatrix[n]])
rowReduce = RowReduce[Reverse /@ matrix]
rules = {allTerms[[i]] -> -Rest[row] . allTerms}  -- 选每行第一个非零元
```

`AlgTensorTermReduce` 用 `DoubleCosetRepresentative[SymmetryOfAlgTensor, perm, DummiesGenSet[1, pairs]]` 把单项化成规范形式。

### 10.4 未完成的部分

`ToAlgTensor`、`RiemannToAlgTensor`、`AlgTensorToRiemann` 只有 `::usage`，没有定义；`IndexedAlgTensor`/`ExtractAlgTensor` 提供了从"带指标名的表达式"到 `AlgTensorTerm` 的部分转换，但不完整。

## 11. Presets 的实现

### 11.1 SetupRiemannManifold

```text
ClearAll[slot]; DefSimpleSlotType[slot, dim, {a..z, DefaultIndex}]
ClearAll[metric]; DefSimpleMetric[metric, slot, 1]
ClearAll[cd]; DefTensorDerivativeOperator[cd, {slot}, {}]; cd[_metric, _] = 0
MetricOfSlotType[slot] ^= metric
DefRiemannCurvature[slot, R, Ric, Rs]
返回 <|"Slot"->slot, "Metric"->metric, "CovD"->cd, "Riemann"->R, ...|>
```

`ClearAll` 选项控制是否清除同名符号；`PPRiemannDisplayName` 传给 `DefRiemannCurvature`。

### 11.2 ComputeCurvature

```text
metric = input["Metric"]; metricInv = input["InverseMetric"]
pd = PdArrayFromPdSpec[input["PdSpec"]] 或 input["Pd"]
subRiemann = RiemannPdFromPdSpec[input["PdSpec"]] 或 Lookup[input, "RiemannPd", 0]

chris  = post@LeviCivitaChristoffelValue[Null, {pd, 0}, metric, metricInv]
riemann = post[RiemannDifferenceValue[Null, {pd, 0}, chris] + subRiemann]
ricci   = post@ITensorSum[ITensorTranspose[riemann, {1, 3, 2, 3}], {3}]
rs      = post@ITensorSum[ITensorTranspose[ITensorFixedContract[Times, metricInv, ricci, 1, 1], {1, 1}], {1}]
```

`pd` 是一个"导数算子数组"：每个分量是 `ETensor[pd, {Null}]`（纯坐标导数）或 `ETensor[cd[Null, DI@a], {a}]`（已有协变导数）。`RiemannDifferenceValue` 计算块导数算符相对于基准导数算符的黎曼张量之差（`RiemannDifference`）；`subRiemann` 收集它表现不了的项：属于同一协变导数的块给出该导数的曲率张量，`"Pd"` 与 `"CovD"` 混合的块给出两者之间的联络差（`LeviCivitaChristoffelDer`，符号取决于块的顺序）。

`LeviCivitaChristoffelValue[slot, cd, metric, metricInv]` 用 `NICovD` 把协变导数作用于度规，再 `NITensorReduce` + `ExtractNITensor` 得到分量数组。`RiemannDifferenceValue` 同理。

`RiemannPdFromPdSpec` 对每一对导数规范 `({"Pd", pd}` 或 `{"CovD", cd, data}`）生成一个块矩阵元素：

- 两个 `"Pd"` → 0；
- `"Pd"` 与 `"CovD"` → `LeviCivitaChristoffelDer[cd, pd, metric, metricInv]` 的 ETensor；
- 两个相同的 `"CovD"` → 该协变导数的曲率 ETensor，由 `ApplyRiemannTensor[data, {DI@a, DI@b, DI@c, d}]` 得到；
- 其他 → 0。

规范里的 `data` 有两种写法：

- Association：键 `"Metric"`、`"InverseMetric"`（可选）、`"Riemann"`；`"Riemann"` 可以是张量头（如 `R`），也可以是算子（如 `SymmetricRiemann[gd]`）；
- 直接给曲率张量：`Function` 按指标列表调用（如 `k*SymmetricRiemann[gd, #] &`），其他形式按 `riemann[i1, i2, i3, i4]` 调用。

块度规由 `PdSpecMetric[cd, data]` 决定：优先取 Association 的 `"Metric"`，否则取 `cd` 的槽位类型对应的 `MetricOfSlotType`（槽位类型由 `FindIndicesSlots` 从 `cd` 得到；无法唯一确定时发 `PdSpecMetric::nometric` 消息并按缺省值处理）。

`PdArrayFromPdSpec` 生成 `pd` 数组；`DeltaTensorFromPdSpec` 生成度规/δ 块矩阵。

### 11.3 把抽象符号接到具体分量

`MakeCurvatureValueRules[syms, values]` 生成规则：

- `metric[a_?NonDIQ, b_?NonDIQ]` → 逆度规的 `NITensor`；
- `metric[DI@a_, DI@b_]` → 度规的 `NITensor`；
- `covd -> Function[{expr, a}, AdaptNITensorCovD[...]]`；
- `riemann[inds__]` / `ricci[inds__]` → `AdaptNITensor[value, slots, {inds}, adapter]`；
- `ricciScalar -> value`；
- `MakeTensorValueRules` 为 `values` 里其他的符号键生成同样的规则。

`MakeMetricAdapter[syms, values]` 构造 `MetricAdapter[{slot -> {metric, invMetric}}]`。

`PopulateCurvatureTReducerRules[name, syms, values, slot]` 把同样的信息挂到 `name` 上：

```text
name /: StoredTensorData[name, "Metric", metric] = {slot, metricValue, metricInvValue}
name /: StoredTensorData[name, "CovD", covd] = {pdValue, {{slot, slot} -> chrisValue}}
name /: StoredTensorData[name, "Riemann", R] = {riemannValue, {DI@slot, ...}}
...
name /: NITensorReduce[metric[a_, b_], frees_, name] := ...
name /: NITensorReduce[covd[expr_, a_], frees_, name] := ...
name /: NITensorReduce[tensor_[inds__], frees_, name] := ...
```

`MetricAdapterOfTReducer[name]` 从 `UpValues[name]` 里提取 `StoredTensorData[name, "Metric", ...]`，构造 `MetricAdapter`。这就是 `NITensorReduce[expr, frees, name]` 的"第三个参数 reducer"机制。

### 11.4 变分

`DefTensorVariationOperator[name, opts]`：

- 用 `ExpandDerivativeRules` 生成 `name[expr_?DerivativeExpandableQ, var_, rest_] :> ExpandDerivativeWithRest[name[#1, var, #2 * rest] &, expr]`；
- `name[expr_, var_] := name[expr, var, 1]`；`name[var_][expr_] := name[expr, var]`；
- `VarCovDs`：对 `name[cd[expr, a], var, rest]` 生成 `name[expr, var, -cd[rest, a]]`（分部积分）；
- `VarParamDs`：对参数导数类似，符号是 `(-1)^n`；
- `name[var_, var_, rest_] := rest`（场对自身的变分）；
- 张量规则：`name[tensor[inds1], tensor[inds2], rest]` 检查非指标槽位一致，然后对 `SymmetryOfExpression` 的群元素取平均，每个槽位乘上 `MetricOfSlotType`；
- `name[_, _, _] = 0`。

注意 `VarCovDs` 的规则在 `rest = 1` 时会产生 `cd[1, a]`，而 `cd` 会把常数导数化为 0，因此协变导数的分部积分目前不可靠；这是实验性功能。

### 11.5 其他

- `DefLabelledTensor[name, slots, sym]` = `DefSimpleTensor[name, Prepend[slots, TFLabelSlot["(", ")"]]]`；
- `DefScalarFunction[name, displayName]`：`DerFunctionQ[name] ^= True`，并设置 `MakeBoxes`；
- `SetupPerturbation`：定义接受 3 个参数，只调用 `DefPerturbationOperator[pert]`，`vard` 未使用，返回值也不稳定；建议直接用 `DefPerturbationOperator`。

## 12. 调试、测试与性能

### 12.1 调试打印

`Internal.wl` 提供：

```text
$xToolsDebugPrint   -- 默认是 Print，可以替换成自定义函数
$xToolsDebugFilter  -- All / None / 标签列表 / 谓词
xToolsDebugPrint[tag, msg...]
```

`ITensorReduceOneTermSorted` 里有多处 `xToolsDebugPrint[ITensorReduceOneTermSorted, ...]`，分别打印输入、分组结果、规范指标、置换、槽位对称性、双陪集结果等。启用方式：

```text
$xToolsDebugFilter = All;          (* 或 {ITensorReduceOneTermSorted} *)
```

调试输出会混在正常输出里，建议只在交互式会话里打开。

### 12.2 置换后端日志

```text
PPermOpenLogFile["pperm.log"];
SetOptions[DoubleCosetRepresentative, PPermVerbose -> True];
...
PPermCloseLogFile[];
```

日志只在 `PPERM_DEBUG` 构建里有效（`Makefile` 默认调试构建带 `-DPPERM_DEBUG`），内容包含每一步的基、轨道、选中的 `sgd` 等。仓库根目录的 `hkm.txt` 和 `mathlink/hkm.txt` 是这类日志的历史文件（已被 `.gitignore` 忽略）。

### 12.3 测试

| 文件 | 内容 | 现状 |
|---|---|---|
| `Core.wlt` | 指标、规范化、ETensor、乘积、`MakeIndexRule` 等 27 个测试 | 通过 |
| `DiffGeo.wlt` | 球对称度规的 Christoffel、Ricci 标量、标签张量等 3 个测试 | 通过 |
| `xPerm.wlt` | 与 xAct`xPerm 的对拍（使用 vendored 的 `xPerm.wl`） | 需要 xAct 或 vendored 文件 |
| `compare.wl` | 随机黎曼单项式与 xAct`xPerm 的 `CanonicalPerm` 对拍 | 需要 xAct |
| `tests.wls` | 用 `TestReport` 跑所有 `.wlt` | 引用了 `xTools`$SkipLongTest`（本仓库未定义） |

`xPerm.wl` 是从 xAct`xPerm 复制的参考实现，被 `.gitignore` 的 `xPerm.*` 忽略，不参与包加载。

文档示例由 `docs/run_examples.py` 执行：它解析 Markdown 里的 `mathematica` 代码块，按文件累积 `<!-- setup -->` 块，逐个用 `wolframscript` 在独立内核里运行，遇到消息、非零退出码或缺少结束标记就报错。

### 12.4 性能

- 主要开销在 `DoubleCosetRepresentative`：代价随槽位数、哑指标对数、群阶增长；`GroupOrderFromStrongGenSet` 也要遍历稳定子链。
- `ConstructStrongGenSet` 只在用户给出非强生成元时需要；内置生成元已经是强生成元，不要再包一层。
- `ISort` 会调用 `GroupOrderFromStrongGenSet` 给每个因子算群阶，因子很多时可以用 `ISort` 的自定义 sorter 降低开销，或者直接对已经排序的表达式调用 `ITensorReduceOneTermSorted`。
- `RenameDummies -> False` 只跳过改名，不跳过规范化。
- `FixedPoint -> True` 可能多次运行整个管线；只在有自定义规则产生新缩并时需要。
- `UseTwoStep -> True`（Portugal 两步法）在代码注释里提到 profile 结果不如单步，默认关闭。
- 对最终的标量表达式，`Simplify`/`FullSimplify` 通常比继续 `ITensorReduce` 更有效；`ContractMetric` 之后再 `ITensorReduce` 是常见组合（`ComputeCurvature` 的默认 `PostCurvatureCompute` 就是 `Simplify @* ITensorReduce @* ContractMetric`）。
