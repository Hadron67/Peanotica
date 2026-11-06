# Peanotica 特性与用法

本文面向使用 Peanotica 的用户，介绍这个包能做什么、怎么用。算法的内部实现放在 [implementation.md](implementation.md)，全部公开符号的索引放在 [api.md](api.md)。

Peanotica 是 Mathematica 的张量指标表达式包。与 xAct 等包不同，它不预设流形、丛或抽象指标：一个表达式"有哪些指标槽位、这些槽位有什么对称性"完全由用户（或子包）通过两个函数声明，其余的张量运算、协变导数、曲率、扰动、变分都在这个声明之上工作。这种自底向上的设计让它可以处理任意槽位类型、任意数量的指标，以及"分块"张量。

文中的 Mathematica 示例可以用 `python3 docs/run_examples.py` 执行（少数环境相关的块用 `<!-- skip -->` 标记）；`(* => ... *)` 是期望结果，便于对照。

## 目录

1. [安装与加载](#1-安装与加载)
2. [快速开始](#2-快速开始)
3. [核心概念](#3-核心概念)
4. [张量代数与规范化](#4-张量代数与规范化)
5. [分块张量](#5-分块张量)
6. [分量计算：ETensor 与 NITensor](#6-分量计算etensor-与-nitensor)
7. [微分几何](#7-微分几何)
8. [代数化简：AlgSimp](#8-代数化简algsimp)
9. [Presets：开箱即用的工作流](#9-presets开箱即用的工作流)
10. [选项与约定速查](#10-选项与约定速查)
11. [已知限制](#11-已知限制)
12. [延伸阅读](#12-延伸阅读)

## 1. 安装与加载

### 1.1 目录要求

把整个 `Peanotica` 目录放到 Mathematica 的 `$Path` 能找到的位置，也就是让**父目录**出现在 `$Path` 中：

```text
/path/to/packages/
└── Peanotica/
    ├── Kernel/init.m
    ├── Core.wl
    ├── Perm.wl
    ├── DiffGeo.wl
    ├── AlgSimp.wl
    ├── Presets.wl
    └── mathlink/pperm-ml
```

加载：

<!-- skip -->
```mathematica
PrependTo[$Path, "/path/to/packages"];
Needs["Peanotica`"];
```

`Needs["Peanotica`"]` 会依次加载 <code>Peanotica`Internal`</code>、<code>Peanotica`Perm`</code>、<code>Peanotica`Core`</code>、<code>Peanotica`DiffGeo`</code>、<code>Peanotica`AlgSimp`</code> 和 <code>Peanotica`Presets`</code>。由于包没有单独创建顶层 <code>Peanotica`</code> 上下文，Mathematica 会提示一次

```text
Needs::nocont: Context Peanotica` was not created when Needs was evaluated.
```

这条消息不影响使用。如果不希望看到它，可以写

```mathematica
Quiet[Needs["Peanotica`"], Needs::nocont];
```

或者直接 `Get` 入口文件（路径按实际位置替换）：

<!-- skip -->
```mathematica
Get["/path/to/packages/Peanotica/Kernel/init.m"];
```

### 1.2 外部程序

<code>Peanotica`Perm`</code> 的置换群运算由一个 WSTP（MathLink）程序 `mathlink/pperm-ml` 完成。仓库里带的是 macOS ARM64 的预编译版本；换平台或架构时需要重新编译：

```bash
cd Peanotica/mathlink
make            # 调试构建
make RELEASE=true
```

`Makefile` 默认从 `/Applications/Mathematica.app/...` 查找 WSTP 开发包，也可以通过 `MLINKDIR`、`SYS` 覆盖。首次调用 `ITensorReduce`、`GroupOrderFromStrongGenSet` 等函数时会自动 `Install` 这个程序；如果找不到，会出现 `Install::...` 或 `LinkObject` 相关错误。

当前 `Makefile` 只在 macOS 和部分 Linux 上配置了默认路径，Windows 需要手动指定 `MLINKDIR`/`SYS`。

### 1.3 版本

文档示例在 Mathematica 13.2（macOS ARM64）上验证。

## 2. 快速开始

下面定义一个四维指标空间、一个对称的二阶张量，并做规范化。

<!-- setup -->
```mathematica
DefSimpleSlotType[mf, 4, {Symbol /@ CharacterRange["a", "z"], DefaultIndex}];
DefSimpleMetric[g, mf, 1, DisplayName -> "g"];
DefSimpleTensor[T, {mf, mf}, {SCycles@{1, 2}}];
```

```mathematica
ITensorReduce[T[b, a]]
(* => T[a, b] *)
```

`DefSimpleSlotType[mf, 4, {...}]` 定义了一个维数为 4 的槽位类型 `mf`；`{Symbol /@ CharacterRange["a","z"], DefaultIndex}` 是"可用指标名 + 兜底生成器"。

`DefSimpleTensor[T, {mf, mf}, {SCycles@{1,2}}]` 定义二阶张量 `T`，两个槽位都属于 `mf`，并且在交换两个槽位时不变。`SCycles@{1,2}` 表示"交换槽位 1 和 2"这一置换；如果要表示反对称，用 `-SCycles@{1, 2}`。

`ITensorReduce` 会把只差一个槽位置换或哑指标重命名的表达式化成同一个规范形式：

```mathematica
ITensorReduce[T[a, b] + T[b, a]]
(* => 2 T[a, b] *)

DefSimpleTensor[S, {mf, mf}, {-SCycles@{1, 2}}];
ITensorReduce[S[a, a]]
(* => 0 *)

ITensorReduce[S[a, b] + S[b, a]]
(* => 0 *)
```

最后一个例子体现了符号的作用：反对称张量缩并同一个指标必然为 0。

## 3. 核心概念

### 3.1 指标

- 上指标直接写符号：`a`、`b`；
- 下指标`DI[a]`；`DI[DI[a]]` 会化回 `a`；
- `LabelI[...]` 是标签指标：不参与缩并、不会被重命名，常用于扰动阶数或张量分量的编号；
- `TempIndex[n]`、`UInd[]` 是内部使用的唯一临时指标，`ITensorReduce` 的结果中通常会被重命名掉；
- `DefaultIndex[n]` 是在候选指标名用尽时的兜底生成器。

```mathematica
DI[DI[a]]
(* => a *)

SeparateIndexName[DI@a]
(* => {a, DI[IndexNameSlot]} *)
```

`SeparateIndexName` 把一个指标拆成"名字"和"指标类型模板"（`IndexNameSlot` 表示上指标，`DI[IndexNameSlot]` 表示下指标）。只有名字相同、模板也相同时，两个指标才被视为同一个指标。

默认情况下，所有符号指标名都属于同一个"重命名组"，可以在规范化时互换；整数指标和 `LabelI` 不属于任何重命名组，因此不会被改名：

```mathematica
ITensorReduce[T[b, x] T[DI@x, c]]
(* => T[b, a] T[c, DI[a]] *)

ITensorReduce[T[b, x] T[DI@x, c], RenameDummies -> False]
(* => T[b, x] T[c, DI[x]] *)
```

`RenameDummies -> False` 会保留用户写的哑指标名，便于和别的表达式逐项对比。

### 3.2 槽位与两个协议

Peanotica 只要求一个表达式回答两个问题：

- `FindIndicesSlots[expr]`：指标槽位在表达式中的位置，以及每个槽位的类型；
- `SymmetryOfExpression[expr]`：槽位的对称性，用带符号的置换生成元表示。

```mathematica
FindIndicesSlots[T[a, b]]
(* => {{1} -> mf, {2} -> mf} *)

SymmetryOfExpression[T[a, b]]
(* => {SCycles[{1, 2}]} *)
```

对一般的复合表达式，位置是完整的位置列表；`FindIndicesSlots[expr, prefix]` 可以给所有位置加上前缀，方便在自定义函数里组合：

```mathematica
FindIndicesSlots[T[a, b], {9}]
(* => {{9, 1} -> mf, {9, 2} -> mf} *)
```

### 3.3 自定义对象

任何表达式都可以通过给这两个函数下定义来"变成张量"。例如定义一个"作用在张量上的算符" `op[expr, a]`，它在末尾增加一个槽位：

```mathematica
FindIndicesSlots[op[expr_, a_]] ^:= Join[FindIndicesSlots[expr, {1}], {{2} -> mf}];
SymmetryOfExpression[op[expr_, a_]] ^:= SymmetryOfExpression[expr];

FindIndicesSlots[op[T[a, b], c]]
(* => {{1, 1} -> mf, {1, 2} -> mf, {2} -> mf} *)
```

注意两点：

1. `FindIndicesSlots` 有 `HoldFirst` 属性，所以可以用模式定义；
2. `SymmetryOfExpression` 返回的必须是**强生成元**（strong generating set，基为 `[1, 2, ..., n]`）。内置的 `SymmetricGenSet`、`RiemannSymmetricGenSet`、`BlockSymmetricGenSet` 等已经满足这一点；如果你只有任意生成元，可以先用 `ConstructStrongGenSet` 转换：

```mathematica
ConstructStrongGenSet[{SCycles@{1, 2, 3}}]
(* => {SCycles[{1, 2, 3}], SCycles[{1, 3, 2}]} *)
```

如果给的不是强生成元，规范化仍可能运行，但对某些输入可能得到不一致的结果。

### 3.4 自由指标与哑指标

默认约定是：

- 在单项中只出现一次的指标是**自由指标**；
- 出现两次的指标是**哑指标**，可以任意改名；
- 出现更多次时，用 `DummyIndex[name, repeats]` 提示它代表的缩并重数，见 `PopulateDummyIndexHint`。

大多数函数会从表达式自动推断自由指标；需要显式指定时用 `FreeIndexNames` 选项：

```mathematica
DefSimpleTensor[A, {mf}, {}];
ITensorReduce[A[b], FreeIndexNames -> {}]
(* => A[a] *)
```

这里 `A` 只有一个自由指标，但显式声明"没有自由指标"后，它被当成可改名的哑指标，于是被重命名成 `a`。

### 3.5 槽位类型与度规

槽位类型可以是任意表达式，不必是符号。通过以下定义扩展：

<!-- skip -->
```mathematica
DimensionOfSlotType[type] ^= dim;
IndicesCandidateOfSlotType[type] ^= {names, fallback};
MetricOfSlotType[type] ^= metric;
```

`DefSimpleSlotType` 是最常用的封装。不同槽位类型之间不会互相缩并；它们的指标名也可以重名，包会按槽位类型区分。

`MetricOfSlotType[type]` 是该类型的默认度规：可以把它直接当度规张量用，也可以用 `^=` 指定一个具体张量。`MetricOfSlotType[type][a, DI@b]` 这类"一上一下"的度规会自动当作恒等映射参与缩并。

## 4. 张量代数与规范化

### 4.1 定义张量与度规

<!-- setup -->
```mathematica
DefSimpleSlotType[mf, 4, {Symbol /@ CharacterRange["a", "z"], DefaultIndex}];
DefSimpleMetric[g, mf, 1, DisplayName -> "g"];
DefSimpleTensor[T, {mf, mf}, {SCycles@{1, 2}}];
DefSimpleTensor[A, {mf}, {}];
```

`DefSimpleMetric[g, mf, sign]` 的 `sign` 取 `1`、`-1` 或 `0`，分别表示对称、反对称和无对称性。度规会自动获得缩并规则（见 4.3）。

也可以用更底层的 `DefSimpleTensor[name, slots, symmetry]`，其中 `slots` 是槽位类型列表，`symmetry` 是置换生成元列表：

```mathematica
DefSimpleTensor[Rtemp, {mf, mf, mf, mf}, RiemannSymmetricGenSet[1]];
ITensorReduce[Rtemp[b, a, d, c]]
(* => Rtemp[a, b, c, d] *)
```

### 4.2 ITensorReduce

`ITensorReduce[expr]` 是主要的规范化函数，支持 `Plus`、`List`、`Rule`、`ETensor` 和 `NITensor`。常见选项：

| 选项 | 默认值 | 作用 |
|---|---|---|
| `FreeIndexNames` | `Automatic` | 指定哪些指标是自由指标；`Automatic` 表示按出现次数推断 |
| `FreeIndicesSymmetry` | `{}` | 给自由指标附加对称性（视作与某个对称张量缩并） |
| `RenameDummies` | `True` | 是否把哑指标改写成规范名字 |
| `UseMetricOnSlots` | `All` | 哪些槽位类型允许用度规对称性交换上下指标对 |
| `FixedPoint` | `False` | 是否反复规范化直到不再变化 |

规范化后的表达式满足：只差槽位对称性和哑指标改名的项会被化成同一个形式。一个容易被误解的例子：

```mathematica
ITensorReduce[T[a, b] A[a]]
(* => A[a] T[b, a] *)
```

这不是错误：`A[a] T[b, a]` 和 `A[a] T[a, b]` 由 `T` 的对称性相等，包只是选了其中一个作为规范代表。表达式不同不代表不相等，判断相等应该比较 `ITensorReduce` 之后的结果。

`ITensorReduceOneTerm` 只处理单项、不展开 `Plus`；当你明确知道输入是一个单项时更快。

### 4.3 度规与指标升降

度规缩并由 `ContractMetric` 完成：

```mathematica
ContractMetric[T[a, b] g[DI@b, DI@c]]
(* => T[a, DI[c]] *)

ContractMetric[g[a, DI@b] T[b, c]]
(* => T[a, c] *)

ContractMetric[g[DI@a, b] T[b, c]]
(* => T[a, c] *)
```

- `g[DI@b, DI@c]` 是协变度规，用来降低指标；
- `g[a, b]` 是逆变度规，用来升高指标；
- `g[a, DI@b]` 是"一上一下"的度规，等价于 `δ`，直接替换指标。

`ITensorReduce` 也会自动应用 `MetricOfSlotType[...]` 的一上一下规则，但不会主动用 `g[DI@_, DI@_]` 或 `g[_, _]` 做升降；后者需要显式调用 `ContractMetric`。

缩并整个度规会给出维数：

```mathematica
ITensorReduce[g[DI@a, a] T[b, c]]
(* => 4 T[b, c] *)
```

`ContractMetric[expr, metrics]` 可以只缩并指定的度规；`ContractMetric[expr]` 或 `ContractMetric[expr, All]` 缩并所有 `ContractableMetricQ` 为真的度规。`DeepContractMetric` 还会进入 `IndexScope` 内部。

### 4.4 显式求和与作用域

`IndexedSum[expr, {i, type}]` 表示对指标 `i` 的显式求和（而不是张量缩并）：

```mathematica
IndexedSum[5, {a, mf}]
(* => 20 *)

FindIndicesSlots[IndexedSum[T[a, b], {a, mf}]]
(* => {{1, 2} -> mf} *)
```

如果被求和的指标在表达式中不出现，`IndexedSum` 直接乘上该槽位类型的维数；如果出现，它保持符号形式，`ITensorReduce` 会把不含该指标的因子提到求和号外。

`IndexScope[expr]` 把 `expr` 当作一个标量：内部指标都是局部哑指标，不会和外层缩并，也不会影响外层的指标命名。它常用于曲率标量等"整体作为标量因子"的表达式：

```mathematica
FindIndicesSlots[IndexScope[T[a, a]]]
(* => {} *)
```

在笔记本里它显示为红色圆括号。

### 4.5 把表达式变成规则或函数

`MakeIndexRule` 和 `MakeIndexFunction` 用于把"带自由指标的表达式"转成可复用的规则/函数；哑指标会自动唯一化：

```mathematica
DefSimpleTensor[p, {mf}, {}];

g[DI@a, DI@b] /. MakeIndexRule[g[a, b] -> p[a] p[b] p[c] p[DI@c], {a, b}] // ITensorReduce
(* => p[c] p[DI[a]] p[DI[b]] p[DI[c]] *)

MakeIndexFunction[p[a] p[b] p[c] p[DI@c], {a, b}][DI@a, a] // ITensorReduce
(* => p[a] p[b] p[DI[a]] p[DI[b]] *)
```

`ToIndexFunction`、`ToIndexedExprFunction`、`IndexedExprFunction` 提供更底层的同类功能。

## 5. 分块张量

### 5.1 什么是分块张量

Peanotica 允许把**数组的每个分量写成 `ETensor`**。外层数组的指标可以理解为"块坐标"：它把整个指标空间分成若干块，而每个块内部的 `ETensor` 还可以带自己的自由指标。于是同一个张量的不同分量可以有不同的自由指标结构，这就是"分块张量"。

`ETensor[expr, frees]` 的 `frees` 中，每个位置对应张量的一个指标槽位：

- 写一个指标名（如 `a`）：该槽位有一个自由指标；
- 写 `Null`：该槽位没有自由指标（指标被块坐标吸收，或该分量是标量）；
- `frees` 必须是扁平的，不支持嵌套列表。

`ETensor` 的加法会按位置合并不同的自由指标结构，这正是"不同块的分量可以相加"的关键。

### 5.2 例子：任意维球对称时空

考虑度规

$$ds^2 = -h(r)dt^2 + \frac{dr^2}{f(r)} + r^2 \hat g_{ab}\,d\theta^a d\theta^b,$$

其中 $\hat g_{ab}$ 是 $\dim_x$ 维球面的度规，$\dim_x$ 保持为符号。把 $(t, r, \text{角度})$ 当作三个块坐标，度规就是一个 3×3 的分块对角矩阵：

<!-- setup -->
```mathematica
SetAttributes[dimf, Constant];
SetAttributes[dimx, Constant];

DefSimpleSlotType[mf4, dimf, {Symbol /@ CharacterRange["a", "z"], DefaultIndex}];
DefSimpleSlotType[mx, dimx, {Symbol /@ CharacterRange["a", "z"], DefaultIndex}];

DefSimpleMetric[g4, mf4, 1];
DefSimpleMetric[gd, mx, 1];
MetricOfSlotType[mf4] ^= g4;
MetricOfSlotType[mx] ^= gd;

DefTensorDerivativeOperator[cd4, {mf4}, {}];
DefTensorDerivativeOperator[cdd, {mx}, {}];
cd4[g4[_, _], _] = 0;
cdd[gd[_, _], _] = 0;
cdd[r, _] = 0;
cdd[t, _] = 0;

DefParametreDerivativeOperator[paramd, DerParameters -> {t, r}];
paramd[gd[_, _], _] = 0;
DerFunctionQ[h] ^= True;
DerFunctionQ[f] ^= True;
```

<!-- setup -->
```mathematica
gBlock = SparseArray[{
    {1, 1} -> ETensor[-h[r], {Null, Null}],
    {2, 2} -> ETensor[1/f[r], {Null, Null}],
    {3, 3} -> ETensor[r^2 gd[DI@a, DI@b], {a, b}]
}];
gBlockInv = SparseArray[{
    {1, 1} -> ETensor[-1/h[r], {Null, Null}],
    {2, 2} -> ETensor[f[r], {Null, Null}],
    {3, 3} -> ETensor[r^-2 gd[a, b], {a, b}]
}];
pds = {
    ETensor[paramd[Null, t], {Null}],
    ETensor[paramd[Null, r], {Null}],
    ETensor[cdd[Null, DI@a], {a}]
};
```

- 块 `(1,1)`、`(2,2)` 是标量块，`frees = {Null, Null}` 表示这两个槽位没有剩余自由指标；
- 块 `(3,3)` 是二维球面块，`frees = {a, b}` 表示这个分量还带两个角度指标；
- 外层的 3×3 位置分别标记 $t$、$r$、角度三个方向。

现在可以像普通度规一样计算曲率：

```mathematica
res = ComputeCurvature[<|"Metric" -> gBlock, "InverseMetric" -> gBlockInv, "Pd" -> pds|>];

Simplify[res["Christoffel"][[2, 3, 3]][[1]]]
(* => -r f[r] gd[DI[a], DI[b]] *)
```

注意 `[[2,3,3]]` 取到的是块分量，`[[1]]` 取到 `ETensor` 里的表达式。角度指标 `a, b` 原样保留在结果里，所以这一个分量仍然是一个二维张量。

不同块的分量可以相加，`ETensor` 会自动对齐自由指标结构：

```mathematica
ETensor[A[a], {a, Null}] + ETensor[A[b], {Null, b}]
(* => ETensor[A[a] + A[b], {a, b}] *)
```

### 5.3 为什么这很有用

- 角度部分不需要展开成显式坐标，`dimx` 可以作为符号参与运算，结果对任意维数成立；
- 计算过程中不同块会自然产生不同的自由指标结构（例如黎曼张量的分量可能是 `{Null,Null,Null,Null}`、`{Null,a,Null,b}`、`{a,b,c,d}` 等），加法、缩并、求迹都能正确处理；
- 同一个框架也能描述 Kaluza–Klein 式的乘积空间、以及"某一块用另一套数值/符号表示"的情形。

`DiffGeo.wlt` 里有一个完整的球对称例子：它在角度块上额外加入球面的内禀曲率 $k$，并验证了任意 `dimx` 下的 Ricci 标量公式。

### 5.4 使用限制

- `frees` 必须是扁平的，`Null` 只能出现在需要表示"该槽位无自由指标"的位置；
- 数组的每个非零分量都应该是 `ETensor`（或 `0`），不要混入裸数字，否则在 `ITensorOuter` 等运算中会出现 `nonzeronontensor` 消息；
- 不同分量的 `frees` 长度必须一致，否则相加时会报 `ETensor::plusincompat`。

## 6. 分量计算：ETensor 与 NITensor

### 6.1 ETensor

`ETensor[expr, frees]` 表示"表达式 + 自由指标结构"，可以把指标名抽象掉，只保留自由指标的位置。它支持常见的代数运算：

<!-- setup -->
```mathematica
DefSimpleSlotType[mf, 4, {Symbol /@ CharacterRange["a", "z"], DefaultIndex}];
DefSimpleTensor[T, {mf, mf}, {SCycles@{1, 2}}];
DefSimpleTensor[A, {mf}, {}];
```

```mathematica
ETensor[T[a, b], {a, b}] + ETensor[T[b, a], {a, b}] // ITensorReduce
(* => ETensor[2 T[a, b], {a, b}] *)
```

`ETensor[expr, frees][inds]` 把自由指标按位置替换成给定指标：

```mathematica
ETensor[T[a, b], {a, b}][m, n]
(* => T[m, n] *)
```

常用运算：

```mathematica
ITensorTranspose[ETensor[T[a, b], {a, b}], {2, 1}]
(* => ETensor[T[a, b], {b, a}] *)

ITensorOuter[Times, ETensor[A[a], {a}], ETensor[T[b, c], {b, c}], {}]
(* => ETensor[A[a] T[b, c], {a, b, c}] *)

ITensorContractTwo[Times, ETensor[A[a], {a}], ETensor[A[b], {b}], {{1, 1}}]
(* => IndexScope[A[a]^2] *)

ITensorFixedContract[Times, ETensor[T[a, b], {a, b}], ETensor[A[c], {c}], 2, 1]
(* => ETensor[A[b] T[a, b], {a}] *)
```

`ITensorTranspose` 的置换参数可以包含重复项，用来把多个槽位合并（缩并）；`ITensorContractTwo` 和 `ITensorFixedContract` 会把外积和随后的缩并一步完成。`ITensorSum` 对指定槽位求和。

### 6.2 ETensor 数组

`ETensor` 可以放在数组的每个分量里，形成"数组 + 自由指标结构"的混合表示。`ETensorArray` 提供了一种批量生成 `ETensor` 分量的底层写法（参数约定见 [api.md](api.md)）。第 5 节的分块张量就是这种用法的典型。

### 6.3 NITensor

`NITensor[expr, inds]` 是"带具体指标名的中间表示"，适合先做缩并、最后再套回指标。`NITensorReduce[expr, frees]` 会自动挑选取缩并对并完成缩并，`ExtractNITensor[expr, inds]` 把结果取回成普通表达式。

`NITensor` 主要用于微分几何中的协变导数和分量求值（见第 7、9 节）。普通用户也可以用它来避免过早代入自由指标；它的完整接口见 [api.md](api.md)。

> 注意：`NITensor` 的乘积会被 Mathematica 自动合并成 `Power`（例如两个相同的 `NITensor` 相乘），而 `NITensorReduce` 目前不处理 `Power`。如果两个因子可能全等，先用不同的指标名或把它们放在不同的因子位置上。

### 6.4 MakeIndexRule / MakeIndexFunction

见 4.5。它们把"带自由指标的表达式"变成规则或函数，是实现自定义张量替换的推荐方式。

## 7. 微分几何

### 7.1 协变导数

用 `DefTensorDerivativeOperator` 定义协变导数：

<!-- setup -->
```mathematica
DefSimpleSlotType[mf, 4, {Symbol /@ CharacterRange["a", "z"], DefaultIndex}];
DefSimpleMetric[g, mf, 1];
MetricOfSlotType[mf] ^= g;
DefTensorDerivativeOperator[cd, {mf}, {}];
cd[g[_, _], _] = 0;
DefRiemannCurvature[mf, R, Ric, Rs];
DefSimpleTensor[T, {mf, mf}, {SCycles@{1, 2}}];
```

```mathematica
ITensorReduce[cd[T[a, b], c]]
(* => cd[T[a, b], c] *)
```

协变导数作用在抽象张量（例如 `T[a, b]`）上时保持符号形式；它会在和、乘积、标量函数、常数上按规则展开。`cd[g[_, _], _] = 0` 声明度规与 `cd` 相容（度规的协变导数为零）。

导数算子会按以下规则展开：

- 常数（带 `Constant` 属性的符号，或 `DerConstantQ` 为真的表达式）的导数为 0。推荐用 `SetAttributes[foo, Constant]` 声明常量符号；只有不能设置属性的表达式（复合表达式、受保护的符号等）才用 `DerConstantQ[expr] ^= True`；
- 标量函数（`DerFunctionQ` 为真）按链式法则展开；
- `Derivative[...][f][...]` 总是按标量函数处理；
- 乘积按 Leibniz 法则展开。

用 `DefScalarFunction[f, "f"]` 可以把 `f` 标记为标量函数并设置显示名。

### 7.2 曲率

`DefRiemannCurvature[slot, R, Ric, Rs]` 一次性定义黎曼张量、Ricci 张量和 Ricci 标量，以及它们之间的缩并规则：

```mathematica
ITensorReduce[R[b, a, d, c]]
(* => R[a, b, c, d] *)

ITensorReduce[R[a, b, c, DI@b]]
(* => Ric[a, c] *)

ITensorReduce[R[a, b, DI@b, c]]
(* => -Ric[a, c] *)

ITensorReduce[Ric[DI@a, a]]
(* => Rs *)
```

黎曼张量的对称性是 `RiemannSymmetricGenSet[1]`：前两个指标反对称、后两个指标反对称、前后两对交换对称。`DefRiemannCurvature` 设置的规则会自动处理各种 Ricci 缩并的符号。

`ContractMetric[R[a, b, c, d] g[DI@b, DI@d]]` 也会得到 `Ric[a, c]`。

### 7.3 协变导数的交换子

`SortCovD` 会把嵌套的协变导数按指标顺序整理；交换顺序时，用交换子替换：

```mathematica
SortCovD[cd[cd[T[a, b], d], c], cd, CovDCommutatorOfRiemann[R]]
(* => cd[cd[T[a, b], c], d] - R[c, d, DI[TempIndex[1]], b] T[a, TempIndex[1]]
       - R[c, d, DI[TempIndex[1]], a] T[TempIndex[1], b] *)
```

再用 `ITensorReduce` 把临时指标改名：

```mathematica
ITensorReduce[SortCovD[cd[cd[T[a, b], d], c], cd, CovDCommutatorOfRiemann[R]]]
(* => cd[cd[T[a, b], c], d] + R[b, e, c, d] T[a, DI[e]] + R[a, e, c, d] T[b, DI[e]] *)
```

`CovDCommutatorNoTorsion[expr, a, b, R]` 是无挠情形下的交换子；`CovDCommutatorOfRiemann[R]` 是它的算子形式，适合直接传给 `SortCovD`。

### 7.4 Bianchi 与交换子恒等式

`RiemannBianchiRelations` 生成黎曼张量的 Bianchi 关系：

```mathematica
RiemannBianchiRelations[R[a, b, c, d], cd, R]
(* => {R[a, b, c, d] + R[a, c, d, b] + R[a, d, b, c]} *)

RiemannBianchiRelations[cd[R[a, b, c, d], e], cd, R]
(* => {cd[R[c, d, a, b], e] + cd[R[d, e, a, b], c] + cd[R[e, c, a, b], d],
       cd[R[a, b, c, d], e] + cd[R[b, e, c, d], a] + cd[R[e, a, c, d], b],
       cd[R[a, b, c, d], e] + cd[R[a, c, d, b], e] + cd[R[a, d, b, c], e]} *)
```

`CovDCommutatorRelations` 生成协变导数交换子的定义关系。这些关系是"要满足的等式"，可以配合 `Simplify` 或 `AlgSimp` 使用。

### 7.5 指标适配与升降

`EnsureUpDownIndices`、`AdaptIndices`、`AdaptNITensor` 处理"抽象指标顺序/上下号与具体分量数组的槽位不一致"的情况。`MetricAdapter[{slot -> {metric, invMetric}, ...}]` 描述每种槽位类型用哪个度规升降。

这些函数主要供 `DiffGeo` 和 `Presets` 内部使用；普通用户通常只需要 `ContractMetric` 和 `ITensorReduce`。

### 7.6 扰动

`DefPerturbationOperator[pert]` 定义一个扰动算子。`pert[expr, n]` 表示 `expr` 的 n 阶扰动；`pert[expr, 0]` 是 `expr` 本身。

度量扰动可以逐阶展开：

```mathematica
DefPerturbationOperator[pert];
ExpandMetricPerturbation[pert[g[DI@a, DI@b], 1], {pert, g, h}]
(* => h[1, DI[a], DI[b]] *)

ExpandMetricPerturbation[pert[g[a, b], 1], {pert, g, h}]
(* => -h[1, a, b] *)
```

这里 `h` 是度规扰动的符号，`h[n, i, j]` 表示 n 阶扰动分量。逆度规的高阶展开可以用 `VarInverseMatrix[g[a, b], h, n]`。

相关函数：

- `ExpandMetricPerturbation`、`ExpandTensorDetPerturbation`：度量与行列式的扰动；
- `ExpandRiemannPerturbation`、`ExpandCovDPerturbation`：黎曼张量和协变导数的扰动；
- `PerturbationLeviCivitaChristoffel`、`PerturbationLeviCivitaRiemann`：Levi-Civita 联络和黎曼张量的逐阶扰动；
- `ExpandAllPerturbations`（Presets）：把以上步骤串起来。

### 7.7 其他几何对象

- `SymmetricRiemann[metric, {a,b,c,d}]`：由度规构造的"对称化"黎曼张量（常用来表示常曲率空间的曲率）；
- `WeylToRiemann`、`WeylToRiemannSchouten`：用 Schouten 张量表示外尔张量；
- `LovelockDensity[riemann, n]`：Lovelock 密度；
- `RiemannScalars[riemann, n]`：n 阶黎曼单项式（标量）。

## 8. 代数化简：AlgSimp

`ITensorReduce` 只使用**单项**对称性（槽位置换、哑指标改名）。像 Bianchi 恒等式这样的**多项**线性关系不在它的处理范围内，需要 <code>Peanotica`AlgSimp`</code>。

AlgSimp 的基本对象是带导数的抽象黎曼张量：

- `AlgDRiemann[n]`：n 阶协变导数的黎曼张量，槽位数为 `n + 4`；
- `AlgDRicci[n]`：n 阶导数的 Ricci 张量；
- `AlgEpsilon[n]`：n 维 Levi-Civita 符号；
- `AlgDRiemannList[{n1, n2, ...}]`：多个因子的乘积。

`AlgTensorSimplifyData[tensor, frees]` 返回 `{basis, rules}`：

- `basis`：所有不等价的单项（由槽位对称性和哑指标对称性枚举）；
- `rules`：用基表示其余单项的重写规则。

```mathematica
Needs["Peanotica`AlgSimp`"];

data = AlgTensorSimplifyData[AlgDRiemannList[{0, 0}], 0];
Length[data[[1]]]
(* => 4 *)
Length[data[[2]]]
(* => 1 *)
```

这里 `R^2` 型标量有 4 个不等价单项，Bianchi 关系给出 1 条线性关系，因此独立单项实际上是 3 个。规则可以直接作用在单项上：

```mathematica
data = AlgTensorSimplifyData[AlgDRiemannList[{0, 0}], 0];
data[[1, 4]] /. data[[2]]
(* => AlgTensorTerm[AlgDRiemann[0] AlgDRiemann[0], 0, {1, 3, 5, 7, 2, 4, 6, 8}] / 2 *)
```

`AlgTensorTerm[tensor, frees, perm]` 是 AlgSimp 的单项数据表示：`perm` 描述各槽位与指标位置的对应关系。`AlgTensorTermReduce` 会把它化成规范单项，`AlgRelations[tensor, frees, perm]` 给出该单项参与的 Bianchi 关系。

> **实验性**：AlgSimp 目前主要在 `AlgDRiemann`/`AlgDRicci`/`AlgEpsilon` 这些抽象单项式上工作；`ToAlgTensor`、`RiemannToAlgTensor`、`AlgTensorToRiemann` 等"与真实黎曼张量互转"的接口虽然声明了用法，但尚未实现。请把它当作推导恒等式的辅助工具，而不是完整的张量化简器。

## 9. Presets：开箱即用的工作流

<code>Peanotica`Presets`</code> 把"定义流形 + 算曲率 + 代入分量"的常见流程封装成几个函数。

### 9.1 定义黎曼流形

<!-- setup -->
```mathematica
sphSyms = SetupRiemannManifold[sph, 2, gs, cds, {Rsph, Ricsph, Rssph}];
(* => <|"Slot" -> sph, "Metric" -> gs, "CovD" -> cds,
       "Riemann" -> Rsph, "Ricci" -> Ricsph, "RicciScalar" -> Rssph|> *)
```

它等价于手动执行：

- 定义槽位类型 `sph`（维数 2，指标名 `a..z`）；
- 定义度规 `gs`、协变导数 `cds`（并声明度规相容）；
- 用 `DefRiemannCurvature` 定义 `Rsph`、`Ricsph`、`Rssph`。

选项：`ClearAll -> True`（默认清除同名符号）、`PPRiemannDisplayName -> "R"`（黎曼张量的显示名）。返回值是一个 Association，把符号名集中在一起，方便后续传给 `MakeCurvatureValueRules`、`PopulateCurvatureTReducerRules` 等函数。

### 9.2 从度规计算曲率

`ComputeCurvature` 接受一个 Association，至少包含度规、逆度规和坐标导数，返回 Christoffel、Riemann、Ricci、RicciScalar。下面用单位二维球面演示：

<!-- setup -->
```mathematica
gsVal = SparseArray[{
    {1, 1} -> ETensor[1, {Null, Null}],
    {2, 2} -> ETensor[Sin[t]^2, {Null, Null}]
}];
gsInv = SparseArray[{
    {1, 1} -> ETensor[1, {Null, Null}],
    {2, 2} -> ETensor[Csc[t]^2, {Null, Null}]
}];

DefParametreDerivativeOperator[pd2, DerParameters -> {t, phi}];
pds = {
    ETensor[pd2[Null, t], {Null}],
    ETensor[pd2[Null, phi], {Null}]
};

sphVals = ComputeCurvature[<|"Metric" -> gsVal, "InverseMetric" -> gsInv, "Pd" -> pds|>];
```

```mathematica
Simplify[sphVals["RicciScalar"]]
(* => 2 *)

Simplify[sphVals["Christoffel"][[1, 2, 2]][[1]]]
(* => -Cos[t] Sin[t] *)

Simplify[sphVals["Christoffel"][[2, 1, 2]][[1]]]
(* => Cot[t] *)
```

这是单位二维球面：`Ricci = g`、`RicciScalar = 2`。块坐标的编号是 1 = t、2 = φ；`sphVals["Christoffel"]` 是 2×2×2 的数组，每个分量是 `ETensor`，`[[1]]` 取其中的表达式。

`ComputeCurvature` 的输入还可以包含：

- `"PdSpec"`：混合"纯坐标导数"和"已有协变导数"的坐标规范，值为 `{"Pd", pd}` 或 `{"CovD", cd, data}` 的列表；
- `"RiemannPd"`：坐标导数部分的子黎曼张量（例如球面块的内禀曲率）；
- `PostCurvatureCompute`：每一步之后的化简函数，默认 `Simplify @* ITensorReduce @* ContractMetric`。

### 9.3 把抽象表达式代回具体分量

`MakeCurvatureValueRules[syms, values]` 把 `SetupRiemannManifold` 定义的抽象符号和 `ComputeCurvature` 得到的具体分量连接起来。之后用 `NITensorReduce` 计算、用 `ExtractNITensor` 取回普通数组：

```mathematica
rules = MakeCurvatureValueRules[sphSyms, sphVals];

arr = NITensorReduce[Ricsph[DI@a, DI@b] /. rules, {a, b}] // ExtractNITensor[{a, b}];
Simplify[Normal[arr] /. ETensor[e_, _] :> e]
(* => {{1, 0}, {0, Sin[t]^2}} *)
```

这里 `Ricsph[DI@a, DI@b]` 是抽象写法，`rules` 把它换成具体分量，`NITensorReduce` 完成缩并/指标适配，`ExtractNITensor` 取出 2×2 数组，最后把每个分量从 `ETensor` 包装里取出来并化简。

同样可以代入黎曼张量或更复杂的标量表达式：

```mathematica
rules = MakeCurvatureValueRules[sphSyms, sphVals];
rabcd = NITensorReduce[Rsph[DI@a, DI@b, DI@c, DI@d] /. rules, {a, b, c, d}] // ExtractNITensor[{a, b, c, d}];
Simplify[Normal[rabcd] /. ETensor[e_, _] :> e]
(* => 2×2×2×2 的分量数组 *)
```

对于更复杂的表达式，`PopulateCurvatureTReducerRules[name, syms, values]` 可以把这些规则挂在一个名字上，然后用 `NITensorReduce[expr, frees, name]` 直接求值。这个接口目前是实验性的，参数约定见 [api.md](api.md)。

### 9.4 变分

`DefTensorVariationOperator[var]` 定义变分算子：

```mathematica
DefTensorVariationOperator[var];

var[T[a, b], T[c, d]]
(* => (g[a, d] g[b, c] + g[a, c] g[b, d]) / 2 *)
```

第一个参数是被变分的表达式，第二个参数是变分所针对的张量；结果是对称化后的"变分核"。乘积规则、常数、`IndexScope`、`IndexedSum` 等会自动展开。

`VarCovDs`、`VarParamDs` 选项用于把协变导数/参数导数的分部积分规则也纳入变分，但目前属于实验性功能。

### 9.5 带标签的张量与标量函数

`DefLabelledTensor[name, slots, symmetry]` 在槽位列表前加一个标签槽位（`TFLabelSlot`），得到 `name[label, i1, i2, ...]` 形式的张量：

```mathematica
DefLabelledTensor[ricL, {mf, mf}, {SCycles@{1, 2}}];

FindIndicesSlots[ricL[LabelI[1], a, b]]
(* => {{2} -> mf, {3} -> mf} *)

ITensorReduce[ricL[LabelI[1], b, a]]
(* => ricL[LabelI[1], a, b] *)
```

标签指标 `LabelI[...]` 不参与缩并、不参与重命名，适合表示扰动阶、分量编号等。

`DefScalarFunction[f, "f"]` 把 `f` 标记为标量函数（导数按链式法则展开），并可设置显示名。

## 10. 选项与约定速查

### 10.1 ITensorReduce

| 选项 | 说明 |
|---|---|
| `FreeIndexNames -> Automatic` | 自动推断自由指标（出现一次）；也可给列表，或 `{}` |
| `FreeIndicesSymmetry -> {}` | 自由指标附带的对称性，视作与一个对称张量缩并 |
| `RenameDummies -> True` | 是否把哑指标改成规范名字 |
| `UseMetricOnSlots -> All` | 允许用度规交换上下指标对的槽位类型；`{}` 表示都禁用 |
| `FixedPoint -> False` | 反复化简直到稳定 |

### 10.2 指标约定

| 写法 | 含义 |
|---|---|
| `a` | 上指标 |
| `DI[a]` | 下指标（唯一写法；不要用 `-a`） |
| `LabelI[i]` | 标签指标（不缩并、不改名） |
| `TempIndex[n]` | 临时指标 |
| `DefaultIndex[n]` | 候选名用尽时的兜底指标 |
| `IndexNameSlot` | 指标类型模板（上） |
| `DI[IndexNameSlot]` | 指标类型模板（下） |

### 10.3 度规约定

| 写法 | 含义 |
|---|---|
| `g[DI@a, DI@b]` | 协变度规，降低指标 |
| `g[a, b]` | 逆变度规，升高指标 |
| `g[a, DI@b]` | 恒等映射（δ） |
| `MetricOfSlotType[type]` | 该槽位类型的默认度规 |

### 10.4 对称性约定

| 写法 | 含义 |
|---|---|
| `SCycles@{1, 2}` | 交换槽位 1 和 2，不变号 |
| `-SCycles@{1, 2}` | 交换槽位 1 和 2，变号（反对称） |
| `SymmetricGenSet[n]` | n 个槽位的全对称 |
| `RiemannSymmetricGenSet[n]` | 从槽位 n 开始的黎曼对称性 |
| `BlockSymmetricGenSet[list1, list2, ...]` | 块之间的对称性 |

## 11. 已知限制

以下是当前版本中已知的问题或未完成的部分。它们不影响文档中已验证的用法，但在实际使用中可能会遇到。

1. **`Needs::nocont`**：加载时提示一次，可忽略或用 `Quiet` 抑制（见 1.1）。
2. **平台相关的外部程序**：`mathlink/pperm-ml` 需要与平台匹配；仓库自带的是 macOS ARM64 版本，其他平台需要重新编译。
3. **未实现的符号**：以下符号已经声明了 `::usage`，但没有实现或只有占位定义：
   - `ToAlgTensor`、`RiemannToAlgTensor`、`AlgTensorToRiemann`；
   - `CETensor`、`ScalarValue`、`WrapETensor`、`ITensorRowReduce`；
   - `GroupByTensors`、`IndexedToFunction`、`MapIndexedGroupedIndexList`、`FoldListGroupedIndexList`；
   - `ConvertPermutationNotation`、`RubiksCubeGenSet`、`RelationClosureTableToArray`；
   - `CoRiemannValue`、`DefTensorPerturbation`、`PerturbationCovD`、`Cocurvature`、`Cotorsion`。
4. **`NoIndicesQ`**：实现里漏了参数，当前对所有输入都返回 `True`，不要依赖它。
5. **`SetupPerturbation`**：文档字符串写的是两个参数，实际定义需要三个参数（`pert, vard, info`），且 `vard` 未被使用；建议直接使用 `DefPerturbationOperator` 和 `Expand*Perturbation`。
6. **`PostprocessCanonicalIndicesList`**：`ITensorReduce` 接受这个选项，但当前没有读取它，设置无效。
7. **`AlgSimp` 的互转接口**：见第 8 节，目前只能在抽象单项式上工作。
8. **`VarCovDs` / `VarParamDs`**：变分算子对协变导数的处理尚未稳定，视为实验性功能。
9. **`IndexedSum` 的语义**：它表示显式求和，不是张量缩并；不要把它和同名指标缩并混为一谈。
10. **`ETensor` 的 `frees` 必须扁平**：不支持嵌套列表；`Null` 用来标记"该槽位没有自由指标"。
11. **数组分量类型**：`SparseArray`/普通数组里的非零分量应统一是 `ETensor`（或 `0`），否则会触发 `ITensorOuter::nonzeronontensor`。
12. **测试文件里的旧写法**：`Core.wlt` 中仍有少量使用 `None` 的历史用例；当前版本应使用 `Null`。
13. **没有 Paclet/帮助中心集成**：文档是 Markdown 形式，不能通过 Mathematica 的 F1 帮助查看。

## 12. 延伸阅读

- [implementation.md](implementation.md)：规范化算法、表示层、置换后端、各子包的实现细节；
- [api.md](api.md)：按模块列出的全部公开符号、签名、选项与说明；
- `Core.wlt`、`DiffGeo.wlt`：可直接运行的验证测试；
- `xPerm.wlt`、`compare.wl`：与 xAct`xPerm 的交叉验证（需要安装 xAct）。
