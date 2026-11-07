# Peanotica 快速开始

本文给出几个常用计算的完整、可执行例子：张量规范化、曲率张量、分块坐标下的张量表达式、变分和微扰。安装与核心概念见 [features.md](features.md)；全部公开符号见 [api.md](api.md)。

约定：

- `dimx` 表示空间维数 \(d\)；
- (d+2) 维球对称时空用 `dimf = dimx + 2`；
- (d+1) 维 FLRW 时空用 `dimf = dimx + 1`。

每个小节里标记为 `setup` 的代码块会被该节后面的代码块继承；复制时请把同一小节的 `setup` 一起复制。也可以用

```bash
python3 docs/run_examples.py docs/quick-start.md
```

统一验证。

## 0. 加载

假设 Peanotica 的父目录已经在 `$Path` 中（见 [features.md §1.1](features.md#11-目录要求)）。

<!-- setup -->
```mathematica
Quiet[Needs["Peanotica`"], Needs::nocont];
```

## 1. 张量规范化：features.md 的快速开始

定义一个四维指标空间、一个对称二阶张量，并做规范化。

<!-- setup -->
```mathematica
DefSimpleSlotType[mf, 4, {Symbol /@ CharacterRange["a", "z"], DefaultIndex}];
DefSimpleMetric[g, mf, 1, DisplayName -> "g"];
DefSimpleTensor[T, {mf, mf}, {SCycles@{1, 2}}];
```

```mathematica
ITensorReduce[T[b, a]]
(* => T[a, b] *)

ITensorReduce[T[a, b] + T[b, a]]
(* => 2 T[a, b] *)
```

`SCycles@{1, 2}` 表示交换两个槽位的置换；`DefSimpleTensor` 的第三个参数给它一个对称性生成元，因此 `T[b, a]` 会被化成 `T[a, b]`。要表示反对称张量，把生成元取负：

```mathematica
DefSimpleTensor[S, {mf, mf}, {-SCycles@{1, 2}}];

ITensorReduce[S[a, a]]
(* => 0 *)

ITensorReduce[S[a, b] + S[b, a]]
(* => 0 *)
```

反对称张量缩并同一个指标必然为 0，所以两个表达式都归零。

## 2. 用 ComputeCurvature 计算曲率张量

`ComputeCurvature` 接受度规、逆度规和坐标导数，返回 Christoffel、Riemann、Ricci 和 Ricci 标量。下面用单位二维球面演示：

\[
ds^2 = dt^2 + \sin^2 t\, d\phi^2 .
\]

<!-- setup -->
```mathematica
sphereMetric = SparseArray[{
    {1, 1} -> ETensor[1, {Null, Null}],
    {2, 2} -> ETensor[Sin[t]^2, {Null, Null}]
}];
sphereMetricInv = SparseArray[{
    {1, 1} -> ETensor[1, {Null, Null}],
    {2, 2} -> ETensor[Csc[t]^2, {Null, Null}]
}];

DefParametreDerivativeOperator[pd2, DerParameters -> {t, phi}];
spherePd = {
    ETensor[pd2[Null, t], {Null}],
    ETensor[pd2[Null, phi], {Null}]
};

sphereVals = ComputeCurvature[<|
    "Metric" -> sphereMetric,
    "InverseMetric" -> sphereMetricInv,
    "Pd" -> spherePd
|>];
```

```mathematica
sphereVals["Christoffel"][[1, 2, 2]][[1]]
(* => -Cos[t] Sin[t] *)

Simplify[sphereVals["Riemann"][[1, 2, 1, 2]][[1]]]
(* => 1 *)

Simplify[sphereVals["Riemann"][[1, 2, 1, 2]][[1]] sphereVals["Metric"][[2, 2]][[1]]]
(* => Sin[t]^2 *)

sphereVals["Ricci"][[2, 2]][[1]]
(* => Sin[t]^2 *)

sphereVals["RicciScalar"]
(* => 2 *)
```

块坐标 1 对应 \(t\)，2 对应 \(\phi\)。注意 `"Riemann"` 的最后一个指标是**上指标**：`sphereVals["Riemann"][[1,2,1,2]][[1]]` 是 \(R_{t\phi t}{}^{\phi}=1\)，乘以 \(g_{\phi\phi}\) 才得到全下指标的 \(R_{t\phi t\phi}=\sin^2 t\)。

## 3. 分块坐标下的张量表达式

`PopulateCurvatureTReducerRules[name, syms, values]` 把 `SetupRiemannManifold` 的抽象张量和 `ComputeCurvature` 的具体分量挂到名字 `name` 上。之后 `NITensorReduceExtract[expr, frees, name]` 就能把抽象张量表达式展开成块坐标下的分量数组。

### 3.1 (d+2) 维球对称时空

考虑度规

\[
ds^2 = -h(r)\,dt^2 + \frac{dr^2}{f(r)} + r^2 \hat g_{ab}\,d\theta^a d\theta^b ,
\]

其中 \(\hat g_{ab}\) 是 `dimx` 维常曲率空间度规，曲率因子为 `k`。把 \((t,r,\text{角度})\) 当作三个块坐标，度规是一个 3×3 分块对角矩阵；角度块仍然带自由指标 `a, b`。

<!-- setup -->
```mathematica
SetAttributes[dimx, Constant];
dimf = dimx + 2;

DerFunctionQ[h] ^= True;
DerFunctionQ[f] ^= True;

sphInfo = SetupRiemannManifold[sphMf, dimf, gsph, cdsph,
    {Rsph, Ricsph, Rssph}];
sphXInfo = SetupRiemannManifold[sphMx, dimx, gsphX, cdsphX,
    {RsphX, RicsphX, RssphX}];

(* r、t 在角度流形上是常数 *)
cdsphX[r, _] = 0;
cdsphX[t, _] = 0;

DefParametreDerivativeOperator[pdSph, DerParameters -> {t, r}];
pdSph[gsphX[_, _], _] = 0;

sphMetric = SparseArray[{
    {1, 1} -> ETensor[-h[r], {Null, Null}],
    {2, 2} -> ETensor[1/f[r], {Null, Null}],
    {3, 3} -> ETensor[r^2 gsphX[DI@a, DI@b], {a, b}]
}];
sphMetricInv = SparseArray[{
    {1, 1} -> ETensor[-1/h[r], {Null, Null}],
    {2, 2} -> ETensor[f[r], {Null, Null}],
    {3, 3} -> ETensor[r^-2 gsphX[a, b], {a, b}]
}];

sphPdSpec = {
    {"Pd", pdSph[#, t] &},
    {"Pd", pdSph[#, r] &},
    {"CovD", cdsphX, k*SymmetricRiemann[gsphX, #] &}
};

sphVals = ComputeCurvature[<|
    "Metric" -> sphMetric,
    "InverseMetric" -> sphMetricInv,
    "PdSpec" -> sphPdSpec
|>];

PopulateCurvatureTReducerRules[sphR, sphInfo, sphVals];
```

下面代码里的 \(\gamma_{ab}\) 就是 `gsphX[DI@a, DI@b]`。取 \(h=f=1-r^2,\ k=1\)，得到单位半径的 (d+2) 维 de Sitter 时空的静态坐标。下面把 Ric 分量、de Sitter 校验和二阶 Gauss–Bonnet 密度放在同一个代码块里；后两者依赖前面算出的 `ricciSph` 和 `dssRules`。

```mathematica
(* 一般 h(r), f(r), k 下的 Ric 分量 *)
ricciSph = Simplify[DeepContractMetric[
    NITensorReduceExtract[Ricsph[DI@a, DI@b], {a, b}, sphR]
]];

ricciSph[[1, 1, 1]]
(* => (f'[r] h'[r] + f[r] (2 d h'[r]/r - h'[r]^2/h[r] + 2 h''[r]))/4 *)

ricciSph[[3, 3, 1]]
(* => γ_ab (h[r] (2(d-1)k - 2(d-1)f[r] - r f'[r]) - r f[r] h'[r])/(2 h[r]) *)

(* 单位半径 de Sitter：h = f = 1 - r^2, k = 1 *)
dssRules = {
    h -> Function[{r}, 1 - r^2],
    f -> Function[{r}, 1 - r^2],
    k -> 1
};

Simplify[NITensorReduceExtract[Rssph, {}, sphR] /. dssRules]
(* => (d+1)(d+2) *)

Simplify[ricciSph /. dssRules]
(* => Ric = (d+1) g，三个块分别是
       (d+1)(-1+r^2),  (d+1)/(1-r^2),  (d+1) r^2 γ_ab *)

(* 二阶 Gauss–Bonnet 密度 *)
gbSph = Simplify[DeepContractMetric[
    NITensorReduceExtract[LovelockDensity[Rsph, 2], {}, sphR]
]];

Simplify[gbSph /. dssRules]
(* => d(d-1)(d+1)(d+2) *)
```

`gbSph` 本身是任意 \(h(r), f(r), k\) 下的 Gauss–Bonnet 密度；上面的最后一行只是把它代入 de Sitter 特例。

### 3.2 (d+1) 维 FLRW 时空

考虑

\[
ds^2 = -dt^2 + a(t)^2 \hat g_{ab}\,d\theta^a d\theta^b ,
\]

其中 \(\hat g_{ab}\) 是 `dimx` 维空间切片度规，空间曲率因子为 `k`。这是 `inflation.nb.pdf` 中 `frw` reducer 的同一套写法，只是这里保留一般 `k`。

<!-- setup -->
```mathematica
dimf = dimx + 1;

DefScalarFunction[scaleA, "a"];

flrwInfo = SetupRiemannManifold[flrwMf, dimf, gFlrw, cdFlrw,
    {Rflrw, Ricflrw, Rsflrw}];
flrwXInfo = SetupRiemannManifold[flrwMx, dimx, gFlrwX, cdFlrwX,
    {RflrwX, RicflrwX, RsflrwX}];

DefParametreDerivativeOperator[pdFlrw, DerParameters -> {t}];
pdFlrw[gFlrwX[_, _], _] = 0;

(* a(t) 及其时间导数在空间切片上是常数 *)
cdFlrwX[scaleA, _] = 0;
cdFlrwX[t, _] = 0;
cdFlrwX[pdFlrw[expr_, ders__], a_] := pdFlrw[cdFlrwX[expr, a], ders];

flrwMetric = SparseArray[{
    {1, 1} -> ETensor[-1, {Null, Null}],
    {2, 2} -> ETensor[scaleA^2 gFlrwX[DI@a, DI@b], {a, b}]
}];
flrwMetricInv = SparseArray[{
    {1, 1} -> ETensor[-1, {Null, Null}],
    {2, 2} -> ETensor[scaleA^-2 gFlrwX[a, b], {a, b}]
}];

flrwPdSpec = {
    {"Pd", pdFlrw[#, t] &},
    {"CovD", cdFlrwX, k*SymmetricRiemann[gFlrwX, #] &}
};

flrwVals = ComputeCurvature[<|
    "Metric" -> flrwMetric,
    "InverseMetric" -> flrwMetricInv,
    "PdSpec" -> flrwPdSpec
|>];

PopulateCurvatureTReducerRules[flrwR, flrwInfo, flrwVals];
```

下面计算 Ricci 标量、爱因斯坦张量 \(G_{ab}=R_{ab}-\frac12 R g_{ab}\) 和二阶 Gauss–Bonnet 密度。这里 `gFlrwX[DI@a, DI@b]` 是空间切片度规 \(\gamma_{ab}\)；完整的空间度规是 \(a^2\gamma_{ab}\)。

```mathematica
(* Ricci 标量 *)
Simplify[NITensorReduceExtract[Rsflrw, {}, flrwR]]
(* => d ((d-1)(k + a'^2) + 2 a a'')/a^2 *)

(* 爱因斯坦张量 *)
gEinstein = Simplify[DeepContractMetric[
    NITensorReduceExtract[
        Ricflrw[DI@a, DI@b] - 1/2 Rsflrw gFlrw[DI@a, DI@b],
        {a, b}, flrwR
    ]
]];

gEinstein[[1, 1, 1]]
(* => d(d-1)(k + a'^2)/(2 a^2) *)

gEinstein[[2, 2, 1]]
(* => -1/2 (d-1) γ_ab ((d-2)(k + a'^2) + 2 a a'') *)

(* 二阶 Gauss–Bonnet 密度 *)
gbFlrw = Simplify[DeepContractMetric[
    NITensorReduceExtract[LovelockDensity[Rflrw, 2], {}, flrwR]
]];

gbFlrw
(* => d(d-1)(d-2)(k + a'^2)((d-3)(k + a'^2) + 4 a a'')/a^4 *)
```

## 4. 张量变分：DefTensorVariationOperator

`DefTensorVariationOperator[var]` 定义变分算符 `var[expr, field]`。先看对称二阶张量的变分核：

<!-- setup -->
```mathematica
SetAttributes[κv, Constant];

vInfo = SetupRiemannManifold[vMf, 4, vG, vCd, {vR, vRic, vRs}];
DefSimpleTensor[Tv, {vMf, vMf}, {SCycles@{1, 2}}];

DefScalarFunction[phi, "φ"];
DefScalarFunction[V, "V"];

DefTensorVariationOperator[var, VarCovDs -> {vCd}];
```

```mathematica
var[Tv[a, b], Tv[c, d]]
(* => (vG[a, d] vG[b, c] + vG[a, c] vG[b, d])/2 *)
```

变分一个标量场作用量：

\[
S = \int d^4x \sqrt{-g}\left(\frac{R}{2\kappa}
    - \frac12 \nabla_a\phi\nabla^a\phi - V(\phi)\right).
\]

这里省略了 \(\sqrt{-g}\) 的变分，只看对 \(\phi\) 的 Euler–Lagrange 表达式：

```mathematica
lag = vRs/(2 κv) - 1/2 vCd[phi, DI@a] vCd[phi, a] - V[phi];

var[lag, phi] // ITensorReduce
(* => vCd[vCd[phi, a], DI[a]] - V'[φ] *)
```

`VarCovDs -> {vCd}` 让协变导数通过分部积分转移到变分上，因此结果是 \(\nabla_a\nabla^a\phi - V'(\phi)\)，即标量场的运动方程。

## 5. 张量微扰：DefPerturbationOperator

`DefPerturbationOperator[pert]` 定义扰动算符：`pert[expr, n]` 是 `expr` 的 n 阶扰动，`pert[expr, 0]` 是 `expr` 本身。为避免和 3.1 节的标量函数 `h(r)` 冲突，这里用 `hPert` 表示度规扰动。

<!-- setup -->
```mathematica
pInfo = SetupRiemannManifold[pMf, 4, pG, pCd, {pR, pRic, pRs}];
DefLabelledTensor[hPert, {pMf, pMf}, {SCycles@{1, 2}}];
DefPerturbationOperator[pert];
```

先看度规和逆度规的一阶扰动：

```mathematica
ExpandMetricPerturbation[pert[pG[DI@a, DI@b], 1], {pert, pG, hPert}]
(* => hPert[1, DI[a], DI[b]] *)

ExpandMetricPerturbation[pert[pG[a, b], 1], {pert, pG, hPert}]
(* => -hPert[1, a, b] *)
```

也就是说，`hPert[1, DI[a], DI[b]]` 是 \(h_{ab}\) 的一阶扰动，`-hPert[1, a, b]` 是逆度规的一阶扰动 \(-h^{ab}\)。

再展开 Ricci 张量的一阶扰动：

```mathematica
ExpandRiemannPerturbation[
    pert[pRic[DI@a, DI@b], 1],
    {pert, pG, hPert},
    {pCd, pR, pRic, pRs}
] // ITensorReduce
(* => -1/2 pCd[pCd[hPert[1, c, DI[c]], DI[b]], DI[a]]
       + 1/2 pCd[pCd[hPert[1, DI[a], c], DI[b]], DI[c]]
       - 1/2 pCd[pCd[hPert[1, DI[a], DI[b]], c], DI[c]]
       + 1/2 pCd[pCd[hPert[1, DI[b], c], DI[a]], DI[c]] *)
```

用抽象指标写就是

\[
\delta R_{ab}
= -\frac12 \nabla^c\nabla_c h_{ab}
+ \frac12\left(\nabla_a\nabla^c h_{cb} + \nabla_b\nabla^c h_{ca}\right)
- \frac12 \nabla_a\nabla_b h ,
\]

其中 \(h = h^c{}_c\)。更完整的曲率、标量扰动展开（`ExpandAllPerturbations`）见 [features.md §7.6](features.md#76-扰动) 和 `inflation.nb.pdf` 的 Perturbation 部分。

## 6. 更实际的例子：Einstein–Gauss–Bonnet 球对称方程

考虑作用量

\[
S = \int d^{d+2}x\,\sqrt{-g}\left(R - 2\Lambda + \alpha L_{GB}\right),
\qquad
L_{GB} = R^2 - 4R_{ab}R^{ab} + R_{abcd}R^{abcd}.
\]

沿用 [3.1 节](#31-d2-维球对称时空) 的球对称分块度规

\[
ds^2 = -h(r)\,dt^2 + \frac{dr^2}{f(r)} + r^2 \hat g_{ab}\,d\theta^a d\theta^b ,
\]

其中 \(\hat g_{ab}\) 是 `dimx` 维常曲率空间度规，曲率因子 `k` 保留为符号：\(k=1\) 是球面，\(k=0\) 是平面，\(k=-1\) 是双曲面。

用 VarL 方法：先对 \(\sqrt{-g}L\) 做一阶度规微扰，再对一阶微扰变分。对完整张量，这给出

\[
E_{ab}
= R_{ab} - \frac12 R g_{ab} + \Lambda g_{ab}
+ \alpha\left(2R R_{ab} - 4R_{ac}R_b{}^c - 4R_{acbd}R^{cd}
+ 2R_{acde}R_b{}^{cde} - \frac12 L_{GB}g_{ab}\right)
= 0 .
\]

代码骨架如下（下面用 `egbPert`/`egbVar` 具体实现）：

```mathematica
L = PutIndexScope[Sqrt[-detg] (R - 2 Λ + α LGB), {}];
L1 = pert[L, 1] // ExpandAllPerturbations[pert, info] // IndexScope //
     DeepContractMetric // ITensorReduce;
eom = var[L1, pert[g][1, DI@a, DI@b]]/Sqrt[-detg];
```

注意作用量必须先用 `PutIndexScope` 把哑指标作用域处理好；否则 GB 项的哑指标会在变分时重复。下面沿用 3.1 节的 `sphMf`、`gsph`、`cdsph`、`Rsph`、`Ricsph`、`Rssph` 和 `sphR`，对球对称 ansatz 实际做这个计算。

<!-- setup -->
```mathematica
SetAttributes[k, Constant];
SetAttributes[Λ, Constant];
SetAttributes[α, Constant];
```

```mathematica
detgSph = detg;
cdsph[detgSph, _] = 0;
info = Append[sphInfo, "DetMetric" -> detgSph];

DefPerturbationOperator[egbPert];
DefTensorVariationOperator[egbVar, VarCovDs -> {cdsph}];

(* 确保指标符号没有遗留值 *)
ClearAll[a, b, c, d];
LGB = Rssph^2 - 4 Ricsph[a, b] Ricsph[DI@a, DI@b] + Rsph[a, b, c, d] Rsph[DI@a, DI@b, DI@c, DI@d];

(* 作用量必须先用 PutIndexScope 处理哑指标 *)
L = PutIndexScope[Sqrt[-detgSph] (Rssph - 2 Λ + α LGB), {}];

L1 = egbPert[L, 1] // ExpandAllPerturbations[egbPert, info] // IndexScope //
     DeepContractMetric // ITensorReduce;

eom = egbVar[L1, egbPert[gsph][1, DI@a, DI@b]];
eom = Simplify[eom/Sqrt[-detgSph]];

eomSph = NITensorReduceExtract[eom, {a, b}, sphR] // DeepContractMetric;

Ett = Simplify[eomSph[[1, 1, 1]]];
Err = Simplify[eomSph[[2, 2, 1]]];

EttEq = Simplify[2 r^4 Ett/h[r]] == 0;
ErrEq = Simplify[-2 r^4 f[r] h[r] Err] == 0;

EttEq
ErrEq

Solve[EttEq, Derivative[1][f][r]] // Simplify
Solve[ErrEq, Derivative[1][h][r]] // Simplify
```

`EttEq` 是 \(E_{tt}=0\)（去掉非零公共因子 \(h/(2r^4)\)），只含 \(f, f'\)，不含 \(h\)；`ErrEq` 是 \(E_{rr}=0\)（去掉非零公共因子 \(-1/(2r^4 f h)\)），含 \(h, h'\) 和 \(f\)。整理后可以写成

\[
\begin{aligned}
E_h:\quad
&-d(d-1)k r^2 - d(d-1)(d-2)(d-3)\alpha k^2 + 2r^4\Lambda
- d(d-1)(d-2)(d-3)\alpha f^2 \\
&+ d r\left[r^2 + 2(d-1)(d-2)\alpha k\right]f'
+ d(d-1)f\left[r^2 + 2(d-2)(d-3)\alpha k - 2(d-2)r\alpha f'\right] = 0,
\end{aligned}
\]

\[
\begin{aligned}
E_f:\quad
&\left[-d(d-1)k r^2 - d(d-1)(d-2)(d-3)\alpha k^2 + 2r^4\Lambda
+ d(d-1)\left(r^2 + 2(d-2)(d-3)\alpha k\right)f
- d(d-1)(d-2)(d-3)\alpha f^2\right]h \\
&+ d r f\left[r^2 + 2(d-1)(d-2)\alpha(k-f)\right]h' = 0 .
\end{aligned}
\]

最后两行 `Solve` 的输出就是 \(f'\) 和 \(h'\) 的显式形式。\(\alpha=0\) 时回到 Einstein 方程；\(k=1,0,-1\) 分别对应球、平面、双曲对称。例如在 \(d=3\)、\(h=f=1-r^2/L^2\)、\(k=1\) 时，\(E_h\) 和 \(E_f\) 都给出 \(\Lambda=6/L^2+12\alpha/L^4\)，可作为一致性检查。

## 7. 下一步

- 张量与分块张量的完整用法：[features.md](features.md)
- 全部公开符号与选项：[api.md](api.md)
- 算法与内部表示：[implementation.md](implementation.md)

`PopulateCurvatureTReducerRules` 目前仍是实验性接口，参数约定可能变化；如果只需要一次性代入，也可以用 `MakeCurvatureValueRules`（见 [features.md §9.3](features.md#93-把抽象表达式代回具体分量)）。
