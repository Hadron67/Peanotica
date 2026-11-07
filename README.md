# Peanotica

Indexed expression package for Mathematica. Designed for simplicity and flexibility. Still experimental.

## Documentation

- [快速开始](docs/quick-start.md)：曲率、分块坐标、Gauss–Bonnet、变分、微扰和 Einstein–Gauss–Bonnet 球对称方程的可执行示例。
- [特性与用法](docs/features.md)：从安装到张量、分块张量、微分几何、AlgSimp、Presets 的使用指南。
- [实现细节](docs/implementation.md)：规范化算法、表示层、置换后端、各子包的内部结构。
- [API 参考](docs/api.md)：按模块列出的公开符号、签名、选项与状态。

文档里的 Mathematica 代码块可以用 `python3 docs/run_examples.py` 执行；
`docs/api.md` 由 `python3 docs/gen_api.py` 从源码中的 `::usage` 生成。

## Brief explanations
下面是一份较早的简要介绍，说明基本设计思想；更完整的说明见上面的文档。

Most existing symbolic tensor computation packages follow a top-down approach, which typically defines manifold, vector bundles, and then tensors, covariant derivatives, etc. Peanotica instead follows a bottom-up approach which focus on the behaviours of the tensor expressions, *i.e.*, what indices slots an expression has and what symmetry does these slots have. All objects, including tensors, can be defined on top of that.

The two most important functions in Peanotica are `FindIndicesSlots` and `SymmetryOfExpression`. For example, to define a rank-2 symmetric tensor `T[a, b]`,
```mathematica
FindIndicesSlots[_T] ^= {{1} -> Null, {2} -> Null};
SymmetryOfExpression[_T] ^= {SCycles@{1, 2}};
```
`FindIndicesSlots[expr]` returns a list of positions and types of the index slots, whereas `SymmetryOfExpression[expr]` gives the generators of the symmetry group of the slots. Then you can canonicalize expressions using `ITensorReduce`
```mathematica
In[1] := ITensorReduce[T[b, a]]
Out[1] = T[a, b]
```
note that there's no need to define abstract indices.

This makes expressions in Peanotica highly flexible. Here are more examples:
* Symmetric tensor `t[a, b, ...]` with an arbitary number of indices
```mathematica
FindIndicesSlots[expr_t] ^= Array[{#} -> Null &, Length@expr];
SymmetryOfExpression[expr_t] ^= Array[SCycles@{#, # + 1} &, Length@expr - 1];
```
* Covariant derivative `covd[A, a]`, which represents $\nabla_a A$, may be defined by
```mathematica
FindIndicesSlots[covd[expr_, a_]] ^:= Join[FindIndicesSlots[expr, {1}], {{2} -> Null}];
SymmetryOfExpression[covd[expr_, a_]] ^:= SymmetryOfExpression@expr;
```
Here `FindIndicesSlots[expr, pos]` prefixes `pos` to each positions it returns. This definition is used by the differential geometry subpackage DiffGeo.

## Subpackage Peanotica\`Perm\`
The most important algorithm of this package is *double coset representative* algorithm.

## License
GNU General Public License. See `LICENSE` for detail.
