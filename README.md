# Peanotica

Indexed expression package for Mathematica. Designed for simplicity and flexibility. Still experimental.

## Brief explanations
This is an incomplete introduction of this package, explaining basic design ideas. A complete documentation of the package will be added in the future.

Unlike other symbolic tensor computation packages, which directly define tensors, covariant derivatives, perturbations, etc, as foundamental objects, Peanotica only cares about indices and symmetries, all objects are defined on top of that.

The two most important functions in Peanotica are `FindIndicesSlots` and `SymmetryOfExpression`. For example, to define a rank-2 symmetric tensor `T`,
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
* Symmetric tensor with an arbitary number of indices
```mathematica
FindIndicesSlots[expr_t] ^= Array[{#} -> Null &, Length@expr];
SymmetryOfExpression[expr_t] ^= Array[SCycles@{#, # + 1} &, Length@expr - 1];
```
* Covariant derivative can be defined by
```mathematica
FindIndicesSlots[covd[expr_, a_]] ^:= Join[FindIndicesSlots[expr, {1}], {{2} -> Null}];
SymmetryOfExpression[covd[expr_, a_]] ^:= SymmetryOfExpression@expr;
```
with `covd[A, a]` representing $\nabla_a A$. Here `FindIndicesSlots[expr, pos]` prefixes `pos` to each positions it returns. This definition is used by the differential geometry subpackage DiffGeo.

## Subpackage Peanotica\`Perm\`
The most important algorithm of this package is *double coset representative* algorithm.

## License
GNU General Public License. See `LICENSE` for detail.
