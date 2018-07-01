# Change log

## 0.2

* #86: Add `hasSelfLoop` into the API.
* #79: Improve the API consistency: rename `IntAdjacencyMap` to `AdjacencyIntMap`,
       and then rename the function that extracts its adjacency map to
       `adjacencyIntMap` to avoid the clash with `AdjacencyMap.adjacencyMap`,
       which has incompatible type.
* #76: Remove benchmarks.
* #74: Drop dependency of `Algebra.Graph` on graph type classes.
* #62: Move King-Launchbury graphs into `Data.Graph.Typed`.
* #67, #68, #69, #77, #81: Various performance improvements.
* #66, #72: Add missing `NFData` instances.

## 0.1.1.1

* #59: Allow `base-compat-0.10`.

## 0.1.1

* #58: Update documentation.
* #57: Allow newer QuickCheck.

## 0.1.0

* Start complying with PVP.
* #48: Add `starTranspose`.
* #48: Add `foldg` to `ToGraph`.
* #15: Optimise `removeEdge`.
* #39: Factor out difference lists into `Algebra.Graph.Internal`.
* #31: Add `Algebra.Graph.NonEmpty`.
* #32: Remove smart constructor `graph`.
* #27, #55: Support GHC versions 7.8.4, 7.10.3, 8.0.2, 8.2.2, 8.4.1.
* #25: Add `NFData Graph` instance.
* General improvements to code, documentation and tests.

## 0.0.5

* Add `dfs`.
* #19: Move `GraphKL` to an internal module.
* #18: Add `dfsForestFrom`.
* #16: Add support for graph export, in particular in DOT format.
* Make API more consistent, e.g. rename `postset` to `postSet`.
* Improve documentation and tests.
