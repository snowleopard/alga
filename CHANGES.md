# Change log

## 0.3

* #136: Rename `Algebra.Graph.NonEmpty.NonEmptyGraph` to `Algebra.Graph.NonEmpty.Graph`,
* #136: Add `Algebra.Graph.AdjacencyMap.NonEmpty`.
* #126, #131: Implement custom `Ord` instance.
* #122, #125: Further work on labelled algebraic graphs.
* #121: Drop `Foldable` and `Traversable` instances.
* #113: Add `Labelled.AdjacencyMap`.

## 0.2

* #117: Add `sparsify`.
* #115: Add `isDfsForestOf`.
* #114: Add a basic implementation of edge-labelled graphs.
* #107: Drop `starTranspose`.
* #106: Extend `ToGraph` with algorithms based on adjacency maps.
* #106: Add `isAcyclic` and `reachable`.
* #106: Rename `isTopSort` to `isTopSortOf`.
* #102: Switch the master branch to GHC 8.4.3. Add a CI instance for GHC 8.6.1.
* #101: Drop `-O2` from the `ghc-options` section of the Cabal file.
* #100: Rename `fromAdjacencyList` to `stars`.
* #79: Improve the API consistency: rename `IntAdjacencyMap` to `AdjacencyIntMap`,
       and then rename the function that extracts its adjacency map to
       `adjacencyIntMap` to avoid the clash with `AdjacencyMap.adjacencyMap`,
       which has incompatible type.
* #82, #92: Add performance regression suite.
* #76: Remove benchmarks.
* #74: Drop dependency of `Algebra.Graph` on graph type classes.
* #62: Move King-Launchbury graphs into `Data.Graph.Typed`.
* #67, #68, #69, #77, #81, #93, #94, #97, #103, #110: Various performance improvements.
* #66, #72, #96, #98: Add missing `NFData` instances.

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
