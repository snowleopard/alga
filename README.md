# An algebra of graphs

[![Linux & OS X status](https://img.shields.io/travis/snowleopard/alga/master.svg?label=Linux%20%26%20OS%20X)](https://travis-ci.org/snowleopard/alga) [![Windows status](https://img.shields.io/appveyor/ci/snowleopard/alga/master.svg?label=Windows)](https://ci.appveyor.com/project/snowleopard/alga)

A library for algebraic construction and manipulation of graphs in Haskell. See
[this paper](https://github.com/snowleopard/alga-paper) for the motivation behind the library, the underlying
theory and implementation details.

The following series of blog posts also describe the ideas behind the library:
* Introduction: https://blogs.ncl.ac.uk/andreymokhov/an-algebra-of-graphs/
* A few different flavours of the algebra: https://blogs.ncl.ac.uk/andreymokhov/graphs-a-la-carte/
* Graphs in disguise or How to plan you holiday using Haskell: https://blogs.ncl.ac.uk/andreymokhov/graphs-in-disguise/
* Old graphs from new types: https://blogs.ncl.ac.uk/andreymokhov/old-graphs-from-new-types/

You can find more code in this old experimental repo: https://github.com/snowleopard/graph-algebra.

## Performance

Some quick benchmarks with [Criterion](http://www.serpentine.com/criterion/tutorial.html),
I'll keep adding more graph instances, with some high-performance options too.

### 2D mesh

2D meshes are constructed as ``path [1..n] `box` path [1..n]``.

<table>
  <tr>
    <th>Benchmark</th>
    <th>|V|</th>
    <th>|E|</th>
    <th>Runtime</th>
  </tr>
  <tr align="center">
    <td rowspan="4">Compute the size of <strong>VertexSet</strong></td>
    <td>1</td>
    <td>0</td>
    <td>318 ns</td>
  </tr>
  <tr align="center">
    <td>100</td>
    <td>180</td>
    <td>154 μs</td>
  </tr>
  <tr align="center">
    <td>10 000</td>
    <td>19 800</td>
    <td>31.8 ms</td>
  </tr>
  <tr align="center">
    <td>1 000 000</td>
    <td>1 998 000</td>
    <td>7.33 s</td>
  </tr>  
  <tr align="center">
    <td rowspan="4">Compute the size of <strong>Relation</strong></td>
    <td>1</td>
    <td>0</td>
    <td>250 ns</td>
  </tr>
  <tr align="center">
    <td>100</td>
    <td>180</td>
    <td>152 μs</td>
  </tr>
  <tr align="center">
    <td>10 000</td>
    <td>19 800</td>
    <td>35.7 ms</td>
  </tr>
  <tr align="center">
    <td>1 000 000</td>
    <td>1 998 000</td>
    <td>8.79 s</td>
  </tr>  
  <tr align="center">
    <td rowspan="4">Compute the number of edges in <strong>AdjacencyMap</strong></td>
    <td>1</td>
    <td>0</td>
    <td>334 ns</td>
  </tr>
  <tr align="center">
    <td>100</td>
    <td>180</td>
    <td>223 μs</td>
  </tr>
  <tr align="center">
    <td>10 000</td>
    <td>19 800</td>
    <td>49.5 ms</td>
  </tr>
  <tr align="center">
    <td>1 000 000</td>
    <td>1 998 000</td>
    <td>10.5 s</td>
  </tr>
  <tr align="center">
    <td rowspan="4">Compute the size of <strong>Int.VertexSet</strong></td>
    <td>1</td>
    <td>0</td>
    <td>315 ns</td>
  </tr>
  <tr align="center">
    <td>100</td>
    <td>180</td>
    <td>52 μs</td>
  </tr>
  <tr align="center">
    <td>10 000</td>
    <td>19 800</td>
    <td>7.4 ms</td>
  </tr>
  <tr align="center">
    <td>1 000 000</td>
    <td>1 998 000</td>
    <td>1.54 s</td>
  </tr>
  <tr align="center">
    <td rowspan="4">Compute the number of edges in <strong>Int.AdjacencyMap</strong></td>
    <td>1</td>
    <td>0</td>
    <td>303 ns</td>
  </tr>
  <tr align="center">
    <td>100</td>
    <td>180</td>
    <td>98.9 μs</td>
  </tr>
  <tr align="center">
    <td>10 000</td>
    <td>19 800</td>
    <td>18 ms</td>
  </tr>
  <tr align="center">
    <td>1 000 000</td>
    <td>1 998 000</td>
    <td>4.24 s</td>
  </tr>
</table>

### Clique

A clique is simply ``clique [1..n]``. We can handle cliques with billions of edges thanks to the
simple connectivity structure exploited by `AdjacencyMap`. The `VertexSet` instance ignores edges
altogether and eats cliques for breakfast.

<table>
  <tr>
    <th>Benchmark</th>
    <th>|V|</th>
    <th>|E|</th>
    <th>Runtime</th>
  </tr>
  <tr align="center">
    <td rowspan="6">Compute the size of <strong>Int.VertexSet</strong></td>
    <td>1</td>
    <td>1</td>
    <td>43.2 ns</td>
  </tr>
  <tr align="center">
    <td>10</td>
    <td>45</td>
    <td>375 ns</td>
  </tr>
  <tr align="center">
    <td>100</td>
    <td>4 950</td>
    <td>3.67 μs</td>
  </tr>
  <tr align="center">
    <td>1 000</td>
    <td>499 500</td>
    <td>55.3 μs</td>
  </tr>
  <tr align="center">
    <td>10 000</td>
    <td>49 995 000</td>
    <td>2.08 ms</td>
  </tr>
  <tr align="center">
    <td>44 722</td>
    <td>1 000 006 281</td>
    <td>13.8 ms</td>
  </tr> 
  <tr align="center">
    <td rowspan="5">Compute the size of <strong>Relation</strong></td>
    <td>1</td>
    <td>1</td>
    <td>38.9 ns</td>
  </tr>
  <tr align="center">
    <td>10</td>
    <td>45</td>
    <td>7.74 μs</td>
  </tr>
  <tr align="center">
    <td>100</td>
    <td>4 950</td>
    <td>969 μs</td>
  </tr>
  <tr align="center">
    <td>1 000</td>
    <td>499 500</td>
    <td>93.8 ms</td>
  </tr>
  <tr align="center">
    <td>10 000</td>
    <td>49 995 000</td>
    <td>11.2 s</td>
  </tr>
  <tr align="center">
    <td rowspan="6">Compute the number of edges in <strong>Int.AdjacencyMap</strong></td>
    <td>1</td>
    <td>1</td>
    <td>45.4 ns</td>
  </tr>
  <tr align="center">
    <td>10</td>
    <td>45</td>
    <td>1.72 μs</td>
  </tr>
  <tr align="center">
    <td>100</td>
    <td>4 950</td>
    <td>49.3 μs</td>
  </tr>
  <tr align="center">
    <td>1 000</td>
    <td>499 500</td>
    <td>3.68 ms</td>
  </tr>
  <tr align="center">
    <td>10 000</td>
    <td>49 995 000</td>
    <td>376 ms</td>
  </tr>
  <tr align="center">
    <td>44 722</td>
    <td>1 000 006 281</td>
    <td>10.2 s</td>
  </tr> 
</table>

### De Bruijn graphs

De Bruijn graphs are constructed as `deBruijn n "0123456789"`
and as `gmap fastRead $ deBruijn n "0123456789"` for `Int` instances,
where `fastRead = foldr (\c r -> r + ord c - ord '0') 0`. Note that
`gmap fastRead` takes roughly 5% of `Int.AdjacencyMap` runtime and
15% of `Int.VertexMap` runtime.

<table>
  <tr>
    <th>Benchmark</th>
    <th>|V|</th>
    <th>|E|</th>
    <th>Runtime</th>
  </tr>
  <tr align="center">
    <td rowspan="6">Compute the size of <strong>VertexSet</strong></td>
    <td>10</td>
    <td>100</td>
    <td>4.86 μs</td>
  </tr>
  <tr align="center">
    <td>100</td>
    <td>1 000</td>
    <td>104 μs</td>
  </tr>
  <tr align="center">
    <td>1 000</td>
    <td>10 000</td>
    <td>2.09 ms</td>
  </tr>
  <tr align="center">
    <td>10 000</td>
    <td>100 000</td>
    <td>34.8 ms</td>
  </tr>
  <tr align="center">
    <td>100 000</td>
    <td>1 000 000</td>
    <td>430 ms</td>
  </tr>
  <tr align="center">
    <td>1 000 000</td>
    <td>10 000 000</td>
    <td>6.35 s</td>
  </tr>
  <tr align="center">
    <td rowspan="6">Compute the size of <strong>Relation</strong></td>
    <td>10</td>
    <td>100</td>
    <td>7.13 μs</td>
  </tr>
  <tr align="center">
    <td>100</td>
    <td>1 000</td>
    <td>282 μs</td>
  </tr>
  <tr align="center">
    <td>1 000</td>
    <td>10 000</td>
    <td>6.05 ms</td>
  </tr>
  <tr align="center">
    <td>10 000</td>
    <td>100 000</td>
    <td>83.1 ms</td>
  </tr>
  <tr align="center">
    <td>100 000</td>
    <td>1 000 000</td>
    <td>960 ms</td>
  </tr>
  <tr align="center">
    <td>1 000 000</td>
    <td>10 000 000</td>
    <td>11.4 s</td>
  </tr>  
  <tr align="center">
    <td rowspan="6">Compute the number of edges in <strong>AdjacencyMap</strong></td>
    <td>10</td>
    <td>100</td>
    <td>7.83 μs</td>
  </tr>
  <tr align="center">
    <td>100</td>
    <td>1 000</td>
    <td>159 μs</td>
  </tr>
  <tr align="center">
    <td>1 000</td>
    <td>10 000</td>
    <td>3.04 ms</td>
  </tr>
  <tr align="center">
    <td>10 000</td>
    <td>100 000</td>
    <td>47.5 ms</td>
  </tr>
  <tr align="center">
    <td>100 000</td>
    <td>1 000 000</td>
    <td>644 ms</td>
  </tr>
  <tr align="center">
    <td>1 000 000</td>
    <td>10 000 000</td>
    <td>8.13 s</td>
  </tr>
  <tr align="center">
    <td rowspan="6">Compute the size of <strong>Int.VertexSet</strong></td>
    <td>10</td>
    <td>100</td>
    <td>5.13 μs</td>
  </tr>
  <tr align="center">
    <td>100</td>
    <td>1 000</td>
    <td>61.8 μs</td>
  </tr>
  <tr align="center">
    <td>1 000</td>
    <td>10 000</td>
    <td>882 μs</td>
  </tr>
  <tr align="center">
    <td>10 000</td>
    <td>100 000</td>
    <td>8.93 ms</td>
  </tr>
  <tr align="center">
    <td>100 000</td>
    <td>1 000 000</td>
    <td>105 ms</td>
  </tr>
  <tr align="center">
    <td>1 000 000</td>
    <td>10 000 000</td>
    <td>1.10 s</td>
  </tr>
  <tr align="center">
    <td rowspan="6">Compute the number of edges in <strong>Int.AdjacencyMap</strong></td>
    <td>10</td>
    <td>100</td>
    <td>8.59 μs</td>
  </tr>
  <tr align="center">
    <td>100</td>
    <td>1 000</td>
    <td>175 μs</td>
  </tr>
  <tr align="center">
    <td>1 000</td>
    <td>10 000</td>
    <td>2.76 ms</td>
  </tr>
  <tr align="center">
    <td>10 000</td>
    <td>100 000</td>
    <td>30.4 ms</td>
  </tr>
  <tr align="center">
    <td>100 000</td>
    <td>1 000 000</td>
    <td>368 ms</td>
  </tr>
  <tr align="center">
    <td>1 000 000</td>
    <td>10 000 000</td>
    <td>3.77 s</td>
  </tr>
</table>
