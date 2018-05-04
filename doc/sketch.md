This documents intends to give an overview of the time complexity of the different graph transformations implemented by the various data types existent in Alga.

When specifying the time and memory complexity of graph algorithms, _n_ will denote the number of vertices in the graph, _m_ will denote the number of edges in the graph, and _s_ will denote the _size_ of the corresponding `Graph` expression.

**`removeVertex`**: Removes a vertex from a given graph. In `NonEmpty` graphs, returns `Nothing` if the resulting graph is empty.

| removeVertex       | Time          | Memory |
|--------------------|---------------|--------|
| AdjacencyMap       | `O(n * log(n))` | `   ` |
| Fold               | `O(s)` | `O(s)` |
| HigherKinded.Class | `O(s)` | `O(s)` |
| IntAdjacencyMap    | `O(log(n))` | `O(s)` |
| NonEmpty           | `O(s)` | `   ` |
| Relation           | `O(n+m)`| `   ` |

**`removeEdge`**: Removes an edge from a given graph.

|    removeEdge      | Time      | Memory |
|--------------------|-----------|--------|
| AdjacencyMap       | `O(log(n))` | `   ` |
| Fold               | `O(s)` | `O(s)` |
| IntAdjacencyMap    | `O(log(n))` | `   ` |
| NonEmpty           | `O(s)` | `O(s)` |
| Relation           | `O(log(m))` | `   ` |

**`replaceVertex`**: Replaces a vertex `x` with a vertex `y` in a given `AdjacencyMap`. If `y` already exists, `x` and `y` will be merged.

| replaceVertex      | Time              | Memory |
|--------------------|-------------------|--------|
| AdjacencyMap       | `O((n+m)*log(n))` | `   ` |
| Fold               | `O(s)` | `O(s)` |
| HigherKinded.Class | `O(s)` | `O(s)` |
| IntAdjacencyMap    | `O((n+m)*log(n))` | `   ` |
| NonEmpty           | `O(s)` | `O(s)` |
| Relation           | `O((n+m)*log(n))` | `   ` |

**`mergeVertices`**: Merge vertices sastisfying a given predicate into a given vertex.

| mergeVertices      | Time              | Memory |
|--------------------|-------------------|--------|
| AdjacencyMap       | `O((n+m) * log(n))` assuming predicate computes in `O(1)` | `   ` |
| Fold               | `O(s)` assuming predicate computes in `O(1)` | `O(s)` assuming predicate computes in `O(1)` |
| HigherKinded.Class | `O(s)` assuming predicate computes in `O(1)` | `O(s)` assuming predicate computes in `O(1)` |
| IntAdjacencyMap    | `O((n+m)*log(n))` assuming predicate computes in `O(1)` | `   ` |
| NonEmpty           | `O(s)` assuming predicate computes in `O(1)` | `O(s)` assuming predicate computes in `O(1)` |
| Relation           | `O((n+m)*log(n))` assuming predicate computes in `O(1)` | `   ` |


**`transpose`**: Transpose a given graph.

| transpose      | Time              | Memory |
|--------------------|-------------------|--------|
| AdjacencyMap       | `O(m*log(n))` | `O(n+m)` |
| Fold               | `O(s)` | `O(s)` |
| IntAdjacencyMap    | `O(m*log(n))` | `O(n+m)` |
| NonEmpty           | `O(s)` | `O(s)` |
| Relation           | `O(m*log(m))` | `   ` |

**`induce`**: Constructs the induced subgraph of a given graph by removing the vertices that do not satisfy a given predicate. For `NonEmpty` graphs, it returns `Nothing` if the resulting graph is empty.

| induce      | Time              | Memory |
|--------------------|-------------------|--------|
| AdjacencyMap       | `O(m)` assuming predicate computes in `O(1)` | `   ` |
| Fold               | `O(s)` assuming predicate computes in `O(1)` | `O(s)` assuming predicate computes in `O(1)` |
| HigherKinded.Class | `O(s)` assuming predicate computes in `O(1)` | `O(s)` assuming predicate computes in `O(1)` |
| IntAdjacencyMap    | `O(m)` assuming predicate computes in `O(1)` | `   ` |
| NonEmpty           | `O(s)` assuming predicate computes in `O(1)` | `O(s)` assuming predicate computes in `O(1)` |
| Relation           | `O(m)` assuming predicate computes in `O(1)` | `   ` |

**`splitVertex`**: Split a vertex into a list of vertices with the same connectivity. For `NonEmpty` graphs, it returns `Nothing` if the resulting graph is empty.

| splitVertex      | Time              | Memory |
|--------------------|-------------------|--------|
| Fold               | `O(s+k*L)` where `k` is the number of occurrences of the vertex in the expression and `L` the length of the given list | `   ` |
| HigherKinded.Class | `O(s+k*L)` where `k` is the number of occurrences of the vertex in the expression and `L` the length of the given list | `   ` |
| NonEmpty           | `O(s+k*L)` where `k` is the number of occurrences of the vertex in the expression and `L` the length of the given list | `   ` |

**`simplify`**: Simplifies a graph expression in a reasonable time.

| simplify      | Time              | Memory |
|--------------------|-------------------|--------|
| Fold               | `O(s)` | `O(s)` |
| NonEmpty           | `O(s)` | `O(s)` |