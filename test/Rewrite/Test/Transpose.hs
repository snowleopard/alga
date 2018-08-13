{-
transpose/Empty
transpose/Vertex
transpose/Overlay
transpose/Connect
transpose/overlays
transpose/connects
-}

import Algebra.Graph

main :: IO ()
main = do
    print $ transpose (empty :: Graph Int)
    print $ transpose (vertex 1)
    print $ transpose (overlay (vertex 1) (vertex 2))
    print $ transpose (edge 1 2)
    print $ transpose $ overlays $ map vertex [1..10]
    print $ transpose $ connects $ map vertex [1..10]
