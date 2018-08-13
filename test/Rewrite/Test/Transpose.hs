{-
Graph.transpose/Empty
Graph.transpose/Vertex
Graph.transpose/Overlay
Graph.transpose/Connect
Graph.transpose/overlays
Graph.transpose/connects
NonEmpty.transpose/Vertex
NonEmpty.transpose/Overlay
NonEmpty.transpose/Connect
NonEmpty.transpose/overlays1
NonEmpty.transpose/connects1
-}

import qualified Algebra.Graph as G
import qualified Algebra.Graph.NonEmpty as NG
import Data.List.NonEmpty (NonEmpty (..))

main :: IO ()
main = do
    print $ G.transpose (G.empty :: G.Graph Int)
    print $ G.transpose (G.vertex 1)
    print $ G.transpose (G.overlay (G.vertex 1) (G.vertex 2))
    print $ G.transpose (G.edge 1 2)
    print $ G.transpose $ G.overlays $ map G.vertex [1..10]
    print $ G.transpose $ G.connects $ map G.vertex [1..10]

    print $ NG.transpose (NG.vertex 1)
    print $ NG.transpose (NG.overlay (NG.vertex 1) (NG.vertex 2))
    print $ NG.transpose (NG.edge 1 2)
    print $ NG.transpose $ NG.overlays1 $ fmap NG.vertex $ 1 :| [2..10]
    print $ NG.transpose $ NG.connects1 $ fmap NG.vertex $ 1 :| [2..10]

