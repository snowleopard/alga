{-# LANGUAGE TypeFamilies #-}
module Algebra.Graph.Util (transpose) where

import Algebra.Graph

-- Note: Transpose can only transpose polymorphic graphs.
newtype Transpose a = Transpose { transpose :: a }

instance Graph g => Graph (Transpose g) where
    type Vertex (Transpose g) = Vertex g
    empty       = Transpose empty
    vertex      = Transpose . vertex
    overlay x y = Transpose $ overlay (transpose x) (transpose y)
    connect x y = Transpose $ connect (transpose y) (transpose x)

instance (Num g, Graph g) => Num (Transpose g) where
    fromInteger = Transpose . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id
