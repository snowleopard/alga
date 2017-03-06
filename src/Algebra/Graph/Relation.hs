-----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Graph.Relation
-- Copyright   :  (c) Andrey Mokhov 2016-2017
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- An abstract implementation of binary relations.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Relation (
    -- * Binary relation
    Relation, domain, relation,

    -- * Operations on binary relations
    preset, postset, reflexiveClosure, symmetricClosure, transitiveClosure,
    preorderClosure
  ) where

import Algebra.Graph.Relation.Internal
