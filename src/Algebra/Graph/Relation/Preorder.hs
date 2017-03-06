-----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Graph.Relation.Preorder
-- Copyright   :  (c) Andrey Mokhov 2016-2017
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- An abstract implementation of binary transitive relations.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Relation.Preorder (
    -- * Preorder relations
    PreorderRelation, domain, relation,

    -- * Operations on transitive relations
    preset, postset, symmetricClosure
  ) where

import Data.Set (Set)

import qualified Algebra.Graph.Relation.Internal as R
import Algebra.Graph.Relation.Internal (PreorderRelation (..), preorderClosure)

-- | The /domain/ of the relation.
domain :: PreorderRelation a -> Set a
domain = R.domain . fromPreorder

-- | The set of pairs of elements that are /related/. It is guaranteed that each
-- element belongs to the domain and is related to itself, and that if
-- @xRy@ and @yRz@ hold then so does @xRz@.
relation :: Ord a => PreorderRelation a -> Set (a, a)
relation = R.relation . preorderClosure . fromPreorder

-- | The /preset/ of an element @x@ is the set of elements that are related to
-- it on the /left/, i.e. @preset x == { a | aRx }@. In the context of directed
-- preorder graphs, this corresponds to the set of /preorder predecessors/
-- of vertex @x@.
--
-- @
-- preset x 'Algebra.Graph.empty'            == Set.empty
-- preset x ('Algebra.Graph.vertex' x)       == Set.fromList [x]
-- preset x ('Algebra.Graph.edge' x y)       == Set.fromList [x]
-- preset y ('Algebra.Graph.edge' x y)       == Set.fromList [x, y]
-- preset z ('Algebra.Graph.path' [x, y, z]) == Set.fromList [x, y, z]
-- @
preset :: Ord a => a -> PreorderRelation a -> Set a
preset x = R.preset x . preorderClosure . fromPreorder

-- | The /postset/ of an element @x@ is the set of elements that are related to
-- it on the /right/, i.e. @postset x == { a | xRa }@. In the context of directed
-- preorder graphs, this corresponds to the set of /preorder successors/
-- of vertex @x@.
--
-- @
-- postset x 'Algebra.Graph.empty'            == Set.empty
-- postset x ('Algebra.Graph.vertex' x)       == Set.fromList [x]
-- postset x ('Algebra.Graph.edge' x y)       == Set.fromList [x, y]
-- postset y ('Algebra.Graph.edge' x y)       == Set.fromList [y]
-- postset x ('Algebra.Graph.path' [x, y, z]) == Set.fromList [x, y, z]
-- @
postset :: Ord a => a -> PreorderRelation a -> Set a
postset x = R.postset x . preorderClosure . fromPreorder

-- | Compute the /symmetric closure/ of a 'PreorderRelation'.
--
-- @
-- symmetricClosure 'Algebra.Graph.empty'      == 'Algebra.Graph.empty'
-- symmetricClosure ('Algebra.Graph.vertex' x) == 'Algebra.Graph.vertex' x
-- symmetricClosure ('Algebra.Graph.edge' x y) == 'Algebra.Graph.edges' [(x, y), (y, x)]
-- @
symmetricClosure :: Ord a => PreorderRelation a -> PreorderRelation a
symmetricClosure = PreorderRelation . R.symmetricClosure . preorderClosure . fromPreorder
