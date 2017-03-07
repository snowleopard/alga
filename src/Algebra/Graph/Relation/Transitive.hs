-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Relation.Transitive
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- An abstract implementation of binary transitive relations.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Relation.Transitive (
    -- * Transitive relations
    TransitiveRelation, domain, relation,

    -- * Operations on transitive relations
    preset, postset, reflexiveClosure, symmetricClosure
  ) where

import Data.Set (Set)

import qualified Algebra.Graph.Relation.Internal as R
import Algebra.Graph.Relation.Internal (TransitiveRelation (..), transitiveClosure)

-- | The /domain/ of the relation.
domain :: TransitiveRelation a -> Set a
domain = R.domain . fromTransitive

-- | The set of pairs of elements that are /related/. It is guaranteed that each
-- element belongs to the domain, and that if @xRy@ and @yRz@ hold then so does @xRz@.
relation :: Ord a => TransitiveRelation a -> Set (a, a)
relation = R.relation . transitiveClosure . fromTransitive

-- | The /preset/ of an element @x@ is the set of elements that are related to
-- it on the /left/, i.e. @preset x == { a | aRx }@. In the context of directed
-- transitive graphs, this corresponds to the set of /transitive predecessors/
-- of vertex @x@.
--
-- @
-- preset x 'Algebra.Graph.empty'            == Set.empty
-- preset x ('Algebra.Graph.vertex' x)       == Set.empty
-- preset x ('Algebra.Graph.edge' x y)       == Set.empty
-- preset y ('Algebra.Graph.edge' x y)       == Set.fromList [x]
-- preset z ('Algebra.Graph.path' [x, y, z]) == Set.fromList [x, y]
-- @
preset :: Ord a => a -> TransitiveRelation a -> Set a
preset x = R.preset x . transitiveClosure . fromTransitive

-- | The /postset/ of an element @x@ is the set of elements that are related to
-- it on the /right/, i.e. @postset x == { a | xRa }@. In the context of directed
-- transitive graphs, this corresponds to the set of /transitive successors/
-- of vertex @x@.
--
-- @
-- postset x 'Algebra.Graph.empty'            == Set.empty
-- postset x ('Algebra.Graph.vertex' x)       == Set.empty
-- postset x ('Algebra.Graph.edge' x y)       == Set.fromList [y]
-- postset y ('Algebra.Graph.edge' x y)       == Set.empty
-- postset x ('Algebra.Graph.path' [x, y, z]) == Set.fromList [y, z]
-- @
postset :: Ord a => a -> TransitiveRelation a -> Set a
postset x = R.postset x . transitiveClosure . fromTransitive

-- | Compute the /reflexive closure/ of a 'TransitiveRelation'.
--
-- @
-- reflexiveClosure 'Algebra.Graph.empty'      == 'Algebra.Graph.empty'
-- reflexiveClosure ('Algebra.Graph.vertex' x) == 'Algebra.Graph.edge' x x
-- @
reflexiveClosure :: Ord a => TransitiveRelation a -> TransitiveRelation a
reflexiveClosure = TransitiveRelation . R.reflexiveClosure . fromTransitive

-- | Compute the /symmetric closure/ of a 'TransitiveRelation'.
--
-- @
-- symmetricClosure 'Algebra.Graph.empty'      == 'Algebra.Graph.empty'
-- symmetricClosure ('Algebra.Graph.vertex' x) == 'Algebra.Graph.vertex' x
-- symmetricClosure ('Algebra.Graph.edge' x y) == 'Algebra.Graph.edges' [(x, x), (x, y), (y, x), (y, y)]
-- @
symmetricClosure :: Ord a => TransitiveRelation a -> TransitiveRelation a
symmetricClosure = TransitiveRelation . R.symmetricClosure . transitiveClosure . fromTransitive
