-----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Graph.Relation.Reflexive
-- Copyright   :  (c) Andrey Mokhov 2016-2017
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- An abstract implementation of binary reflexive relations.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Relation.Reflexive (
    -- * Reflexive relations
    ReflexiveRelation, domain, relation,

    -- * Operations on reflexive relations
    preset, postset, symmetricClosure, transitiveClosure
  ) where

import Data.Set (Set)

import qualified Algebra.Graph.Relation.Internal as R
import Algebra.Graph.Relation.Internal (ReflexiveRelation (..), reflexiveClosure)

-- | The /domain/ of the relation.
domain :: ReflexiveRelation a -> Set a
domain = R.domain . fromReflexive

-- | The set of pairs of elements that are /related/. It is guaranteed that each
-- element belongs to the domain and is related to itself.
relation :: ReflexiveRelation a -> Set (a, a)
relation = R.relation . fromReflexive

-- | The /preset/ of an element @x@ is the set of elements that are related to
-- it on the /left/, i.e. @preset x == { a | aRx }@. In the context of directed
-- graphs, this corresponds to the set of /direct predecessors/ of vertex @x@.
--
-- @
-- preset x 'Algebra.Graph.empty'      == Set.empty
-- preset x ('Algebra.Graph.vertex' x) == Set.fromList [x]
-- preset x ('Algebra.Graph.edge' x y) == Set.fromList [x]
-- preset y ('Algebra.Graph.edge' x y) == Set.fromList [x, y]
-- @
preset :: Ord a => a -> ReflexiveRelation a -> Set a
preset x = R.preset x . reflexiveClosure . fromReflexive

-- | The /postset/ of an element @x@ is the set of elements that are related to
-- it on the /right/, i.e. @postset x == { a | xRa }@. In the context of directed
-- graphs, this corresponds to the set of /direct successors/ of vertex @x@.
--
-- @
-- postset x 'Algebra.Graph.empty'      == Set.empty
-- postset x ('Algebra.Graph.vertex' x) == Set.fromList [x]
-- postset x ('Algebra.Graph.edge' x y) == Set.fromList [x, y]
-- postset y ('Algebra.Graph.edge' x y) == Set.fromList [y]
-- @
postset :: Ord a => a -> ReflexiveRelation a -> Set a
postset x = R.postset x . reflexiveClosure . fromReflexive

-- | Compute the /symmetric closure/ of a 'ReflexiveRelation'.
--
-- @
-- symmetricClosure 'Algebra.Graph.empty'      == 'Algebra.Graph.empty'
-- symmetricClosure ('Algebra.Graph.vertex' x) == 'Algebra.Graph.vertex' x
-- symmetricClosure ('Algebra.Graph.edge' x y) == 'Algebra.Graph.edges' [(x, y), (y, x)]
-- @
symmetricClosure :: Ord a => ReflexiveRelation a -> ReflexiveRelation a
symmetricClosure = ReflexiveRelation . R.symmetricClosure . fromReflexive

-- | Compute the /transitive closure/ of a 'ReflexiveRelation'.
--
-- @
-- transitiveClosure 'Algebra.Graph.empty'      == 'Algebra.Graph.empty'
-- transitiveClosure ('Algebra.Graph.vertex' x) == 'Algebra.Graph.vertex' x
-- transitiveClosure ('Algebra.Graph.path' xs)  == 'Algebra.Graph.clique' xs
-- @
transitiveClosure :: Ord a => ReflexiveRelation a -> ReflexiveRelation a
transitiveClosure = ReflexiveRelation . R.transitiveClosure . fromReflexive
