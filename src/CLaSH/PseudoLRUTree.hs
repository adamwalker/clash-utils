{-# LANGUAGE UndecidableInstances #-}
module CLaSH.PseudoLRUTree where

import CLaSH.Prelude
import Data.Singletons.Prelude
import Data.Proxy

--
--Combined get and update oldest
--

updateOldestNWay' 
    :: forall n m. (n ~ (m + 1), KnownNat n) => 
       (Vec ((2 ^ n) - 1) Bool -> (Vec n Bool, Vec ((2 ^ n) - 1) Bool)) 
    -> Vec ((2 ^ (n + 1)) - 1) Bool 
    -> (Vec (n + 1) Bool, Vec ((2 ^ (n + 1)) - 1) Bool)
updateOldestNWay' recurse (node :> tree) = (node :> oldestIdx', updatedTree)
    where
    branches :: Vec 2 (Vec ((2 ^ n) - 1) Bool) = unconcat (subSNat (powSNat (SNat @ 2) (SNat @ n)) (SNat @ 1)) tree
    (oldestIdx', updatedSubTree)               = recurse (branches !! node)
    updatedTree                                = not node :> concat (replace node updatedSubTree branches)

--base case corresponds to 2 ways - just a single bit
updateOldestBaseCase :: Vec 1 Bool -> (Vec 1 Bool, Vec 1 Bool)
updateOldestBaseCase x = (x, map not x)

data Step (f :: TyFun Nat *) :: *
type instance Apply Step n = Vec ((2 ^ (n + 1)) - 1) Bool -> (Vec (n + 1) Bool, Vec ((2 ^ (n + 1)) - 1) Bool)

updateOldestNWay :: forall m n. (n ~ (m + 1), KnownNat m) => Vec ((2 ^ n) - 1) Bool -> (Vec n Bool, Vec ((2 ^ n) - 1) Bool)
updateOldestNWay = dfold (Proxy :: Proxy Step) func updateOldestBaseCase (replicate (SNat @ m) ())
    where
    func 
        :: forall n1. SNat n1
        -> ()
        -> (Step @@ n1)
        -> (Step @@ (n1 + 1))
    func SNat () = updateOldestNWay'

--
--Get oldest
--

getOldestNWay' 
    :: forall n m. (n ~ (m + 1), KnownNat n) => 
       (Vec ((2 ^ n) - 1) Bool -> Vec n Bool)
    -> Vec ((2 ^ (n + 1)) - 1) Bool 
    -> Vec (n + 1) Bool
getOldestNWay' recurse (node :> tree) = node :> oldestIdx'
    where
    branches :: Vec 2 (Vec ((2 ^ n) - 1) Bool) = unconcat (subSNat (powSNat (SNat @ 2) (SNat @ n)) (SNat @ 1)) tree
    oldestIdx'                                 = recurse (branches !! node)

data Step2 (f :: TyFun Nat *) :: *
type instance Apply Step2 n = Vec ((2 ^ (n + 1)) - 1) Bool -> Vec (n + 1) Bool

getOldestNWay :: forall m n. (n ~ (m + 1), KnownNat m) => Vec ((2 ^ n) - 1) Bool -> Vec n Bool
getOldestNWay = dfold (Proxy :: Proxy Step2) func id (replicate (SNat @ m) ())
    where
    func 
        :: forall n1. SNat n1
        -> ()
        -> (Step2 @@ n1)
        -> (Step2 @@ (n1 + 1))
    func SNat () = getOldestNWay'

--
--Update
--

updateNWay' 
    :: forall n m. (n ~ (m + 1), KnownNat n) => 
       (Vec n Bool -> Vec ((2 ^ n) - 1) Bool -> Vec ((2 ^ n) - 1) Bool) 
    -> Vec (n + 1) Bool
    -> Vec ((2 ^ (n + 1)) - 1) Bool 
    -> Vec ((2 ^ (n + 1)) - 1) Bool
updateNWay' recurse (this :> rest) (node :> tree) = updatedTree
    where
    branches :: Vec 2 (Vec ((2 ^ n) - 1) Bool) = unconcat (subSNat (powSNat (SNat @ 2) (SNat @ n)) (SNat @ 1)) tree
    updatedSubTree                             = recurse rest (branches !! node)
    updatedTree                                = not this :> concat (replace this updatedSubTree branches)

data Step3 (f :: TyFun Nat *) :: *
type instance Apply Step3 n = Vec (n + 1) Bool -> Vec ((2 ^ (n + 1)) - 1) Bool -> Vec ((2 ^ (n + 1)) - 1) Bool

updateNWay :: forall m n. (n ~ (m + 1), KnownNat m) => Vec n Bool -> Vec ((2 ^ n) - 1) Bool -> Vec ((2 ^ n) - 1) Bool
updateNWay = dfold (Proxy :: Proxy Step3) func (\idx tree -> map not idx) (replicate (SNat @ m) ())
    where
    func 
        :: forall n1. SNat n1
        -> ()
        -> (Step3 @@ n1)
        -> (Step3 @@ (n1 + 1))
    func SNat () = updateNWay'

