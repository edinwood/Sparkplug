{-#Language 
 FlexibleContexts,
 RankNTypes,
 FunctionalDependencies,
 UndecidableInstances, 
 FlexibleInstances,
 MultiParamTypeClasses,
 PolyKinds, 
 GADTs, 
 KindSignatures,
 TypeOperators,
 TypeApplications,
 AllowAmbiguousTypes,
 ScopedTypeVariables,
 DataKinds,
 TypeFamilies
 #-}
module Language.Disentangle (Disentangle (..)) where

import GHC.Types
import GHC.TypeLits
import Data.Proxy

import Language.TypelevelStuff 
import Language.Scope (Variable(..),Vars,Scope(..),BindMany,bindMany,RestrictScope,restrictScope) 
import Language.Vec (Vec(..),VMap(..),ZipWithVec(..),V)
import Language.N
import Language.MISO
import Language.MIMO

{-
A vec of `n' mimo's from `i' inputs to `o' outputs
decouples into a vec of length `o', of a vec of `n' miso's taking `i' inputs
-}

class Disentangle n na nb where
 disentangle :: Vec n (MIMO np p na a nb b) -> Vec nb (Vec n (MISO np p na a b))

instance VMap o => Disentangle (S Z) i o where
 disentangle (VecOne (MIMO misos)) = vmap VecOne misos  

instance (V o,Disentangle ('S n) i o) => Disentangle (S (S n)) i o where
 disentangle (VecN (MIMO misos) xs) = let ys = disentangle xs in zipWithVec ($) (vmap VecN misos) ys
  
