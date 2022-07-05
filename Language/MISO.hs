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
module Language.MISO (MISO(..),runMISO) where

import GHC.Types
import GHC.TypeLits
import Data.Proxy

import Language.TypelevelStuff 
import Language.Scope (Variable(..),Vars,Scope(..),BindMany,bindMany,RestrictScope,restrictScope) 
import Language.Vec (Vec(..),VMap(..))
import Language.N


-- miso system over `i' inputs, can be extended to aritrary inputs containing these by precomposing it by scope restriction
-- by taking a Vec rather than a hetrogenously typed list, the input arguments are restricted to be the same type. 
data MISO (np :: N) p (na :: N) a b where
 MISO :: Vec np p -> (Vec np p -> Vec na a -> b) -> MISO np p na a b

runMISO :: MISO np p na a b -> (Vec na a -> b)
runMISO (MISO p f) s = f p s



