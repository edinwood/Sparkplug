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
module Language.MIMO (MIMO(..),runMIMO,detatch) where

import GHC.Types
import GHC.TypeLits
import Data.Proxy

import Language.TypelevelStuff 
import Language.Scope (Variable(..),Vars,Scope(..),BindMany,bindMany,RestrictScope,restrictScope) 
import Language.Vec (Vec(..),VMap(..),V)
import Language.N
import Language.MISO

data MIMO (np :: N) p (na :: N) a (nb :: N) b
   = MIMO (Vec nb (MISO np p na a b))

runMIMO :: V nb => MIMO np p na a nb b-> (Vec na a -> Vec nb b)
runMIMO (MIMO fs) s = vmap (\f -> runMISO f s) fs

-- note: there is no inverse, construct using Vecs constructor, it does not have an uncons... so this complets a kind of square
detatch :: MIMO np p na a (S nb) b -> (MISO np p na a b,MIMO np p na a nb b)
detatch (MIMO (VecN x xs)) = (x,MIMO xs)