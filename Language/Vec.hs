{-#Language 
 FlexibleContexts,
 ConstraintKinds,
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
 TypeFamilies,
 IncoherentInstances
 #-}
module Language.Vec (Vec(..),VMap(..),ZipWithVec(..),UnzipVec(..),V) where

import Language.TypelevelStuff 
import Language.Scope (Variable(..),Vars,Scope(..),BindMany,bindMany,RestrictScope,restrictScope) 
import GHC.Types
import GHC.TypeLits
import Language.N
import Data.Proxy

data Vec (n::N) a where
 VecOne :: a -> Vec (S Z) a
 VecN :: a -> Vec n a -> Vec (S n) a

instance (ShowVec n a,Show a) => Show (Vec n a) where
 show = ("(" ++) . showVec @n

class Show a => ShowVec n a where
 showVec :: Vec n a -> String

instance Show a => ShowVec (S Z) a where
 showVec (VecOne a) = show a ++ ")"

instance (ShowVec n a,Show a) => ShowVec (S n) a where
 showVec (VecN x xs) = show x ++ "," ++ showVec xs

class VMap (n :: N) where
 vmap :: (a -> b) -> Vec n a -> Vec n b

instance VMap (S Z) where
 vmap f (VecOne a) = VecOne $ f a

instance VMap n => VMap (S n) where
 vmap f (VecN x xs) = VecN (f x) $ vmap f xs

type V o = (ZipWithVec o,VMap o,UnzipVec o)


class UnzipVec n where
 unzipVec :: Vec n (a,b) -> (Vec n a,Vec n b)

instance UnzipVec (S Z) where
 unzipVec (VecOne (a,b)) = (VecOne a,VecOne b)

instance UnzipVec (S n) => UnzipVec (S (S n)) where
 unzipVec (VecN (a,b) xs) = let (x,y) = unzipVec xs in (VecN a x,VecN b y)

class ZipWithVec n where
 zipWithVec :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c

instance ZipWithVec (S Z) where
 zipWithVec f (VecOne x)  (VecOne y)  = VecOne (f x y)

instance ZipWithVec (S n) => ZipWithVec (S (S n)) where
 zipWithVec f (VecN x xs) (VecN y ys) = VecN (f x y) $ zipWithVec f xs ys

