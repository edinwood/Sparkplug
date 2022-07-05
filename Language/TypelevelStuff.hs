{-#Language 
 ConstraintKinds,
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
 TypeFamilyDependencies,
 PolyKinds,
 TypeFamilies
 #-}
module Language.TypelevelStuff where

import GHC.Types
import GHC.TypeLits
import Data.Proxy

----
-- Typelevel Helpers

type family (<) (x :: Nat) (y :: Nat) :: Bool where
 n < 0 = False
 0 < n = True
 n < m = (n-1) < (m-1)

type family (>) (x :: Nat) (y :: Nat) :: Bool where
 0 > n = False
 n > 0 = True
 n > m = (n-1) > (m-1)

type family (&&) (x :: Bool) (y :: Bool) :: Bool where
 (&&) 'True 'True = 'True
 (&&) _ _ = False
 
type family If (b :: Bool) (x :: k) (y :: k) where
 If True x y = x
 If False x y = y

type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
 (++) '[] xs = xs
 (++) ys '[] = ys
 (++) (y ':ys) xs = y ': (ys ++ xs)

type family Nub (xs :: [k]) (ys :: [k]) = (zs :: [k]) where
 Nub xs '[] = xs
 Nub (x ': xs) ys = If (Elem x ys) (x ': Nub xs ys) (Nub xs ys)
 Nub '[] _ = '[]

type family Without (xs :: [k]) (ys :: [k]) = (zs :: [k]) where
 Without xs '[] = xs
 Without (x ': xs) ys = If (Elem x ys) (Without xs ys) (x ': Without xs ys) 
 Without '[] _ = '[]

----
-- List stuff

type family Head (xs :: [k]) :: k where
 Head (x ': xs) = x

type family Concat (xs :: [[k]]) :: [k] where
 Concat '[] = '[]
 Concat (x ': xs) = x ++ Concat xs

type family Take (n :: Nat) (xs :: [k]) :: [k] where
 Take 0 xs = '[] 
 Take n (x ':xs) = x ': Take (n-1) xs

type family Drop (n :: Nat) (xs :: [k]) :: [k] where
 Drop 0 xs = xs 
 Drop n (x ':xs) = Drop (n-1) xs

type family Length (xs :: [k]) :: Nat where
 Length '[] = 0
 Length (x:xs) = 1+(Length xs)

type family Elem (x :: k) (xs :: [k]) :: Bool where
 Elem _ '[] = False
 Elem x (x ': xs) = True
 Elem x (y ': xs) = Elem x xs

type family Contains (xs :: [k]) (ys :: [k]) :: Bool where
 Contains xs '[] = 'True
 Contains xs (y ': ys) = (Elem y xs) && Contains xs ys
