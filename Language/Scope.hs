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
module Language.Scope (Variable(..),Vars,Scope(..),BindMany,bindMany,RestrictScope,restrictScope) where

import Language.TypelevelStuff

import GHC.Types
import GHC.TypeLits
import Data.Proxy

----
-- test

type Scope1 = '[ "hello" , "scope" , "restriction" , "world"]
type Scope2 = '[ "scope" , "restriction" ]

scope1 :: Scope Scope1
scope1 = Bind (Variable 1)
        (Bind (Variable 2) 
        (Bind (Variable 3) 
        (Bind (Variable 4) 
        Clean
        )))

scope2 :: Scope Scope2
scope2 = restrictScope (Proxy @ Scope2) scope1 
scope3 = restrictScope (Proxy @ (Without Scope1 Scope2)) scope1 

test = do
 print scope1
 print scope2
 print scope3

{-
*Language.Scope> test
<hello         : 1.0>
<scope         : 2.0>
<restriction   : 3.0>
<world         : 4.0>

<scope         : 2.0>
<restriction   : 3.0>

<hello         : 1.0>
<world         : 4.0>

*Language.Scope>
-}

----
--

-- Variable
data Variable (name :: Symbol) = Variable Double

-- Vars
type Vars = [Symbol]

-- Scope
data Scope (xs :: Vars) where
 Clean :: Scope '[]
 Bind :: False ~ Elem x xs => Variable x -> Scope xs -> Scope (x:xs)

-- Show Scope
instance Show (Scope '[]) where
 show Clean = ""

instance (KnownSymbol x,Show (Scope xs)) => Show (Scope (x ': xs)) where
 show (Bind (Variable x) xs) = "<" ++ a ++ pad a ++ ": " ++ show x ++ ">\n" ++ show xs
  where
   a = symbolVal (Proxy @ x) 
   pad a = drop (length a) "              "

-- BindMany/bindMany
class BindMany (xs :: Vars) (ys :: Vars) where
 bindMany :: Scope xs -> Scope ys -> Scope (xs ++ ys)
 
instance BindMany '[] ys where
 bindMany _ ys = ys

instance (BindMany xs ys,((x : xs) ++ ys) ~ (x : (xs ++ ys)),Elem x (xs ++ ys) ~ 'False) => BindMany (x ': xs) ys where
 bindMany (Bind x xs) ys = Bind x $ bindMany xs ys

----
-- Scope Restriction

-- RestrictScope/restricScope
class RestrictScope 
 (scopeInputs :: Vars) 
 (scopeRestriction :: Vars) 
 where
 restrictScope :: Proxy scopeRestriction -> Scope scopeInputs -> Scope (Nub scopeInputs scopeRestriction)

instance RestrictScope '[] scopeRestriction where
 restrictScope _ _ = Clean

-- luckily each variable can only appear once by the safe constructor
instance (RestrictScope' (Elem x scopeRestriction) (x ': xs) scopeRestriction,RestrictScope xs scopeRestriction) => RestrictScope (x ': xs) scopeRestriction where
 restrictScope p xs = restrictScope' p (Proxy @ (Elem x scopeRestriction)) xs   -- (Proxy @ b) x xs $ undefined -- restrictScope xs p

class RestrictScope' b xs scopeRestriction where
 restrictScope' :: Proxy scopeRestriction -> Proxy b -> Scope xs -> Scope (Nub xs scopeRestriction)

type C x xs scopeRestriction = 
 (Nub (x : xs) scopeRestriction ~ (x : Nub xs scopeRestriction)
 ,Elem x (Nub xs scopeRestriction) ~ 'False
 ,RestrictScope xs scopeRestriction
 )

instance C x xs scopeRestriction => RestrictScope' 'True (x ': xs) scopeRestriction where
 restrictScope' p _ (Bind x xs) = Bind x (restrictScope p xs)

type D x xs scopeRestriction = 
 (Nub xs scopeRestriction ~ Nub (x : xs) scopeRestriction
 ,RestrictScope xs scopeRestriction
 )

instance D x xs scopeRestriction => RestrictScope' 'False (x ': xs) scopeRestriction where
 restrictScope' p _ (Bind x xs) = restrictScope p xs

