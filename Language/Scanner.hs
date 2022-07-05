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
module Language.Scanner (Scanner(..),decouple) where

import GHC.Types
import GHC.TypeLits
import Data.Proxy

import Language.TypelevelStuff 
import Language.Scope (Variable(..),Vars,Scope(..),BindMany,bindMany,RestrictScope,restrictScope) 
import Language.Vec (Vec(..),VMap(..),UnzipVec(..),V)
import Language.N
import Language.MISO
import Language.MIMO
import Language.Disentangle

data Scanner' np ps po s i o 
   = Scanner 
   { state :: s
   , transfer :: (Vec np ps,Vec np ps -> (s -> i -> s)) 
   , observer :: (Vec np po,Vec np po -> (s -> i -> o))
   }

type Scanner np p x = Scanner' np p p x x x

fromScanner :: Scanner np p x -> (x,MIMO np p (S (S Z)) x (S (S Z)) x)
fromScanner (Scanner s (tp,t) (op,o)) = (s,MIMO m)
 where
  m = VecN a (VecOne b)
  a = MISO tp (\ps (VecN s (VecOne i)) -> t ps s i)
  b = MISO op (\ps (VecN s (VecOne i)) -> o ps s i)

decouple' :: (V n,Disentangle n ('S ('S 'Z)) ('S ('S 'Z))) => Vec n (Scanner np p x) -> (Vec n x,Vec ('S ('S 'Z)) (Vec n (MISO np p ('S ('S 'Z)) x x)))
decouple' = fmap disentangle . unzipVec . vmap fromScanner

-- a Vec of Scanners is decoupled into the states, and a pair of Vecs of MISO systems, the state transfer and observation opperators
decouple
  :: (V n, Disentangle n ('S ('S 'Z)) ('S ('S 'Z))) 
  => Vec n (Scanner np p x)
  -> Kernal n np p x

data Kernal n np p x = Kernal (Vec n x) (Vec n (MISO np p ('S ('S 'Z)) x x)) (Vec n (MISO np p ('S ('S 'Z)) x x))

decouple xs = let (a,(VecN b (VecOne c))) = decouple' xs in Kernal a b c

-- try this with a list of scanners instead of the crazy graph thing first. 
