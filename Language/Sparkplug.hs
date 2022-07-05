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
module Language.Sparkplug (demo) where

import Language.TypelevelStuff 
import Language.Scope
import GHC.Types
import Data.Proxy

demo :: IO ()
demo = print test1

-------------------

type (inputs :: Vars) +-> (outputs :: Vars) = Scope inputs -> Scope outputs

data Function (i :: Vars) (o :: Vars) = Function (i +-> o)

type Signature = (Vars,Vars) 

-- programs are defined backwards annoyingly
data Program (fs :: [Signature]) (i :: Vars) (inputs :: [Vars]) (outputs :: Vars) where
 Outputs  :: (o' +-> o) -> Program '[] o' '[] o -- the last layer just applies the scope restricting function provided. use restrictScope via newProgram
 Define  :: (True ~ ((Concat (i ': inputs)) `Contains` b)) => (a +-> b) -> Program fs i inputs o -> Program ( '(a,b) : fs) a (i : inputs) o

{-
deconstructProgram 
 :: forall a b fs inputs outputs i1 i2 l
  . (i1 ~ Take l inputs,i2 ~ Drop l inputs,l~Length a) 
 => Program ( '(a,b):fs) inputs outputs -> (a +-> b,Program fs i2 outputs)
deconstructProgram (Define f (p :: Program fs i2 outputs)) = (f,p)
-}

newProgram
  :: forall scopeInputs scopeRestriction. RestrictScope scopeInputs scopeRestriction =>
     Proxy scopeRestriction
     -> Program '[] scopeInputs '[] (Nub scopeInputs scopeRestriction)
newProgram p = Outputs $ restrictScope p

-- ! check functions output variables dont contain new functions inputs or outputs
-- ! check functions input  variables dont contain new functions inputs
-- functions inputs can contain new function outputs, and will be deleted from the carried scope of open inputs.
-- it is this final list which the runProgram function takes as arguments. 
-- outputs *must* be mentioned in the carried inputs, its ok not to use functions inputs, despite the deletion, since there is no variable name reuse allowd at all
-- if a variable was deleted it must have been provided for by an output, so the check that functionS outputs dont contain new-function outputs.

-- IF IT WAS PREVIOUSLY MENTIONED AS AN OUTPUT, IT CANT BE REUSED, SO CANT BE USED AT ALL BY THE INPUT FUNCTION. 
-- after having done this check, then the functions inputs can be just checked against the carried functions-inputs.

-- ARGH!!! NO DELETION OCCURS, THE OUTPUTS GENERATED INTERNALLY CAN BE REUSED. 

class RunProgram (fs :: [Signature]) (i :: Vars) (inputs :: [Vars]) (o :: Vars) where
 runProgram :: Program fs i inputs o -> (i +-> o) -- Concat used to be Head !!! FFFS it wont fucking work like this. cant concat them, they dont nub together...

instance RunProgram '[] i '[] o where
 runProgram (Outputs f) i = f i

instance (RunProgram '[] b '[] o) => RunProgram ( '(a,b) ': '[]) a (b ': '[]) o where
 runProgram (Define f p) s = runProgram @('[]) @b @('[]) @o p (f s) 

instance (RunProgram fs b bs o) => RunProgram ( '(a,b) ': fs) a (b ': bs) o where
 runProgram (Define f p) s = runProgram @fs @b @bs @o p (f s) 

egProg1'
  :: forall inputs. RestrictScope inputs '["a"] 
  => Proxy inputs 
  -> Program '[] inputs '[] (Nub inputs '["a"])
egProg1' _ = newProgram @inputs @'["a"] (Proxy @'["a"])

egProg1 :: Program '[] '["a", "b"] '[] '["a"]
egProg1 = (egProg1' (Proxy @'["a","b"])) 

inputs1 =  (Bind @"a" (Variable 0) (Bind @"b" (Variable 1) Clean))

-- discard an unused varible "b" from the scope of the preceding program by evaluation
test1 :: Scope '["a"]
test1 = runProgram egProg1 inputs1

helper  :: (Contains '["a", "b"] b ~ 'True) =>
     (a +-> b) -> Program '[ '(a, b)] a '[ '["a", "b"]] '["a"]
helper = flip Define egProg1

egProg2 :: Program '[ '( '["b"],'["b"])] '["b"] ('[ '["a","b"]]) '["a"]
egProg2 = helper id
-- Define @'[] @'["b","a"] @'["a"] @'["b"] @'["b"] id (egProg1 (Proxy @'["b","a"]))

{-
test2 = runProgram egProg2 zs
 where
  zs :: Scope '["b"]
  zs = (Bind @"b" (Variable 0) (Bind @"a" (Variable 1) Clean))
-}
