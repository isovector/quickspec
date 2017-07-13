-- Pruning support for partial application and the like.
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, RecordWildCards, MultiParamTypeClasses, FlexibleContexts #-}
module QuickSpec.Explore.PartialApplication where

import QuickSpec.Term
import QuickSpec.Type
import QuickSpec.Pruning
import QuickSpec.Prop
import qualified Twee.Base as Twee
import qualified Data.Set as Set
import Data.Set(Set)
import Data.List
import Data.Maybe

data PartiallyApplied f =
    -- A partially-applied function symbol.
    -- The Int describes how many arguments the function expects.
    Partial f Int
    -- The ($) operator, for oversaturated applications.
    -- The type argument is the type of the first argument to ($).
  | Apply Type
  deriving (Eq, Ord)

instance Sized f => Sized (PartiallyApplied f) where
  size (Partial f _) = size f
  size (Apply _) = 0

instance Arity f => Arity (PartiallyApplied f) where
  arity (Partial _ n) = n
  arity (Apply _) = 2

instance Pretty f => Pretty (PartiallyApplied f) where
  pPrint (Partial f _) = pPrint f
  pPrint (Apply _) = text "$"

instance PrettyTerm f => PrettyTerm (PartiallyApplied f) where
  termStyle (Partial f _) = termStyle f
  termStyle (Apply _) = invisible

instance Typed f => Typed (PartiallyApplied f) where
  typ (Apply ty) = build (app (Twee.fun Arrow) [ty, ty])
  typ (Partial f _) = typ f
  otherTypesDL (Apply _) = mempty
  otherTypesDL (Partial f _) = otherTypesDL f
  typeSubst_ sub (Apply ty) = Apply (typeSubst_ sub ty)
  typeSubst_ sub (Partial f n) = Partial (typeSubst_ sub f) n

instance (Arity f, Ord f, Typeable f, Typed f) => Apply (Term (PartiallyApplied f)) where
  tryApply t u = do
    tryApply (typ t) (typ u)
    return $
      case t of
        App (F (Partial f n)) ts | n < arity f ->
          build $ app (fun (Partial f (n+1))) [ts, singleton u]
        _ ->
          simpleApply t u

simpleApply ::
  (Arity f, Ord f, Typeable f, Typed f) =>
  Term (PartiallyApplied f) -> Term (PartiallyApplied f) -> Term (PartiallyApplied f)
simpleApply t u =
  build $ app (fun (Apply (typ t))) [t, u]

data State f =
  State {
    st_pruner :: Pruner (Term (PartiallyApplied f)),
    st_functions :: Set f }

encodePartialApplications :: (Ord f, Arity f, Typeable f, Typed f) =>
  Pruner (Term (PartiallyApplied f)) -> Pruner (Term (PartiallyApplied f))
encodePartialApplications pruner =
  makePruner (normalise . st_pruner) addPartialApplications
    State {
      st_pruner = pruner,
      st_functions = Set.empty }

addPartialApplications :: (Ord f, Typed f, Arity f, Typeable f) =>
  Prop (Term (PartiallyApplied f)) -> State f -> State f
addPartialApplications prop state =
  State{
    st_pruner = add st_pruner prop,
    st_functions = st_functions }
  where
    State{..} =
      foldl' addFunction state (map fun_value (funs prop))

addFunction :: (Ord f, Typed f, Arity f, Typeable f) =>
  State f -> PartiallyApplied f -> State f
addFunction State{..} (Partial f _)
  | f `Set.notMember` st_functions =
    State{
      st_pruner = foldl' add st_pruner (axioms f),
      st_functions = Set.insert f st_functions }
    where
      axioms f =
        [ simpleApply (partial i) (build (vs !! i)) === partial (i+1)
        | i <- [0..arity f-1] ]
      partial i =
        build $ app (fun (Partial f i)) (take i vs)
      vs = map var (zipWith V (typeArgs (typ f)) [0..])
addFunction st _ = st

instance (Applicative f, Eval fun (Value f)) => Eval (PartiallyApplied fun) (Value f) where
  eval var (Partial f _) = eval var f
  eval _ (Apply ty) =
    fromJust $
      cast (build (app (Twee.fun Arrow) [ty, ty]))
        (toValue (pure (($) :: (A -> B) -> (A -> B))))