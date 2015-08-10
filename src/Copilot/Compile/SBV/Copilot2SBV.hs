--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs, ExistentialQuantification, ScopedTypeVariables #-}

module Copilot.Compile.SBV.Copilot2SBV
  ( c2sExpr
  , Inputs(..)
  , Ext
  , ExtQue
  , ExtInput(..)
  , QueInput(..)
  , QueueIn(..)
  ) 
where

import Prelude hiding (id)
import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.SBV as S
--import qualified Data.SBV.Internals as S

import Copilot.Compile.SBV.Common
import qualified Copilot.Compile.SBV.Queue as Q
import qualified Copilot.Compile.SBV.Witness as W

import Copilot.Core (Op1 (..), Op2 (..), Op3 (..), badUsage)
import qualified Copilot.Core as C
import qualified Copilot.Core.Type as C
import Copilot.Core.Error (badUsage, impossible)
import Copilot.Core.Type.Equality ((=~=), coerce, cong)
import Data.Maybe (fromJust)
import Debug.Trace

--------------------------------------------------------------------------------

type Ext = (C.Name, ExtInput)
type ExtQue = (C.Id, QueInput)

-- These are all the inputs to the to the SBV expression we're building.
data Inputs = Inputs
  { extVars  :: [Ext] -- external variables
  , extArrs  :: [Ext] -- external arrays
  , extFuns  :: [Ext] -- external functions
  , extStrs  :: [Ext] -- structs
  , extQues  :: [ExtQue] }

-- External input -- variables, arrays, and functions
data ExtInput = forall a. ExtInput 
  { extInput :: S.SBV a
  , extType  :: C.Type a }

-- Stream queues
data QueInput = forall a. QueInput 
  { arrInput :: QueueIn a }

data QueueIn a = QueueIn
  { queue    :: [S.SBV a]
  , quePtr   :: S.SBV Q.QueueSize 
  , arrType  :: C.Type a }

--------------------------------------------------------------------------------

data Local = forall a . Local
  { localSBVExpr :: S.SBV a
  , localType    :: C.Type a }

type Env = Map C.Name Local

--------------------------------------------------------------------------------

lookupInput :: Eq a => a -> [(a,b)] -> b
lookupInput id prs =
  case lookup id prs of
    Nothing   -> impossible "lookupInput" "copilot-sbv"
    Just val  -> val

--------------------------------------------------------------------------------

c2sExpr :: Inputs -> C.Expr a -> S.SBV a
c2sExpr inputs e = c2sExpr_ e M.empty inputs

--------------------------------------------------------------------------------

-- Translate a Copilot expression into an SBV expression.  The environment
-- passed in is for tracking let expression bindings (in the Copilot language),
-- and the list of inputs are all the external things needed as input to the SBV
-- function.
c2sExpr_ :: C.Expr a -> Env -> Inputs -> S.SBV a
c2sExpr_ e0 env inputs = case e0 of

  C.Const t x ->
    case W.symWordInst t of W.SymWordInst -> S.literal x

  ----------------------------------------------------

  C.Drop t i id -> drop1 t que
    where
    que :: QueInput
    que = lookupInput id (extQues inputs)

    drop1 :: C.Type a -> QueInput -> S.SBV a
    drop1 t1 QueInput { arrInput = QueueIn { queue   = que'
                                           , quePtr  = qPtr
                                           , arrType = t2 } } =
      let Just p = t2 =~= t1 in
      case W.symWordInst t2 of
        W.SymWordInst -> 
          case W.hasSignAndSizeInst t2 of
            W.HasSignAndSizeInst ->
              coerce (cong p) (Q.lookahead i que' qPtr)

  ----------------------------------------------------

  C.Local t1 _ name e1 e2 ->
    let e1' = c2sExpr_ e1 env inputs in
    let env' = M.insert name (Local e1' t1) env in
    c2sExpr_ e2 env' inputs

  ----------------------------------------------------

  C.Var t1 name ->
    let Just local = M.lookup name env
    in
      case local of
        Local
          { localSBVExpr = e
          , localType    = t2
          } ->
            let Just p = t2 =~= t1
            in  coerce (cong p) e

  ----------------------------------------------------

  C.ExternVar t name _ -> 
    getSBV t ext

    where 
    ext :: ExtInput
    ext = lookupInput name (extVars inputs)

    getSBV :: C.Type a -> ExtInput -> S.SBV a
    getSBV t1 ExtInput { extInput = ext'
                       , extType  = t2 } =
      let Just p = t2 =~= t1 in
      coerce (cong p) ext'

  ----------------------------------------------------

  C.ExternArray _ t name _ _ _ tag -> 
    getSBV t getExtArr

    where 
    getExtArr :: ExtInput
    getExtArr = lookupInput (mkExtTmpTag name (tag)) (extArrs inputs)

    getSBV t1 ExtInput { extInput  = v
                       , extType = t2 }
      = let Just p = t2 =~= t1 in
        coerce (cong p) v

  ----------------------------------------------------

  C.ExternFun t name _ _ tag ->
    getSBV t getExtFun

    where
    getExtFun :: ExtInput
    getExtFun = lookupInput (mkExtTmpTag name (tag)) (extFuns inputs)

    getSBV t1 ExtInput { extType  = t2
                       , extInput = v }
      = let Just p = t2 =~= t1 in
        coerce (cong p) v
 
  ----------------------------------------------------

  C.ExternStruct t name _ tag ->
    getSBV t getExtStr

    where
    getExtStr :: ExtInput
    getExtStr = lookupInput (mkExtTmpTag name (tag)) (extStrs inputs)

    getSBV t1 ExtInput { extInput = v
                       , extType  = t2 }
      = let Just p = t2 =~= t1 in
        coerce (cong p) v

  ----------------------------------------------------

  C.GetField _ t struct name ->
    case struct of
      C.ExternStruct _ str_name sargs tag -> getSBV t getStrField
        where
        getStrField :: ExtInput
        getStrField =
          case lookup (mkExtTmpTag name tag) (extStrs inputs) of
            Just val  -> val
            Nothing   -> badUsage ("Struct field is undefined: "++str_name++"."++name)

        getSBV t1 ExtInput { extType = t2
                           , extInput = v }
          = let Just p = t2 =~= t1 in -- Code breaks here, because we pass fields from MetaTable.hs as
                                      -- ExternStructs (c2Args_). These are then passed into Code.hs,
                                      -- where argToInput in the case of ExternStruct ASSUMES that
                                      -- all ExternStructs are of type Bool, when the actual field 
                                      -- is not necessarily a Bool. Thus, we need to figure out how to
                                      -- pass the Type of the field to argToInput, so that the types
                                      -- in Copilot2SBV.hs match up
            coerce (cong p) v
      _ -> badUsage "Non-struct is the first parameter of (#) struct field access"

  ----------------------------------------------------

  C.Op1 op e ->
    let res1 = c2sExpr_ e env inputs in
    c2sOp1 op res1

  ----------------------------------------------------

  C.Op2 op e1 e2 ->
    let res1 = c2sExpr_ e1 env inputs in
    let res2 = c2sExpr_ e2 env inputs in
    c2sOp2 op res1 res2 

  ----------------------------------------------------

  C.Op3 op e1 e2 e3 ->
    let res1 = c2sExpr_ e1 env inputs in
    let res2 = c2sExpr_ e2 env inputs in
    let res3 = c2sExpr_ e3 env inputs in
    c2sOp3 op res1 res2 res3

  C.Label t s e -> case W.symWordInst t of 
                       W.SymWordInst -> S.label s (c2sExpr_ e env inputs)

--------------------------------------------------------------------------------      

noFloatOpsErr :: String -> a
noFloatOpsErr op = 
  badUsage ("The operation you used is not supported by the SBV backend: " 
         ++ "operator " ++ op ++ " not supported. Please change it with your math skills to something supported.")

--------------------------------------------------------------------------------      

c2sOp1 :: C.Op1 a b -> S.SBV a -> S.SBV b
c2sOp1 op = case op of
  Not     -> (S.bnot)
  Abs   t -> case W.symWordInst t of 
                       W.SymWordInst         -> abs 
  Sign  t -> case W.symWordInst t of 
                       W.SymWordInst         -> signum
  BwNot t -> case W.bitsInst    t of 
                       W.BitsInst            -> (S.complement)

  Cast t0 t1 -> case W.castInst t0 t1 of 
                  W.CastInst -> W.sbvCast

  Recip _      -> noFloatOpsErr "recip"

  Exp   C.Float  -> exp
  Exp   C.Double -> exp
  Sqrt  C.Float  -> sqrt
  Sqrt  C.Double -> sqrt
  Log   C.Float  -> log
  Log   C.Double -> log
  Sin   C.Float  -> sin
  Sin   C.Double -> sin
  Cos   C.Float  -> cos
  Cos   C.Double -> cos
  Tan   C.Float  -> tan
  Tan   C.Double -> tan
  Asin  C.Float  -> asin
  Asin  C.Double -> asin
  Acos  C.Float  -> acos
  Acos  C.Double -> acos
  Atan  C.Float  -> atan
  Atan  C.Double -> atan
  Sinh  C.Float  -> sinh
  Sinh  C.Double -> sinh
  Cosh  C.Float  -> cosh
  Cosh  C.Double -> cosh
  Tanh  C.Float  -> tanh
  Tanh  C.Double -> tanh
  Asinh C.Float  -> asinh
  Asinh C.Double -> asinh
  Acosh C.Float  -> acosh
  Acosh C.Double -> acosh
  Atanh C.Float  -> atanh
  Atanh C.Double -> atanh

--------------------------------------------------------------------------------

c2sOp2 :: C.Op2 a b c -> S.SBV a -> S.SBV b -> S.SBV c
c2sOp2 op = case op of
  And     -> (S.&&&)
  Or      -> (S.|||)
  Add   t -> case W.symWordInst  t of W.SymWordInst    ->  (+)
  Sub   t -> case W.symWordInst  t of W.SymWordInst    ->  (-)
  Mul   t -> case W.symWordInst  t of W.SymWordInst    ->  (*)

  Eq    t -> case W.eqInst       t of W.EqInst         ->  (S..==)
  Ne    t -> case W.eqInst       t of W.EqInst         ->  (S../=)
  Le    t -> case W.ordInst      t of W.OrdInst        ->  (S..<=)
  Ge    t -> case W.ordInst      t of W.OrdInst        ->  (S..>=)
  Lt    t -> case W.ordInst      t of W.OrdInst        ->  (S..<)
  Gt    t -> case W.ordInst      t of W.OrdInst        ->  (S..>)

  Div   t -> case W.divInst      t of W.BVDivisibleInst  ->  
                                                  \x y -> fst (S.sQuotRem x y)
  Mod   t -> case W.divInst      t of W.BVDivisibleInst  ->  
                                                  \x y -> snd (S.sQuotRem x y)

  BwAnd t -> case W.bitsInst     t of W.BitsInst       -> (S..&.)
  BwOr  t -> case W.bitsInst     t of W.BitsInst       -> (S..|.)
  BwXor t -> case W.bitsInst     t of W.BitsInst       -> (S.xor)
  BwShiftL tvec tidx -> 
    case W.integralInst tvec of 
      W.IntegralInst -> 
        \vec idx -> case (W.integralInst tidx) of
                      W.IntegralInst -> S.sShiftLeft vec idx
                                --case S.unliteral idx of
                                 --        Nothing -> badUsage "Using the SBV backend, shiftL only supports constant shift indicies"
                                 --        Just x  -> S.shiftL vec (fromIntegral x)
  BwShiftR tvec tidx -> 
    case W.integralInst tvec of 
      W.IntegralInst -> 
        \vec idx -> case (W.integralInst tidx) of
                      W.IntegralInst -> S.sShiftRight vec idx

--    case W.bitsInst tvec of 
--      W.BitsInst -> 
--        \vec idx -> case W.symWordInst tidx of
--                      W.SymWordInst -> case S.unliteral idx of
--                                         Nothing -> badUsage "Using the SBV backend, shiftR only supports constant shift indicies"
--                                         Just x  -> S.shiftR vec (fromIntegral x)

  Fdiv  C.Float  -> case W.numInst         C.Float  of W.NumInst         -> (/)
  Fdiv  C.Double -> case W.numInst         C.Double of W.NumInst         -> (/)
  Pow   C.Float  -> (**)
  Pow   C.Double -> (**)
  Logb  C.Float  -> logBase
  Logb  C.Double -> logBase


c2sOp3 :: C.Op3 a b c d -> S.SBV a -> S.SBV b -> S.SBV c -> S.SBV d
c2sOp3 op = case op of
  Mux t ->
    case W.mergeableInst t of 
      W.MergeableInst -> \b c1 c2 -> S.ite b c1 c2


--------------------------------------------------------------------------------

