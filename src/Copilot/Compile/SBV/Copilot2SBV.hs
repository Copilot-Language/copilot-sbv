--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs, ExistentialQuantification #-}

module Copilot.Compile.SBV.Copilot2SBV
  ( c2sExpr
  , Inputs(..)
  , ExtVar
  , ExtArr
  , ExtQue
  , ExtVarInput(..)
  , ExtArrInput(..)
  , QueInput(..)
  , QueueIn(..)
  ) 
where

import Prelude hiding (id)
import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.SBV as S
import qualified Data.SBV.Internals as S

import qualified Copilot.Compile.SBV.Queue as Q
import qualified Copilot.Compile.SBV.Witness as W

import Copilot.Core (Op1 (..), Op2 (..), Op3 (..), badUsage)
import qualified Copilot.Core as C
import Copilot.Core.Error (impossible)
import Copilot.Core.Type.Equality ((=~=), coerce, cong)

--------------------------------------------------------------------------------

type ExtVar = (C.Name, ExtVarInput)
type ExtArr = (C.Name, ExtArrInput)
type ExtQue = (C.Id, QueInput)

-- These are all the inputs to the to the SBV expression we're building.
data Inputs = Inputs
  { extVars  :: [ExtVar] -- external variables
  , extArrs  :: [ExtArr] -- external arrays
  , extQues  :: [ExtQue] }

-- External variables
data ExtVarInput = forall a. ExtVarInput 
  { extInput :: S.SBV a
  , extType  :: C.Type a }

-- External arrays
data ExtArrInput = 
  forall a b . Integral b => ExtArrInput 
  { extArr      :: [S.SBV a]
  , extArrType  :: C.Type a 
  , extIdx      :: S.SBV b
  , extIdxType  :: C.Type b }

-- Stream queues
data QueInput = forall a. QueInput 
  { arrInput :: QueueIn a }

data QueueIn a = QueueIn
  { queue    :: [S.SBV a]
  , quePtr   :: S.SBV Q.QueueSize 
  , arrType  :: C.Type a }

--------------------------------------------------------------------------------

c2sExpr :: Inputs -> C.Expr a -> S.SBV a
c2sExpr inputs e = c2sExpr_ e M.empty inputs

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

-- Translate a Copilot expression into an SBV expression.  The environment is
-- for tracking let expression bindings (in the Copilot language), and the list
-- of inputs are all the external things needed as input to the SBV function.
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

  C.ExternVar t name -> 
    getSBV t ext

    where 
    ext :: ExtVarInput
    ext = lookupInput name (extVars inputs) 

    getSBV :: C.Type a -> ExtVarInput -> S.SBV a
    getSBV t1 ExtVarInput { extInput = ext'
                          , extType  = t2 } =
      let Just p = t2 =~= t1 in
      coerce (cong p) ext'
      

  ----------------------------------------------------

  C.ExternArray _ tArr name _ _ _ -> 
    getSBV tArr getExtArr

    where 
    getExtArr :: ExtArrInput
    getExtArr = lookupInput name (extArrs inputs)

    getSBV tA1 ExtArrInput { extArr     = extArr'
                           , extArrType = tA2 
                           , extIdxType = tI2
                           , extIdx     = idx } 
      = let Just p = tA2 =~= tA1 in
        case W.symWordInst tA2 of 
          W.SymWordInst -> 
            case W.symWordInst tI2 of
              W.SymWordInst -> 
                  case W.bitsInst tI2 of
                    W.BitsInst -> 
                        coerce (cong p) 
                         (extArrLookup extArr' idx)

  ----------------------------------------------------

  -- externFun t name _ = C2AExpr $ \ _ meta ->
  --   let Just extFunInfo = M.lookup name (externFunInfoMap meta) in
  --   externFun1 t extFunInfo

  --   where
  --   externFun1 t1
  --     ExternFunInfo
  --       { externFunInfoVar  = var
  --       , externFunInfoType = t2
  --       } =
  --     let Just p = t2 =~= t1 in
  --     case W.exprInst t2 of
  --       W.ExprInst -> coerce (cong p) (A.value var)
 
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

extArrLookup :: (S.SymWord a, S.SymWord b, S.Bits b, Integral b) 
             => [S.SBV a] -> S.SBV b -> S.SBV a
extArrLookup arr idx = 
  S.select arr defaultVal idx
  where
  defaultVal = 
    if null arr then impossible "c2Expr_/ExternArray" "copilot-sbv"
      else head arr


--------------------------------------------------------------------------------      

noFloatOpsErr :: String -> a
noFloatOpsErr op = 
  badUsage ("Floating/Double operators not supported for the SBV backend: " 
         ++ "operator " ++ op ++ " not supported.")

--------------------------------------------------------------------------------      

c2sOp1 :: C.Op1 a b -> S.SBV a -> S.SBV b
c2sOp1 op = case op of
  Not     -> \x -> S.ite (x S..== S.false) S.true S.false
  Abs   t -> case W.symWordInst t of 
                       W.SymWordInst         -> abs 
  Sign  t -> case W.symWordInst t of 
                       W.SymWordInst         -> signum
  BwNot t -> case W.bitsInst    t of 
                       W.BitsInst            -> (S.complement)

  Cast t0 t1 -> case W.castInst t0 t1 of 
                  W.CastInst -> W.sbvCast

  Recip _ -> noFloatOpsErr "recip"
  Exp   _ -> noFloatOpsErr "exp"
  Sqrt  _ -> noFloatOpsErr "sqrt"
  Log   _ -> noFloatOpsErr "log"
  Sin   _ -> noFloatOpsErr "sin"
  Tan   _ -> noFloatOpsErr "tan"
  Cos   _ -> noFloatOpsErr "cos"
  Asin  _ -> noFloatOpsErr "asin"
  Atan  _ -> noFloatOpsErr "atan"
  Acos  _ -> noFloatOpsErr "acos"
  Sinh  _ -> noFloatOpsErr "sinh"
  Tanh  _ -> noFloatOpsErr "tanh"
  Cosh  _ -> noFloatOpsErr "cosh"
  Asinh _ -> noFloatOpsErr "asinh"
  Atanh _ -> noFloatOpsErr "atanh"
  Acosh _ -> noFloatOpsErr "acosh"

--------------------------------------------------------------------------------

c2sOp2 :: C.Op2 a b c -> S.SBV a -> S.SBV b -> S.SBV c
c2sOp2 op = case op of
  And     -> \x y -> S.ite (x S..== S.false) 
                                   S.false 
                                   (S.ite (y S..== S.false) S.false S.true)
  Or      -> \x y -> S.ite (x S..== S.false) 
                                   (S.ite (y S..== S.false) S.false S.true)
                                   S.true
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
                                                  \x y -> fst (S.bvQuotRem x y)
  Mod   t -> case W.divInst      t of W.BVDivisibleInst  ->  
                                                  \x y -> snd (S.bvQuotRem x y)

  BwAnd t -> case W.bitsInst     t of W.BitsInst       -> (S..&.)
  BwOr  t -> case W.bitsInst     t of W.BitsInst       -> (S..|.)
  BwXor t -> case W.bitsInst     t of W.BitsInst       -> (S.xor)
  BwShiftL tvec tidx -> 
    case W.bitsInst tvec of 
      W.BitsInst -> 
        \vec idx -> case W.symWordInst tidx of
                      W.SymWordInst -> case S.unliteral idx of
                                         Nothing -> badUsage "Using the SBV backend, shiftL only supports constant shift indicies"
                                         Just x  -> S.shiftL vec (fromIntegral x)
  BwShiftR tvec tidx -> 
    case W.bitsInst tvec of 
      W.BitsInst -> 
        \vec idx -> case W.symWordInst tidx of
                      W.SymWordInst -> case S.unliteral idx of
                                         Nothing -> badUsage "Using the SBV backend, shiftR only supports constant shift indicies"
                                         Just x  -> S.shiftR vec (fromIntegral x)

  Fdiv  _ -> noFloatOpsErr "fdiv"
  Pow   _ -> noFloatOpsErr "pow"
  Logb  _ -> noFloatOpsErr "logb"

c2sOp3 :: C.Op3 a b c d -> S.SBV a -> S.SBV b -> S.SBV c -> S.SBV d
c2sOp3 op = case op of
  Mux t ->
    case W.mergeableInst t of 
      W.MergeableInst -> \b c1 c2 -> S.ite b c1 c2


--------------------------------------------------------------------------------

