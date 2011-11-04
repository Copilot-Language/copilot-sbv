--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs, ExistentialQuantification #-}

module Copilot.Compile.SBV.Copilot2SBV
  ( c2sExpr
  , Input(..)
  , ExtInput(..)
  , ArrInput(..)
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
import Copilot.Core.Type.Equality ((=~=), coerce, cong)


--------------------------------------------------------------------------------

data Input = 
    ExtIn C.Name ExtInput
  | ArrIn C.Id ArrInput

data ExtInput = forall a. ExtInput 
  { extInput :: S.SBV a
  , extType  :: C.Type a }

data ArrInput = forall a. ArrInput 
  { arrInput :: QueueIn a }

data QueueIn a = QueueIn
  { queue  :: [S.SBV a]
  , quePtr :: S.SBV Q.QueueSize 
  , arrType  :: C.Type a }

--------------------------------------------------------------------------------

c2sExpr :: [Input] -> C.Expr a -> S.SBV a
c2sExpr inputs e = c2sExpr_ e M.empty inputs

--------------------------------------------------------------------------------

data Local = forall a . Local
  { localSBVExpr :: S.SBV a
  , localType    :: C.Type a }

type Env = Map C.Name Local

--------------------------------------------------------------------------------

c2sExpr_ :: C.Expr a -> Env -> [Input] -> S.SBV a
c2sExpr_ e0 env inputs = case e0 of

  C.Const t x ->
    case W.symWordInst t of W.SymWordInst -> S.literal x

  ----------------------------------------------------

  C.Drop t i id ->
    let que :: ArrInput
        Just que = foldl 
          ( \acc x -> case x of
                        ArrIn id' q -> if id' == id then Just q 
                                         else acc
                        ExtIn _ _ -> acc ) 
          Nothing
          inputs 
    in 
    drop1 t que

    where
    drop1 :: C.Type a -> ArrInput -> S.SBV a
    drop1 t1 ArrInput { arrInput = QueueIn { queue   = que 
                                           , quePtr  = qPtr
                                           , arrType = t2 } } =
      let Just p = t2 =~= t1 in
      case W.symWordInst t2 of
        W.SymWordInst -> 
          case W.hasSignAndSizeInst t2 of
            W.HasSignAndSizeInst ->
              coerce (cong p) (Q.lookahead i que qPtr)

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
    let ext :: ExtInput
        Just ext = foldl 
          ( \acc x -> case x of
                        ArrIn _ _ -> acc
                        ExtIn nm e -> if nm == name then Just e
                                        else acc ) 
          Nothing
          inputs 
    in getSBV t ext

    where 
    getSBV :: C.Type a -> ExtInput -> S.SBV a
    getSBV t1 ExtInput { extInput = ext
                       , extType  = t2 } =
      let Just p = t2 =~= t1 in
      coerce (cong p) ext

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

  Fdiv  _ -> noFloatOpsErr "fdiv"
  Pow   _ -> noFloatOpsErr "pow"
  Logb  _ -> noFloatOpsErr "logb"

c2sOp3 :: C.Op3 a b c d -> S.SBV a -> S.SBV b -> S.SBV c -> S.SBV d
c2sOp3 op = case op of
  Mux t ->
    case W.mergeableInst t of 
      W.MergeableInst -> \b c1 c2 -> S.ite b c1 c2
