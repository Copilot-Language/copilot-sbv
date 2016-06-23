--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}

module Copilot.Compile.SBV.Code
  ( updateStates
  , updateObservers
  , fireTriggers
  , getExtArrs
  , getExtFuns
  ) where

import Copilot.Compile.SBV.Copilot2SBV
import Copilot.Compile.SBV.MetaTable as T
import qualified Copilot.Compile.SBV.Witness as W
import Copilot.Compile.SBV.Common
import Copilot.Compile.SBV.ACSLexpr

import qualified Copilot.Core.PrettyDot as PD
import qualified Text.PrettyPrint.HughesPJ as PJ

import Copilot.Core as C
import Copilot.Core.Type.Equality ((=~=), coerce, cong)

import qualified Data.SBV as S

import qualified Data.Map as M
import Control.Monad (foldM)
import Prelude hiding (id)

--------------------------------------------------------------------------------

type SBVFunc  = (String, S.SBVCodeGen ())

mkSBVFunc :: String -> S.SBVCodeGen () -> (String, S.SBVCodeGen ())
mkSBVFunc str codeGen = (str, codeGen)

--------------------------------------------------------------------------------

epsilon :: Double
epsilon = 0.1

updateStates :: MetaTable -> C.Spec -> [SBVFunc]
updateStates meta (C.Spec streams _ _ _) =
  map updateStreamState streams

  where
  updateStreamState :: C.Stream -> SBVFunc
  updateStreamState C.Stream { C.streamId       = id
                             , C.streamExpr     = e
                             , C.streamExprType = t1
                                                      }
    = mkSBVFunc (mkUpdateStFn id) $ do
        S.cgAddDecl [("/*test 001*/\n/*DotBegin\n" ++ (PD.prettyPrintExprDot False e) ++ "\nDotEnd*/\n/*@\n assigns \\nothing;\n ensures \\abs(\\result - " ++ (PJ.render $ ppExpr meta $ simpl e) ++ ") <= " ++ show epsilon ++ ";\n*/")]
        inputs <- mkInputs meta (c2Args e)
        let e' = c2sExpr inputs e
        let Just strmInfo = M.lookup id (streamInfoMap meta)
        updateStreamState1 t1 e' strmInfo

  updateStreamState1 :: C.Type a -> S.SBV a -> C.Stream -> S.SBVCodeGen ()
  updateStreamState1 t1 e1 C.Stream { C.streamExprType = t2 }
    = do
    W.SymWordInst <- return (W.symWordInst t2)
    W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t2)
    Just p <- return (t1 =~= t2)
    S.cgReturn $ coerce (cong p) e1

--------------------------------------------------------------------------------

updateObservers :: MetaTable -> C.Spec -> [SBVFunc]
updateObservers meta (C.Spec _ observers _ _) =
  map updateObs observers

  where
  updateObs :: C.Observer -> SBVFunc
  updateObs C.Observer { C.observerName     = name
                       , C.observerExpr     = e
                       , C.observerExprType = t } =
    mkSBVFunc (mkObserverFn name) mkSBVExp

    where
    mkSBVExp =
      do
        S.cgAddDecl [("/*test 005*/\n/*DotBegin\n" ++ (PD.prettyPrintExprDot False e) ++ "\nDotEnd*/\n/*@\n assigns \\nothing;\n ensures \\abs(\\result - " ++ (PJ.render $ ppExpr meta $ simpl e) ++ ") <= " ++ show epsilon ++ ";\n*/")]
        inputs <- mkInputs meta (c2Args e)
        let e' = c2sExpr inputs e
        W.SymWordInst <- return (W.symWordInst t)
        W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
        S.cgReturn e'

--------------------------------------------------------------------------------

fireTriggers :: MetaTable -> C.Spec -> [SBVFunc]
fireTriggers meta (C.Spec _ _ triggers _) =
  concatMap fireTrig triggers

  where
  fireTrig :: C.Trigger -> [SBVFunc]
  fireTrig C.Trigger { C.triggerName  = name
                     , C.triggerGuard = guard
                     , C.triggerArgs  = args } =
      mkSBVFunc (mkTriggerGuardFn name) mkSBVExp
    : map go (mkArgIdx args)
    where
    go (i,e) = mkArgCall meta (mkTriggerArgFn i name) e
    mkSBVExp = do
      S.cgAddDecl [("/*test 006*/\n/*@\n assigns \\nothing;\n ensures \\result == " ++ (PJ.render $ ppExpr meta $ simpl guard) ++ ";\n*/")]
      inputs <- mkInputs meta (c2Args guard)
      let e = c2sExpr inputs guard
      S.cgReturn e

--------------------------------------------------------------------------------

mkArgCall :: MetaTable -> String -> C.UExpr -> SBVFunc
mkArgCall meta fnCallName C.UExpr { C.uExprExpr = e
                            , C.uExprType = t }
  =
  mkSBVFunc fnCallName mkExpr
  where
  mkExpr = do
    S.cgAddDecl [("/*test 003*/\n/*DotBegin\n" ++ (PD.prettyPrintExprDot False e) ++ "\nDotEnd*/\n/*@\n assigns \\nothing;\n ensures \\abs(\\result - " ++ (PJ.render $ ppExpr meta $ simpl e) ++ ") <= " ++ show epsilon ++ ";\n*/")]
    inputs <- mkInputs meta (c2Args e)
    let e' = c2sExpr inputs e
    W.SymWordInst <- return (W.symWordInst t)
    W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
    S.cgReturn e'

--------------------------------------------------------------------------------

-- Generate an SBV function that calculates the Copilot expression to get the
-- next index to sample an external array.
getExtArrs :: MetaTable -> [SBVFunc]
getExtArrs meta@(MetaTable { externArrInfoMap = arrs })
  = map mkIdx (M.toList arrs)

  where
  mkIdx :: (Int, C.ExtArray) -> SBVFunc
  mkIdx (_, C.ExtArray { C.externArrayName    = name
                       , C.externArrayIdx     = idx
                       , C.externArrayIdxType = t    })
    =
    mkSBVFunc (mkExtArrFn name) mkSBVExpr
    where
    mkSBVExpr :: S.SBVCodeGen ()
    mkSBVExpr = do
      S.cgAddDecl [("/*test 002*/\n/*DotBegin\n" ++ (PD.prettyPrintExprDot False idx) ++ "\nDotEnd*/\n/*@\n assigns \\nothing;\n ensures \\abs(\\result - " ++ (PJ.render $ ppExpr meta $ simpl idx) ++ ") <= " ++ show epsilon ++ ";\n*/")]
      inputs <- mkInputs meta (c2Args idx)
      W.SymWordInst <- return (W.symWordInst t)
      W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
      S.cgReturn (c2sExpr inputs idx)

--------------------------------------------------------------------------------

-- Generate an SBV function that calculates the Copilot expression to get the
-- next index to sample an external function.
getExtFuns :: MetaTable -> [SBVFunc]
getExtFuns meta@(MetaTable { externFunInfoMap = exts })
  = concatMap mkExtF (M.toList exts)

  where
  mkExtF :: (Int, C.ExtFun) -> [SBVFunc]
  mkExtF (_, C.ExtFun { C.externFunName = name
                      , C.externFunTag  = tag
                      , C.externFunArgs = args })
    =
    map go (mkArgIdx args)
    where
    go (i,e) = mkArgCall meta (mkExtFunArgFn i name tag) e

--------------------------------------------------------------------------------

-- mkInputs takes the datatype containing the entire spec (meta) as well as all
-- possible arguments to the SBV function generating the expression.  From those
-- arguments, it then generates in the SBVCodeGen monad---the actual Inputs---
-- the queues to hold streams as well as external variables.

-- XXX MUST be put in the monad in the same order as the function call
-- (argToCall from MetaTable.hs).

mkInputs :: MetaTable -> [Arg] -> S.SBVCodeGen Inputs
mkInputs meta args =
  foldM argToInput (Inputs [] [] [] []) args

  where
  argToInput :: Inputs -> Arg -> S.SBVCodeGen Inputs

  -----------------------------------------

  -- External variables
  argToInput acc (Extern name) =
    let extInfos = externVarInfoMap meta in
    let Just extInfo = M.lookup (name) extInfos in
    mkExtInput extInfo

    where
    mkExtInput :: C.ExtVar -> S.SBVCodeGen Inputs
    mkExtInput (C.ExtVar _ C.UType { C.uTypeType = t }) = do
      ext <- mkExtInput_ t (mkExtTmpVar name)
      return acc { extVars = (name, (ExtInput { extInput = ext
                                              , extType  = t })
                             ) : extVars acc }

  -----------------------------------------

  -- External arrays
  argToInput acc (ExternArr name tag) =
    let extInfos = externArrInfoMap meta in
    let Just extInfo = M.lookup tag extInfos in
    mkExtInput extInfo

    where
    mkExtInput :: C.ExtArray -> S.SBVCodeGen Inputs
    mkExtInput C.ExtArray { C.externArrayElemType = t }
      = do
      v <- mkExtInput_ t (mkExtTmpTag name (Just tag))
      return acc { extArrs = ((mkExtTmpTag name (Just tag)), ExtInput
                                      { extInput  = v
                                      , extType   = t }
                             ) : extArrs acc }

  -----------------------------------------

  -- External functions
  argToInput acc (T.ExternFun name tag) =
    let extInfos = externFunInfoMap meta in
    let Just extInfo = M.lookup tag extInfos in
    mkExtInput extInfo

    where
    mkExtInput :: C.ExtFun -> S.SBVCodeGen Inputs
    mkExtInput C.ExtFun { C.externFunType = t }
      = do
      v <- mkExtInput_ t (mkExtTmpTag name (Just tag))
      return acc { extFuns = ((mkExtTmpTag name (Just tag)), ExtInput
                                      { extInput = v
                                      , extType  = t }
                             ) : extFuns acc }

  -----------------------------------------

  -- Stream queues
  argToInput acc (Queue id) =
    let strmInfos = streamInfoMap meta in
    let Just strmInfo = M.lookup id strmInfos in
    mkQueInput strmInfo

    where
    mkQueInput :: C.Stream -> S.SBVCodeGen Inputs
    mkQueInput C.Stream { C.streamBuffer = que
                        , C.streamExprType  = t } = do
      arr <- mkQueInput_ t que
      ptr <- S.cgInput (mkQueuePtrVar id)

      return acc { extQues = (id, QueInput (QueueIn { queue   = arr
                                                    , quePtr  = ptr
                                                    , arrType = t })
                             ) : extQues acc
                 }

    mkQueInput_ :: C.Type a -> [a] -> S.SBVCodeGen [S.SBV a]
    mkQueInput_ t que = do
      W.SymWordInst        <- return (W.symWordInst t)
      W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
      arr <- S.cgInputArr (length que) (mkQueueVar id)
      return arr

-----------------------------------------

mkExtInput_ :: C.Type a -> String -> S.SBVCodeGen (S.SBV a)
mkExtInput_ t name = do
  W.SymWordInst        <- return (W.symWordInst t)
  W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
  ext <- S.cgInput name
  return ext

-----------------------------------------

simpl :: Expr a -> Expr a
simpl = \ case
  Op1 op e            -> simplOp1 (simpl e) op
  Op2 op e1 e2        -> simplOp2 (simpl e1) (simpl e2) op
  Op3 op e1 e2 e3     -> simplOp3 (simpl e1) (simpl e2) (simpl e3) op
  Local t1 t2 n e1 e2 -> Local t1 t2 n (simpl e1) (simpl e2)
  Label t s e         -> Label t s (simpl e)

  c                   -> c

simplOp1 :: Expr a -> Op1 a b -> Expr b
simplOp1 e@(Const Bool x) = \ case
  Not      -> Const Bool $ not x
  op       -> Op1 op e
simplOp1 e@(Const Double x) = \ case
  Abs _    -> Const Double $ abs x
  --Sign _   -> sign x
  --Recip _  ->
  --Exp _    ->
  --Sqrt _   ->
  --Log _    ->
  Sin _    -> Const Double $ sin x
  Tan _    -> Const Double $ tan x
  Cos _    -> Const Double $ cos x
  Asin _   -> Const Double $ asin x
  Atan _   -> Const Double $ atan x
  Acos _   -> Const Double $ acos x
  Sinh _   -> Const Double $ sinh x
  Tanh _   -> Const Double $ tanh x
  Cosh _   -> Const Double $ cosh x
  Asinh _  -> Const Double $ asinh x
  Atanh _  -> Const Double $ atanh x
  Acosh _  -> Const Double $ acosh x
  --BwNot _  ->
  --Cast _ _ ->
  op           -> Op1 op e
simplOp1 e = \ op -> Op1 op e

simplOp2 :: Expr a -> Expr b -> Op2 a b c -> Expr c
simplOp2 e1@(Const Bool x1) e2@(Const Bool x2) = \ case
  And          -> Const Bool $ x1 && x2
  Or           -> Const Bool $ x1 || x2
  Eq       _   -> Const Bool $ x1 == x2
  Ne       _   -> Const Bool $ x1 /= x2
  op           -> Op2 op e1 e2
simplOp2 e1@(Const Double x1) e2@(Const Double x2) = \ case
  Add      _   -> Const Double $ x1 + x2
  Sub      _   -> Const Double $ x1 - x2
  Mul      _   -> Const Double $ x1 * x2
  Div      _   -> Const Double $ x1 / x2
  Mod      _   -> Const Double $ x1 `mod` x2
  --Fdiv     _   ->
  Pow      _   -> Const Double $ x1 ** x2
  -- Logb     _   ->
  Eq       _   -> Const Bool $ x1 == x2
  Ne       _   -> Const Bool $ x1 /= x2
  Le       _   -> Const Bool $ x1 <= x2
  Ge       _   -> Const Bool $ x1 >= x2
  Lt       _   -> Const Bool $ x1 < x2
  Gt       _   -> Const Bool $ x1 > x2
  --BwAnd    _   ->
  --BwOr     _   ->
  --BwXor    _   ->
  --BwShiftL _ _ ->
  --BwShiftR _ _ ->
  op           -> Op2 op e1 e2
simplOp2 e1 e2 = \ op -> Op2 op e1 e2

simplOp3 :: Expr a -> Expr b -> Expr c -> Op3 a b c d -> Expr d
-- simplOp3 (Const _ x1) (Const _ x2) (Const _ x3) = \ case
--   Mux _    -> if x1 then x2 else x3
simplOp3 e1 e2 e3 = \ op -> Op3 op e1 e2 e3

