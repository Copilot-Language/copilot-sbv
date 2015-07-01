--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}

module Copilot.Compile.SBV.Code
  ( updateStates
  , updateObservers
  , fireTriggers
  , getExtArrs
  , getExtFuns
  ) where

import Copilot.Compile.SBV.Copilot2SBV
import Copilot.Compile.SBV.MetaTable
import qualified Copilot.Compile.SBV.Witness as W
import Copilot.Compile.SBV.Common
import Copilot.Compile.SBV.ACSLexpr

import qualified Copilot.Core.PrettyPrint as PP
import qualified Text.PrettyPrint.HughesPJ as PJ

import qualified Copilot.Core as C
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

updateStates :: MetaTable -> C.Spec -> [SBVFunc]
updateStates meta (C.Spec streams _ _ _) =
  map updateStreamState streams

  where
  updateStreamState :: C.Stream -> SBVFunc
  updateStreamState C.Stream { C.streamId       = id
                             , C.streamBuffer   = buffer
                             , C.streamExpr     = e
                             , C.streamExprType = t1
                                                      } 
    = mkSBVFunc (mkUpdateStFn id) $ do
        S.cgAddDecl [("/*test 001*/\n/*ACSL to write\n " ++ (PJ.render $ PP.ppExpr e) ++ "\n*/\n/*@\n assigns \\nothing;\n ensures \\result == " ++ (PJ.render $ ppExpr meta e) ++ ";\n*/")]
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
        S.cgAddDecl [("/*test 005*/\n/*ACSL to write\n " ++ (PJ.render $ PP.ppExpr e) ++ "\n*/\n/*@\n assigns \\nothing;\n ensures \\result == " ++ (PJ.render $ ppExpr meta e) ++ ";\n*/")]
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
      S.cgAddDecl [("/*test 006*/\n/*ACSL to write\n " ++ (PJ.render $ PP.ppExpr guard) ++ "\n*/\n/*@\n assigns \\nothing;\n ensures \\result == " ++ (PJ.render $ ppExpr meta guard) ++ ";\n*/")]
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
    S.cgAddDecl [("/*test 003*/\n/*ACSL to write\n " ++ (PJ.render $ PP.ppExpr e) ++ "\n*/\n/*@\n assigns \\nothing;\n ensures \\result == " ++ (PJ.render $ ppExpr meta e) ++ ";\n*/")]
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
      S.cgAddDecl [("/*test 002*/\n/*ACSL to write\n " ++ (PJ.render $ PP.ppExpr idx) ++ "\n*/\n/*@\n assigns \\nothing;\n ensures \\result == " ++ (PJ.render $ ppExpr meta idx) ++ ";\n*/")]
      inputs <- mkInputs meta (c2Args idx)
      W.SymWordInst <- return (W.symWordInst t)
      W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
      S.cgReturn (c2sExpr inputs idx)

--------------------------------------------------------------------------------

-- Generate an SBV function that calculates the Copilot expression to get the
-- next index to sample an external array.
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
    let Just extInfo = M.lookup name extInfos in
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
  argToInput acc (ExternFun name tag) =
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

