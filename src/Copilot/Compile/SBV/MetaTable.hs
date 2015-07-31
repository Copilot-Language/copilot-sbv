--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification, GADTs #-}

module Copilot.Compile.SBV.MetaTable
  ( StreamInfoMap
  , ExternVarInfoMap
  , ExternArrInfoMap
  , ExternFunInfoMap
  , ExternStrInfoMap
  , TriggerInfo (..)
  , TriggerInfoMap
  , ObserverInfo (..)
  , ObserverInfoMap
  , MetaTable (..)
  , allocMetaTable
  , collectArgs
  , Arg(..)
  , c2Args
  ) where

import Copilot.Compile.SBV.Common
import qualified Copilot.Core as C

import Data.Map (Map)
import Data.List (nub)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Prelude hiding (id)
import Debug.Trace

--------------------------------------------------------------------------------

type StreamInfoMap = Map C.Id C.Stream
type ExternVarInfoMap = Map C.Tag C.ExtVar
type ExternArrInfoMap = Map C.Tag C.ExtArray
type ExternFunInfoMap = Map C.Tag C.ExtFun
type ExternStrInfoMap = Map C.Tag C.ExtStruct

--------------------------------------------------------------------------------

data TriggerInfo = TriggerInfo
  { guardArgs      :: [String]
  , triggerArgArgs :: [[String]] }

type TriggerInfoMap = Map C.Name TriggerInfo

--------------------------------------------------------------------------------

data ObserverInfo = ObserverInfo
  { observerArgs :: [String] }

type ObserverInfoMap = Map C.Name ObserverInfo

--------------------------------------------------------------------------------

data MetaTable = MetaTable
  { streamInfoMap     :: StreamInfoMap
  , externVarInfoMap  :: ExternVarInfoMap
  , externArrInfoMap  :: ExternArrInfoMap
  , externFunInfoMap  :: ExternFunInfoMap
  , externStrInfoMap  :: ExternStrInfoMap
  , triggerInfoMap    :: TriggerInfoMap
  , observerInfoMap   :: ObserverInfoMap }

--------------------------------------------------------------------------------

allocMetaTable :: C.Spec -> MetaTable
allocMetaTable spec =
  MetaTable { streamInfoMap    = streamInfoMap_
            , externVarInfoMap = externVarInfoMap_
            , externArrInfoMap = externArrInfoMap_
            , externFunInfoMap = externFunInfoMap_
            , externStrInfoMap = externStrInfoMap_
            , triggerInfoMap   = triggerInfoMap_
            , observerInfoMap  = observerInfoMap_ }

  where
  streamInfoMap_    = M.fromList $ map allocStream     (C.specStreams spec)
  externVarInfoMap_ = trace ("hahah"++(show $ map fst $ map allocExternVars (C.externVars spec))) $ M.fromList $ map allocExternVars (C.externVars spec)
  externArrInfoMap_ = M.fromList $ map allocExternArrs (C.externArrays spec)
  externFunInfoMap_ = M.fromList $ map allocExternFuns (C.externFuns spec)
  externStrInfoMap_ = M.fromList $ map allocExternStrs (C.externStructs spec)
  triggerInfoMap_   = M.fromList $ map allocTrigger    (C.specTriggers spec)
  observerInfoMap_  = M.fromList $ map allocObserver   (C.specObservers spec)
      
--------------------------------------------------------------------------------

allocStream :: C.Stream -> (C.Id, C.Stream)
allocStream strm = (C.streamId strm, strm)

--------------------------------------------------------------------------------

allocExternVars :: C.ExtVar -> (C.Tag, C.ExtVar)
allocExternVars var = (tagExtract $ C.externVarTag var, var)

--------------------------------------------------------------------------------

allocExternArrs :: C.ExtArray -> (C.Tag, C.ExtArray)
allocExternArrs arr = (tagExtract $ C.externArrayTag arr, arr)

--------------------------------------------------------------------------------

allocExternFuns :: C.ExtFun -> (C.Tag, C.ExtFun)
allocExternFuns fun = (tagExtract $ C.externFunTag fun, fun)

--------------------------------------------------------------------------------

allocExternStrs :: C.ExtStruct -> (C.Tag, C.ExtStruct)
allocExternStrs struct = (tagExtract $ C.externStructTag struct, trace "WASSUP" $ struct)

--------------------------------------------------------------------------------

allocTrigger :: C.Trigger -> (C.Name, TriggerInfo)
allocTrigger C.Trigger { C.triggerName  = name
                       , C.triggerGuard = guard
                       , C.triggerArgs  = args } 
  =
  let mkArgArgs :: C.UExpr -> [String]
      mkArgArgs C.UExpr { C.uExprExpr = e } = collectArgs e in
  let triggerInfo = 
        TriggerInfo { guardArgs      = collectArgs guard 
                    , triggerArgArgs = map mkArgArgs args } in
  (name, triggerInfo)

--------------------------------------------------------------------------------

allocObserver :: C.Observer -> (C.Name, ObserverInfo)
allocObserver C.Observer { C.observerName = name
                         , C.observerExpr = e } 
  = 
  let observerInfo = ObserverInfo { observerArgs = collectArgs e } in
  (name, observerInfo)

--------------------------------------------------------------------------------

-- Kinds of arguments to SBV functions
data Arg = Extern       C.Name C.Tag
         | ExternFun    C.Name C.Tag
         | ExternArr    C.Name C.Tag
         | ExternStruct C.Name C.Tag
         | Queue        C.Id
  deriving Eq

-- | Normal argument calls.
argToCall :: Arg -> [String]
argToCall (Extern name tag)       = [mkExtTmpTag name (Just tag)]
argToCall (ExternArr name tag)    = [mkExtTmpTag name (Just tag)]
argToCall (ExternFun name tag)    = [mkExtTmpTag name (Just tag)]
argToCall (ExternStruct name tag) = [mkExtTmpTag name (Just tag)]
argToCall (Queue id)              = [ mkQueueVar id 
                                    , mkQueuePtrVar id ]

--------------------------------------------------------------------------------

collectArgs :: C.Expr a -> [String]
collectArgs e = concatMap argToCall (nub $ c2Args e)

-- extCollectArgs :: C.Expr a -> [String]
-- extCollectArgs e = nub (concatMap extArgToCall (c2Args e))

--------------------------------------------------------------------------------

-- Gathers the names of the arguments to the SBV updateState function so that we
-- can construct the prototypes.

-- XXX It depends on gathering the the arguments in the same order that SBV uses
-- them.  SBV makes the arguments in the order that the cgInput and cgInputArr
-- are pushed into the SBVCodeGen.  However, there should really be an API for
-- getting the prototypes.

c2Args :: C.Expr a -> [Arg]
c2Args e = nub $ c2Args_ e

c2Args_ :: C.Expr a -> [Arg]
c2Args_ e0 = case e0 of
  C.Const _ _            -> [] 

  C.Drop _ _ id          -> [ Queue id ]
 
  C.Local _ _ _ e1 e2    -> c2Args_ e1 ++ c2Args_ e2

  C.Var _ _              -> []

  C.ExternVar _ name _ tag -> [trace ("jaaepoji"++show name) Extern name (tagExtract tag)]

  C.ExternFun   _ name args _ tag -> 
    (ExternFun name (tagExtract tag)) : 
      concatMap (\C.UExpr { C.uExprExpr = expr } 
                     -> c2Args expr) 
                args

  C.ExternArray _ _ name _ _ _ tag  -> trace ("~~~"++name) $ [ExternArr name (tagExtract tag)] 

  C.ExternStruct _ name sargs tag -> []--concatMap (c2Sargs_ name (tagExtract tag)) sargs

  C.GetField _ _ struct name ->
    case struct of
      C.ExternStruct _ sname sargs tag -> (ExternStruct name (tagExtract tag)) :
                                         concatMap (\(_, C.UExpr { C.uExprExpr = expr }) -> c2Args expr) sargs      
      _                                -> []

  C.Op1 _ e        -> c2Args_ e

  C.Op2 _ e1 e2    -> c2Args_ e1 ++ c2Args_ e2

  C.Op3 _ e1 e2 e3 -> c2Args_ e1 ++ c2Args_ e2 ++ c2Args_ e3

  C.Label _ _ e    -> c2Args_ e

--------------------------------------------------------------------------------

fixArgName :: C.Name -> Arg -> Arg
fixArgName sname (Extern name tag) = trace ("lhfea" ++ sname++"."++name) $ Extern (sname++"."++name) tag
fixArgName sname (ExternFun name tag) = trace ("lhfadea" ++ sname++"."++name) $ ExternFun (sname++"."++name) tag
fixArgName sname (ExternArr name tag) = trace ("lhwqfea" ++ sname++"."++name) $ ExternArr (sname++"."++name) tag
fixArgName sname (ExternStruct name tag) = trace ("lhfhrtea" ++ sname++"."++name) $ ExternStruct (sname++"."++name) tag
fixArgName sname (Queue id) = Queue id

--------------------------------------------------------------------------------
