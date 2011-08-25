--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification, GADTs #-}

-- | Generates the code around the SBV functions to hold the state-updates,
-- external variables, etc.

module Copilot.Compile.SBV.Driver
  ( driver
  , driverName
  ) where

import Prelude hiding (id)
import qualified Data.Map as M
import Data.List (intersperse)
import qualified System.IO as I
import Text.PrettyPrint.HughesPJ

import Copilot.Compile.SBV.MetaTable
import Copilot.Compile.SBV.Queue (QueueSize)
import Copilot.Compile.SBV.Common
import Copilot.Compile.SBV.Params

import qualified Copilot.Core as C
import qualified Copilot.Core.Type.Show as C (showWithType)
import Copilot.Compile.Header.C99 (c99HeaderName)

--------------------------------------------------------------------------------

driverName :: Params -> String
driverName params = withPrefix (prefix params) "driver" ++ ".c"

--------------------------------------------------------------------------------

-- | Define a C function.
mkFunc :: String -> Doc -> Doc
mkFunc fnName doc =
     text "void" <+> text fnName
       <> lparen <> text "void" <> rparen <+> lbrace
  $$ nest 2 doc $$ nest 0 rbrace

mkArgs :: [Doc] -> Doc
mkArgs args = hsep (punctuate comma args)

mkFuncCall :: String -> [Doc] -> Doc
mkFuncCall f args = text f <> lparen <> mkArgs args <> rparen

--------------------------------------------------------------------------------

driver :: Params -> MetaTable -> C.Spec -> String -> String -> IO ()
driver params meta (C.Spec streams observers _) dir fileName = do
  let filePath = dir ++ '/' : driverName params
  h <- I.openFile filePath I.WriteMode
  let wr doc = I.hPutStrLn h (mkStyle doc)

  wr (    text "/*"
      <+> text "Driver for SBV program generated from Copilot."
      <+> text "*/")
  wr (text "/*" <+> text "Edit as you see fit" <+> text "*/")
  wr (text "")

  wr (text "#include <inttypes.h>")
  wr (text "#include <stdbool.h>")
  wr (text "#include <stdint.h>")
  wr (text "#include <stdio.h>")
  wr (text "#include" <+> doubleQuotes (text fileName <> text ".h"))
  wr (text "#include" <+> doubleQuotes (text $ c99HeaderName (prefix params)))
  wr (text "")

  wr (declObservers (prefix params) observers)
  wr (text "")

  wr (varDecls meta)
  wr (text "")

  wr copilot
  wr (text "")
  wr driverFn

  where
  mkStyle :: Doc -> String
  mkStyle = renderStyle (style {lineLength = 80})

  driverFn :: Doc
  driverFn =
    mkFunc (withPrefix (prefix params) "step")
           (   mkFuncCall sampleExtsF    [] <> semi
            $$ mkFuncCall updateStatesF  [] <> semi
            $$ mkFuncCall observersF     [] <> semi
            $$ mkFuncCall triggersF      [] <> semi
            $$ mkFuncCall updateBuffersF [] <> semi
            $$ mkFuncCall updatePtrsF    [] <> semi)

  copilot = vcat $ intersperse (text "")
    [ sampleExts meta
    , updateStates streams
    , fireTriggers meta
    , updateObservers params meta
    , updateBuffers meta
    , updatePtrs meta ]

--------------------------------------------------------------------------------

-- Declare gloabl variables.

data Decl = Decl { retT    :: Doc
                 , declVar :: Doc
                 , initVal :: Doc }

varDecls :: MetaTable -> Doc
varDecls meta = vcat $ map varDecl (getVars meta)

  where
  getVars :: MetaTable -> [Decl] 
  getVars MetaTable { streamInfoMap = streams 
                    , externInfoMap = externs } = 
       map getTmpStVars (M.toList streams)
    ++ map getQueueVars (M.toList streams)
    ++ map getQueuePtrVars (map fst $ M.toList streams)
    ++ map getExtVars (M.toList externs)

  getTmpStVars :: (C.Id, StreamInfo) -> Decl
  getTmpStVars (id, StreamInfo { streamInfoType  = t
                               , streamInfoQueue = que }) = 
    Decl (retType t) (text $ mkTmpStVar id) getFirst
    where 
    -- ASSUME queue is nonempty!
    getFirst = text (cShow $ C.showWithType t (headErr que))
    headErr [] = error "Error in Copilot.Compile.SBV.Driver: queue nonempty"
    headErr xs = head xs
  
  getQueueVars :: (C.Id, StreamInfo) -> Decl
  getQueueVars (id, StreamInfo { streamInfoType = t
                               , streamInfoQueue = que }) =
    Decl (retType t) 
         (text (mkQueueVar id) <> lbrack <> int (length que) <> rbrack)
         getInits
    where 
    getInits = lbrace <+> vals <+> rbrace
      where 
      vals = hcat $ punctuate (comma <> text " ") 
                              (map (text . cShow . C.showWithType t) que)

  getQueuePtrVars :: C.Id -> Decl
  getQueuePtrVars id = 
    Decl (retType queSize) (text $ mkQueuePtrVar id) (int 0)
    where 
    queSize :: C.Type QueueSize
    queSize = C.typeOf 

  getExtVars :: (C.Name, C.UType) -> Decl
  getExtVars (var, C.UType { C.uTypeType = t }) = 
    Decl (retType t) (text $ mkExtTmpVar var) (int 0)

  varDecl :: Decl -> Doc
  varDecl Decl { retT = t, declVar = v, initVal = i } =
    t <+> v <+> equals <+> i <> semi

  cShow :: String -> String
  cShow "True"  = show (1::Int)
  cShow "False" = show (0::Int)
  cShow x       = x

--------------------------------------------------------------------------------

declObservers :: Maybe String -> [C.Observer] -> Doc
declObservers prfx = vcat . map declObserver

  where
  declObserver :: C.Observer -> Doc
  declObserver
    C.Observer
      { C.observerName     = name
      , C.observerExprType = t } =
    retType t <+> text (withPrefix prfx name) <> text ";"

--------------------------------------------------------------------------------

sampleExts :: MetaTable -> Doc
sampleExts MetaTable { externInfoMap = extMap } =
  mkFunc sampleExtsF $ vcat $ map sampleExt ((fst . unzip . M.toList) extMap)

  where
  sampleExt :: C.Name -> Doc
  sampleExt name = text (mkExtTmpVar name) <+> equals <+> text name <> semi

--------------------------------------------------------------------------------

updateStates :: [C.Stream] -> Doc
updateStates streams =
  mkFunc updateStatesF $ vcat $ map updateSt streams

  where
  -- tmp_X = updateState(arg0, arg1, ... );
  updateSt :: C.Stream -> Doc
  updateSt C.Stream { C.streamId   = id
                    , C.streamExpr = e } =
    text (mkTmpStVar id) <+> equals
      <+> mkFuncCall (mkUpdateStFn id)
                     (map text getArgs)
      <> semi
    where
    getArgs :: [String]
    getArgs = concatMap argToCall (c2Args e)

--------------------------------------------------------------------------------

updateObservers :: Params -> MetaTable -> Doc
updateObservers params MetaTable { observerInfoMap = observers } =

  mkFunc observersF $ vcat $ map updateObsv (M.toList observers)

  where
  updateObsv :: (C.Name, ObserverInfo) -> Doc
  updateObsv (name, ObserverInfo { observerArgs = args }) =
    text (withPrefix (prefix params) name) <+> text "=" <+>
    mkFuncCall (mkObserverFn name) (map text args) <> semi

--------------------------------------------------------------------------------

fireTriggers :: MetaTable -> Doc
fireTriggers MetaTable { triggerInfoMap = triggers } =

  mkFunc triggersF $ vcat $ map fireTrig (M.toList triggers)

  where
  -- if (guard) trigger(args);
  fireTrig :: (C.Name, TriggerInfo) -> Doc
  fireTrig (name, TriggerInfo { guardArgs      = gArgs
                              , triggerArgArgs = argArgs }) =
    text "if" <+> lparen <> guardF <> rparen <+>
      mkFuncCall name (map mkArg (mkTriggerArgIdx argArgs)) <> semi
    where
    guardF :: Doc
    guardF = mkFuncCall (mkTriggerGuardFn name) (map text gArgs)
    mkArg :: (Int, [String]) -> Doc
    mkArg (i, args) =
      mkFuncCall (mkTriggerArgFn i name) (map text args)

--------------------------------------------------------------------------------

updateBuffers :: MetaTable -> Doc
updateBuffers MetaTable { streamInfoMap = strMap } =
  mkFunc updateBuffersF $ vcat $ map updateBuf (M.toList strMap)

  where
  updateBuf :: (C.Id, StreamInfo) -> Doc
  updateBuf (id, _) =
    updateFunc (mkQueueVar id) (mkQueuePtrVar id) (mkTmpStVar id)

  -- queue_strX[ptr] = newVal;
  updateFunc :: String -> String -> String -> Doc
  updateFunc que ptr tmp =
    text que <> lbrack <> text ptr <> rbrack <+> equals <+> text tmp <> semi

--------------------------------------------------------------------------------

updatePtrs :: MetaTable -> Doc
updatePtrs MetaTable { streamInfoMap = strMap } =
  mkFunc updatePtrsF $ vcat $ map varAndUpdate (M.toList strMap)

  where 
  varAndUpdate :: (C.Id, StreamInfo) -> Doc
  varAndUpdate (id, StreamInfo { streamInfoQueue = que }) =
    updateFunc (fromIntegral $ length que) (mkQueuePtrVar id)

  -- idx = (idx + 1) % queueSize;
  updateFunc :: QueueSize -> String -> Doc
  updateFunc sz ptr =
    text ptr <+> equals 
      <+> lparen <> text ptr <+> text "+" <+> int 1 <> rparen 
      <+> text "%" <+> int (fromIntegral sz) <> semi

--------------------------------------------------------------------------------

sampleExtsF, triggersF, observersF, updatePtrsF, updateBuffersF, updateStatesF :: String
updatePtrsF    = "updatePtrs"
updateBuffersF = "updateBuffers"
updateStatesF  = "updateStates"
triggersF      = "fireTriggers"
observersF     = "updateObservers"
sampleExtsF    = "sampleExts"

--------------------------------------------------------------------------------

retType :: C.Type a -> Doc
retType t = text $
  case t of
    C.Bool  -> "SBool"

    C.Int8  -> "SInt8"
    C.Int16 -> "SInt16"
    C.Int32 -> "SInt32"
    C.Int64 -> "SInt64"

    C.Word8  -> "SWord8"
    C.Word16 -> "SWord16"
    C.Word32 -> "SWord32"
    C.Word64 -> "SWord64"

    _          -> error "Error in retType: non-SBV type."
