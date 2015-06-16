--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification, GADTs #-}

-- | Generates the code around the SBV functions to hold the state-updates,
-- external variables, etc.  Note: this just creates calls to SBV-generated
-- functions, it does not create them!  (Use the names from Common.hs to ensure
-- agreement on names.)

module Copilot.Compile.SBV.Driver
  ( driver
  , driverName
  ) where

import Prelude hiding (id)
import qualified Data.Map as M
import Data.List (intersperse, concat)
import qualified System.IO as I

import Copilot.Compile.SBV.MetaTable
import Copilot.Compile.SBV.Queue (QueueSize)
import Copilot.Compile.SBV.Common
import Copilot.Compile.SBV.Params

import qualified Copilot.Core as C
import qualified Copilot.Core.Type.Show as C (showWithType, ShowType(..))
import Copilot.Compile.Header.C99 (c99HeaderName)

--------------------------------------------------------------------------------

driverName :: Params -> String
driverName params = withPrefix (prefix params) "driver" ++ ".c"

punctuateS :: String -> [String] -> [String]
punctuateS p l = map (\x -> (x ++ p)) l

--------------------------------------------------------------------------------

-- | Define a C function.
mkFunc :: String -> String -> String
mkFunc fnName doc =
     "void " ++ fnName ++ "(" ++ "void" ++ ")\n{\n"
  ++ doc ++ "\n}"

mkArgs :: [String] -> String
mkArgs args = concat (punctuateS "," args)

-- | Call a C function.
mkFuncCall :: String -> [String] -> String
mkFuncCall f args = f ++ "(" ++ (mkArgs args) ++ ")"

--------------------------------------------------------------------------------

driver :: Params -> MetaTable -> C.Spec -> String -> String -> IO ()
driver params meta (C.Spec streams observers _ _) dir fileName = do
  let filePath = dir ++ '/' : driverName params
  h <- I.openFile filePath I.WriteMode
  let ws strin = I.putStrLn (strin)

  ws (    "/*\n"
      ++ "Driver for SBV program generated from Copilot.\n"
      ++ "*/")
  ws ("/*" ++ "Edit as you see fit" ++ "*/")
  ws ("\n")

  ws ("#include <inttypes.h>")
  ws ("#include <stdbool.h>")
  ws ("#include <stdint.h>")
  ws ("#include <stdio.h>")
  ws ("#include" ++ "\"" ++ (fileName ++ ".h") ++ "\"")
  ws ("#include" ++ "\"" ++ (c99HeaderName (prefix params)) ++ "\"")
  ws ("")

  ws ("/* Observers */")
  ws (declObservers (prefix params) observers)
  ws ("")

  ws ("/* Variables */")

  ws (varDecls meta)
  ws (writeACSLqueues meta)
  ws ("")

  ws copilot
  ws ("")
  ws driverFn

  where
  
  driverFn :: String
  driverFn =
    mkFunc (withPrefix (prefix params) "step")
           (   mkFuncCall sampleExtsF    [] ++ ";\n"
            ++ mkFuncCall triggersF      [] ++ ";\n"
            ++ mkFuncCall observersF     [] ++ ";\n"
            ++ mkFuncCall updateStatesF  [] ++ ";\n"
            ++ mkFuncCall updateBuffersF [] ++ ";\n"
            ++ mkFuncCall updatePtrsF    [] ++ ";\n"
           )

  copilot = concat $ intersperse ("\n")
    [ sampleExts meta
    , fireTriggers meta
    , updateObservers params meta
    , updateStates streams
    , updateBuffers meta
    , updatePtrs meta 
    ]

--------------------------------------------------------------------------------

-- Declare global variables.

data Decl = Decl { retT    :: String
                 , declVar :: String
                 , initVal :: String }

varDecls :: MetaTable -> String
varDecls meta = concat $ intersperse ("\n") (map varDecl (getVars meta))

  where
  getVars :: MetaTable -> [Decl] 
  getVars MetaTable { streamInfoMap    = streams 
                    , externVarInfoMap = externs 
                    , externArrInfoMap = externArrs
                    , externFunInfoMap = externFuns }
    = 
       map getTmpStVars strLst
    ++ map getQueueVars strLst
    ++ map getQueuePtrVars (map fst strLst)
    ++ map getExtVars (M.toList externs)
    ++ map getExtArrs (M.toList externArrs)
    ++ map getExtFuns (M.toList externFuns)
    where
    strLst = M.toList streams

  getTmpStVars :: (C.Id, C.Stream) -> Decl
  getTmpStVars (id, C.Stream { C.streamExprType  = t
                               , C.streamBuffer = que }) = 
    Decl (retType t) (mkTmpStVar id) getFirst
    where 
    -- ASSUME queue is nonempty!
    getFirst = (cShow $ C.showWithType C.Haskell t (headErr que))
    headErr [] = C.impossible "headErr" "copilot-sbv"
    headErr xs = head xs
  
  getQueueVars :: (C.Id, C.Stream) -> Decl
  getQueueVars (id, C.Stream { C.streamExprType = t
                             , C.streamBuffer = que }) =
    Decl (retType t) 
         ((mkQueueVar id) ++ "[" ++ (show (length que)) ++ "]")
         getInits
    where 
    getInits = "{" ++ vals ++ "}"
      where 
      vals = concat $ punctuateS (",\n") 
                              (map (cShow . C.showWithType C.Haskell t) 
                                   que)

  getQueuePtrVars :: C.Id -> Decl
  getQueuePtrVars id = 
    Decl (retType queSize) (mkQueuePtrVar id) (show 0)
    where 
    queSize :: C.Type QueueSize
    queSize = C.typeOf 

  getExtVars :: (C.Name, C.ExtVar) -> Decl
  getExtVars (var, C.ExtVar _ (C.UType { C.uTypeType = t })) = 
    Decl (retType t) (mkExtTmpVar var) (show 0)

  getExtArrs :: (Int, C.ExtArray) -> Decl 
  getExtArrs (_, C.ExtArray { C.externArrayName     = name
                            , C.externArrayElemType = t 
                            , C.externArrayTag      = tag  })
    =
    Decl (retType t) (mkExtTmpTag name tag) (show 0)

  getExtFuns :: (Int, C.ExtFun) -> Decl
  getExtFuns (_, C.ExtFun { C.externFunName = name
                          , C.externFunType = t
                          , C.externFunTag  = tag  })
    =
    Decl (retType t) (mkExtTmpTag name tag) (show 0)

  varDecl :: Decl -> String
  varDecl Decl { retT = t, declVar = v, initVal = i } =
    t ++ " " ++ v ++ " = " ++ i ++ ";"

  cShow :: String -> String
  cShow "True"  = show (1::Int)
  cShow "False" = show (0::Int)
  cShow x       = x

--------------------------------------------------------------------------------

declObservers :: Maybe String -> [C.Observer] -> String
declObservers prfx ll = concat $ intersperse ("\n") (map declObserver ll)

  where
  declObserver :: C.Observer -> String
  declObserver
    C.Observer
      { C.observerName     = name
      , C.observerExprType = t } =
    retType t ++ (withPrefix prfx name) ++ ";"

--------------------------------------------------------------------------------

sampleExts :: MetaTable -> String
sampleExts MetaTable { externVarInfoMap = extVMap
                     , externArrInfoMap = extAMap
                     , externFunInfoMap = extFMap } 
  =
  -- Arrays and functions have to come after vars.  This is because we may use
  -- the assignment of extVars in the definition of extArrs.  The Analyzer.hs
  -- copilot-core prevents arrays or functions from being used in arrays or
  -- functions.
  extACSL ++ "\n" ++ (mkFunc sampleExtsF $ concat $ intersperse ("\n") (extVars ++ extArrs ++ extFuns))

  where
  extACSL = concat $ intersperse ("\n") ["/*@",concat $ intersperse ("\n") (sampleVExtACSL extVMap), concat $ intersperse ("\n") (sampleAExtACSL extAMap), concat $ intersperse ("\n") (sampleFExtACSL extFMap) ,"*/"]
  extVars = map sampleVExt ((fst . unzip . M.toList) extVMap)
  extArrs = map sampleAExt (M.toList extAMap)
  extFuns = map sampleFExt (M.toList extFMap)

--------------------------------------------------------------------------------

-- Variables

sampleVExtACSL :: M.Map C.Name C.ExtVar -> [String]
sampleVExtACSL extVMap = 
  (map sampleVExtACSL1 ((fst . unzip . M.toList) extVMap)) ++ (map sampleVExtACSL2 ((fst . unzip . M.toList) extVMap))
sampleVExtACSL1 :: C.Name -> String
sampleVExtACSL1 name = 
  "assigns " ++ (mkExtTmpVar name) ++ ";"
sampleVExtACSL2 :: C.Name -> String
sampleVExtACSL2 name = 
  "//ensures " ++ (mkExtTmpVar name) ++ " == " ++ name ++ ";"

sampleVExt :: C.Name -> String
sampleVExt name = 
  (mkExtTmpVar name) ++ " = " ++ name ++ ";"

--------------------------------------------------------------------------------
-- Arrays

-- Currenty, Analyze.hs in copilot-language forbids recurssion in external
-- arrays or functions (i.e., an external array can't use another external array
-- to compute it's index).
sampleAExtACSL :: M.Map C.Tag C.ExtArray -> [String]
sampleAExtACSL extAMap = 
  (map sampleAExtACSL1 (M.toList extAMap)) ++ (map sampleAExtACSL2 (M.toList extAMap))

sampleAExtACSL1 :: (Int, C.ExtArray) -> String
sampleAExtACSL1 (_, C.ExtArray { C.externArrayName = name
                          , C.externArrayIdx = idx 
                          , C.externArrayTag = t     })
  = 
  "assigns " ++ (mkExtTmpTag name t) ++ ";"


sampleAExtACSL2 :: (Int, C.ExtArray) -> String
sampleAExtACSL2 (_, C.ExtArray { C.externArrayName = name
                          , C.externArrayIdx = idx 
                          , C.externArrayTag = t     })
  = 
  "//ensures " ++ (mkExtTmpTag name t) ++ " == " ++ ("tmp_"++name) ++ ";"



sampleAExt :: (Int, C.ExtArray) -> String
sampleAExt (_, C.ExtArray { C.externArrayName = name
                          , C.externArrayIdx = idx 
			  , C.externArrayElemType = tttt 
                          , C.externArrayTag = t     })
  = (retType tttt) ++ (" tmp_"++name) ++ " = " ++ arrIdx name idx ++ "\n" ++
  (mkExtTmpTag name t) ++ " = " ++ ("tmp_"++name) ++ ";"
 
  where 
  arrIdx :: C.Name -> C.Expr a -> String
  arrIdx name' e = name' ++ "[" ++ idxFCall e ++ "];"

  -- Ok, because the analyzer disallows arrays or function calls in index
  -- expressions, and we assign all variables before arrays.
  idxFCall :: C.Expr a -> String
  idxFCall e = 
    mkFuncCall (mkExtArrFn name) (collectArgs e)

--------------------------------------------------------------------------------

sampleFExtACSL :: M.Map C.Tag C.ExtFun -> [String]
sampleFExtACSL extFMap =
  (map sampleFExtACSL1 (M.toList extFMap)) ++ (map sampleFExtACSL2 (M.toList extFMap))

sampleFExtACSL1 :: (Int, C.ExtFun) -> String
sampleFExtACSL1 (_, C.ExtFun { C.externFunName = name
                        , C.externFunArgs = args 
                        , C.externFunTag  = tag  })
  = 
  "assigns " ++ (mkExtTmpTag name tag) ++";"

sampleFExtACSL2 :: (Int, C.ExtFun) -> String
sampleFExtACSL2 (_, C.ExtFun { C.externFunName = name
                        , C.externFunArgs = args 
                        , C.externFunTag  = tag  })
  = 
  "//ensures" ++ (mkExtTmpTag name tag) ++ " == " ++ ("tmp_"++name) ++ ";"


-- External functions
sampleFExt :: (Int, C.ExtFun) -> String
sampleFExt (_, C.ExtFun { C.externFunName = name
                        , C.externFunArgs = args 
                        , C.externFunType = tttt
                        , C.externFunTag  = tag  })
  = 
  (retType tttt) ++ (" tmp_"++ name) ++ " = " ++ name ++ "("
    ++ concat (punctuateS ", " $ map mkArgCall (zip [(0 :: Int) ..] args))
    ++ ");\n" ++
  (mkExtTmpTag name tag) ++ " = " ++ ("tmp_"++name) ++ ";"

     where
     mkArgCall :: (Int, C.UExpr) -> String
     mkArgCall (i, C.UExpr { C.uExprExpr = e }) = 
       mkFuncCall (mkExtFunArgFn i name tag) (collectArgs e)

--------------------------------------------------------------------------------

updateStates :: [C.Stream] -> String
updateStates streams = ("/*@\n") ++ (concat $ intersperse ("\n") $ map updateStACSL (streams)) ++ ("*/\n") ++
  (mkFunc updateStatesF $ concat $ intersperse ("\n") $ map updateSt streams)
  where
  updateStACSL :: C.Stream -> String
  updateStACSL C.Stream { C.streamId   = id
                    , C.streamExpr = e } =
    "assigns " ++ (mkTmpStVar id) ++ ";"

  updateSt :: C.Stream -> String
  updateSt C.Stream { C.streamId   = id
                    , C.streamExpr = e } =
    (mkTmpStVar id) ++ " = " ++ (mkFuncCall (mkUpdateStFn id) (collectArgs e)) ++ ";"

--------------------------------------------------------------------------------

updateObservers :: Params -> MetaTable -> String
updateObservers params MetaTable { observerInfoMap = observers } 
  = ("/*@\n") ++ (concat $ intersperse ("\n") $ map updateObsvACSL (M.toList observers)) ++ "*/\n" ++
  (mkFunc observersF $ concat $ intersperse ("\n") $ map updateObsv (M.toList observers))
  where
  updateObsvACSL :: (C.Name, ObserverInfo) -> String
  updateObsvACSL (name, ObserverInfo { observerArgs = args }) =
    "assigns " ++ (withPrefix (prefix params) name) ++ ";"

  updateObsv :: (C.Name, ObserverInfo) -> String
  updateObsv (name, ObserverInfo { observerArgs = args }) =
    (withPrefix (prefix params) name) ++ " = " ++
    (mkFuncCall (mkObserverFn name) (args)) ++ ";"

--------------------------------------------------------------------------------

fireTriggers :: MetaTable -> String
fireTriggers MetaTable { triggerInfoMap = triggers } 
  = "/*@\n assigns \\nothing; \n*/\n" ++
  (mkFunc triggersF $ concat $ intersperse ("\n") $ map fireTrig (M.toList triggers))

  where
  -- if (guard) trigger(args);
  fireTrig :: (C.Name, TriggerInfo) -> String
  fireTrig (name, TriggerInfo { guardArgs      = gArgs
                              , triggerArgArgs = argArgs }) 
    = 
    "if (" ++ guardF ++ ")\n{\n" ++ f ++ "}\n"

    where
    f = name ++ "(" ++ 
          (concat $ (punctuateS ",\n" $ map mkArg (mkArgIdx argArgs)) )
          ++ ");"

    guardF :: String
    guardF = mkFuncCall (mkTriggerGuardFn name) (gArgs)

    mkArg :: (Int, [String]) -> String
    mkArg (i, args) = mkFuncCall (mkTriggerArgFn i name) (args)

--------------------------------------------------------------------------------

writeACSLqueues :: MetaTable -> String
writeACSLqueues MetaTable { streamInfoMap = strMap } =
  concat $ intersperse ("\n") $ ("/*ACSL following*/\n/*@"):(map varAndUpdate (M.toList strMap)) ++ [( "*/")]

  where 
  varAndUpdate :: (C.Id, C.Stream) -> String
  varAndUpdate (id, C.Stream { C.streamBuffer = que }) =
    updateFunc (mkQueueVar id) (fromIntegral $ length que) (mkQueuePtrVar id)

  -- idx = (idx + 1) % queueSize;
  updateFunc :: String -> QueueSize -> String -> String
  updateFunc que sz ptr =
    ("global invariant a_bound_"++ ptr ++": ") ++ ptr ++ " < " ++ show (fromIntegral sz) ++ ";" ++
    ("\nglobal invariant a_pos_"++ ptr ++": ") ++ ptr ++ ">= 0;" ++
    ("\nglobal invariant a_valid_"++ ptr ++": \\valid (") ++ que ++ " + (0.." ++ show ((fromIntegral sz) - 1) ++" ));"

--------------------------------------------------------------------------------

updateBuffers :: MetaTable -> String
updateBuffers MetaTable { streamInfoMap = strMap } 
  = ("/*@\n ") ++ (concat $ intersperse ("\n") $ map updateBufACSL (M.toList strMap)) ++ "*/\n" ++
  (mkFunc updateBuffersF $ concat $ intersperse ("\n") $ map updateBuf (M.toList strMap))

  where

  updateBufACSL :: (C.Id, C.Stream) -> String
  updateBufACSL (id, _) =
    updateFuncACSL (mkQueueVar id) (mkQueuePtrVar id) (mkTmpStVar id)

  -- queue_strX[ptr] = newVal;
  updateFuncACSL :: String -> String -> String -> String
  updateFuncACSL que ptr tmp =
    "assigns " ++ que ++ "[" ++ ptr ++ "];\n" ++
    ("//ensures " ++ que ++ "[" ++ ptr ++ "] = " ++ tmp ++ ";")

  updateBuf :: (C.Id, C.Stream) -> String
  updateBuf (id, _) =
    updateFunc (mkQueueVar id) (mkQueuePtrVar id) (mkTmpStVar id)

  -- queue_strX[ptr] = newVal;
  updateFunc :: String -> String -> String -> String
  updateFunc que ptr tmp =
    que ++ "[" ++ ptr ++ "] = " ++ tmp ++ ";"

--------------------------------------------------------------------------------

updatePtrs :: MetaTable -> String
updatePtrs MetaTable { streamInfoMap = strMap } =
  ("/*@\n") ++ (concat $ intersperse ("\n") $ map varAndUpdateACSL (M.toList strMap)) ++ "*/\n" ++
  (mkFunc updatePtrsF $ concat $ intersperse ("\n") $ map varAndUpdate (M.toList strMap))

  where 

  varAndUpdateACSL :: (C.Id, C.Stream) -> String
  varAndUpdateACSL (id, C.Stream { C.streamBuffer = que }) =
    updateFuncACSL (fromIntegral $ length que) (mkQueuePtrVar id)

  -- idx = (idx + 1) % queueSize;
  updateFuncACSL :: QueueSize -> String -> String
  updateFuncACSL sz ptr =
    "assigns " ++ ptr ++ (";\n ensures " ++ ptr ++ " = (" ++ ptr ++ " + 1) %" ++ show (fromIntegral sz) ++ ";")


  varAndUpdate :: (C.Id, C.Stream) -> String
  varAndUpdate (id, C.Stream { C.streamBuffer = que }) =
    updateFunc (fromIntegral $ length que) (mkQueuePtrVar id)

  -- idx = (idx + 1) % queueSize;
  updateFunc :: QueueSize -> String -> String
  updateFunc sz ptr =
    ptr ++ " = (" ++ ptr ++ " + 1) % " ++ show (fromIntegral sz) ++ ";"

--------------------------------------------------------------------------------

sampleExtsF, triggersF, observersF, updatePtrsF :: String
updateBuffersF, updateStatesF :: String
updatePtrsF    = "updatePtrs"
updateBuffersF = "updateBuffers"
updateStatesF  = "updateStates"
triggersF      = "fireTriggers"
observersF     = "updateObservers"
sampleExtsF    = "sampleExts"

--------------------------------------------------------------------------------

retType :: C.Type a -> String
retType t = 
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

    _          -> C.badUsage "You've tried to compile a Copilot program to SBV with a type SBV does not support.  SBV does not support floats or doubles.  To compile programs using these types, use the copilot-c99 (Atom) backend.  See README.md for more information."
