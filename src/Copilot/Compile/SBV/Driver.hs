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
import Data.List (intersperse)
import qualified System.IO as I
import Text.PrettyPrint.HughesPJ
import System.Directory

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

--------------------------------------------------------------------------------

-- | Define a C function.
mkFunc :: String -> Doc -> Doc
mkFunc fnName doc =
     text "void" <+> text fnName
       <> lparen <> text "void" <> rparen <+> lbrace
  $$ nest 2 doc $$ nest 0 rbrace

mkArgs :: [Doc] -> Doc
mkArgs args = hsep (punctuate comma args)

-- | Call a C function.
mkFuncCall :: String -> [Doc] -> Doc
mkFuncCall f args = text f <> lparen <> mkArgs args <> rparen

--------------------------------------------------------------------------------

driver :: Params -> MetaTable -> C.Spec -> String -> String -> IO ()
driver params meta (C.Spec streams observers _ _) dir fileName = do
  let filePath = dir ++ '/' : driverName params
  let wr doc = I.appendFile filePath ((mkStyle doc) ++ "\n")
  wr (text "a")
  removeFile filePath

  wr (    text "/*"
      <+> text "Driver for SBV program generated from Copilot."
      <+> text "*/")
  wr (text "/*" <+> text "Edit as you see fit" <+> text "*/")
  wr (text "")

  wr (text "#include" <+> doubleQuotes (text fileName <> text ".h"))
  wr (text "#include" <+> doubleQuotes (text $ c99HeaderName (prefix params)))
  wr (text "")

  wr (text "/* Observers */")
  wr (declObservers (prefix params) observers)
  wr (text "")

  wr (text "/* Variables */")

  wr (varDecls meta)
--  wr (writeACSLqueues meta)
  wr (text "")

  wr copilot

  wr (text "/* Idents */\n")
  wr (text "/*@\n assigns \\nothing;\n */\nSBool ident_bool(SBool a) {return a;}")
  wr (text "/*@\n assigns \\nothing;\n */\nSWord8 ident_word8(SWord8 a) {return a;}")
  wr (text "/*@\n assigns \\nothing;\n */\nSWord16 ident_word16(SWord16 a) {return a;}")
  wr (text "/*@\n assigns \\nothing;\n */\nSWord32 ident_word32(SWord32 a) {return a;}")
  wr (text "/*@\n assigns \\nothing;\n */\nSWord64 ident_word64(SWord64 a) {return a;}")
  wr (text "/*@\n assigns \\nothing;\n */\nSInt8 ident_int8(SInt8 a) {return a;}")
  wr (text "/*@\n assigns \\nothing;\n */\nSInt16 ident_int16(SInt16 a) {return a;}")
  wr (text "/*@\n assigns \\nothing;\n */\nSInt32 ident_int32(SInt32 a) {return a;}")
  wr (text "/*@\n assigns \\nothing;\n */\nSInt64 ident_int64(SInt64 a) {return a;}")
  wr (text "/*@\n assigns \\nothing;\n */\nSFloat ident_float(SFloat a) {return a;}")
  wr (text "/*@\n assigns \\nothing;\n */\nSDouble ident_double(SDouble a) {return a;}")

  wr (text "")
  wr driverFn
  wr (text "")
  wr testFn

  where
  mkStyle :: Doc -> String
  mkStyle = renderStyle (style {lineLength = 80})

  driverFn :: Doc
  driverFn =
    mkFunc (withPrefix (prefix params) "step")
           (   mkFuncCall sampleExtsF    [] <> semi
            $$ mkFuncCall triggersF      [] <> semi
            $$ mkFuncCall observersF     [] <> semi
            $$ mkFuncCall updateStatesF  [] <> semi
            $$ mkFuncCall updateBuffersF [] <> semi
            $$ mkFuncCall updatePtrsF    [] <> semi
           )
  testFn :: Doc
  testFn =
    mkFunc (withPrefix (prefix params) "testing")
           (   text "for(;;) step()"<> semi
           )

  copilot = vcat $ intersperse (text "")
    [ sampleExts meta
    , fireTriggers meta
    , updateObservers params meta
    , updateStates streams
    , updateBuffers meta
    , updatePtrs meta
    ]

--------------------------------------------------------------------------------

-- Declare global variables.

data Decl = Decl { retT    :: Doc
                 , declVar :: Doc
                 , initVal :: Doc }

varDecls :: MetaTable -> Doc
varDecls meta = vcat $ map varDecl (getVars meta)

  where
  getVars :: MetaTable -> [Decl]
  getVars MetaTable { streamInfoMap    = streams
                    , externVarInfoMap = externs
                    , externArrInfoMap = externArrs
                    , externVecInfoMap = externVecs
                    , externMatInfoMap = externMats
                    , externFunInfoMap = externFuns }
    =
       map getTmpStVars strLst
    ++ map getQueueVars strLst
    ++ map getQueuePtrVars (map fst strLst)
    ++ map getExtVars (M.toList externs)
    ++ map getExtArrs (M.toList externArrs)
    ++ map getExtVecs (M.toList externVecs)
    ++ map getExtMats (M.toList externMats)
    ++ map getExtFuns (M.toList externFuns)
    where
    strLst = M.toList streams

  getTmpStVars :: (C.Id, C.Stream) -> Decl
  getTmpStVars (id, C.Stream { C.streamExprType  = t
                               , C.streamBuffer = que }) =
    Decl (retType t) (text $ mkTmpStVar id) getFirst
    where
    -- ASSUME queue is nonempty!
    getFirst = text (cShow $ C.showWithType C.Haskell t (headErr que))
    headErr [] = C.impossible "headErr" "copilot-sbv"
    headErr xs = head xs

  getQueueVars :: (C.Id, C.Stream) -> Decl
  getQueueVars (id, C.Stream { C.streamExprType = t
                             , C.streamBuffer = que }) =
    Decl (retType t)
         (text (mkQueueVar id) <> lbrack <> int (length que) <> rbrack)
         getInits
    where
    getInits = lbrace <+> vals <+> rbrace
      where
      vals = hcat $ punctuate (comma <> text " ")
                              (map (text . cShow . C.showWithType C.Haskell t)
                                   que)

  getQueuePtrVars :: C.Id -> Decl
  getQueuePtrVars id =
    Decl (retType queSize) (text $ mkQueuePtrVar id) (int 0)
    where
    queSize :: C.Type QueueSize
    queSize = C.typeOf

  getExtVars :: (C.Name, C.ExtVar) -> Decl
  getExtVars (var, C.ExtVar _ (C.UType { C.uTypeType = t })) =
    Decl (retType t) (text $ mkExtTmpVar var) (int 0)

  getExtArrs :: (Int, C.ExtArray) -> Decl
  getExtArrs (_, C.ExtArray { C.externArrayName     = name
                            , C.externArrayElemType = t
                            , C.externArrayTag      = tag  })
    =
    Decl (retType t) (text $ mkExtTmpTag name tag) (int 0)

  getExtVecs :: (Int, C.ExtVector) -> Decl
  getExtVecs (_, C.ExtVector { C.externVectorName     = name
                             , C.externVectorElemType = t
                             , C.externVectorTag      = tag  })
    =
    Decl (retType t) (text $ mkExtTmpTag name tag) (int 0)

  getExtMats :: (Int, C.ExtMatrix) -> Decl
  getExtMats (_, C.ExtMatrix { C.externMatrixName     = name
                             , C.externMatrixElemType = t
                             , C.externMatrixTag      = tag  })
    =
    Decl (retType t) (text $ mkExtTmpTag name tag) (int 0)

  getExtFuns :: (Int, C.ExtFun) -> Decl
  getExtFuns (_, C.ExtFun { C.externFunName = name
                          , C.externFunType = t
                          , C.externFunTag  = tag  })
    =
    Decl (retType t) (text $ mkExtTmpTag name tag) (int 0)

  varDecl :: Decl -> Doc
  varDecl Decl { retT = t, declVar = v, initVal = i } =
    text "static"<+> t <+> v <+> equals <+> i <> semi

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
    retType t <+> text (withPrefix prfx name) <> semi

--------------------------------------------------------------------------------

sampleExts :: MetaTable -> Doc
sampleExts MetaTable { externVarInfoMap = extVMap
                     , externArrInfoMap = extAMap
                     , externVecInfoMap = extVeMap
                     , externMatInfoMap = extMMap
                     , externFunInfoMap = extFMap }
  =
  -- Arrays and functions have to come after vars.  This is because we may use
  -- the assignment of extVars in the definition of extArrs.  The Analyzer.hs
  -- copilot-core prevents arrays or functions from being used in arrays or
  -- functions.
  --extACSL $$
  (mkFunc ("static " ++ sampleExtsF) $ vcat (extVars ++ extArrs ++ extVecs ++ extMats ++  extFuns))

  where
  --ll = sampleVExtACSL extVMap ++ sampleAExtACSL extAMap ++ sampleFExtACSL extFMap
  --extACSL = vcat [text "/*@",
--			(case ll of
  --			[] -> text " assigns \\nothing;"
--			_ -> vcat $ ll)
--			,text "*/"]
  extVars = map sampleVExt ((fst . unzip . M.toList) extVMap)
--  extADecl = map sampleAExt1 (M.toList extAMap)
--  extFDecl = map sampleFExt1 (M.toList extFMap)
  extArrs = map sampleAExt (M.toList extAMap)
  extVecs = map sampleVeExt (M.toList extVeMap)
  extMats = map sampleMExt (M.toList extMMap)
  extFuns = map sampleFExt (M.toList extFMap)

--------------------------------------------------------------------------------

-- Variables

--sampleVExtACSL :: M.Map C.Name C.ExtVar -> [Doc]
--sampleVExtACSL extVMap =
--  (map sampleVExtACSL1 ((fst . unzip . M.toList) extVMap)) ++ (map sampleVExtACSL2 ((fst . unzip . M.toList) extVMap))
--sampleVExtACSL1 :: C.Name -> Doc
--sampleVExtACSL1 name =
--  text " assigns" <+> text (mkExtTmpVar name) <> semi
--sampleVExtACSL2 :: C.Name -> Doc
--sampleVExtACSL2 name =
--  text " //ensures" <+> text (mkExtTmpVar name) <+> text "==" <+> text name <> semi

sampleVExt :: C.Name -> Doc
sampleVExt name =
  text (mkExtTmpVar name) <+> equals <+> text name <> semi

--------------------------------------------------------------------------------
-- Arrays

-- Currenty, Analyze.hs in copilot-language forbids recurssion in external
-- arrays or functions (i.e., an external array can't use another external array
-- to compute it's index).
--sampleAExtACSL :: M.Map C.Tag C.ExtArray -> [Doc]
--sampleAExtACSL extAMap =
--  (map sampleAExtACSL1 (M.toList extAMap)) ++ (map sampleAExtACSL2 (M.toList extAMap))
--
--sampleAExtACSL1 :: (Int, C.ExtArray) -> Doc
--sampleAExtACSL1 (_, C.ExtArray { C.externArrayName = name
--                          , C.externArrayIdx = idx
--                          , C.externArrayTag = t     })
--  =
--  text " assigns" <+> text (mkExtTmpTag name t) <+> semi


--sampleAExtACSL2 :: (Int, C.ExtArray) -> Doc
--sampleAExtACSL2 (_, C.ExtArray { C.externArrayName = name
--                          , C.externArrayIdx = idx
--                          , C.externArrayTag = t     })
--  =
--  text " //ensures" <+> text (mkExtTmpTag name t) <+> text "==" <+> text ("tmp_"++(mkExtTmpTag name t)) <> semi

--sampleAExt1 :: (Int, C.ExtArray) -> Doc
--sampleAExt1 (_, C.ExtArray { C.externArrayName = name
--                          , C.externArrayIdx = idx
--			  , C.externArrayElemType = tttt
--                          , C.externArrayTag = t     })
--  = (retType tttt) <+> text ("tmp_"++(mkExtTmpTag name t)) <+> equals <+> arrIdx name idx
--
--  where
--  arrIdx :: C.Name -> C.Expr a -> Doc
--  arrIdx name' e = text name' <> lbrack <> idxFCall e <> rbrack <> semi

  -- Ok, because the analyzer disallows arrays or function calls in index
  -- expressions, and we assign all variables before arrays.
--  idxFCall :: C.Expr a -> Doc
--  idxFCall e =
--    mkFuncCall (mkExtArrFn name) (map text $ collectArgs e)

sampleAExt :: (Int, C.ExtArray) -> Doc
sampleAExt (_, C.ExtArray { C.externArrayName = name
                          , C.externArrayIdx = idx
			  , C.externArrayElemType = tttt
                          , C.externArrayTag = t     })
  = text (mkExtTmpTag name t) <+> equals <+> arrIdx name idx <> semi
  where
  arrIdx :: C.Name -> C.Expr a -> Doc
  arrIdx name' e = text name' <> lbrack <> idxFCall e <> rbrack <> semi

  -- Ok, because the analyzer disallows arrays or function calls in index
  -- expressions, and we assign all variables before arrays.
  idxFCall :: C.Expr a -> Doc
  idxFCall e =
    mkFuncCall (mkExtArrFn name) (map text $ collectArgs e)



sampleVeExt :: (Int, C.ExtVector) -> Doc
sampleVeExt (_, C.ExtVector { C.externVectorName = name
                           , C.externVectorTag = tag     })
    = error "sampleMExt: Not implemented yet"

sampleMExt :: (Int, C.ExtMatrix) -> Doc
sampleMExt (_, C.ExtMatrix { C.externMatrixName = name
                           , C.externMatrixTag = tag     })
    = error "sampleMExt: Not implemented yet"
--  = text (mkExtTmpTag name tag) <+> equals <+> arrIdx name idxr <> arrIdx name idxc <> semi
--  where
--  arrIdx :: C.Name -> C.Expr a -> Doc
--  arrIdx name' e = text name' <> lbrack <> idxFCall e <> rbrack <> semi

--  -- Ok, because the analyzer disallows arrays or function calls in index
--  -- expressions, and we assign all variables before arrays.
--  idxFCall :: C.Expr a -> Doc
--  idxFCall e =
--    mkFuncCall (mkExtArrFn name) (map text $ collectArgs e)

--------------------------------------------------------------------------------

--sampleFExtACSL :: M.Map C.Tag C.ExtFun -> [Doc]
--sampleFExtACSL extFMap =
--  (map sampleFExtACSL1 (M.toList extFMap)) ++ (map sampleFExtACSL2 (M.toList extFMap))

--sampleFExtACSL1 :: (Int, C.ExtFun) -> Doc
--sampleFExtACSL1 (_, C.ExtFun { C.externFunName = name
--                        , C.externFunArgs = args
--                        , C.externFunTag  = tag  })
--  =
--  text " assigns" <+> text (mkExtTmpTag name tag) <> semi
--sampleFExtACSL2 :: (Int, C.ExtFun) -> Doc
--sampleFExtACSL2 (_, C.ExtFun { C.externFunName = name
--                        , C.externFunArgs = args
--                        , C.externFunTag  = tag  })
--  =
--  text " //ensures" <+> text (mkExtTmpTag name tag) <+> text "==" <+> text ("tmp_"++(mkExtTmpTag name tag)) <> semi


-- External functions
--sampleFExt1 :: (Int, C.ExtFun) -> Doc
--sampleFExt1 (_, C.ExtFun { C.externFunName = name
--                        , C.externFunArgs = args
--                        , C.externFunType = tttt
--                        , C.externFunTag  = tag  })
--  =
--  (retType tttt) <+> text ("tmp_"++(mkExtTmpTag name tag)) <+> equals <+> text name <> lparen
--    <> hsep (punctuate comma $ map mkArgCall (zip [(0 :: Int) ..] args))
--    <> rparen <> semi
--
--     where
--     mkArgCall :: (Int, C.UExpr) -> Doc
--     mkArgCall (i, C.UExpr { C.uExprExpr = e }) =
--       mkFuncCall (mkExtFunArgFn i name tag) (map text $ collectArgs e)

sampleFExt :: (Int, C.ExtFun) -> Doc
sampleFExt (_, C.ExtFun { C.externFunName = name
                        , C.externFunArgs = args
                        , C.externFunType = tttt
                        , C.externFunTag  = tag  })
  =
  text (mkExtTmpTag name tag) <+> equals <+> text name <> lparen
    <> hsep (punctuate comma $ map mkArgCall (zip [(0 :: Int) ..] args))
    <> rparen <> semi
  where
     mkArgCall :: (Int, C.UExpr) -> Doc
     mkArgCall (i, C.UExpr { C.uExprExpr = e }) =
       mkFuncCall (mkExtFunArgFn i name tag) (map text $ collectArgs e)

--------------------------------------------------------------------------------

updateStates :: [C.Stream] -> Doc
updateStates [] = (text "/*@\n assigns \\nothing;\n */") $$ (mkFunc ("static " ++ updateStatesF) $ vcat $ map updateSt [])
  where
  updateSt :: C.Stream -> Doc
  updateSt C.Stream { C.streamId   = id
                    , C.streamExpr = e } =
    text (mkTmpStVar id) <+> equals
      <+> mkFuncCall (mkUpdateStFn id)
                     (map text $ collectArgs e)
      <>  semi

updateStates streams = (text "/*@\n") <> (hcat $ map updateStACSL (streams)) <+> (text "*/") $$
  (mkFunc ("static " ++ updateStatesF) $ vcat $ map updateSt streams)
  where
  updateStACSL :: C.Stream -> Doc
  updateStACSL C.Stream { C.streamId   = id
                    , C.streamExpr = e } =
    text " assigns "<> text (mkTmpStVar id) <> semi <> text "\n"


  updateSt :: C.Stream -> Doc
  updateSt C.Stream { C.streamId   = id
                    , C.streamExpr = e } =
    text (mkTmpStVar id) <+> equals
      <+> mkFuncCall (mkUpdateStFn id)
                     (map text $ collectArgs e)
      <>  semi

--------------------------------------------------------------------------------

updateObservers :: Params -> MetaTable -> Doc
updateObservers params MetaTable { observerInfoMap = observers }
  = let ll = M.toList observers
  in (case ll of
  [] -> text "/*@\n assigns \\nothing;\n */"
  _ -> (text "/*@\n") <> (hcat $ map updateObsvACSL (ll)) <+> (text "*/")
  )$$
  (mkFunc ("static " ++ observersF) $ vcat $ map updateObsv (ll))
  where
  updateObsvACSL :: (C.Name, ObserverInfo) -> Doc
  updateObsvACSL (name, ObserverInfo { observerArgs = args }) =
    text " assigns" <+> text (withPrefix (prefix params) name) <> semi <> text "\n"

  updateObsv :: (C.Name, ObserverInfo) -> Doc
  updateObsv (name, ObserverInfo { observerArgs = args }) =
    text (withPrefix (prefix params) name) <+> text "=" <+>
    mkFuncCall (mkObserverFn name) (map text args) <> semi

--------------------------------------------------------------------------------

fireTriggers :: MetaTable -> Doc
fireTriggers MetaTable { triggerInfoMap = triggers }
  = -- text "/*@\n assigns \\nothing; \n*/" $$
  (mkFunc ("static " ++ triggersF) $ vcat $ map fireTrig (M.toList triggers))

  where
  -- if (guard) trigger(args);
  fireTrig :: (C.Name, TriggerInfo) -> Doc
  fireTrig (name, TriggerInfo { guardArgs      = gArgs
                              , triggerArgArgs = argArgs })
    =
    text "if" <+> lparen <> guardF <> rparen $+$ nest 2 f

    where
    f = text name <> lparen
          <> vcat (punctuate comma $ map mkArg (mkArgIdx argArgs))
          <> rparen <> semi

    guardF :: Doc
    guardF = mkFuncCall (mkTriggerGuardFn name) (map text gArgs)

    mkArg :: (Int, [String]) -> Doc
    mkArg (i, args) = mkFuncCall (mkTriggerArgFn i name) (map text args)

--------------------------------------------------------------------------------

writeACSLqueues :: MetaTable -> Doc
writeACSLqueues MetaTable { streamInfoMap = strMap } =
  let ll = M.toList strMap
  in

  vcat $ (text "/*ACSL following*/\n"):(case ll of
  [] -> [text ""]
  _ -> (text "/*@" :(map varAndUpdate (ll) ++ [(text "*/")]))
  )

  where
  varAndUpdate :: (C.Id, C.Stream) -> Doc
  varAndUpdate (id, C.Stream { C.streamBuffer = que }) =
    updateFunc (mkQueueVar id) (fromIntegral $ length que) (mkQueuePtrVar id)

  -- idx = (idx + 1) % queueSize;
  updateFunc :: String -> QueueSize -> String -> Doc
  updateFunc que sz ptr =
    text (" global invariant a_bound_"++ ptr ++":") <+>text ptr <+> text "<" <+> int (fromIntegral sz) <+> semi <+>
    text ("\n global invariant a_pos_"++ ptr ++":") <+>text ptr <+> text ">=" <+> int (0) <+> semi <+>
    text ("\n global invariant a_valid_"++ ptr ++": \\valid") <+> lparen <> text que <+> text "+" <+> lparen <> text "0.." <+> int ((fromIntegral sz) - 1) <+> rparen <> rparen <> semi

--------------------------------------------------------------------------------

updateBuffers :: MetaTable -> Doc
updateBuffers MetaTable { streamInfoMap = strMap }
  = let ll = M.toList strMap
  in (case ll of
  [] -> text "/*@\n assigns \\nothing;\n */"
  _ -> (text "/*@\n") <> (hcat $ map updateBufACSL (ll)) <+> (text "*/")
  )$$
  (mkFunc ("static " ++ updateBuffersF) $ vcat $ map updateBuf (ll))

  where

  updateBufACSL :: (C.Id, C.Stream) -> Doc
  updateBufACSL (id, _) =
    updateFuncACSL (mkQueueVar id) (mkQueuePtrVar id) (mkTmpStVar id)

  -- queue_strX[ptr] = newVal;
  updateFuncACSL :: String -> String -> String -> Doc
  updateFuncACSL que ptr tmp =
    text " assigns" <+> text que <> lbrack <> text ptr <> rbrack <> semi <>
    (text "\n ensures" <+> text que <> lbrack <> text ptr <> rbrack <+>  text "==" <+> text tmp <> semi <> text "\n")

  updateBuf :: (C.Id, C.Stream) -> Doc
  updateBuf (id, _) =
    updateFunc (mkQueueVar id) (mkQueuePtrVar id) (mkTmpStVar id)

  -- queue_strX[ptr] = newVal;
  updateFunc :: String -> String -> String -> Doc
  updateFunc que ptr tmp =
    text que <> lbrack <> text ptr <> rbrack <+> equals <+> text tmp <> semi

--------------------------------------------------------------------------------

updatePtrs :: MetaTable -> Doc
updatePtrs MetaTable { streamInfoMap = strMap } =
  let ll = M.toList strMap
  in (case ll of
  [] -> text "/*@\n assigns \\nothing;\n */"
  _ -> (text "/*@\n") <> (hcat $ map varAndUpdateACSL (ll)) <+> (text "*/")
  )$$
  (mkFunc ("static " ++ updatePtrsF) $ vcat $ map varAndUpdate (ll))

  where

  varAndUpdateACSL :: (C.Id, C.Stream) -> Doc
  varAndUpdateACSL (id, C.Stream { C.streamBuffer = que }) =
    updateFuncACSL (fromIntegral $ length que) (mkQueuePtrVar id)

  -- idx = (idx + 1) % queueSize;
  updateFuncACSL :: QueueSize -> String -> Doc
  updateFuncACSL sz ptr =
    text " assigns" <+> text ptr <> semi <> (text "\n ensures" <+> text ptr <+> text "=="
      <+> lparen <> text "\\old (" <> text ptr <+> text ") +" <+> int 1 <> rparen
      <+> text "%" <+> int (fromIntegral sz) <> semi <> text "\n")


  varAndUpdate :: (C.Id, C.Stream) -> Doc
  varAndUpdate (id, C.Stream { C.streamBuffer = que }) =
    updateFunc (fromIntegral $ length que) (mkQueuePtrVar id)

  -- idx = (idx + 1) % queueSize;
  updateFunc :: QueueSize -> String -> Doc
  updateFunc sz ptr =
    text ptr <+> equals
      <+> lparen <> text ptr <+> text "+" <+> int 1 <> rparen
      <+> text "%" <+> int (fromIntegral sz) <> semi

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

    C.Float  -> "SFloat"
    C.Double -> "SDouble"

    _          -> C.badUsage "You've tried to compile a Copilot program to SBV with a type SBV does not support.  SBV does not support floats or doubles.  To compile programs using these types, use the copilot-c99 (Atom) backend.  See README.md for more information."
