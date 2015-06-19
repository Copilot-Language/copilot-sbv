--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.SBV
  ( compile
  , compileWithSBV
  , sbvDirName
  , module Copilot.Compile.SBV.Params
  ) where

import qualified Copilot.Core as C
import Copilot.Compile.Header.C99 (c99HeaderName, genC99Header)

import qualified Data.SBV as S

import Copilot.Compile.SBV.Driver (driver, driverName)
import Copilot.Compile.SBV.Makefile (makefile, makefileName)
import Copilot.Compile.SBV.Code 
  (updateStates, updateObservers, fireTriggers, getExtArrs, getExtFuns)
import Copilot.Compile.SBV.MetaTable (allocMetaTable)
import Copilot.Compile.SBV.Params

import System.FilePath (combine)

--------------------------------------------------------------------------------

-- Note: we put everything in a directory named by the dirName.

sbvDirName :: String
sbvDirName = "copilot-sbv-codegen"

compile :: Params -> C.Spec -> IO ()
compile p s = compileWithSBV p [] s

-- | sbvs are optional additional SBVCodeGens to generate.
compileWithSBV :: Params -> [(String, S.SBVCodeGen (), String)] -> C.Spec -> IO ()
compileWithSBV params sbvs spec0 = do
  let meta    = allocMetaTable spec
      dirName = withPrefix (prefix params) sbvDirName
      sbvName = withPrefix (prefix params) "internal"
  putStrLn "Compiling SBV-generated functions .."

  S.compileToCLib
    (Just dirName)
    sbvName $ omitSBVDriver
    (  updateStates    meta spec
    ++ updateObservers meta spec
    ++ fireTriggers    meta spec 
    ++ getExtArrs      meta 
    ++ getExtFuns      meta 
    ++ sbvs
    )

  putStrLn ""
  putStrLn $ "Generating Copilot driver " ++ driverName params ++ " .."
  driver params meta spec dirName sbvName

  putStrLn ""
  putStrLn $ "Generating Copilot header " ++ c99HeaderName (prefix params) ++ " .."
  genC99Header (prefix params) dirName spec

  putStrLn ""
  putStrLn $ "Generating Copilot driver Makefile rules .."
               ++ makefileName params ++ " .."
  makefile params dirName sbvName

  putStrLn ""
  putStrLn "Writing README .."
  writeFile (combine dirName "README") (unlines readme)
  putStrLn ""

  putStrLn "Done."

  where spec = C.makeTags spec0

--------------------------------------------------------------------------------

readme :: [String]
readme = 
  [ "These files are automatically generated by Copilot using the SBV code generator backend."
  , ""
  , "To build, you will need to ensure that all external variables and triggers are visible."
  , "Also, modify driver.c to include a main() function."
  , "Also, modify copilot_stdint.h to include your own definition of uint 8, 16, 32, 64 and int 8, 16, 32, 64."
  , "Once you have a valid C program, execute"
  , ""
  , "  > make driver"
  , ""
  , "Modify the Makefile rules (Makefile and copilot.mk) as you see fit."
  , ""
  , "Please report bugs to lee pike at gmail . com (remove all spaces)."
  ]


--------------------------------------------------------------------------------

omitSBVDriver :: [(a, S.SBVCodeGen (), String)] -> [(a, S.SBVCodeGen (), String)]
omitSBVDriver = map omit 
  where
  omit (a, cg, cc) = (a, S.cgGenerateDriver False >> cg, cc)

--------------------------------------------------------------------------------
