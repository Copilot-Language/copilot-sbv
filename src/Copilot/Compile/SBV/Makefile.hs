--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.SBV.Makefile
  ( makefile
  , makefileName 
  ) where

import Copilot.Compile.SBV.Driver (driverName)
import Copilot.Compile.SBV.Params
import Text.PrettyPrint.HughesPJ
import qualified System.IO as I

import System.Directory

--------------------------------------------------------------------------------

makefileName :: Params -> String
makefileName params = withPrefix (prefix params) "copilot" ++ ".mk"

--------------------------------------------------------------------------------

makefile :: Params -> String -> String -> IO ()
makefile params dir sbvName = do
  let filePath = dir ++ '/' : (makefileName params)
      fileName = "copilot"
  let wr doc = I.appendFile filePath ((mkStyle doc) ++ "\n")

  wr (text "a")
  removeFile filePath
  wr (text "# Makefile rules for the Copilot driver.")
  wr (text "\nCCFLAGS=-fnone \nCC=ccomp")
  wr (text "")
  wr $ text "driver" <> colon 
        <+> text (driverName params) <+> text (withPre fileName) <> text ".h" 
        <+> archive
  wr $ text "\t" 
         <> (hsep [ text "$" <> braces (text "CC")
                  , text "$" <> braces (text "CCFLAGS")
                  , text "$<"
                  , text "-o"
                  , text "$@"
                  , archive])
  wr $ text "\nfval" <> colon 
        <+> text ("\n\tframa-c -val -main testing -slevel 10000000 *.h *.c | tee logval")
  wr $ text "\nfwp" <> colon 
        <+> text ("\n\tparallel frama-c -wp -wp-out . -wp-prover CVC4 -wp-split {} ::: *.c | tee >logfwp >(grep 'Proved\\|Unknown\\|Failed\\|Qed:\\s\\|CVC4:\\s\\|Parsing .*\\.c' > logfwpcompact) >(grep 'Proved\\|Qed:\\s\\|CVC4:\\s\\|Unknown\\|Failed\\|Parsing .*\\.c')")
  wr $ text "\nsplint" <> colon 
        <+> text ("\n\tsplint -comment-char % *.h *.c | tee logsplint")

  where 
  archive = text sbvName <> text ".a"
  withPre nm = withPrefix (prefix params) nm
  mkStyle :: Doc -> String
  mkStyle = renderStyle (style {lineLength = 80})

--------------------------------------------------------------------------------

