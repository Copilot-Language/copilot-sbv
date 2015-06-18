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
  wr (text "\n#CCFLAGS= -std=c99 -Werror -Wall -Wextra -Wbad-function-cast -Wcast-align -Wcast-qual -Wconversion -Wdisabled-optimization -Wformat=2 -Winit-self -Winline -Wnested-externs -Wold-style-definition -Wpointer-arith -Wredundant-decls -Wstrict-prototypes -Wswitch-default -Wswitch-enum -Wundef -Wwrite-strings -Wshadow -Wno-unused-variable -Wno-unused-but-set-variable -Wno-unused-parameter -Wno-unused-label -O2 -march=native  -c\nCCFLAGS= \nCC=ccomp")
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
        <+> text ("\n\tframa-c -val -main step internal.h copilot.h *.c")

  where 
  archive = text sbvName <> text ".a"
  withPre nm = withPrefix (prefix params) nm
  mkStyle :: Doc -> String
  mkStyle = renderStyle (style {lineLength = 80})

--------------------------------------------------------------------------------

