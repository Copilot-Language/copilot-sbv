--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- Builds names of functions and variables used.

module Copilot.Compile.SBV.Common
  ( mkTmpStVar
  , mkUpdateStFn
  , mkQueueVar
  , mkQueuePtrVar
  , mkExtTmpVar
  , mkExtTmpTag
  , mkExtArrFn
  , mkExtFunFn
  , mkExtFunArgFn
  , mkObserverFn
  , mkTriggerGuardFn
  , mkTriggerArgFn
  , mkArgIdx
  ) where

import Copilot.Core (Id, Tag, impossible)
import Prelude hiding (id)

mkVar :: String -> Id -> String
mkVar str id = str ++ show id

mkTmpStVar :: Id -> String
mkTmpStVar = mkVar "tmp_"

mkUpdateStFn :: Id -> String
mkUpdateStFn = mkVar "update_state_" 

mkQueueVar :: Id -> String
mkQueueVar = mkVar "queue_" 

mkQueuePtrVar :: Id -> String
mkQueuePtrVar = mkVar "ptr_" 

mkExtTmpVar :: String -> String
mkExtTmpVar = ("ext_" ++)

mkExtTmpTag :: String -> Maybe Tag -> String
mkExtTmpTag name tag = "ext_" ++ name ++ "_" ++ show tag'
  where tag' = case tag of
                 Nothing -> impossible "mkExtTmpTag" "copilot-sbv"
                 Just t  -> t

mkExtArrFn :: String -> String
mkExtArrFn = (++) "ext_arr_"

mkExtFunFn :: String -> String
mkExtFunFn = (++) "ext_fun_"

mkExtFunArgFn :: Int -> String -> Maybe Tag -> String
mkExtFunArgFn i nm tag = "ext_" ++ nm ++ show tag' ++ "_arg_" ++ show i
  where tag' = case tag of
                 Nothing -> impossible "mkExtFunArgFn" "copilot-sbv"
                 Just t  -> t

mkObserverFn :: String -> String
mkObserverFn = ("observer_" ++)

mkTriggerGuardFn :: String -> String
mkTriggerGuardFn = ("trigger_guard_" ++)

mkTriggerArgFn :: Int -> String -> String
mkTriggerArgFn i nm = "trigger_" ++ nm ++ "_arg_" ++ show i

mkArgIdx :: [a] -> [(Int, a)]
mkArgIdx args = zip [0,1 ..] args
