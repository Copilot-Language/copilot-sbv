--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs, ExistentialQuantification #-}

module Copilot.Compile.SBV.ACSLproof
  ( transformProofACSL
  )
where

import Prelude hiding (id)
import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.SBV as S
--import qualified Data.SBV.Internals as S

import qualified Copilot.Compile.SBV.Queue as Q
import qualified Copilot.Compile.SBV.Witness as W

import Copilot.Core
import Copilot.Core.Type
import Copilot.Core.Error (impossible)
import Copilot.Core.Type.Equality ((=~=), coerce, cong)


--------------------------------------------------------------------------------
-- | A stream.
--data Stream = forall a. Typed a => Stream
--  { streamId         :: Id
--  , streamBuffer     :: [a]
--  , streamExpr       :: Expr a
--  , streamExprType   :: Type a }

transformStream :: Stream -> Stream
transformStream Stream
      { streamId         = id
      , streamBuffer     = xs
      , streamExpr       = e
      , streamExprType   = t } =
  Stream
  { streamId         = id
  , streamBuffer     = xs
  , streamExpr       = transformExpr e
  , streamExprType   = t }

--------------------------------------------------------------------------------

-- | An observer.
--data Observer = forall a. Observer
--  { observerName     :: Name
--  , observerExpr     :: Expr a
--  , observerExprType :: Type a }

transformObserver :: Observer -> Observer
transformObserver Observer
      { observerName     = name
      , observerExpr     = e
      , observerExprType = t } =
  Observer
  { observerName     = name
  , observerExpr     = transformExpr e
  , observerExprType = t }

--------------------------------------------------------------------------------

-- | A trigger.
--data Trigger = Trigger
--  { triggerName      :: Name
--  , triggerGuard     :: Expr Bool
--  , triggerArgs      :: [UExpr] }

transformTrigger :: Trigger -> Trigger
transformTrigger Trigger
  { triggerName      = name
  , triggerGuard     = guard
  , triggerArgs      = uexprl } =
  Trigger
  { triggerName      = name
  , triggerGuard     = transformExpr guard
  , triggerArgs      = map transformUExpr uexprl }
--------------------------------------------------------------------------------

-- | A property.
--data Property = Property
--  { propertyName     :: Name
--  , propertyExpr     :: Expr Bool }


transformProperty :: Property -> Property
transformProperty Property
  { propertyName     = name
  , propertyExpr     = bexpr } =
  Property
  { propertyName     = name
  , propertyExpr     = transformExpr bexpr }

--------------------------------------------------------------------------------

-- | A Copilot specification consists of a list of variables bound to anonymous
-- streams, a lost of anomymous streams, a list of observers, and a list of
-- triggers.
--data Spec = Spec
--  { specStreams      :: [Stream]
--  , specObservers    :: [Observer]
--  , specTriggers     :: [Trigger]
--  , specProperties   :: [Property] }

transformProofACSL :: Spec -> Spec
transformProofACSL Spec
    { specStreams    = strms
    , specObservers  = obsvs
    , specTriggers   = trigs
    , specProperties = props
    } =
  Spec
    { specStreams    = map transformStream strms
    , specObservers  = map transformObserver obsvs
    , specTriggers   = map transformTrigger trigs
    , specProperties = map transformProperty props
    }



--------------------------------------------------------------------------------
--data UExpr = forall a. UExpr
--  { uExprType :: Type a
--  , uExprExpr :: Expr a }
transformUExpr :: UExpr -> UExpr
transformUExpr UExpr { uExprExpr = e, uExprType = t } =
  UExpr { uExprExpr = transformExpr e, uExprType = t }


--------------------------------------------------------------------------------
-- Expr transformation
--
-- NO TREPASSING BEYOND THIS LINE
--
--------------------------------------------------------------------------------

--data Expr a where
--  Const        :: Type a -> a -> Expr a
--  Drop         :: Type a -> DropIdx -> Id -> Expr a
--  Local        :: Type a -> Type b -> Name -> Expr a -> Expr b -> Expr b
--  Var          :: Type a -> Name -> Expr a
--  ExternVar    :: Type a -> Name -> Maybe [a] -> Expr a
--  ExternFun    :: Type a -> Name -> [UExpr] -> Maybe (Expr a)
--               -> Maybe Tag -> Expr a
--  ExternArray  :: Integral a => Type a -> Type b -> Name -> Int -> Expr a
--               -> Maybe [[b]] -> Maybe Tag -> Expr b
--  Op1          :: Op1 a b -> Expr a -> Expr b
--  Op2          :: Op2 a b c -> Expr a -> Expr b -> Expr c
--  Op3          :: Op3 a b c d -> Expr a -> Expr b -> Expr c -> Expr d

transformExpr :: Expr a -> Expr a
transformExpr e0 = case e0 of
  Const t x                       -> Const t x
  Drop t k id                     -> Drop t k id
  Local t1 t2 name e1 e2          -> Local t1 t2 name (transformExpr e1) (transformExpr e2)
  Var t name                      -> Var t name
  ExternVar t name e              -> ExternVar t name e
  ExternFun t name args contxt yy -> ExternFun t name (map transformUExpr args) contxt yy
  ExternArray t1 t2 name size idx context yy
                                  -> ExternArray t1 t2 name size (transformExpr idx) context yy
  ExternMatrix t1 t2 name rows cols idxr idxc context yy
                                  -> ExternMatrix t1 t2 name rows cols (transformExpr idxr) (transformExpr idxc) context yy
  Op1 op e                        -> transformOp1 op e
  Op2 op e1 e2                    -> transformOp2 op e1 e2
  Op3 op e1 e2 e3                 -> transformOp3 op e1 e2 e3

  Label t s e                    -> case s of
    '?':m -> ExternFun t ("ident_"++(showType t)) [UExpr {uExprExpr = transformExpr $ Label t m $ e, uExprType = t}] Nothing Nothing
    _     -> Label t s $ transformExpr e


showType :: Type a -> String
showType t = case t of
  Bool  -> "bool"
  Int8  -> "int8"
  Int16 -> "int16"
  Int32 -> "int32"
  Int64 -> "int64"
  Word8 -> "word8"
  Word16-> "word16"
  Word32-> "word32"
  Word64-> "word64"
  Float -> "float"
  Double-> "double"

transformOp1 :: Op1 a b -> Expr a -> Expr b
transformOp1 op e = case op of
    -- Boolean operators.
  Not          -> Op1 Not $ transformExpr e
  -- Numeric operators.
  Abs   t      -> Op2 (Mul t) (transformExpr e) (transformExpr $ Label t "?absolute_value_splitting" $ Op1 (Sign t) $ e)
  Sign  t      -> Op1 (Sign t) $ transformExpr e
  -- Fractional operators.
  Recip a      -> Op2 (Fdiv a) (Const a 1.0) (transformExpr e)
  -- Floating operators.
  Exp Float    -> ExternFun Float "expf"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Float }] Nothing Nothing
  Exp Double   -> ExternFun Double "exp"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Double }] Nothing Nothing
  Sqrt Float   -> ExternFun Float "sqrtf"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Float }] Nothing Nothing
  Sqrt Double  -> ExternFun Double "sqrt"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Double }] Nothing Nothing
  Log Float    -> ExternFun Float "logf"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Float }] Nothing Nothing
  Log Double   -> ExternFun Double "log"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Double }] Nothing Nothing
  Sin Float    -> ExternFun Float "sinf"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Float }] Nothing Nothing
  Sin Double   -> ExternFun Double "sin"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Double }] Nothing Nothing
  Cos Float    -> ExternFun Float "cosf"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Float }] Nothing Nothing
  Cos Double   -> ExternFun Double "cos"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Double }] Nothing Nothing
  Tan Float    -> ExternFun Float "tanf"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Float }] Nothing Nothing
  Tan Double   -> ExternFun Double "tan"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Double }] Nothing Nothing
  Asin Float   -> ExternFun Float "asinf"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Float }] Nothing Nothing
  Asin Double  -> ExternFun Double "asin"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Double }] Nothing Nothing
  Acos Float   -> ExternFun Float "acosf"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Float }] Nothing Nothing
  Acos Double  -> ExternFun Double "acos"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Double }] Nothing Nothing
  Atan Float   -> ExternFun Float "atanf"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Float }] Nothing Nothing
  Atan Double  -> ExternFun Double "atan"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Double }] Nothing Nothing
  Sinh Float   -> ExternFun Float "sinhf"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Float }] Nothing Nothing
  Sinh Double  -> ExternFun Double "sinh"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Double }] Nothing Nothing
  Cosh Float   -> ExternFun Float "coshf"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Float }] Nothing Nothing
  Cosh Double  -> ExternFun Double "cosh"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Double }] Nothing Nothing
  Tanh Float   -> ExternFun Float "tanhf"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Float }] Nothing Nothing
  Tanh Double  -> ExternFun Double "tanh"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Double }] Nothing Nothing
  Asinh Float  -> ExternFun Float "asinhf"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Float }] Nothing Nothing
  Asinh Double -> ExternFun Double "asinh"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Double }] Nothing Nothing
  Acosh Float  -> ExternFun Float "acoshf"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Float }] Nothing Nothing
  Acosh Double -> ExternFun Double "acosh"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Double }] Nothing Nothing
  Atanh Float  -> ExternFun Float "atanhf"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Float }] Nothing Nothing
  Atanh Double -> ExternFun Double "atanh"
                    [UExpr { uExprExpr = transformExpr e, uExprType = Double }] Nothing Nothing
  -- Bitwise operators.
  BwNot    t   -> Op1 (BwNot t) $ transformExpr e
  -- Casting operator.
  Cast     t s -> Op1 (Cast t s) $ transformExpr e

transformOp2 :: Op2 a b c -> Expr a -> Expr b -> Expr c
transformOp2 op e1 e2 = case op of
  -- Boolean operators.
  And          -> Op2 And (transformExpr e1) (transformExpr e2)
  Or           -> Op2 Or (transformExpr e1) (transformExpr e2)
  -- Numeric operators.
  Add    t     -> Op2 (Add t) (transformExpr e1) (transformExpr e2)
  Sub    t     -> Op2 (Sub t) (transformExpr e1) (transformExpr e2)
  Mul    t     -> Op2 (Mul t) (transformExpr e1) (transformExpr e2)
  -- Integral operators.
  Mod    t     -> Op2 (Mod t) (transformExpr e1) (transformExpr e2)
  Div    t     -> Op2 (Div t) (transformExpr e1) (transformExpr e2)
  -- Fractional operators.
  Fdiv   t     -> Op2 (Fdiv t) (transformExpr e1) (transformExpr e2)
  -- Floating operators.
  Pow    Float -> ExternFun Float "powf"
                    [UExpr { uExprExpr = transformExpr e1, uExprType = Float }
                    , UExpr { uExprExpr = transformExpr e2, uExprType = Float }] Nothing Nothing
  Pow    Double-> ExternFun Double "pow"
                    [UExpr { uExprExpr = transformExpr e1, uExprType = Double }
                    , UExpr { uExprExpr = transformExpr e2, uExprType = Double }] Nothing Nothing
  Logb   t     -> Op2 (Fdiv t) (transformExpr $ Op1 (Log t) e1) (transformExpr $ Op1 (Log t) e2)
  -- Equality operators.
  Eq    t      -> Op2 (Eq t) (transformExpr e1) (transformExpr e2)
  Ne    t      -> Op2 (Ne t) (transformExpr e1) (transformExpr e2)
  -- Relational operators.
  Le    t      -> Op2 (Le t) (transformExpr e1) (transformExpr e2)
  Ge    t      -> Op2 (Ge t) (transformExpr e1) (transformExpr e2)
  Lt    t      -> Op2 (Lt t) (transformExpr e1) (transformExpr e2)
  Gt    t      -> Op2 (Gt t) (transformExpr e1) (transformExpr e2)
  -- Bitwise operators.
  BwAnd t      -> Op2 (BwAnd t) (transformExpr e1) (transformExpr e2)
  BwOr  t      -> Op2 (BwOr t) (transformExpr e1) (transformExpr e2)
  BwXor t      -> Op2 (BwXor t) (transformExpr e1) (transformExpr e2)
  BwShiftL t s -> Op2 (BwShiftL t s) (transformExpr e1) (transformExpr e2)
  BwShiftR t s -> Op2 (BwShiftR t s) (transformExpr e1) (transformExpr e2)


transformOp3 :: Op3 a b c d -> Expr a -> Expr b -> Expr c -> Expr d
transformOp3 op e1 e2 e3 = case op of
  Mux   Bool   -> Op2 Or (transformExpr $ Op2 And (e2) (e1)) (transformExpr $ Op2 And (e3) (Op1 Not e1))
  Mux   t      -> Op3 (Mux t) (transformExpr e1) (transformExpr e2) (transformExpr e3)
