--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs, ExistentialQuantification, LambdaCase #-}

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

-- | A Copilot specification consists of a list of variables bound to anonymous
-- streams, a list of anomymous streams, a list of observers, and a list of
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
    , specProperties = []
    }

--------------------------------------------------------------------------------
--data UExpr = forall a. UExpr
--  { uExprType :: Type a
--  , uExprExpr :: Expr a }
transformUExpr :: UExpr -> UExpr
transformUExpr UExpr { uExprExpr = e, uExprType = t } =
  UExpr { uExprExpr = transformExpr e, uExprType = t }

transformUExpr' :: UExpr -> UExpr
transformUExpr' UExpr { uExprExpr = e, uExprType = t } =
  UExpr { uExprExpr = transformExpr' e, uExprType = t }

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
transformExpr = simpl . transformExpr'

transformExpr' :: Expr a -> Expr a
transformExpr' e0 = case e0 of
  Const t x                       -> Const t x
  Drop t k id                     -> Drop t k id
  Local t1 t2 name e1 e2          -> Local t1 t2 name (transformExpr' e1) (transformExpr e2)
  Var t name                      -> Var t name
  ExternVar t name e              -> ExternVar t name e
  ExternFun t name args contxt yy -> ExternFun t name (map transformUExpr' args) contxt yy
  ExternArray t1 t2 name
              size idx context yy -> ExternArray t1 t2 name size (transformExpr' idx) context yy
  Op1 op e                        -> transformOp1 op (transformExpr' e)
  Op2 op e1 e2                    -> transformOp2 op (transformExpr' e1) (transformExpr' e2)
  Op3 op e1 e2 e3                 -> transformOp3 op (transformExpr' e1) (transformExpr' e2) (transformExpr' e3)

  Label t s e                     -> case s of
    '?':m -> ExternFun t ("ident_" ++ showType t) [UExpr {uExprExpr = Label t m $ transformExpr' e, uExprType = t}] Nothing Nothing
    _     -> Label t s $ transformExpr' e

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
  Not          -> Op1 Not e
  -- Numeric operators.
--  Abs   t      -> Op2 (Mul t) e (Label t "?absolute_value_splitting" $ Op1 (Sign t) e)
  Sign  t      -> Op1 (Sign t) e
  -- Fractional operators.
  Recip t      -> Op2 (Fdiv t) (Const t 1.0) e
  -- Floating operators.
--  Exp Float    -> ExternFun Float "expf"
--                    [UExpr { uExprExpr = e, uExprType = Float }] Nothing Nothing
--  Exp Double   -> ExternFun Double "exp"
--                    [UExpr { uExprExpr = e, uExprType = Double }] Nothing Nothing
-- Sqrt t       -> Op2 (Pow t) (Const t 0.5) e
--  Log Float    -> ExternFun Float "logf"
--                    [UExpr { uExprExpr = e, uExprType = Float }] Nothing Nothing
--  Log Double   -> ExternFun Double "log"
--                    [UExpr { uExprExpr = e, uExprType = Double }] Nothing Nothing
--  Sin Float    -> ExternFun Float "sinf"
--                    [UExpr { uExprExpr = e, uExprType = Float }] Nothing Nothing
--  Sin Double   -> ExternFun Double "sin"
--                    [UExpr { uExprExpr = e, uExprType = Double }] Nothing Nothing
--  Cos Float    -> ExternFun Float "cosf"
--                    [UExpr { uExprExpr = e, uExprType = Float }] Nothing Nothing
--  Cos Double   -> ExternFun Double "cos"
--                    [UExpr { uExprExpr = e, uExprType = Double }] Nothing Nothing
--  Tan Float    -> ExternFun Float "tanf"
--                    [UExpr { uExprExpr = e, uExprType = Float }] Nothing Nothing
--  Tan Double   -> ExternFun Double "tan"
--                    [UExpr { uExprExpr = e, uExprType = Double }] Nothing Nothing
--  Asin Float   -> ExternFun Float "asinf"
--                    [UExpr { uExprExpr = e, uExprType = Float }] Nothing Nothing
--  Asin Double  -> ExternFun Double "asin"
--                    [UExpr { uExprExpr = e, uExprType = Double }] Nothing Nothing
--  Acos Float   -> ExternFun Float "acosf"
--                    [UExpr { uExprExpr = e, uExprType = Float }] Nothing Nothing
--  Acos Double  -> ExternFun Double "acos"
--                    [UExpr { uExprExpr = e, uExprType = Double }] Nothing Nothing
--  Atan Float   -> ExternFun Float "atanf"
--                    [UExpr { uExprExpr = e, uExprType = Float }] Nothing Nothing
--  Atan Double  -> ExternFun Double "atan"
--                    [UExpr { uExprExpr = e, uExprType = Double }] Nothing Nothing
--  Sinh Float   -> ExternFun Float "sinhf"
--                    [UExpr { uExprExpr = e, uExprType = Float }] Nothing Nothing
--  Sinh Double  -> ExternFun Double "sinh"
--                    [UExpr { uExprExpr = e, uExprType = Double }] Nothing Nothing
--  Cosh Float   -> ExternFun Float "coshf"
--                    [UExpr { uExprExpr = e, uExprType = Float }] Nothing Nothing
--  Cosh Double  -> ExternFun Double "cosh"
--                    [UExpr { uExprExpr = e, uExprType = Double }] Nothing Nothing
--  Tanh Float   -> ExternFun Float "tanhf"
--                    [UExpr { uExprExpr = e, uExprType = Float }] Nothing Nothing
--  Tanh Double  -> ExternFun Double "tanh"
--                    [UExpr { uExprExpr = e, uExprType = Double }] Nothing Nothing
--  Asinh Float  -> ExternFun Float "asinhf"
--                    [UExpr { uExprExpr = e, uExprType = Float }] Nothing Nothing
--  Asinh Double -> ExternFun Double "asinh"
--                    [UExpr { uExprExpr = e, uExprType = Double }] Nothing Nothing
--  Acosh Float  -> ExternFun Float "acoshf"
--                    [UExpr { uExprExpr = e, uExprType = Float }] Nothing Nothing
--  Acosh Double -> ExternFun Double "acosh"
--                    [UExpr { uExprExpr = e, uExprType = Double }] Nothing Nothing
--  Atanh Float  -> ExternFun Float "atanhf"
--                    [UExpr { uExprExpr = e, uExprType = Float }] Nothing Nothing
--  Atanh Double -> ExternFun Double "atanh"
--                    [UExpr { uExprExpr = e, uExprType = Double }] Nothing Nothing
  -- Bitwise operators.
  BwNot    t   -> Op1 (BwNot t) e
  -- Casting operator.
  Cast     t s -> Op1 (Cast t s) e
  op -> Op1 op e

transformOp2 :: Op2 a b c -> Expr a -> Expr b -> Expr c
transformOp2 op e1 e2 = case op of
  -- Floating operators.
  -- Pow    Float -> ExternFun Float "powf"
  --                   [UExpr { uExprExpr = e1, uExprType = Float }
  --                   , UExpr { uExprExpr = e2, uExprType = Float }] Nothing Nothing
  -- Pow    Double-> ExternFun Double "pow"
  --                   [UExpr { uExprExpr = e1, uExprType = Double }
  --                   , UExpr { uExprExpr = e2, uExprType = Double }] Nothing Nothing
  Logb   t     -> Op2 (Fdiv t) (transformExpr' $ Op1 (Log t) e1) (transformExpr' $ Op1 (Log t) e2)
  op           -> Op2 op e1 e2


transformOp3 :: Op3 a b c d -> Expr a -> Expr b -> Expr c -> Expr d
transformOp3 op e1 e2 e3 = case op of
  Mux   Bool   -> Op2 Or (transformExpr' $ Op2 And e2 e1) (transformExpr' $ Op2 And (e3) (Op1 Not e1))
  Mux   t      -> Op3 (Mux t) e1 e2 e3

-----------------------------------------

simpl :: Expr a -> Expr a
simpl = \ case
  Op1 op e                  -> simplOp1 (simpl e) op
  Op2 op e1 e2              -> simplOp2 (simpl e1) (simpl e2) op
  Op3 op e1 e2 e3           -> simplOp3 (simpl e1) (simpl e2) (simpl e3) op
  Local t1 t2 n e1 e2       -> Local t1 t2 n (simpl e1) (simpl e2)
  Label _ _ e               -> simpl e
  ExternFun t n args ctx yy -> ExternFun t n (map simplUExpr args) ctx yy

  c                         -> c

simplUExpr :: UExpr -> UExpr
simplUExpr UExpr { uExprExpr = e, uExprType = t } =
  UExpr { uExprExpr = simpl e, uExprType = t }

simplOp1 :: Expr a -> Op1 a b -> Expr b
simplOp1 e@(Const Bool x) = \ case
  Not      -> Const Bool $ not x
  op       -> Op1 op e
simplOp1 e@(Const Double x) = \ case
  Abs _    -> Const Double $ abs x
  --Sign _   -> sign x
  --Recip _  ->
  --Exp _    ->
  Sqrt _   -> Const Double $ sqrt x
  --Log _    ->
  Sin _    -> Const Double $ sin x
  Tan _    -> Const Double $ tan x
  Cos _    -> Const Double $ cos x
  Asin _   -> Const Double $ asin x
  Atan _   -> Const Double $ atan x
  Acos _   -> Const Double $ acos x
  Sinh _   -> Const Double $ sinh x
  Tanh _   -> Const Double $ tanh x
  Cosh _   -> Const Double $ cosh x
  Asinh _  -> Const Double $ asinh x
  Atanh _  -> Const Double $ atanh x
  Acosh _  -> Const Double $ acosh x
  --BwNot _  ->
  --Cast _ _ ->
  op           -> Op1 op e
simplOp1 e = \ op -> Op1 op e

simplOp2 :: Expr a -> Expr b -> Op2 a b c -> Expr c
simplOp2 e1@(Const Bool x1) e2@(Const Bool x2) = \ case
  And          -> Const Bool $ x1 && x2
  Or           -> Const Bool $ x1 || x2
  Eq       _   -> Const Bool $ x1 == x2
  Ne       _   -> Const Bool $ x1 /= x2
  op           -> Op2 op e1 e2
simplOp2 e1@(Const t x1) e2@(Const _ x2) = \ case
  Add      _   -> Const t $ x1 + x2
  Sub      _   -> Const t $ x1 - x2
  Mul      _   -> Const t $ x1 * x2
  Div      _   -> Const t $ x1 `div` x2
  Mod      _   -> Const t $ x1 `mod` x2
  Fdiv     _   -> Const t $ x1 / x2
  Pow      _   -> Const t $ x1 ** x2
  --Logb     _   ->
  Eq       _   -> Const Bool $ x1 == x2
  Ne       _   -> Const Bool $ x1 /= x2
  Le       _   -> Const Bool $ x1 <= x2
  Ge       _   -> Const Bool $ x1 >= x2
  Lt       _   -> Const Bool $ x1 < x2
  Gt       _   -> Const Bool $ x1 > x2
  --BwAnd    _   ->
  --BwOr     _   ->
  --BwXor    _   ->
  --BwShiftL _ _ ->
  --BwShiftR _ _ ->
  op           -> Op2 op e1 e2
simplOp2 e1 e2 = \ op -> Op2 op e1 e2

simplOp3 :: Expr a -> Expr b -> Expr c -> Op3 a b c d -> Expr d
-- simplOp3 (Const _ x1) (Const _ x2) (Const _ x3) = \ case
--   Mux _    -> if x1 then x2 else x3
simplOp3 e1 e2 e3 = \ op -> Op3 op e1 e2 e3

