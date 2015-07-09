--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, GADTs, MultiParamTypeClasses #-}

module Copilot.Compile.SBV.Witness
  ( SymWordInst(..)       , symWordInst
  , NumInst(..)           , numInst
  , HasSignAndSizeInst(..), hasSignAndSizeInst
  , EqInst(..)            , eqInst 
  , CastInst(..)          , castInst   , sbvCast
  , BVDivisibleInst(..)   , divInst
  , OrdInst(..)           , ordInst 
  , MergeableInst(..)     , mergeableInst 
  , BitsInst(..)          , bitsInst 
  , IntegralInst(..)      , integralInst 
  ) where

import qualified Data.SBV as S
import qualified Copilot.Core as C

import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Bits

--------------------------------------------------------------------------------

badInst :: a
badInst =  error "Fatal cast in the witnesses of SBV in copilot-sbv.  Are you sure that SBV supports the type you are using?  (It doesn't support floats or doubles.)  If you you are, or you don't understand the error, email leepike @ gmail . com (remove spaces) or file a bug report on github.com"

badFloat :: a
badFloat =  error "Fatal error : You did something illegal with floating points numbers (a mod b, or something like that)"

--------------------------------------------------------------------------------

data SymWordInst a = S.SymWord a => SymWordInst

symWordInst :: C.Type a -> SymWordInst a
symWordInst t =
  case t of
    C.Bool   -> SymWordInst
    C.Int8   -> SymWordInst ; C.Int16  -> SymWordInst
    C.Int32  -> SymWordInst ; C.Int64  -> SymWordInst
    C.Word8  -> SymWordInst ; C.Word16 -> SymWordInst
    C.Word32 -> SymWordInst ; C.Word64 -> SymWordInst
    C.Float  -> SymWordInst
    C.Double -> SymWordInst

--------------------------------------------------------------------------------

data NumInst a = Num a => NumInst

numInst :: C.Type a -> NumInst a
numInst t =
  case t of
    C.Bool   -> badInst
    C.Int8   -> NumInst ; C.Int16  -> NumInst
    C.Int32  -> NumInst ; C.Int64  -> NumInst
    C.Word8  -> NumInst ; C.Word16 -> NumInst
    C.Word32 -> NumInst ; C.Word64 -> NumInst
    C.Float  -> NumInst
    C.Double -> NumInst

--------------------------------------------------------------------------------

data HasSignAndSizeInst a = S.SymWord a => HasSignAndSizeInst

hasSignAndSizeInst :: C.Type a -> HasSignAndSizeInst a
hasSignAndSizeInst t =
  case t of
    C.Bool   -> HasSignAndSizeInst
    C.Int8   -> HasSignAndSizeInst 
    C.Int16  -> HasSignAndSizeInst
    C.Int32  -> HasSignAndSizeInst 
    C.Int64  -> HasSignAndSizeInst
    C.Word8  -> HasSignAndSizeInst 
    C.Word16 -> HasSignAndSizeInst
    C.Word32 -> HasSignAndSizeInst 
    C.Word64 -> HasSignAndSizeInst
    C.Float  -> HasSignAndSizeInst
    C.Double -> HasSignAndSizeInst

--------------------------------------------------------------------------------

data EqInst a = S.EqSymbolic (S.SBV a) => EqInst

eqInst :: C.Type a -> EqInst a
eqInst t =
  case t of
    C.Bool   -> EqInst
    C.Int8   -> EqInst ; C.Int16  -> EqInst
    C.Int32  -> EqInst ; C.Int64  -> EqInst
    C.Word8  -> EqInst ; C.Word16 -> EqInst
    C.Word32 -> EqInst ; C.Word64 -> EqInst
    C.Float  -> EqInst
    C.Double -> EqInst

--------------------------------------------------------------------------------

data BVDivisibleInst a = S.SDivisible (S.SBV a) => BVDivisibleInst

divInst :: C.Type a -> BVDivisibleInst a
divInst t =
  case t of
    C.Bool   -> badInst
    C.Int8   -> BVDivisibleInst
    C.Int16  -> BVDivisibleInst
    C.Int32  -> BVDivisibleInst
    C.Int64  -> BVDivisibleInst
    C.Word8  -> BVDivisibleInst
    C.Word16 -> BVDivisibleInst
    C.Word32 -> BVDivisibleInst
    C.Word64 -> BVDivisibleInst
    C.Float  -> badFloat
    C.Double -> badFloat

--------------------------------------------------------------------------------

data OrdInst a = S.OrdSymbolic (S.SBV a) => OrdInst

ordInst :: C.Type a -> OrdInst a
ordInst t =
  case t of
    C.Bool   -> OrdInst
    C.Int8   -> OrdInst ; C.Int16  -> OrdInst
    C.Int32  -> OrdInst ; C.Int64  -> OrdInst
    C.Word8  -> OrdInst ; C.Word16 -> OrdInst
    C.Word32 -> OrdInst ; C.Word64 -> OrdInst
    C.Float  -> OrdInst 
    C.Double -> OrdInst

--------------------------------------------------------------------------------

data MergeableInst a = S.Mergeable (S.SBV a) => MergeableInst

mergeableInst :: C.Type a -> MergeableInst a
mergeableInst t =
  case t of
    C.Bool   -> MergeableInst
    C.Int8   -> MergeableInst ; C.Int16  -> MergeableInst
    C.Int32  -> MergeableInst ; C.Int64  -> MergeableInst
    C.Word8  -> MergeableInst ; C.Word16 -> MergeableInst
    C.Word32 -> MergeableInst ; C.Word64 -> MergeableInst
    C.Float  -> MergeableInst
    C.Double -> MergeableInst

--------------------------------------------------------------------------------

data BitsInst a = (Bits a, S.Bits (S.SBV a)) => BitsInst

bitsInst :: C.Type a -> BitsInst a
bitsInst t =
  case t of
    C.Bool   -> badInst
    C.Int8   -> BitsInst ; C.Int16  -> BitsInst
    C.Int32  -> BitsInst ; C.Int64  -> BitsInst
    C.Word8  -> BitsInst ; C.Word16 -> BitsInst
    C.Word32 -> BitsInst ; C.Word64 -> BitsInst
    C.Float  -> badFloat
    C.Double -> badFloat

--------------------------------------------------------------------------------

data IntegralInst a = S.SIntegral a => IntegralInst

integralInst :: C.Type a -> IntegralInst a
integralInst t =
  case t of
    C.Bool   -> badInst
    C.Int8   -> IntegralInst ; C.Int16  -> IntegralInst
    C.Int32  -> IntegralInst ; C.Int64  -> IntegralInst
    C.Word8  -> IntegralInst ; C.Word16 -> IntegralInst
    C.Word32 -> IntegralInst ; C.Word64 -> IntegralInst
    C.Float  -> badFloat
    C.Double -> badFloat

--------------------------------------------------------------------------------

data CastInst a b = SBVCast a b => CastInst

castInst :: C.Type a -> C.Type b -> CastInst a b
castInst t0 t1 = 
  case t0 of
    C.Bool   -> case t1 of
                  C.Bool    -> CastInst
                  C.Word8   -> CastInst
                  C.Word16  -> CastInst
                  C.Word32  -> CastInst
                  C.Word64  -> CastInst
                  C.Int8    -> CastInst
                  C.Int16   -> CastInst
                  C.Int32   -> CastInst
                  C.Int64   -> CastInst
                  _         -> badInst

    C.Word8  -> case t1 of
                  C.Word8   -> CastInst
                  C.Word16  -> CastInst
                  C.Word32  -> CastInst
                  C.Word64  -> CastInst
                  C.Int8    -> CastInst
                  C.Int16   -> CastInst
                  C.Int32   -> CastInst
                  C.Int64   -> CastInst
                  _         -> badInst
    C.Word16 -> case t1 of
                  C.Word8   -> CastInst
                  C.Word16  -> CastInst
                  C.Word32  -> CastInst
                  C.Word64  -> CastInst
                  C.Int8    -> CastInst
                  C.Int16   -> CastInst
                  C.Int32   -> CastInst
                  C.Int64   -> CastInst
                  _         -> badInst
    C.Word32 -> case t1 of
                  C.Word8   -> CastInst
                  C.Word16  -> CastInst
                  C.Word32  -> CastInst
                  C.Word64  -> CastInst
                  C.Int8    -> CastInst
                  C.Int16   -> CastInst
                  C.Int32   -> CastInst
                  C.Int64   -> CastInst
                  _         -> badInst
    C.Word64 -> case t1 of
                  C.Word8   -> CastInst
                  C.Word16  -> CastInst
                  C.Word32  -> CastInst
                  C.Word64  -> CastInst
                  C.Int8    -> CastInst
                  C.Int16   -> CastInst
                  C.Int32   -> CastInst
                  C.Int64   -> CastInst
                  _         -> badInst

    C.Int8   -> case t1 of
                  C.Word8   -> CastInst
                  C.Word16  -> CastInst
                  C.Word32  -> CastInst
                  C.Word64  -> CastInst
                  C.Int8    -> CastInst
                  C.Int16   -> CastInst
                  C.Int32   -> CastInst
                  C.Int64   -> CastInst
                  _         -> badInst
    C.Int16  -> case t1 of
                  C.Word8   -> CastInst
                  C.Word16  -> CastInst
                  C.Word32  -> CastInst
                  C.Word64  -> CastInst
                  C.Int8    -> CastInst
                  C.Int16   -> CastInst
                  C.Int32   -> CastInst
                  C.Int64   -> CastInst
                  _         -> badInst
    C.Int32  -> case t1 of
                  C.Word8   -> CastInst
                  C.Word16  -> CastInst
                  C.Word32  -> CastInst
                  C.Word64  -> CastInst
                  C.Int8    -> CastInst
                  C.Int16   -> CastInst
                  C.Int32   -> CastInst
                  C.Int64   -> CastInst
                  _         -> badInst
    C.Int64  -> case t1 of
                  C.Word8   -> CastInst
                  C.Word16  -> CastInst
                  C.Word32  -> CastInst
                  C.Word64  -> CastInst
                  C.Int8    -> CastInst
                  C.Int16   -> CastInst
                  C.Int32   -> CastInst
                  C.Int64   -> CastInst
                  _         -> badInst
    C.Float  -> badInst
    C.Double -> badInst

--------------------------------------------------------------------------------
-- | A class for casting SBV values.  We return errors for casts allowed by
-- Copilot.

class SBVCast a b where
  sbvCast :: S.SBV a -> S.SBV b

--------------------------------------------------------------------------------

castBool :: (Num a, S.SymWord a) => S.SBV Bool -> S.SBV a
castBool x = case S.unliteral x of
               Just bool -> if bool then 1 else 0
               Nothing   -> S.ite x 1 0

castErr :: a
castErr = C.badUsage $ "the SBV backend does not currently support casts to signed word types"

--------------------------------------------------------------------------------

instance SBVCast Bool Bool where
  sbvCast = id
instance SBVCast Bool Word8 where
  sbvCast = castBool
instance SBVCast Bool Word16 where
  sbvCast = castBool
instance SBVCast Bool Word32 where
  sbvCast = castBool
instance SBVCast Bool Word64 where
  sbvCast = castBool

instance SBVCast Bool Int8 where
  sbvCast = castBool 
instance SBVCast Bool Int16 where
  sbvCast = castBool 
instance SBVCast Bool Int32 where
  sbvCast = castBool 
instance SBVCast Bool Int64 where
  sbvCast = castBool 

--------------------------------------------------------------------------------

instance SBVCast Word8 Word8 where
  sbvCast = S.sFromIntegral 
instance SBVCast Word8 Word16 where
  sbvCast = S.sFromIntegral
instance SBVCast Word8 Word32 where
  sbvCast = S.sFromIntegral
instance SBVCast Word8 Word64 where
  sbvCast = S.sFromIntegral

instance SBVCast Word8 Int8 where
  sbvCast = S.sFromIntegral 
instance SBVCast Word8 Int16 where
  sbvCast = S.sFromIntegral 
instance SBVCast Word8 Int32 where
  sbvCast = S.sFromIntegral 
instance SBVCast Word8 Int64 where
  sbvCast = S.sFromIntegral 

--------------------------------------------------------------------------------

instance SBVCast Word16 Word8 where
  sbvCast = S.sFromIntegral 
instance SBVCast Word16 Word16 where
  sbvCast = S.sFromIntegral 
instance SBVCast Word16 Word32 where
  sbvCast = S.sFromIntegral
instance SBVCast Word16 Word64 where
  sbvCast = S.sFromIntegral

instance SBVCast Word16 Int8 where
  sbvCast = S.sFromIntegral 
instance SBVCast Word16 Int16 where
  sbvCast = S.sFromIntegral 
instance SBVCast Word16 Int32 where
  sbvCast = S.sFromIntegral 
instance SBVCast Word16 Int64 where
  sbvCast = S.sFromIntegral 

--------------------------------------------------------------------------------

instance SBVCast Word32 Word8 where
  sbvCast = S.sFromIntegral 
instance SBVCast Word32 Word16 where
  sbvCast = S.sFromIntegral 
instance SBVCast Word32 Word32 where
  sbvCast = S.sFromIntegral 
instance SBVCast Word32 Word64 where
  sbvCast = S.sFromIntegral

instance SBVCast Word32 Int8 where
  sbvCast = S.sFromIntegral 
instance SBVCast Word32 Int16 where
  sbvCast = S.sFromIntegral 
instance SBVCast Word32 Int32 where
  sbvCast = S.sFromIntegral 
instance SBVCast Word32 Int64 where
  sbvCast = S.sFromIntegral 

--------------------------------------------------------------------------------

instance SBVCast Word64 Word8 where
  sbvCast = S.sFromIntegral 
instance SBVCast Word64 Word16 where
  sbvCast = S.sFromIntegral 
instance SBVCast Word64 Word32 where
  sbvCast = S.sFromIntegral 
instance SBVCast Word64 Word64 where
  sbvCast = S.sFromIntegral 

instance SBVCast Word64 Int8 where
  sbvCast = S.sFromIntegral 
instance SBVCast Word64 Int16 where
  sbvCast = S.sFromIntegral 
instance SBVCast Word64 Int32 where
  sbvCast = S.sFromIntegral 
instance SBVCast Word64 Int64 where
  sbvCast = S.sFromIntegral 

--------------------------------------------------------------------------------

instance SBVCast Int8 Int8 where
  sbvCast = S.sFromIntegral 
instance SBVCast Int8 Int16 where
  sbvCast = S.sFromIntegral  
instance SBVCast Int8 Int32 where
  sbvCast = S.sFromIntegral  
instance SBVCast Int8 Int64 where
  sbvCast = S.sFromIntegral  

instance SBVCast Int8 Word8 where
  sbvCast = S.sFromIntegral 
instance SBVCast Int8 Word16 where
  sbvCast = S.sFromIntegral 
instance SBVCast Int8 Word32 where
  sbvCast = S.sFromIntegral 
instance SBVCast Int8 Word64 where
  sbvCast = S.sFromIntegral 

--------------------------------------------------------------------------------

instance SBVCast Int16 Int8 where
  sbvCast = S.sFromIntegral 
instance SBVCast Int16 Int16 where
  sbvCast = S.sFromIntegral  
instance SBVCast Int16 Int32 where
  sbvCast = S.sFromIntegral 
instance SBVCast Int16 Int64 where
  sbvCast = S.sFromIntegral 

instance SBVCast Int16 Word8 where
  sbvCast = S.sFromIntegral 
instance SBVCast Int16 Word16 where
  sbvCast = S.sFromIntegral 
instance SBVCast Int16 Word32 where
  sbvCast = S.sFromIntegral 
instance SBVCast Int16 Word64 where
  sbvCast = S.sFromIntegral 
--------------------------------------------------------------------------------

instance SBVCast Int32 Int8 where
  sbvCast = S.sFromIntegral 
instance SBVCast Int32 Int16 where
  sbvCast = S.sFromIntegral  
instance SBVCast Int32 Int32 where
  sbvCast = S.sFromIntegral
instance SBVCast Int32 Int64 where
  sbvCast = S.sFromIntegral 

instance SBVCast Int32 Word8 where
  sbvCast = S.sFromIntegral 
instance SBVCast Int32 Word16 where
  sbvCast = S.sFromIntegral 
instance SBVCast Int32 Word32 where
  sbvCast = S.sFromIntegral 
instance SBVCast Int32 Word64 where
  sbvCast = S.sFromIntegral 
--------------------------------------------------------------------------------

instance SBVCast Int64 Int8 where
  sbvCast = S.sFromIntegral 
instance SBVCast Int64 Int16 where
  sbvCast = S.sFromIntegral  
instance SBVCast Int64 Int32 where
  sbvCast = S.sFromIntegral
instance SBVCast Int64 Int64 where
  sbvCast = S.sFromIntegral

instance SBVCast Int64 Word8 where
  sbvCast = S.sFromIntegral 
instance SBVCast Int64 Word16 where
  sbvCast = S.sFromIntegral 
instance SBVCast Int64 Word32 where
  sbvCast = S.sFromIntegral 
instance SBVCast Int64 Word64 where
  sbvCast = S.sFromIntegral 
--------------------------------------------------------------------------------

