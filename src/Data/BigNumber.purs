module Data.BigNumber
  ( BIGNUMBER, BigNumber, parseBigNumber, config
  , ConfigParams, RoundingMode
  , roundUp, roundDown, roundCeil, roundFloor
  , roundHalfUp, roundHalfDown, roundHalfEven, roundHalfCeil, roundHalfFloor
  , ModuloMode, modRoundUp, modRoundDown, modRoundFloor, modRoundHalfEven
  , modEuclid
  , isBigNumber, randomBigNumber
  ) where

import Prelude
import Data.Int as Int
import Data.Either (Either (..))
import Data.Function.Uncurried (Fn3, runFn3, Fn2, runFn2, Fn5, runFn5)
import Data.Record.Class (class Subrow)
import Data.Tuple.Native (T2)
-- import Data.Semiring (class Semiring)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Uncurried (EffFn1, runEffFn1)
import Partial.Unsafe (unsafePartial)


foreign import data BIGNUMBER :: Effect

foreign import data BigNumber :: Type


foreign import parseBigNumberImpl :: Fn3 (forall e a. e -> Either e a) (forall e a. a -> Either e a) String (Either Error BigNumber)

parseBigNumber :: String -> Either Error BigNumber
parseBigNumber = runFn3 parseBigNumberImpl Left Right

foreign import configImpl :: forall o eff. EffFn1 (bigNumber :: BIGNUMBER | eff) o Unit

newtype RoundingMode = RoundingMode Int

roundUp :: RoundingMode
roundUp = RoundingMode 0
roundDown :: RoundingMode
roundDown = RoundingMode 1
roundCeil :: RoundingMode
roundCeil = RoundingMode 2
roundFloor :: RoundingMode
roundFloor = RoundingMode 3
roundHalfUp :: RoundingMode
roundHalfUp = RoundingMode 4
roundHalfDown :: RoundingMode
roundHalfDown = RoundingMode 5
roundHalfEven :: RoundingMode
roundHalfEven = RoundingMode 6
roundHalfCeil :: RoundingMode
roundHalfCeil = RoundingMode 7
roundHalfFloor :: RoundingMode
roundHalfFloor = RoundingMode 8


newtype ModuloMode = ModuloMode Int

modRoundUp :: ModuloMode
modRoundUp = ModuloMode 0
modRoundDown :: ModuloMode
modRoundDown = ModuloMode 1
modRoundFloor :: ModuloMode
modRoundFloor = ModuloMode 3
modRoundHalfEven :: ModuloMode
modRoundHalfEven = ModuloMode 6
modEuclid :: ModuloMode
modEuclid = ModuloMode 9


type ConfigParams format =
  ( "DECIMAL_PLACES" :: Int
  , "ROUNDING_MODE" :: RoundingMode
  , "EXPONENTIAL_AT" :: T2 Int Int
  , "RANGE" :: T2 Int Int
  , "CRYPTO" :: Boolean
  , "MODULO_MODE" :: ModuloMode
  , "POW_PRECISION" :: Int
  , "FORMAT" :: format
  , "ALPHABET" :: String
  )

config :: forall o format eff
        . Subrow o (ConfigParams format)
       => { | o } -> Eff (bigNumber :: BIGNUMBER | eff) Unit
config = runEffFn1 configImpl


foreign import isBigNumber :: forall a. a -> Boolean
foreign import randomBigNumber :: forall eff. Eff (bigNumber :: BIGNUMBER | eff) BigNumber
foreign import compareBigNumberImpl :: Fn5 Ordering Ordering Ordering BigNumber BigNumber Ordering
foreign import absBigNumber :: BigNumber -> BigNumber
foreign import divBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber
foreign import idivBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber
foreign import powBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber
foreign import intValue :: BigNumber -> BigNumber
foreign import eqBigNumberImpl :: Fn2 BigNumber BigNumber Boolean
foreign import isFinite :: BigNumber -> Boolean
foreign import gtBigNumberImpl :: Fn2 BigNumber BigNumber Boolean
foreign import gteBigNumberImpl :: Fn2 BigNumber BigNumber Boolean
foreign import isInteger :: BigNumber -> Boolean
foreign import ltBigNumberImpl :: Fn2 BigNumber BigNumber Boolean
foreign import lteBigNumberImpl :: Fn2 BigNumber BigNumber Boolean
foreign import isNaN :: BigNumber -> Boolean
foreign import isNegative :: BigNumber -> Boolean
foreign import isPositive :: BigNumber -> Boolean
foreign import isZero :: BigNumber -> Boolean
foreign import plusBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber
foreign import minusBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber
foreign import moduloBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber
foreign import timesBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber
foreign import negateBigNumber :: BigNumber -> BigNumber
foreign import precisionBigNumberImpl :: Fn2 BigNumber Int BigNumber
foreign import toNumber :: BigNumber -> Number
foreign import toString :: BigNumber -> String
foreign import sqrt :: BigNumber -> BigNumber


instance eqBigNumber :: Eq BigNumber where
  eq = runFn2 eqBigNumberImpl

instance ordBigNumber :: Ord BigNumber where
  compare = runFn5 compareBigNumberImpl LT EQ GT

instance showBigNumber :: Show BigNumber where
  show = toString

instance semiringBigNumber :: Semiring BigNumber where
  add = runFn2 plusBigNumberImpl
  zero = unsafePartial $ case parseBigNumber "0" of
    Right x -> x
  one = unsafePartial $ case parseBigNumber "1" of
    Right x -> x
  mul = runFn2 timesBigNumberImpl

instance ringBigNumber :: Ring BigNumber where
  sub = runFn2 minusBigNumberImpl

instance commutativeRingBigNumber :: CommutativeRing BigNumber

instance divisionRingBigNumber :: DivisionRing BigNumber where
  recip = runFn2 divBigNumberImpl one

instance euclideanRingBigNumber :: EuclideanRing BigNumber where
  degree = Int.floor <<< toNumber <<< intValue <<< absBigNumber
  div = runFn2 divBigNumberImpl
  mod = runFn2 moduloBigNumberImpl

instance fieldBigNumber :: Field BigNumber
