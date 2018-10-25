module Data.BigNumber
  ( BigNumber, parseBigNumber, config
  , ConfigParams, RoundingMode
  , roundUp, roundDown, roundCeil, roundFloor
  , roundHalfUp, roundHalfDown, roundHalfEven, roundHalfCeil, roundHalfFloor
  , ModuloMode, modRoundUp, modRoundDown, modRoundFloor, modRoundHalfEven
  , modEuclid
  , isBigNumber, isInteger, isFinite, isNaN, isNegative, isPositive, isZero
  , toNumber, toString, toExponential, toFixed, toFormat, toFraction, valueOf
  , abs', negate', idiv, sqrt, pow
  , intValue, precision, decimalPlaces, shiftedBy, randomBigNumber
  ) where

import Prelude
import Data.Int as Int
import Data.Either (Either (..))
import Data.Tuple (Tuple (..))
import Data.Function.Uncurried (Fn3, runFn3, Fn2, runFn2, Fn5, runFn5)
import Data.Tuple.Native (T2, prj)
import Data.Typelevel.Num.Reps (d0, d1)
import Data.Generic.Rep (class Generic, Constructor (..), Argument (..))
import Row.Class (class SubRow)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Partial.Unsafe (unsafePartial)



foreign import data BigNumber :: Type

instance genericBigNumber :: Generic BigNumber (Constructor "BigNumber" (Argument String)) where
  from x = Constructor (Argument (toString x))
  to (Constructor (Argument s)) = unsafePartial $ case parseBigNumber s of
    Right x -> x


foreign import parseBigNumberImpl :: Fn3 (forall e a. e -> Either e a) (forall e a. a -> Either e a) String (Either Error BigNumber)

parseBigNumber :: String -> Either Error BigNumber
parseBigNumber = runFn3 parseBigNumberImpl Left Right

foreign import configImpl :: forall o. EffectFn1 o Unit

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


type FormatParams =
  ( decimalSeparator :: String
  , groupSeparator :: String
  , groupSize :: Int
  , secondaryGroupSize :: Int
  , fractionGroupSeparator :: String
  , fractionGroupSize :: Int
  )


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

config :: forall o format
        . SubRow o (ConfigParams { | format })
       => SubRow format FormatParams
       => { | o } -> Effect Unit
config = runEffectFn1 configImpl


foreign import isBigNumber :: forall a. a -> Boolean
foreign import isInteger :: BigNumber -> Boolean
foreign import isFinite :: BigNumber -> Boolean
foreign import isNaN :: BigNumber -> Boolean
foreign import isNegative :: BigNumber -> Boolean
foreign import isPositive :: BigNumber -> Boolean
foreign import isZero :: BigNumber -> Boolean

foreign import toNumber :: BigNumber -> Number
foreign import toString :: BigNumber -> String
foreign import toExponential :: BigNumber -> String
foreign import toFixed :: BigNumber -> String
foreign import toFormat :: BigNumber -> String
foreign import toFractionImpl :: Fn2 BigNumber BigNumber (T2 String String)
foreign import valueOf :: BigNumber -> String

toFraction :: BigNumber -> BigNumber -> Tuple String String
toFraction a b =
  let x = runFn2 toFractionImpl a b
  in  Tuple (prj d0 x) (prj d1 x)

foreign import eqBigNumberImpl :: Fn2 BigNumber BigNumber Boolean
foreign import gtBigNumberImpl :: Fn2 BigNumber BigNumber Boolean
foreign import gteBigNumberImpl :: Fn2 BigNumber BigNumber Boolean
foreign import ltBigNumberImpl :: Fn2 BigNumber BigNumber Boolean
foreign import lteBigNumberImpl :: Fn2 BigNumber BigNumber Boolean
foreign import compareBigNumberImpl :: Fn5 Ordering Ordering Ordering BigNumber BigNumber Ordering

foreign import absImpl :: BigNumber -> BigNumber
foreign import plusBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber
foreign import minusBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber
foreign import negateImpl :: BigNumber -> BigNumber
foreign import timesBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber
foreign import divBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber
foreign import idivBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber
foreign import moduloBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber
foreign import sqrt :: BigNumber -> BigNumber
foreign import powBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber

abs' :: BigNumber -> BigNumber
abs' = absImpl

negate' :: BigNumber -> BigNumber
negate' = negateImpl

idiv :: BigNumber -> BigNumber -> BigNumber
idiv = runFn2 idivBigNumberImpl

pow :: BigNumber -> BigNumber -> BigNumber
pow = runFn2 powBigNumberImpl

foreign import intValue :: BigNumber -> BigNumber
foreign import precisionImpl :: Fn2 BigNumber Int BigNumber
foreign import decimalPlacesImpl :: Fn2 BigNumber Int BigNumber
foreign import randomBigNumber :: Effect BigNumber
foreign import shiftedByImpl :: Fn2 BigNumber Int BigNumber

precision :: BigNumber -> Int -> BigNumber
precision = runFn2 precisionImpl

decimalPlaces :: BigNumber -> Int -> BigNumber
decimalPlaces = runFn2 decimalPlacesImpl

shiftedBy :: BigNumber -> Int -> BigNumber
shiftedBy = runFn2 shiftedByImpl




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
  degree = Int.floor <<< toNumber <<< intValue <<< abs'
  div = runFn2 divBigNumberImpl
  mod = runFn2 moduloBigNumberImpl
