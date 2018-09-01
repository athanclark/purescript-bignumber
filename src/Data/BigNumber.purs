module Data.BigNumber
  ( BIGNUMBER, BigNumber, parseBigNumber, config
  , ConfigParams, RoundingMode
  , roundUp, roundDown, roundCeil, roundFloor
  , roundHalfUp, roundHalfDown, roundHalfEven, roundHalfCeil, roundHalfFloor
  , ModuloMode, modRoundUp, modRoundDown, modRoundFloor, modRoundHalfEven
  , modEuclid
  , isBigNumber, maxBigNumber, minBigNumber, randomBigNumber
  ) where

import Prelude
import Data.Either (Either (..))
import Data.Function.Uncurried (Fn3, runFn3, Fn2, runFn2)
import Data.Record.Class (class Subrow)
import Data.Tuple.Native (T2)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Uncurried (EffFn1, runEffFn1)


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

foreign import maxBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber

maxBigNumber :: BigNumber -> BigNumber -> BigNumber
maxBigNumber = runFn2 maxBigNumberImpl

foreign import minBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber

minBigNumber :: BigNumber -> BigNumber -> BigNumber
minBigNumber = runFn2 minBigNumberImpl

foreign import randomBigNumber :: forall eff. Eff (bigNumber :: BIGNUMBER | eff) BigNumber
