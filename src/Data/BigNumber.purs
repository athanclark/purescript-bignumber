module Data.BigNumber where

import Prelude
import Data.Either (Either)
import Data.Function.Uncurried (Fn3)
import Data.Record.Class (class Subrow)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Uncurried (EffFn1, runEffFn1)


foreign import data BIGNUMBER :: Effect

foreign import data BigNumber :: Type


foreign import parseBigNumber :: Fn3 (forall e a. e -> Either e a) (forall e a. a -> Either e a) String (Either Error BigNumber)

foreign import configImpl :: forall o eff. EffFn1 (bigNumber :: BIGNUMBER | eff) o Unit

type ConfigParams =
  ( "DECIMAL_PLACES" :: Int
  )

config :: forall o eff
        . Subrow o ConfigParams
       => { | o } -> Eff (bigNumber :: BIGNUMBER | eff) Unit
config = runEffFn1 configImpl
