{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cauldron
  ( 
    Cauldron,
    Constructor,
    constructor
  )
where

import Data.Kind
import Data.SOP (All, K (..))
import Data.SOP.NP
import Data.Typeable
import Multicurryable
import Data.Map.Strict

newtype Cauldron = Cauldron (Map TypeRep Constructor)

data Constructor where
  Constructor ::
    (All Typeable as, Typeable b) =>
    { argumentReps :: [TypeRep],
      resultRep :: TypeRep,
      uncurried :: NP I as -> b
    } ->
    Constructor

constructor ::
  forall (as :: [Type]) (b :: Type) curried.
  ( All Typeable as,
    Typeable b,
    MulticurryableF as b curried (IsFunction curried)
  ) =>
  curried ->
  Constructor
constructor curried =
  Constructor
    { argumentReps =
        collapse_NP $
          cpure_NP @_ @as
            (Proxy @Typeable)
            typeRepHelper,
      resultRep = typeRep (Proxy @b),
      uncurried = multiuncurry @(->) @as @b @curried curried
    }
  where
    typeRepHelper :: forall a. (Typeable a) => K TypeRep a
    typeRepHelper = K $ typeRep (Proxy @a)