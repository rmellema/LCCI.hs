{-|
 - This modeul breaks the cyclical reference between Syntax and Model
-}
module Model (
    Event,
    UpdateModel(..),
) where

newtype Event = Event Int
instance Ord Event
instance Eq Event
instance Show Event

data UpdateModel

instance Show UpdateModel
instance Eq UpdateModel
