{-|
 - This module breaks the cyclical reference between Syntax and Model
-}
module LCCI.Model (
    Event,
    UpdateModel(..),
) where
import LCCI.Util (PrettyShow)

newtype Event = Event Int
instance Ord Event
instance Eq Event
instance Show Event
instance PrettyShow Event

data UpdateModel

instance Show UpdateModel
instance Eq UpdateModel
instance PrettyShow UpdateModel
