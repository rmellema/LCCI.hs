{-|
 - This modeul breaks the cyclical reference between Syntax and Model
-}
module Model (
    Event,
    UpdateModel(..),
) where
import Util (PrettyShow)

newtype Event = Event Int
instance Ord Event
instance Eq Event
instance Show Event
instance PrettyShow Event

data UpdateModel

instance Show UpdateModel
instance Eq UpdateModel
instance PrettyShow UpdateModel
