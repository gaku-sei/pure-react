module Button (component) where

import Prelude
import Affjax as Ajax
import Affjax.ResponseFormat as ResponseFormat
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data State
  = Off
  | Loading
  | On

derive instance genericState :: Generic State _

instance showState :: Show State where
  show = genericShow

data Action
  = Toggle

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = Off

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.button
    [ HP.title label
    , HE.onClick \_ -> Just Toggle
    ]
    [ HH.text label ]
  where
  label = show state

handleAction ∷ forall o m. MonadAff m => Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  Toggle -> do
    H.modify_ $ const Loading
    response <- H.liftAff $ Ajax.get ResponseFormat.string "https://jsonplaceholder.typicode.com/todos"
    void $ liftEffect $ traverse log (_.body <$> response)
    H.modify_ $ const On
