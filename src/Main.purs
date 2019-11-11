module Main where

import Prelude
import Affjax as Ajax
import Affjax.ResponseFormat as ResponseFormat
import Concur.Core.FRP (Signal, dyn, step)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Control.Alternative ((<|>))
import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Generic (class Decode, decodeJSON, defaultOptions, genericDecode)

newtype User
  = User { id :: Int, name :: String, email :: String }

derive instance newtypeUser :: Newtype User _

derive instance genericUser :: Generic User _

instance showUser :: Show (User) where
  show = genericShow

instance decodeUser :: Decode User where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

data State
  = Init
  | Loading
  | Error
  | Success (Array User)

fetchUsersSignal :: State -> Signal HTML State
fetchUsersSignal state =
  step state
    $ case state of
        Init -> return =<< fetchButton
        Loading -> do
          resp <-
            (liftAff $ Ajax.get ResponseFormat.string "https://jsonplaceholder.typicode.com/users")
              <|> D.button' [ D.text "Loading..." ]
          return $ maybe Error Success $ (hush <<< runExcept <<< decodeJSON <<< _.body) =<< hush resp
        Error -> return =<< fetchButton <|> D.div' [ D.text "An error occured, please try again" ]
        Success users -> do
          let
            names = (_.name <<< unwrap) <$> users
          liftEffect $ log $ show names
          void $ D.div' [ D.text "It worked!", D.div' $ (D.div' <<< pure <<< D.text) <$> names ]
          return state
  where
  fetchButton = D.button [ P.onClick $> Loading ] [ D.text "Fetch users" ]

  return = pure <<< fetchUsersSignal

counterSignal :: Int -> Signal HTML Int
counterSignal count =
  step count do
    n <-
      D.div'
        [ D.p' [ D.text $ "State: " <> show count ]
        , D.button [ P.onClick ] [ D.text "Increment" ] $> count + 1
        , D.button [ P.onClick ] [ D.text "Decrement" ] $> count - 1
        , dyn $ fetchUsersSignal Init
        ]
    liftEffect $ log $ "Count is now: " <> show n
    pure $ counterSignal n

main :: Effect Unit
main = do
  log "Running"
  runWidgetInDom "root" $ dyn $ counterSignal 0
