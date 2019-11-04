module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import React.Basic (ReactComponent)
import React.Basic.DOM (render)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (component, element, useState, (/\))
import React.Basic.Hooks as React
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

mkCounter :: Effect (ReactComponent {})
mkCounter = do
  component "Counter" \props -> React.do
    counter /\ setCounter <- useState 0
    pure
      $ R.button
          { onClick: handler_ $ setCounter ((+) 1)
          , children: [ R.text $ "Increment: " <> show counter ]
          }

main :: Effect Unit
main = do
  container <- window >>= document <#> toNonElementParentNode >>= getElementById "root"
  counter <- mkCounter
  case container of
    Just c -> do
      _ <- log "Let's go!"
      let
        app = element counter {}
      render app c
    Nothing -> log "No root found..."
