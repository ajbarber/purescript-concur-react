module Concur.React where

import Prelude

import Concur.Core.Discharge (discharge, dischargePartialEffect)
import Concur.Core.Types (Widget, display)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import React as R

type HTML
  = Array R.ReactElement

-- React apparently requires wrapping state inside an object
type ComponentState
  = {view :: HTML}

mkComponentState :: HTML -> ComponentState
mkComponentState v = { view: v }

componentClassWithMount :: forall a. Effect Unit -> Effect Unit -> Widget HTML a -> R.ReactClass {}
componentClassWithMount onMount onUnmount winit = R.component "Concur" component
  where
    component this = do
      Tuple winit' v <- dischargePartialEffect winit
      pure { state: mkComponentState v
           , render: render <$> R.getState this
           , componentDidMount: onMount *> handler this (Right winit')
           , componentWillUnmount: onUnmount
           }
    handler this (Right r) = do
      v <- discharge (handler this) r
      void $ R.writeState this (mkComponentState v)
    handler _ (Left err) = do
      log ("FAILED! " <> show err)
      pure unit
    render st = R.toElement st.view

componentClass :: forall a. Widget HTML a -> R.ReactClass {}
componentClass = componentClassWithMount mempty mempty

renderComponent :: forall a. Widget HTML a -> R.ReactElement
renderComponent init = R.createLeafElement (componentClass init) {}

renderComponentWithLifeCycle ::
  forall a.
  Effect Unit ->
  Effect Unit ->
  Widget HTML a ->
  R.ReactElement
renderComponentWithLifeCycle onMount onUnmount w = R.createLeafElement (componentClassWithMount onMount onUnmount w) {}

runAsComponent ::
  forall a.
  Effect Unit ->
  Effect Unit ->
  Widget HTML a ->
  Widget HTML a
runAsComponent onMount onUnmount w = display jsx
  where
    jsx = [ renderComponentWithLifeCycle onMount onUnmount w ]
