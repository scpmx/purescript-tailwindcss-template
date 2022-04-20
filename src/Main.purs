module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type State = Int

data Action = Increment | Decrement

component :: ∀ query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: ∀ input. input -> State
initialState _ = 0

render :: ∀ m. State -> H.ComponentHTML Action () m
render _ =
  HH.div [ css "px-8 py-12" ]
    [  
    ]

handleAction :: ∀ output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Decrement ->
    H.modify_ \state -> state - 1

  Increment ->
    H.modify_ \state -> state + 1

css :: ∀ r i. String -> HH.IProp (class :: String | r) i
css = HP.class_ <<< HH.ClassName