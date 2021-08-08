module Main where

import Prelude

import Effect (Effect)
import Spork.PureApp (PureApp, makeWithSelector)
import Spork.Html as Html
import Spork.Html (Html)
import Spork.Html.Events (always_, onClick)
import Spork.Html.Properties (Style(..), styles)
import Web.UIEvent.MouseEvent (MouseEvent)
import Data.Maybe (Maybe)

data Msg = Incr | Decr
type Model = Int

msgOnClick :: forall r. Msg -> Html.IProp (onClick:: MouseEvent | r) Msg
msgOnClick msg = onClick alwaysIncr
    where
      alwaysIncr :: forall x. x -> Maybe Msg
      alwaysIncr =  always_ msg

columnStyle  :: forall r. Html.IProp (style :: String | r) Msg
columnStyle = styles
    [ Style  "display" "flex"
    , Style "flex-direction" "column"
    ]

rowStyle  :: forall r. Html.IProp (style :: String | r) Msg
rowStyle = styles
    [ Style  "display" "flex"
    , Style "flex-direction" "row"
    ]

column :: Array (Html Msg) -> Html Msg
column = Html.div [columnStyle]
row :: Array (Html Msg) -> Html Msg
row = Html.div [rowStyle]

app :: PureApp Model Msg
app = {
    init: init,
    render: render,
    update: update
}

init :: Model
init = 1

update :: Model -> Msg -> Model
update model action = case action of
    Incr -> model + 1
    Decr -> model - 1

render :: Model -> Html Msg
render x = column
   [ Html.text ("Count: " <> show x)
   , row
       [ Html.button [msgOnClick Incr] [Html.text "up (+)"]
       , Html.button [msgOnClick Decr] [Html.text "down (-)"]
       ]
   ]

main :: Effect Unit
main = do
    inst <- makeWithSelector app "body"
    inst.run