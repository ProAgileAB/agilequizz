module Main where

import Prelude

import Effect (Effect)
import Spork.PureApp (PureApp, makeWithSelector)
import Spork.Html as Html
import Spork.Html.Events (always_, onClick)
import Spork.Html.Properties (Style(..), styles)
import Web.UIEvent.MouseEvent (MouseEvent)
import Data.Maybe (Maybe)

data Msg = Incr

incrOnClick :: forall r. Html.IProp (onClick:: MouseEvent | r) Msg
incrOnClick = onClick alwaysIncr
    where
      alwaysIncr :: forall x. x -> Maybe Msg
      alwaysIncr =  always_ Incr

column  :: forall r. Html.IProp (style :: String | r) Msg
column = styles
    [ Style  "display" "flex"
    , Style "flex-direction" "column"
    ]

app :: PureApp Int Msg
app = {
    init: 0,
    render: \x ->
        Html.div
            [ column ]
            [ Html.text ("Count: " <> show x)
            , Html.button [incrOnClick] [Html.text "click me!"]
            ],
    update: \x Incr -> x + 1
}

main :: Effect Unit
main = do
    inst <- makeWithSelector app "body"
    inst.run