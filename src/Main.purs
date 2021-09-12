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
import Data.List (List(..), (:))

data Msg = Correct | Wrong
type Question =
    { prompt :: String
    , choice1 :: String
    , choiceX :: String
    , choice2 :: String
    }
type Model =
    { questions :: List Question
    , questionNumber :: Int
    , points :: Int
    }

msgOnClick :: forall r. Msg -> Html.IProp (onClick:: MouseEvent | r) Msg
msgOnClick msg = onClick alwaysIncr
    where
      alwaysIncr :: forall x. x -> Maybe Msg
      alwaysIncr =  always_ msg

columnStyle  :: forall r. Html.IProp (style :: String | r) Msg
columnStyle = styles
    [ Style  "display" "flex"
    , Style "flex-direction" "column"
    , Style "align-items" "center"
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
app =
    { init: init
    , render: render
    , update: update
    }

init :: Model
init = { questions , questionNumber: 1 , points: 0 } where
   questions =
     { prompt: "Which is an agile principle?"
     , choice1: "Customer satisfaction through early and continuous software delivery"
     , choiceX: "Customer collaboration over contract negotiation"
     , choice2: "Emphasizes the performance of the entire system"
     } :
     { prompt: "Which is an agile principle?"
     , choice1: "Welcome changing requirements, even late in development"
     , choiceX: "Responding to change over following a plan"
     , choice2: "Create the right to left feedback loops"
     } :
     Nil

update :: Model -> Msg -> Model
update model@{questions: Nil} action = model
update model@{questions: Cons _ tail, questionNumber} action =
    case action of
        Correct -> newModel {points = newModel.points + 1}
        Wrong -> newModel
    where newModel = model {questions = tail, questionNumber = questionNumber + 1}

render :: Model -> Html Msg
render {questions: Nil, points} =
    column [Html.text $ "Succsess " <> show points <>"/2 points!"]
render {questions: Cons {prompt, choice1, choiceX, choice2} _, questionNumber} = column
   [ Html.text ("Question: " <> show questionNumber <> ". " <> prompt)
   , column
       [ Html.button [msgOnClick Correct] [Html.text $ "1. " <> choice1]
       , Html.button [msgOnClick Wrong] [Html.text $ "X. " <> choiceX]
       , Html.button [msgOnClick Wrong] [Html.text $ "2. " <> choice2]
       ]
   ]

main :: Effect Unit
main = do
    inst <- makeWithSelector app "body"
    inst.run