
import Signal exposing (Address, Mailbox)
import Task exposing (Task)
import Html exposing (..)
--import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import StartApp
import Effects exposing (Effects, Never)
import Alerts


app : StartApp.App Model
app =
  StartApp.start
    { init = ( initialModel, Effects.none )
    , update = update
    , view = view
    , inputs = []
    }


main : Signal Html
main =
  app.html


-- MODEL

type alias Model =
  { counter1 : Int
  , counter2 : Int
  , alerts : Alerts.Model
  }


initialModel : Model
initialModel =
  { counter1 = 1
  , counter2 = 2
  , alerts = Alerts.init
  }


-- UPDATE

type Action
  = NoOp
  | IncrementCounter1
  | IncrementCounter2
  | PerformEffect
  | AlertsAction Alerts.Action
  --| CreateAlert


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case Debug.log "Main" action of
    NoOp ->
      ( model
      , Effects.none
      )

    IncrementCounter1 ->
      ( { model | counter1 = model.counter1 + 1 }, Effects.none )

    IncrementCounter2 ->
      ( { model | counter2 = model.counter2 + 1 }, Effects.none )

    PerformEffect ->
      ( model
      , Effects.batch
          [ Task.succeed ()
            |> Effects.task
            |> Effects.map (always IncrementCounter1)
          , Task.succeed ()
            |> Effects.task
            |> Effects.map (always IncrementCounter2)
          ]
      )

    AlertsAction subAction ->
      let
        (newAlerts, effects) =
          Alerts.update subAction model.alerts
      in
        ( { model | alerts = newAlerts }
        , Effects.map AlertsAction effects
        )
      --( { model | alerts = Alerts.update subAction model.alerts }
      --, Effects.none
      --)


-- VIEW

view : Address Action -> Model -> Html
view address model =
  div []
    [ div [] [ text <| toString model ]
    , div []
        [ button
            [ onClick address PerformEffect ]
            [ text "Perform the increment Effect" ]
        ]
    , div []
        [ button []
            --[ onClick address CreateAlert ]
            [ text "Click to create an alert" ]
        ]
    ]


-- SIGNALS

port tasks : Signal (Task Never ())
port tasks =
  app.tasks
