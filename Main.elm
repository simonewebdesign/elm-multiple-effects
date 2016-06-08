
import Signal exposing (Address, Mailbox)
import Task exposing (Task)
import Html exposing (..)
--import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import StartApp
import Effects exposing (Effects, Never)
import Alerts exposing (..)
import Alert exposing (..)
--import Alerts exposing (Action(..))
--import Alert exposing (Alert, Kind(..))

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
  | IncrementBoth
  | AlertsAction Alerts.Action


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

    IncrementBoth ->
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


-- VIEW

view : Address Action -> Model -> Html
view address model =
  div []
    [ div [] [ text <| toString model ]
    , div []
        [ button
            [ onClick address IncrementBoth ]
            [ text "Increment both" ]
        ]
    , div []
        [ button
            [ onClick address <| AlertsAction <| Alerts.Add Alert.initialModel ]
            [ text "Add alert" ]
        ]
    , Alerts.view (Signal.forwardTo address AlertsAction) model.alerts
    ]


-- SIGNALS

port tasks : Signal (Task Never ())
port tasks =
  app.tasks
