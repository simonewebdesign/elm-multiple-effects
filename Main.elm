
import Signal exposing (Address, Mailbox)
import Task exposing (Task)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import StartApp
import Effects exposing (Effects, Never)


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
  }


initialModel : Model
initialModel =
  { counter1 = 1
  , counter2 = 2
  }


-- UPDATE

type Action
  = NoOp
  | IncrementCounter1
  | IncrementCounter2
  | PerformEffect


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
      ( model, anEffect )


anEffect =
  Effects.batch
    [ Task.succeed ()
      |> Effects.task
      |> Effects.map (always IncrementCounter1)
    , Task.succeed ()
      |> Effects.task
      |> Effects.map (always IncrementCounter2)
    ]


-- VIEW

view : Address Action -> Model -> Html
view address model =
  div []
    [
      text <| toString model
    , button
        [ onClick address PerformEffect ]
        [ text "Perform the increment Effect" ]
    ]


-- SIGNALS

port tasks : Signal (Task Never ())
port tasks =
  app.tasks
