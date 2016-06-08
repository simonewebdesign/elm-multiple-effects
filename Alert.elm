module Alert where
  --( Alert
  --, Model
  --, initialModel
  --, Action
  --, update
  --, view
  --, Kind(..)
  --) where

import Signal exposing (Address)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Task exposing (sleep, andThen, succeed)
import Effects exposing (Effects, map, none, task)


type alias Alert = Model


type alias Model =
  { kind : Kind
  , message : List Html
  , dismissed : Bool
  }


initialModel : Model
initialModel =
  { kind = Success
  , message = [ text "Default message" ]
  , dismissed = False
  }


type Kind
  = Success
  | Info
  | Warning
  | Danger


type Action
  = ScheduleDismission
  | Dismiss


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    ScheduleDismission ->
      ( model
      , sleep 2000 `andThen` succeed
        |> task
        |> map (always Dismiss)
      )

    Dismiss ->
      ( { model | dismissed = True }
      , none
      )


view : Address Action -> Model -> Html
view address model =
  div
    [ classList [ ( "alert alert-dismissible alert-" ++ (kindToString model.kind), True )
                , ( "hide", model.dismissed )
                ]
    , attribute "role" "alert"
    ]
    [ span
        [ class "alert-message" ]
        model.message
    , button
        [ type' "button"
        , class "close"
        , attribute "aria-label" "Close"
        , onClick address Dismiss
        ]
        [ span
            [ attribute "aria-hidden" "true" ]
            [ text "×" ]
        , span
            [ class "sr-only" ]
            [ text "Close" ]
        ]
    ]


kindToString : Kind -> String
kindToString kind =
  case kind of
    Success -> "success"
    Info    -> "info"
    Warning -> "warning"
    Danger  -> "danger"
