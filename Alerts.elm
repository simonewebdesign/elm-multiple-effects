module Alerts
  (alert, alertText, alertSuccess, alertInfo, alertWarning, alertError
  , init
  , Model
  , view
  , Action
  , update
  ) where

{-| This module provides utility functions for displaying alert notifications
to the user. Check out the Public API section to see what's on offer.

In order to use this module, you need to wire it up with your Model using
[the Elm Architecture][arch].

[arch]: https://github.com/evancz/elm-architecture-tutorial

# Basic functions
@docs alert, alertText

# Specialized functions
These functions display alerts in all possible flavors:
`alert`, `info`, `warning`, and `error`.
@docs alertSuccess, alertInfo, alertWarning, alertError

# Elm Architecture
@docs Model, init, view, Action, update
-}

import Signal exposing (Address)
import Html exposing (..)
import Html.Attributes exposing (..)
import Alert exposing (Kind (..))
import Dict exposing (Dict)
import Maybe
import Effects exposing (Effects)


-- PUBLIC API

{-| The most basic and generic function. Displays an alert.

    alert Success [ text "Report created. Yay!" ] model
-}
alert : Alert.Kind -> List Html -> WithAlerts m -> WithAlerts m
alert kind message withAlerts =
  let
    newAlert =
      Alert.Model kind message False
  in
    { withAlerts | alerts = add newAlert withAlerts.alerts }


{-| Display a pure-text alert.

    alert Warning "Something's not looking good." model
-}
alertText : Alert.Kind -> String -> WithAlerts m -> WithAlerts m
alertText kind message model =
  alert kind [ text message ] model


{-| Display a success message.

    alertSuccess "Message" model
-}
alertSuccess : String -> WithAlerts m -> WithAlerts m
alertSuccess message model =
  alertText Success message model


{-| Display an informative message.

    alertInfo "Message" model
-}
alertInfo : String -> WithAlerts m -> WithAlerts m
alertInfo message model =
  alertText Info message model


{-| Display a warning message.

    alertWarning "Message" model
-}
alertWarning : String -> WithAlerts m -> WithAlerts m
alertWarning message model =
  alertText Warning message model


{-| Display an error message.

    alertError "Message" model
-}
alertError : String -> WithAlerts m -> WithAlerts m
alertError message model =
  alertText Danger message model


-- MODEL

type alias Model =
  { alerts : Dict ID Alert.Model
  , nextID : ID
  }


type alias ID =
  Int


type alias WithAlerts m =
  { m | alerts : Model }


init : Model
init =
  { alerts = Dict.empty
  , nextID = 0
  }


-- UPDATE

type Action
  = Add Alert.Model
  | Modify ID Alert.Action


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Add newAlert ->
      ( model |> add newAlert
      , Effects.none
      )

    Modify id alertAction ->
      let
        currentAlert =
          Dict.get id model.alerts
          |> Maybe.withDefault Alert.initialModel

        (updatedAlert, effects) =
          Alert.update alertAction currentAlert

        --updateAlert alert =
        --  case alert of
        --    Just anAlert ->
        --      (updatedAlert, effects) =
        --        Alert.update 
        --    Nothing ->
        --      Alert.initialModel

        --(updatedAlert, effects) =
        --  Al§rt.update
        --    alertAction
        --    (Dict.get id model.alerts
        --    |> Maybe.withDefault Alert.initialModel
        --    )
      in
        ( { model | alerts = Dict.update id (\_ -> Just updatedAlert) model.alerts }
        , Effects.none --Effects.map (Modify id) effects
        )


-- VIEW

view : Address Action -> Model -> Html
view address model =
  div
    [ class "alerts" ]
    (Dict.toList model.alerts
      |> List.map (viewAlert address)
    )


-- PRIVATE

viewAlert : Address Action -> ( ID, Alert.Model ) -> Html
viewAlert address ( id, model ) =
  Alert.view (Signal.forwardTo address (Modify id)) model


add : Alert.Model -> Model -> Model
add newAlert model =
  { model
    | alerts = Dict.insert model.nextID newAlert model.alerts
    , nextID = model.nextID + 1
  }
