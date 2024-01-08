module Main exposing (main)

import Browser
import Html exposing (..)
import Http



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Model
    = Failure
    | Loading
    | Success


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, Cmd.none )



-- UPDATE


type Msg
    = GotAdvice (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotAdvice (Ok _) ->
            ( Success, Cmd.none )

        GotAdvice (Err _) ->
            ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            div [] [ text "Failure" ]

        Loading ->
            div [] [ text "Loading" ]

        Success ->
            div [] [ text "Success" ]
