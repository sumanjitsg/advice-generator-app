module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (alt, class, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)



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
    = Error
    | Loading
    | Success Advice


type alias Advice =
    { id : Int
    , text : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getAdvice )



-- UPDATE


type Msg
    = GotAdvice (Result Http.Error Advice)
    | Clicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAdvice (Ok advice) ->
            ( Success advice, Cmd.none )

        GotAdvice (Err _) ->
            ( Error, Cmd.none )

        Clicked ->
            ( model, getAdvice )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Error ->
            main_ [] [ p [] [ text "Error!" ] ]

        Loading ->
            main_ [] [ p [] [ text "Loading..." ] ]

        Success advice ->
            main_ []
                [ div {- card container -} [ class "card-container" ]
                    [ h1 []
                        [ text ("ADVICE #" ++ String.fromInt advice.id) ]
                    , p []
                        [ text ("\"" ++ advice.text ++ "\"") ]
                    , img [ src "../public/images/pattern-divider-desktop.svg", alt "" ] []
                    ]
                , div {- button container -} [ class "btn-container" ]
                    [ button [ onClick Clicked ]
                        [ img [ src "../public/images/icon-dice.svg", alt "" ] [] ]
                    ]
                ]



-- HTTP
{- GET random advice from https://api.adviceslip.com/ -}


getAdvice : Cmd Msg
getAdvice =
    Http.get
        { url = "https://api.adviceslip.com/advice"
        , expect = Http.expectJson GotAdvice adviceDecoder
        }



-- DECODERS
{-
   Decode the API response into Advice type
   Example:
   {
       "slip": {
           "id": 123,
           "advice": "Don't be afraid to ask questions."
       }
   }
-}


adviceDecoder : Decoder Advice
adviceDecoder =
    Decode.field "slip" slipDecoder


slipDecoder : Decoder Advice
slipDecoder =
    Decode.map2
        Advice
        (Decode.field "id" Decode.int)
        (Decode.field "advice" Decode.string)
