module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, src)
import Html.Events exposing (on, onClick)
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
    | Success
        { advice : Advice
        , isPointerOver : Bool
        }


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
    | ButtonClicked
    | PointerOver
    | PointerOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAdvice (Ok advice) ->
            case model of
                Loading ->
                    ( Success { advice = advice, isPointerOver = False }, Cmd.none )

                Success state ->
                    ( Success { state | advice = advice }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotAdvice (Err _) ->
            ( Error, Cmd.none )

        ButtonClicked ->
            ( model, getAdvice )

        PointerOver ->
            case model of
                Success state ->
                    ( Success { state | isPointerOver = True }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PointerOut ->
            case model of
                Success state ->
                    ( Success { state | isPointerOver = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EVENT HANDLERS


onPointerOver : msg -> Attribute msg
onPointerOver msg =
    on "pointerover" (Decode.succeed msg)


onPointerOut : msg -> Attribute msg
onPointerOut msg =
    on "pointerout" (Decode.succeed msg)



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Error ->
            main_ [] [ p [] [ text "Error!" ] ]

        Loading ->
            main_ [] [ p [] [ text "Loading..." ] ]

        Success state ->
            main_ []
                [ div {- card container -} [ class "card-container" ]
                    [ h1 []
                        [ text ("ADVICE #" ++ String.fromInt state.advice.id) ]
                    , p []
                        [ text ("\"" ++ state.advice.text ++ "\"") ]
                    , div [ class "pattern-divider" ] []
                    ]
                , div {- button container -} [ class "btn-container" ]
                    [ button
                        [ onPointerOver PointerOver
                        , onPointerOut PointerOut
                        , onClick ButtonClicked
                        , classList [ ( "pointer-over", state.isPointerOver ) ]
                        ]
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
