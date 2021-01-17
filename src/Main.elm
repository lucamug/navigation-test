port module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Url
import Url.Parser



-- ██████   ██████  ██    ██ ████████ ███████
-- ██   ██ ██    ██ ██    ██    ██    ██
-- ██████  ██    ██ ██    ██    ██    █████
-- ██   ██ ██    ██ ██    ██    ██    ██
-- ██   ██  ██████   ██████     ██    ███████


type Route
    = Route_Disabled
    | Route_01
    | Route_02
    | Route_03
    | Route_04


routeToMaybeFragment : Route -> Maybe Fragment
routeToMaybeFragment route =
    case route of
        Route_Disabled ->
            Nothing

        Route_01 ->
            Just <| Fragment "Route_01"

        Route_02 ->
            Just <| Fragment "Route_02"

        Route_03 ->
            Just <| Fragment "Route_03"

        Route_04 ->
            Just <| Fragment "Route_04"


routeToFragment : Fragment -> Route -> Fragment
routeToFragment defaultFragment route =
    Maybe.withDefault defaultFragment (routeToMaybeFragment route)


routeToHREF : Route -> HREF
routeToHREF route =
    HREF <| "#/" ++ (fragmentToString <| routeToFragment (Fragment "") route)


routeToString : Route -> String
routeToString route =
    fragmentToString <| routeToFragment (Fragment "Disabled") route


maybeRouteFromHREF : HREF -> Maybe Route
maybeRouteFromHREF href =
    let
        dummyUrl =
            --
            -- Url.toString dummyUrl == "https://"
            --
            { protocol = Url.Https
            , host = ""
            , port_ = Nothing
            , path = ""
            , query = Nothing
            , fragment = Nothing
            }

        url =
            Maybe.withDefault dummyUrl (Url.fromString (hrefToString href))

        modifiedUrl =
            --
            -- We copy the fragment in to the path first because the
            -- parser only works on the path
            --
            -- https://example.com/#/ciao -> https://example.com/ciao
            --
            { url
                | path = Maybe.withDefault "" url.fragment
                , fragment = Nothing
            }
    in
    Url.Parser.parse urlParser modifiedUrl


urlParser : Url.Parser.Parser (Route -> a) a
urlParser =
    Url.Parser.oneOf
        [ Url.Parser.map Route_Disabled Url.Parser.top
        , urlParserSingleRoute Route_01
        , urlParserSingleRoute Route_02
        , urlParserSingleRoute Route_03
        , urlParserSingleRoute Route_04
        ]


urlParserSingleRoute : Route -> Url.Parser.Parser (Route -> c) c
urlParserSingleRoute route =
    Url.Parser.map route (Url.Parser.s (fragmentToString <| routeToFragment (Fragment "") route))



-- ███████ ████████  █████  ████████ ███████
-- ██         ██    ██   ██    ██    ██
-- ███████    ██    ███████    ██    █████
--      ██    ██    ██   ██    ██    ██
-- ███████    ██    ██   ██    ██    ███████


type
    Payload
    -- This is in reality a complex payload
    -- that cannot go in the url
    = Payload String


payloadToString : Payload -> String
payloadToString (Payload string) =
    string


type State
    = State_Disabled
    | State_01 Payload
    | State_02 Payload
    | State_03 Payload
    | State_04 Payload


stateToRoute : State -> Route
stateToRoute state =
    case state of
        State_Disabled ->
            Route_Disabled

        State_01 _ ->
            Route_01

        State_02 _ ->
            Route_02

        State_03 _ ->
            Route_03

        State_04 _ ->
            Route_04


changePayload : Payload -> State -> State
changePayload payload state =
    case state of
        State_Disabled ->
            State_Disabled

        State_01 _ ->
            State_01 payload

        State_02 _ ->
            State_02 payload

        State_03 _ ->
            State_03 payload

        State_04 _ ->
            State_04 payload


stateToPayload : State -> Maybe Payload
stateToPayload state =
    case state of
        State_Disabled ->
            Nothing

        State_01 payload ->
            Just payload

        State_02 payload ->
            Just payload

        State_03 payload ->
            Just payload

        State_04 payload ->
            Just payload


initialState : State
initialState =
    -- State 01 should be the only state that can be generated.
    -- All other state require a payload that exsist already.
    State_01 <| Payload "abc"


stateToHREF : State -> HREF
stateToHREF state =
    routeToHREF (stateToRoute state)


stateToString : State -> String
stateToString state =
    String.replace "Route_" "" (routeToString (stateToRoute state))
        ++ " "
        ++ (payloadToString <| Maybe.withDefault (Payload "") (stateToPayload state))



-- ███    ███  ██████  ██████  ███████ ██
-- ████  ████ ██    ██ ██   ██ ██      ██
-- ██ ████ ██ ██    ██ ██   ██ █████   ██
-- ██  ██  ██ ██    ██ ██   ██ ██      ██
-- ██      ██  ██████  ██████  ███████ ███████


type alias Model =
    { statesBefore : List State
    , state : State
    , statesAfter : List State
    , log : List LogItem
    }



-- ██ ███    ██ ██ ████████
-- ██ ████   ██ ██    ██
-- ██ ██ ██  ██ ██    ██
-- ██ ██  ██ ██ ██    ██
-- ██ ██   ████ ██    ██


init : Flags -> ( Model, Cmd Msg )
init flags =
    case maybeRouteFromHREF (HREF flags.hrefAsString) of
        Nothing ->
            initHelper
                { log = LogItem "init: Routes == Nothing! Starting from State_01"
                , initState = initialState
                , cmd = cmdToChangeTitle initialState
                }

        Just routeFromHREF ->
            if routeFromHREF == Route_Disabled then
                initHelper
                    { log = LogItem "init: Route_Disabled, so starting from State_Disabled"
                    , initState = State_Disabled
                    , cmd = cmdToChangeTitle State_Disabled
                    }

            else if routeFromHREF == stateToRoute initialState then
                initHelper
                    { log = LogItem "init: This is the right initial state"
                    , initState = initialState
                    , cmd = cmdToChangeTitle initialState
                    }

            else
                initHelper
                    { log = LogItem "init: The only initial state allowed is State_01, redirecting"
                    , initState = initialState
                    , cmd = cmdToGoToState initialState
                    }


initHelper : { log : LogItem, initState : State, cmd : Cmd Msg } -> ( Model, Cmd Msg )
initHelper { log, initState, cmd } =
    ( { statesBefore = []
      , state = initState
      , statesAfter = []
      , log = [ log ]
      }
    , cmd
    )



-- ███    ███ ███████  ██████
-- ████  ████ ██      ██
-- ██ ████ ██ ███████ ██   ███
-- ██  ██  ██      ██ ██    ██
-- ██      ██ ███████  ██████


type Msg
    = GoTo State
    | OnPopState String
    | PayloadChange Payload
    | ResetLog



-- ██    ██ ██████  ██████   █████  ████████ ███████
-- ██    ██ ██   ██ ██   ██ ██   ██    ██    ██
-- ██    ██ ██████  ██   ██ ███████    ██    █████
-- ██    ██ ██      ██   ██ ██   ██    ██    ██
--  ██████  ██      ██████  ██   ██    ██    ███████


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnPopState hrefAsString ->
            ( model, Cmd.none )
                |> newLocationHREF (HREF hrefAsString)

        GoTo state ->
            model
                |> goToState state

        PayloadChange payload ->
            ( { model | state = changePayload payload model.state }
                |> resetHistoryAfter
            , Cmd.none
            )

        ResetLog ->
            ( { model | log = [] }, Cmd.none )



-- ██    ██ ██████  ██████   █████  ████████ ███████     ██   ██ ███████ ██      ██████  ███████ ██████  ███████
-- ██    ██ ██   ██ ██   ██ ██   ██    ██    ██          ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
-- ██    ██ ██████  ██   ██ ███████    ██    █████       ███████ █████   ██      ██████  █████   ██████  ███████
-- ██    ██ ██      ██   ██ ██   ██    ██    ██          ██   ██ ██      ██      ██      ██      ██   ██      ██
--  ██████  ██      ██████  ██   ██    ██    ███████     ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████


newLocationHREF : HREF -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
newLocationHREF href ( model, cmd ) =
    case maybeRouteFromHREF href of
        Nothing ->
            { model | log = LogItem "newLocationHREF: Routes == Nothing! Going to State_01" :: model.log }
                |> resetHistoryAfter
                |> goToState initialState

        Just routeFromHREF ->
            let
                routeFromState =
                    stateToRoute model.state
            in
            if routeFromHREF == routeFromState then
                ( { model | log = LogItem "newLocationHREF: Correct flow because routes from HREF and from State are the same" :: model.log }, cmd )

            else if Just routeFromHREF == Maybe.map stateToRoute (List.head model.statesBefore) then
                ( { model
                    | log = LogItem "newLocationHREF: It seems that the user moved back" :: model.log
                    , statesBefore = Maybe.withDefault [] <| List.tail model.statesBefore
                    , state = Maybe.withDefault State_Disabled <| List.head model.statesBefore
                    , statesAfter = model.state :: model.statesAfter
                  }
                , cmd
                )

            else if Just routeFromHREF == Maybe.map stateToRoute (List.head model.statesAfter) then
                ( { model
                    | log = LogItem "newLocationHREF: It seems that the user moved forward" :: model.log
                    , statesBefore = model.state :: model.statesBefore
                    , state = Maybe.withDefault State_Disabled <| List.head model.statesAfter
                    , statesAfter = Maybe.withDefault [] <| List.tail model.statesAfter
                  }
                , cmd
                )

            else
                { model
                    | log =
                        (LogItem <|
                            "newLocationHREF: Routes not synced with current state. One is \""
                                ++ routeToString routeFromHREF
                                ++ "\", the other is \""
                                ++ routeToString routeFromState
                                ++ "\". Going to State_01"
                        )
                            :: model.log
                }
                    |> resetHistoryAfter
                    |> goToState initialState


goToState : State -> Model -> ( Model, Cmd Msg )
goToState state model =
    ( { model
        | state = state
        , statesBefore = model.state :: model.statesBefore
        , log = (LogItem <| "goToState: Going to the new state " ++ routeToString (stateToRoute state)) :: model.log
      }
    , cmdToGoToState state
    )


resetHistoryAfter : Model -> Model
resetHistoryAfter model =
    { model
        | log = LogItem "resetHistoryAfter:" :: model.log
        , statesAfter = []
    }



-- ██   ██ ███████ ██      ██████  ███████ ██████  ███████
-- ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
-- ███████ █████   ██      ██████  █████   ██████  ███████
-- ██   ██ ██      ██      ██      ██      ██   ██      ██
-- ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████


type HREF
    = HREF String


hrefToString : HREF -> String
hrefToString (HREF string) =
    string


type LogItem
    = LogItem String


logItemToString : LogItem -> String
logItemToString (LogItem string) =
    string


type Fragment
    = Fragment String


fragmentToString : Fragment -> String
fragmentToString (Fragment string) =
    string


type alias Flags =
    { hrefAsString : String }


cmdToChangeTitle : State -> Cmd msg
cmdToChangeTitle state =
    state
        |> stateToString
        |> changeTitle


cmdToGoToState : State -> Cmd msg
cmdToGoToState state =
    { hrefAsString = hrefToString <| stateToHREF state
    , title = stateToString state
    }
        |> historyPushState



-- ██    ██ ██ ███████ ██     ██
-- ██    ██ ██ ██      ██     ██
-- ██    ██ ██ █████   ██  █  ██
--  ██  ██  ██ ██      ██ ███ ██
--   ████   ██ ███████  ███ ███


view : Model -> Html.Html Msg
view model =
    layout [ padding 30, Font.size 16 ] <|
        column [ spacing 20, width fill ]
            [ el [ Font.size 24 ] <| text "Navigation Test"
            , wrappedRow [ spacing 10 ]
                ([]
                    ++ [ button model.state State_Disabled ]
                    ++ [ button model.state initialState ]
                    ++ (case stateToPayload model.state of
                            Nothing ->
                                []

                            Just payload ->
                                [ button model.state (State_02 payload)
                                , button model.state (State_03 payload)
                                , button model.state (State_04 payload)
                                ]
                       )
                )
            , wrappedRow [ spacing 10 ]
                [ link (buttonAttrs False ++ [ Background.color <| rgba 1 0 0 0.3 ]) { label = text "#/wrong", url = "#/wrong" }
                , wrongButton Route_01
                , wrongButton Route_02
                , wrongButton Route_03
                , wrongButton Route_04
                ]
            , wrappedRow [ spacing 10 ]
                [ Input.button (buttonAttrs False ++ [ Background.color <| rgba 0 0 0 0.2 ]) { label = text "Reset Log", onPress = Just ResetLog } ]
            , case stateToPayload model.state of
                Nothing ->
                    el [ height <| px 40 ] <| el [ centerY ] <| text "Payload: Not available"

                Just payload ->
                    Input.text [ height <| px 40 ]
                        { label = Input.labelLeft [] <| text "Payload: "
                        , onChange = \string -> PayloadChange (Payload string)
                        , placeholder = Nothing
                        , text = payloadToString payload
                        }
            , row [ width fill, spacing 20 ]
                [ logView "Log" model.log logItemToString
                , column [ width fill, spacing 20, alignTop ]
                    [ logView "States After" model.statesAfter stateToString
                    , logView "State Current" [ model.state ] stateToString
                    , logView "States Before" model.statesBefore stateToString
                    ]
                ]
            ]



-- ██    ██ ██ ███████ ██     ██     ██   ██ ███████ ██      ██████  ███████ ██████  ███████
-- ██    ██ ██ ██      ██     ██     ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
-- ██    ██ ██ █████   ██  █  ██     ███████ █████   ██      ██████  █████   ██████  ███████
--  ██  ██  ██ ██      ██ ███ ██     ██   ██ ██      ██      ██      ██      ██   ██      ██
--   ████   ██ ███████  ███ ███      ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████


buttonAttrs : Bool -> List (Attribute msg)
buttonAttrs active =
    [ Border.rounded 30
    , Background.color <|
        if active then
            rgba 0.5 1 0 0.8

        else
            rgba 0.5 1 0 0.4
    , paddingXY 30 12
    ]


button : State -> State -> Element Msg
button currentState newState =
    Input.button (buttonAttrs (stateToRoute newState == stateToRoute currentState))
        { label =
            stateToRoute newState
                |> routeToString
                |> String.replace "Route_" ""
                |> text
        , onPress = Just (GoTo <| newState)
        }


wrongButton : Route -> Element msg
wrongButton route =
    link (buttonAttrs False ++ [ Background.color <| rgba 1 0 0 0.3 ])
        { label = text <| hrefToString <| routeToHREF route
        , url = hrefToString <| routeToHREF route
        }


logView : String -> List a -> (a -> String) -> Element msg
logView title log toString =
    let
        attrs1 =
            [ spacing 20
            , Border.width 1
            , Border.color <| rgba 0 0 0 0.2
            , Border.rounded 10
            , width fill
            , alignTop
            ]

        attrs2 =
            [ Background.color <| rgba 1 1 0 0.5
            , width fill
            , padding 10
            , Border.rounded 10
            , Font.bold
            ]

        attrs3 =
            [ Border.widthEach { bottom = 0, left = 0, right = 0, top = 1 }
            , Border.color <| rgba 0 0 0 0.2
            , padding 10
            , width fill
            ]

        attrs4 =
            [ spacing 10
            , width fill
            ]
    in
    column attrs1
        [ el attrs2 <| text title
        , log
            |> List.map (\state -> paragraph attrs3 [ text <| toString state ])
            |> column attrs4
        ]



-- ██████   ██████  ██████  ████████ ███████
-- ██   ██ ██    ██ ██   ██    ██    ██
-- ██████  ██    ██ ██████     ██    ███████
-- ██      ██    ██ ██   ██    ██         ██
-- ██       ██████  ██   ██    ██    ███████


port onPopState : (String -> msg) -> Sub msg


port historyPushState : { hrefAsString : String, title : String } -> Cmd msg


port changeTitle : String -> Cmd msg



-- ███    ███  █████  ██ ███    ██
-- ████  ████ ██   ██ ██ ████   ██
-- ██ ████ ██ ███████ ██ ██ ██  ██
-- ██  ██  ██ ██   ██ ██ ██  ██ ██
-- ██      ██ ██   ██ ██ ██   ████


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> onPopState OnPopState
        }
