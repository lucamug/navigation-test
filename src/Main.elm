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


routeToMaybeFragment : Route -> Maybe String
routeToMaybeFragment route =
    case route of
        Route_Disabled ->
            Nothing

        Route_01 ->
            Just "Route_01"

        Route_02 ->
            Just "Route_02"

        Route_03 ->
            Just "Route_03"

        Route_04 ->
            Just "Route_04"


routeToFragment : String -> Route -> String
routeToFragment defaultFragment route =
    Maybe.withDefault defaultFragment (routeToMaybeFragment route)


routeToHref : Route -> String
routeToHref route =
    "#/" ++ routeToFragment "" route


maybeRouteFromHref : Href -> Maybe Route
maybeRouteFromHref href =
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
            Maybe.withDefault dummyUrl (Url.fromString href)

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
    Url.Parser.map route (Url.Parser.s (routeToFragment "" route))



-- ███████ ████████  █████  ████████ ███████
-- ██         ██    ██   ██    ██    ██
-- ███████    ██    ███████    ██    █████
--      ██    ██    ██   ██    ██    ██
-- ███████    ██    ██   ██    ██    ███████


type alias Payload =
    -- This is in reality a complex payload
    -- that cannot go in the url
    String


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
    State_01 "abc"


stateToHref : State -> String
stateToHref state =
    routeToHref (stateToRoute state)


stateToString : State -> String
stateToString state =
    String.replace "Route_" "" (routeToFragment disabledLabel (stateToRoute state))
        ++ " "
        ++ Maybe.withDefault "" (stateToPayload state)



-- ███    ███  ██████  ██████  ███████ ██
-- ████  ████ ██    ██ ██   ██ ██      ██
-- ██ ████ ██ ██    ██ ██   ██ █████   ██
-- ██  ██  ██ ██    ██ ██   ██ ██      ██
-- ██      ██  ██████  ██████  ███████ ███████


type alias Model =
    { statesBefore : List State
    , state : State
    , statesAfter : List State
    , log : List String
    }



-- ██ ███    ██ ██ ████████
-- ██ ████   ██ ██    ██
-- ██ ██ ██  ██ ██    ██
-- ██ ██  ██ ██ ██    ██
-- ██ ██   ████ ██    ██


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( log, initState, cmd ) =
            case maybeRouteFromHref flags.href of
                Nothing ->
                    ( "init: Routes == Nothing! Starting from State_01"
                    , initialState
                    , cmdToChangeTitle initialState
                    )

                Just routeFromHref ->
                    if routeFromHref == Route_Disabled then
                        ( "init: Route_Disabled, so starting from State_Disabled"
                        , State_Disabled
                        , cmdToChangeTitle State_Disabled
                        )

                    else if routeFromHref == stateToRoute initialState then
                        ( "init: This is the right initial state"
                        , initialState
                        , cmdToChangeTitle initialState
                        )

                    else
                        ( "init: The only initial state allowed is State_01, redirecting"
                        , initialState
                        , cmdToGoToState initialState
                        )
    in
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
    | OnPopState Href
    | PayloadChange String
    | ResetLog



-- ██    ██ ██████  ██████   █████  ████████ ███████
-- ██    ██ ██   ██ ██   ██ ██   ██    ██    ██
-- ██    ██ ██████  ██   ██ ███████    ██    █████
-- ██    ██ ██      ██   ██ ██   ██    ██    ██
--  ██████  ██      ██████  ██   ██    ██    ███████


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnPopState href ->
            ( model, Cmd.none )
                |> newLocationHref href

        GoTo state ->
            model
                |> goTo state

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


newLocationHref : Href -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
newLocationHref href ( model, cmd ) =
    case maybeRouteFromHref href of
        Nothing ->
            { model | log = "newLocationHref: Routes == Nothing! Going to State_01" :: model.log }
                |> resetHistoryAfter
                |> goTo initialState

        Just routeFromHref ->
            let
                routeFromState =
                    stateToRoute model.state
            in
            if routeFromHref == routeFromState then
                ( { model | log = "newLocationHref: Correct flow because routes from HREF and from State are the same" :: model.log }, cmd )

            else if Just routeFromHref == Maybe.map stateToRoute (List.head model.statesBefore) then
                ( { model
                    | log = "newLocationHref: It seems that the user moved back" :: model.log
                    , statesBefore = Maybe.withDefault [] <| List.tail model.statesBefore
                    , state = Maybe.withDefault State_Disabled <| List.head model.statesBefore
                    , statesAfter = model.state :: model.statesAfter
                  }
                , cmd
                )

            else if Just routeFromHref == Maybe.map stateToRoute (List.head model.statesAfter) then
                ( { model
                    | log = "newLocationHref: It seems that the user moved forward" :: model.log
                    , statesBefore = model.state :: model.statesBefore
                    , state = Maybe.withDefault State_Disabled <| List.head model.statesAfter
                    , statesAfter = Maybe.withDefault [] <| List.tail model.statesAfter
                  }
                , cmd
                )

            else
                { model
                    | log =
                        ("newLocationHref: Routes not synced with current state. One is \""
                            ++ routeToFragment "" routeFromHref
                            ++ "\", the other is \""
                            ++ routeToFragment "" routeFromState
                            ++ "\". Going to State_01"
                        )
                            :: model.log
                }
                    |> resetHistoryAfter
                    |> goTo initialState


goTo : State -> Model -> ( Model, Cmd Msg )
goTo state model =
    ( { model
        | state = state
        , statesBefore = model.state :: model.statesBefore
        , log = ("goTo: Going to new state " ++ routeToFragment disabledLabel (stateToRoute state)) :: model.log
      }
    , cmdToGoToState state
    )


resetHistoryAfter : Model -> Model
resetHistoryAfter model =
    { model
        | log = "resetHistoryAfter:" :: model.log
        , statesAfter = []
    }



-- ██   ██ ███████ ██      ██████  ███████ ██████  ███████
-- ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
-- ███████ █████   ██      ██████  █████   ██████  ███████
-- ██   ██ ██      ██      ██      ██      ██   ██      ██
-- ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████


type alias Href =
    String


type alias Flags =
    { href : Href }


cmdToChangeTitle : State -> Cmd msg
cmdToChangeTitle state =
    changeTitle <| stateToString state


cmdToGoToState : State -> Cmd msg
cmdToGoToState state =
    historyPushState <| { href = stateToHref state, title = stateToString state }


disabledLabel : String
disabledLabel =
    "Disabled"



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
                        , onChange = PayloadChange
                        , placeholder = Nothing
                        , text = payload
                        }
            , row [ width fill, spacing 20 ]
                [ logView "Log" model.log identity
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


cleanRoute : String -> String
cleanRoute =
    String.replace "Route_" ""


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
        { label = text <| cleanRoute <| routeToFragment disabledLabel (stateToRoute newState)
        , onPress = Just (GoTo <| newState)
        }


wrongButton : Route -> Element msg
wrongButton route =
    link (buttonAttrs False ++ [ Background.color <| rgba 1 0 0 0.3 ]) { label = text <| routeToHref route, url = routeToHref route }


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


port historyPushState : { href : Href, title : String } -> Cmd msg


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
