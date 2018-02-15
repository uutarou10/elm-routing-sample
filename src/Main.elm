module Main exposing (..)

import Html exposing (Html, text, div, h1, h2, ul, li, a)
import Html.Attributes exposing (href)
import Navigation exposing (Location)
import UrlParser exposing (..)


---- ROUTING ----


type Route
    = Root
    | About
    | Blog Int
    | NotFound



--RouteのURLを定義する…的な感じだろうか


matcher : Parser (Route -> a) a
matcher =
    oneOf
        [ map Root top
        , map About (s "about")
        , map Blog (s "blog" </> int)
        ]


parseLocation : Location -> Route
parseLocation location =
    case (parseHash matcher location) of
        Just route ->
            route

        Nothing ->
            NotFound



---- MODEL ----


type alias Model =
    { route : Route }


init : Location -> ( Model, Cmd Msg )
init initLocation =
    let
        initRoute =
            parseLocation initLocation
    in
        ( { route = initRoute }, Cmd.none )



---- UPDATE ----


type Msg
    = ChangeLocation Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeLocation location ->
            let
                newRoute =
                    parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ links, page model ]


page : Model -> Html Msg
page model =
    case model.route of
        Root ->
            h2 [] [ text "トップページだよ" ]

        About ->
            h2 [] [ text "aboutページだよ" ]

        Blog articleId ->
            h2 [] [ text <| (toString articleId) ++ "番の記事だよ" ]

        NotFound ->
            h2 [] [ text "んなページねーよ" ]


links : Html Msg
links =
    div []
        [ ul []
            [ li [] [ a [ href "#/" ] [ text "Top" ] ]
            , li [] [ a [ href "#/about" ] [ text "About" ] ]
            , li [] [ a [ href "#/blog/1" ] [ text "Blog(1)" ] ]
            ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Navigation.program ChangeLocation
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
