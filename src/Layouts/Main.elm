module Layouts.Main exposing (Model, Msg, Props, layout)

import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Layout exposing (Layout)
import Route exposing (Route)
import Shared
import View exposing (View)

import W.Styles
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid

type alias Props =
    {}


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init _ =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view { toContentMsg, model, content } =
    { title = content.title
    , body =
        [ CDN.stylesheet
        , CDN.fontAwesome
        , W.Styles.globalStyles
        , W.Styles.baseTheme
        , Html.node "link" [ HtmlAttr.rel "stylesheet", HtmlAttr.href "/magsview.css" ] []
        , Html.node "link" [ HtmlAttr.rel "stylesheet", HtmlAttr.href "/style.css" ] []
        , Grid.containerFluid []
            [
                Html.nav []
                    [Grid.simpleRow
                        [ Grid.col [] [ Html.a [HtmlAttr.href "/"] [Html.text "Home"]]
                        , Grid.col [] [ Html.a [HtmlAttr.href "/taxonomy"] [Html.text "Taxonomy"]]
                        , Grid.col [] [ Html.a [HtmlAttr.href "/genomes"] [Html.text "Genomes"]]
                        , Grid.col [] [ Html.a [HtmlAttr.href "/other"] [Html.text "Other Data"]]
                        , Grid.col [] [ Html.a [HtmlAttr.href "/about"] [Html.text "About&Contact"]]
                        ]
                    ]
            , Grid.simpleRow [ Grid.col []
                [Html.main_ [] content.body ]
                ]

            , Html.div [HtmlAttr.id "footer"]
                [ Html.p []
                    [ Html.text "For more information about the data, please see "
                    , Html.a
                        [HtmlAttr.href "/manuscript"]
                        [ Html.text "(Cuscó et al. 2025)"]
                    ]
                ]
            ]
        ]
    }
