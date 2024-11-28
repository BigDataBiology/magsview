module Shared exposing (Data, Model, Msg(..), SharedMsg(..), template)

import BackendTask exposing (BackendTask)
import Effect exposing (Effect)
import FatalError exposing (FatalError)
import Html exposing (..)
import Html.Events
import Html.Attributes as Hattr exposing (..)
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import UrlPath exposing (UrlPath)
import Route exposing (Route)
import SharedTemplate exposing (SharedTemplate)
import View exposing (View)

import Components.Navbar exposing (navbar)
import Components.BottomNav exposing (bottomNav)

import Bootstrap.CDN as CDN

template : SharedTemplate Msg Model Data msg
template =
    { init = init
    , update = update
    , view = view
    , data = data
    , subscriptions = subscriptions
    , onPageChange = Nothing
    }

type Msg
    = SharedMsg SharedMsg
    | MenuClicked

type alias Data =
    ()

type SharedMsg
    = NoOp

type alias Model =
    { showMenu : Bool
    }

init :
    Pages.Flags.Flags
    ->
        Maybe
            { path :
                { path : UrlPath
                , query : Maybe String
                , fragment : Maybe String
                }
            , metadata : route
            , pageUrl : Maybe PageUrl
            }
    -> ( Model, Effect Msg )
init flags maybePagePath =
    ( { showMenu = False }
    , Effect.none
    )

update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SharedMsg globalMsg ->
            ( model, Effect.none )

        MenuClicked ->
            ( { model | showMenu = not model.showMenu }, Effect.none )


subscriptions : UrlPath -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


data : BackendTask FatalError Data
data =
    BackendTask.succeed ()


view sharedData page model toMsg pageView =
    { body =
        [ CDN.stylesheet
        , div [Hattr.class "app"] 
            [ navbar
            , Html.main_ [] pageView.body
            ]
        , bottomNav
        ]
    , title = pageView.title
    }
