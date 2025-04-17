module Route.Taxonomy exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import BackendTask.Http
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import Html
import Html.Attributes as HtmlAttr
import Html.Events as HE
import Set

import Pages.Url
import PagesMsg exposing (PagesMsg)
import RouteBuilder exposing (App, StatefulRoute, StatelessRoute)
import Server.Request as Request exposing (Request)
import Server.Response as Response exposing (Response)
import Shared
import Effect exposing (Effect)
import View exposing (View)

import W.InputCheckbox as InputCheckbox
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table

import DataModel exposing (MAG)

type TreeNode =
    CollapsedNode String (List MAG)
    | ExpandedNode String (List TreeNode)


type alias Model =
    { tree : TreeNode
    }


type Msg =
    ExpandNode String

type alias RouteParams =
    {}


route : StatefulRoute RouteParams Data ActionData Model Msg
route =
    RouteBuilder.preRender
        { head = head
        , data = data
        , pages = BackendTask.succeed [{}]
        }
        |> RouteBuilder.buildWithLocalState
        { init = init
        , subscriptions = (\rp u sm m -> Sub.none)
        , update = update
        , view = view
        }


type alias Data = Shared.Data

type alias ActionData =
    {}


init :
    App Data ActionData RouteParams
    -> Shared.Model
    -> (Model, Effect Msg)
init app shared =
    let
        model =
            { tree = CollapsedNode "root" app.data.mags
            }
    in
        ( model
        , Effect.none
        )
data : RouteParams -> BackendTask FatalError Data
data routeParams = Shared.template.data

update :
    App Data ActionData RouteParams
    -> Shared.Model
    -> Msg
    -> Model
    -> (Model, Effect Msg)
update _ sm msg model =
    let
        nmodel = case msg of
            ExpandNode target -> { model | tree = expandNode 0 target model.tree }
    in
        ( nmodel
        , Effect.none
        )

expand1 : Int -> List MAG -> List TreeNode
expand1 level mags =
    let
        getTaxon : MAG -> String
        getTaxon mag =
            mag.taxonomy
                |> String.split ";"
                |> getIx level
        getIx : Int -> List String -> String
        getIx i =
            List.drop i
                >> List.head
                >> Maybe.withDefault ""
        taxa : List String
        taxa = mags
                |> List.map getTaxon
                |> Set.fromList
                |> Set.toList
    in
        taxa
            |> List.map (\t ->
                    mags
                        |> List.filter (\m -> getTaxon m == t)
                        |> CollapsedNode t
                    )

expandNode : Int -> String -> TreeNode -> TreeNode
expandNode level target treeNode =
    case treeNode of
        CollapsedNode name children ->
            if name == target then
                ExpandedNode name (expand1 level children)
            else
                treeNode
        ExpandedNode name children ->
            ExpandedNode name
                (List.map (expandNode (level + 1) target) children)

head :
    App Data ActionData RouteParams
    -> List Head.Tag
head app =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "TODO"
        , locale = Nothing
        , title = "Genome collection browser"
        }
        |> Seo.website


view :
    App Data ActionData RouteParams
    -> Shared.Model
    -> Model
    -> View (PagesMsg Msg)
view app shared model =
    let
        sel = app.data.mags
    in
        { title = "Taxonomy browser"
        , body =
            [ Html.div []
                [ Html.h1 []
                    [ Html.text "Genome browser" ]
                ]
            , Grid.simpleRow
                [ Grid.col [ ]
                    [ Html.p []
                        [ Html.text "This is a genome collection browser. You can filter the genomes by taxonomy and sort them by completeness or contamination." ]
                    ]
                ]
            , Html.map PagesMsg.fromMsg
                (showTree model.tree)
            ]
        }

showTree : TreeNode -> Html.Html Msg
showTree treeNode = case treeNode of
    CollapsedNode name children ->
        Html.div []
            [ Html.h2 []
                [ Html.text name ]
            , Html.p []
                [ Html.text ("Number of genomes: " ++ String.fromInt (List.length children)) ]
            , Html.p [HE.onClick (ExpandNode name)]
                [ Html.text ("Click to expand "++name) ]
            ]
    ExpandedNode name children ->
        Html.div []
            [ Html.h2 []
                [ Html.text name ]
            , Html.div [
                HtmlAttr.style
                    "padding-left" "20px"
                ]
                (List.map showTree children)
            ]
