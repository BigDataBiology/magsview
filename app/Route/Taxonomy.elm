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
    | LeafNode String (List MAG)

nameOf : TreeNode -> String
nameOf treeNode =
    case treeNode of
        CollapsedNode name _ -> name
        ExpandedNode name _ -> name
        LeafNode name _ -> name


type alias Model =
    { tree : TreeNode
    }


type Msg =
    ExpandNode String
    | CollapseNode String

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
            CollapseNode target -> { model | tree = collapseNode target model.tree }
    in
        ( nmodel
        , Effect.none
        )

expand1 : Int -> List MAG -> List TreeNode
expand1 level mags =
    let
        getTaxon : Int -> MAG -> String
        getTaxon ell mag =
            mag.taxonomy
                |> String.split ";"
                |> getIx ell
        getIx : Int -> List String -> String
        getIx i =
            List.drop i
                >> List.head
                >> Maybe.withDefault ""
        taxa : List String
        taxa = mags
                |> List.map (getTaxon level)
                |> Set.fromList
                |> Set.toList
        autoexpand : Int -> String -> List MAG -> TreeNode
        autoexpand sublevel taxon submags =
            let
                subtaxa = submags
                    |> List.map (getTaxon sublevel)
                    |> Set.fromList
                    |> Set.toList
            in
                if String.startsWith "s__" taxon || String.isEmpty taxon
                then
                    LeafNode taxon submags
                else
                    case subtaxa of
                        [subt] -> ExpandedNode taxon [autoexpand (sublevel + 1) subt submags]
                        _ -> CollapsedNode taxon submags
    in
        taxa
            |> List.map (\t ->
                    mags
                        |> List.filter (\m -> getTaxon level m == t)
                        |> autoexpand (level + 1) t
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
        LeafNode _ _ ->
            treeNode

collapseNode : String -> TreeNode -> TreeNode
collapseNode target treeNode =
    case treeNode of
        CollapsedNode name children -> treeNode
        ExpandedNode name children ->
            if name == target then
                CollapsedNode name (getAllMAGs children)
            else
                ExpandedNode name (List.map (collapseNode target) children)
        LeafNode _ _ ->
            treeNode

getAllMAGs : List TreeNode -> List MAG
getAllMAGs =
        List.map (\child ->
                case child of
                    CollapsedNode _ mags -> mags
                    ExpandedNode _ mags -> getAllMAGs mags
                    LeafNode _ mags -> mags
            )
        >> List.concat

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
showTree treeNode =
    Html.div [HtmlAttr.class "tree-node"]
    (case treeNode of
        CollapsedNode name children ->
            [ Html.p [HtmlAttr.class "taxonomy-header"]
                [ Html.text <| nameOf treeNode
                , Html.span [HE.onClick (ExpandNode name)]
                    [ Html.text " [expand]" ]
                ]
            , Html.p []
                [ Html.text ("Number of genomes: " ++ String.fromInt (List.length children))
                ]
            ]
        ExpandedNode _ children ->
            [ Html.p [HtmlAttr.class "taxonomy-header"]
                [ Html.text <| nameOf treeNode
                , Html.span [HE.onClick (CollapseNode (nameOf treeNode))]
                    [ Html.text " [collapse]" ]
                ]
            , Html.div [
                ]
                (List.map showTree children)
            ]
        LeafNode _ children ->
            [ Html.p [HtmlAttr.class "taxonomy-header"]
                [ Html.text <| nameOf treeNode ]
            , Html.ul []
                ( children
                    |> List.sortBy .id
                    |> List.map (\mag ->
                        Html.li (if mag.isRepresentative
                                    then [HtmlAttr.class "representative"]
                                    else [])
                            [ Html.a [ HtmlAttr.href "#" ]
                                [ Html.text <|
                                        (mag.id ++ " (" ++ String.fromFloat mag.completeness ++ "% completness/" ++ String.fromFloat mag.contamination ++ "% contamination)")
                                ]
                            ]
                    )
                )
            ]
    )
