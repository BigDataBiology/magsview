module Pages.Taxonomy exposing (page, Model, Msg)

import Html
import Html.Attributes as HtmlAttr
import Html.Events as HE
import Set

import Route exposing (Route)
import Page exposing (Page)
import View exposing (View)

import Shared
import Effect exposing (Effect)
import View exposing (View)

import W.InputCheckbox as InputCheckbox
import W.Modal as Modal
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table

import DataModel exposing (MAG)
import Data exposing (mags)
import Layouts


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
    , showDownloadModal : Maybe (List MAG)
    }


type Msg =
    ExpandNode String
    | CollapseNode String
    | DownloadMAGs (List MAG)
    | ClearDownload

type alias RouteParams =
    {}

page : Shared.Model -> Route () -> Page Model Msg
page shared r =
    Page.new
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
    |> Page.withLayout (\_ -> Layouts.Main {})


type alias Data =
    { mags : List MAG }

type alias ActionData =
    {}


init :
    ()
    -> (Model, Effect Msg)
init _ =
    let
        model =
            { tree = CollapsedNode "r__Root" mags
            , showDownloadModal = Nothing
            }
    in
        ( model
        , Effect.none
        )
update :
    Msg
    -> Model
    -> (Model, Effect Msg)
update msg model =
    let
        nmodel = case msg of
            ExpandNode target -> { model | tree = expandNode 0 target model.tree }
            CollapseNode target -> { model | tree = collapseNode target model.tree }
            DownloadMAGs ms -> { model | showDownloadModal = Just ms }
            ClearDownload -> { model | showDownloadModal = Nothing }
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
        isSingle : Bool
        isSingle = List.length taxa == 1
    in
        taxa
            |> List.map (\t ->
                    mags
                        |> List.filter (\m -> getTaxon level m == t)
                        |> if String.startsWith "s__" t
                            then LeafNode t
                            else if isSingle
                            then ExpandedNode t << expand1 (level + 1)
                            else CollapsedNode t
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


view :
    Model
    -> View Msg
view model =
    let
        m = model
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
            , showTree model.showDownloadModal model.tree
            ]
        }

showTree : Maybe (List MAG) -> TreeNode -> Html.Html Msg
showTree showDownloadModal treeNode =
    let
        name = nameOf treeNode
        tlevel = case String.split "__" name of
            [] -> "root"
            (x::_) -> case x of
                "r" -> "root"
                "d" -> "domain"
                "k" -> "kingdom"
                "p" -> "phylum"
                "c" -> "class"
                "o" -> "order"
                "f" -> "family"
                "g" -> "genus"
                "s" -> "species"
                _ -> "unknown"
        sname = case String.split "__" name of
            [_, x] -> String.replace "_" " " x
            _ -> ""
        card =
            let isC = case treeNode of
                    CollapsedNode _ _ -> True
                    ExpandedNode _ _ -> False
                    LeafNode _ _ -> False
                isL = case treeNode of
                    CollapsedNode _ _ -> False
                    ExpandedNode _ _ -> False
                    LeafNode _ _ -> True
            in Html.p []
                    [ Html.span [HtmlAttr.class "taxonomy-header"]
                        [ if String.isEmpty sname
                            then Html.em [] [Html.text "unnamed"]
                            else Html.text sname]
                    , Html.span [HtmlAttr.class "taxonomy-class"]
                        [Html.text (" ("++tlevel++")")]
                    , if isL
                        then Html.span [] []
                        else Html.span
                            [HE.onClick ((if isC then ExpandNode else CollapseNode) name)]
                            [ Html.text (" ["++ (if isC then "+" else "-")++ "]")]
                    ]
    in Html.div [ HtmlAttr.class "tree-node"
                , HtmlAttr.class ("taxonomy-node-" ++ tlevel)]
        (card :: (case treeNode of
            CollapsedNode _ children ->
                ( Html.p []
                    [ Html.text ("Number of genomes: " ++ String.fromInt (List.length children))
                    ]
                ::
                (if name == "r__Root" && List.length children > 1 then
                    []
                else
                    [ Html.p []
                        [ Html.a [ HtmlAttr.href ("/genomes?taxonomy=" ++ name)]
                            [ Html.text "[Genomes in table]" ]
                        ]
                    ]
                ))
            ExpandedNode _ children ->
                (List.map (showTree showDownloadModal) children)
            LeafNode _ children ->
                [ Html.ol []
                    ( children
                        |> List.sortBy .id
                        |> List.map (\mag ->
                            Html.li (if mag.isRepresentative
                                        then [HtmlAttr.class "representative"]
                                        else [])
                                [ Html.a [ HtmlAttr.href ("/genome/"++mag.id)]
                                    [ Html.text <|
                                            (mag.id ++ " (" ++ String.fromFloat mag.completeness ++ "% completness/" ++ String.fromFloat mag.contamination ++ "% contamination)")
                                    ]
                                ]
                        )
                    )
                , Html.p [HtmlAttr.style "font-size" "small"]
                    [ Html.text "Bolded elements are representative genomes." ]
                , Html.p [HtmlAttr.style "text-align" "right"]
                    [ ButtonGroup.buttonGroup
                        [ ButtonGroup.small ]
                        [ ButtonGroup.button [ Button.outlinePrimary, Button.small
                            , Button.onClick (DownloadMAGs <| List.filter (.isRepresentative) children) ]
                            [ Html.text "Download representatives" ]
                        , ButtonGroup.button
                            [ Button.outlinePrimary , Button.small
                            , Button.onClick (DownloadMAGs children) ]
                            [ Html.text "Download all" ]
                        ]
                    ]
                , Modal.view []
                    { isOpen = showDownloadModal /= Nothing
                    , onClose = Just ClearDownload
                    , content = [Html.div []
                        [Html.pre []
                        ((Html.text "# Run this command to download the genomes:\n")::
                        (showDownloadModal
                            |> Maybe.withDefault []
                            |> List.map (\m ->
                                Html.text <| "wget https://mags-base.big-data-biology.org/SH_DOGS/" ++ m.id ++ ".fna.gz\n"
                            )
                        ))]
                        ]
                    }
                ]
        ))
