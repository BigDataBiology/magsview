module Route.Index exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import Html exposing (..)
import Html.Attributes as Hattr exposing (..)
import Pages.Url
import PagesMsg exposing (PagesMsg)
import UrlPath
import Route
import RouteBuilder exposing (App, StatelessRoute)
import Shared
import View exposing (View)


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    {}


type alias Data =
    { message : String
    }


type alias ActionData =
    {}


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.single
        { head = head
        , data = data
        }
        |> RouteBuilder.buildNoState { view = view }


data : BackendTask FatalError Data
data =
    BackendTask.succeed Data
        |> BackendTask.andMap
            (BackendTask.succeed "Hello!")


head :
    App Data ActionData RouteParams
    -> List Head.Tag
head app =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "MagsView"
        , image =
            { url = [ "images", "icon-png.png" ] |> UrlPath.join |> Pages.Url.fromPath
            , alt = "MagsView logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "MagsView"
        , locale = Nothing
        , title = "MagsView"
        }
        |> Seo.website


view :
    App Data ActionData RouteParams
    -> Shared.Model
    -> View (PagesMsg Msg)
view app shared =
    { title = "MagsView"
    , body =
        [ div [Hattr.class "indexPage_aboutSection"]
            [ div [Hattr.class "indexPage_aboutSection_content"] 
                [ h1 [] [text """A web interface to visualize & explore
                metagenome-assembled genomes (MAGs), their taxonomic
                classifications & functional annotations"""]
                ] 
            ]
        , div [Hattr.class "indexPage_exploreSection"] [
            div [Hattr.class "indexPage_exploreSection_element"] 
                [ h2 [] [text "Explore Genomes"]
                , ul [Hattr.class "indexPage_exploreSection_element_description"] [li [] [text "Download genomes based on filters"], li [] [text "View genome size, completeness, contamination & total coding sequences"]]
                , button [Hattr.class "actionButton"] [a [Hattr.href "/genomes"] [text "Go to Genomes"]]
                    
                ],
            div [Hattr.class "indexPage_exploreSection_element"] 
                [ h2 [] [text "Explore Taxonomy"]
                , ul [Hattr.class "indexPage_exploreSection_element_description"] [li [] [text "View taxonomic annotations of MAGs"]]
                , button [Hattr.class "actionButton"] [a [Hattr.href "/taxonomy"] [text "Go to Taxonomy"]]
                ]
        ]
        ]
    }
