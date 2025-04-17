module SiteMarkdown exposing (mdToHtml)

import Html exposing (Html)
import Markdown


markdownOptions : Markdown.Options
markdownOptions =
    { githubFlavored = Just { tables = True, breaks = False }
    , defaultHighlighting = Nothing
    , sanitize = False
    , smartypants = False
    }

mdToHtml : String -> Html.Html msg
mdToHtml body = Markdown.toHtmlWith markdownOptions [] body
