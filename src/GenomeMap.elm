module GenomeMap exposing
    ( ContigInfo
    , groupByContig
    , maxPos
    , geneOrder
    , State
    , init
    , Msg
    , update
    , subscriptions
    , Config
    , consumeDragClick
    , zoomAround
    , panBy
    , scrollToGene
    , visibleRange
    , rRnaColor
    , buttonGroup
    , viewControls
    , view
    , viewHelp
    , viewLegend
    , viewNeighborhood
    )

{-| The genome map: an SVG drawing of the contigs of a genome (one row per
contig) with its genes as coloured arrows, which can be zoomed and panned with
buttons, the mouse, the wheel/trackpad and the keyboard.

The map keeps its own view `State` and `Msg`; selecting genes is up to the
page, which passes the messages to use in a `Config`.
-}

import Browser.Dom
import Browser.Events
import Html
import Html.Attributes as HtmlAttr
import Html.Events as HE
import Json.Decode as D
import Svg
import Svg.Attributes as SvgAttr
import Svg.Events as SvgEvents
import Task

import Cog
import EMapper exposing (EMapperGene, GeneType(..))
import Utils exposing (legendItem, showWithCommas)


-- CONTIGS


type alias ContigInfo =
    { name : String
    , maxPos : Int
    , genes : List EMapperGene
    }


{-| Group genes by contig, longest contig first. A contig's length is taken to
be the end of its last gene.
-}
groupByContig : List EMapperGene -> List ContigInfo
groupByContig genes =
    let
        contigNames =
            genes
                |> List.map .contig
                |> List.foldl (\c acc -> if List.member c acc then acc else acc ++ [c]) []
    in
    contigNames
        |> List.map (\cname ->
            let
                cGenes = List.filter (\g -> g.contig == cname) genes
                maxP = cGenes
                    |> List.map .end
                    |> List.maximum
                    |> Maybe.withDefault 0
            in
            { name = cname, maxPos = maxP, genes = cGenes }
        )
        |> List.sortBy (\c -> negate c.maxPos)


{-| The length of the longest contig, which sets the scale of the map.
-}
maxPos : List ContigInfo -> Int
maxPos contigs =
    contigs
        |> List.map .maxPos
        |> List.maximum
        |> Maybe.withDefault 1


{-| Genes in reading order: contig by contig (top to bottom), then by
position. Used for keyboard navigation.
-}
geneOrder : List ContigInfo -> List EMapperGene
geneOrder contigs =
    List.concatMap (\c -> List.sortBy .start c.genes) contigs


-- STATE


{-| Drag-to-pan state of the map. `DragJustEnded` remembers that the last
mouse press panned the map, so that the click which the browser fires on
release does not also select the gene under the pointer.
-}
type Drag
    = NotDragging
    | Dragging { startX : Float, startOffset : Float, moved : Bool }
    | DragJustEnded


{-| The offset runs from 0 (view at the left end) to 1 (view at the right
end) and is only meaningful when zoomed in.
-}
type alias State =
    { zoom : Float
    , offset : Float
    , drag : Drag
    }


init : State
init =
    { zoom = 1.0
    , offset = 0.0
    , drag = NotDragging
    }


type Msg
    = ZoomIn
    | ZoomOut
    | ZoomReset
    | ScrollLeft
    | ScrollRight
    | DragStart Float
    | DragMove Float
    | DragEnd
    | WheelZoom Float Float
    | WheelZoomAt Float Float (Result Browser.Dom.Error Browser.Dom.Element)
    | WheelPan Float


{-| Messages that the map sends to the page.
-}
type alias Config msg =
    { toMsg : Msg -> msg
    , onGeneClick : EMapperGene -> msg
    , onSelectAdjacent : Int -> msg
    , onClose : msg
    }


layout :
    { labelWidth : Float
    , mapWidth : Float
    , rowHeight : Int
    , rowSpacing : Int
    }
layout =
    { labelWidth = 120
    , mapWidth = 970
    , rowHeight = 40
    , rowSpacing = 8
    }


maxZoom : Float
maxZoom =
    50


svgId : String
svgId =
    "genome-map-svg"


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ZoomIn ->
            ( zoomAround 1.5 0.5 state, Cmd.none )

        ZoomOut ->
            ( zoomAround (1 / 1.5) 0.5 state, Cmd.none )

        ZoomReset ->
            ( { state | zoom = 1.0, offset = 0.0 }, Cmd.none )

        ScrollLeft ->
            ( panBy (-0.2) state, Cmd.none )

        ScrollRight ->
            ( panBy 0.2 state, Cmd.none )

        DragStart x ->
            ( { state
                | drag =
                    if state.zoom > 1 then
                        Dragging { startX = x, startOffset = state.offset, moved = False }
                    else
                        NotDragging
              }
            , Cmd.none
            )

        DragMove x ->
            case state.drag of
                Dragging drag ->
                    let
                        dx = x - drag.startX
                        moved = drag.moved || abs dx > 3
                    in
                    ( { state
                        | drag = Dragging { drag | moved = moved }
                        , offset =
                            if moved then
                                panOffset state.zoom drag.startOffset (-dx / layout.mapWidth)
                            else
                                state.offset
                      }
                    , Cmd.none
                    )
                _ ->
                    ( state, Cmd.none )

        DragEnd ->
            ( { state
                | drag =
                    case state.drag of
                        Dragging { moved } ->
                            if moved then DragJustEnded else NotDragging
                        other ->
                            other
              }
            , Cmd.none
            )

        WheelZoom factor pageX ->
            -- The pointer position must be related to where the map is on
            -- the page, which only the DOM knows
            ( state
            , Task.attempt (WheelZoomAt factor pageX) (Browser.Dom.getElement svgId)
            )

        WheelZoomAt factor pageX element ->
            let
                anchor =
                    case element of
                        Ok el ->
                            pointerFraction el pageX
                        Err _ ->
                            0.5
            in
            ( zoomAround factor anchor state, Cmd.none )

        WheelPan px ->
            ( panBy (px / layout.mapWidth) state, Cmd.none )


subscriptions : State -> Sub Msg
subscriptions state =
    case state.drag of
        -- Listen on the whole document so that the drag keeps working when
        -- the pointer leaves the map
        Dragging _ ->
            Sub.batch
                [ Browser.Events.onMouseMove (D.map DragMove (D.field "pageX" D.float))
                , Browser.Events.onMouseUp (D.succeed DragEnd)
                ]
        _ ->
            Sub.none


{-| The browser follows a mouse press that panned the map with a click on
whatever is under the pointer. Call this when a gene is clicked: it returns
the new state if the click is the end of a drag (and so should be ignored),
or `Nothing` if it is a real click.
-}
consumeDragClick : State -> Maybe State
consumeDragClick state =
    if state.drag == DragJustEnded then
        Just { state | drag = NotDragging }
    else
        Nothing


-- PAN AND ZOOM


{-| The genome fraction where the view starts is `offset * (1 - 1/zoom)`.
These two convert between the offset and that starting fraction.
-}
viewStartFromOffset : Float -> Float -> Float
viewStartFromOffset zoom offset =
    offset * (1 - 1 / zoom)


offsetFromViewStart : Float -> Float -> Float
offsetFromViewStart zoom viewStart =
    if zoom <= 1 then
        0
    else
        Basics.clamp 0 1 (viewStart / (1 - 1 / zoom))


panOffset : Float -> Float -> Float -> Float
panOffset zoom offset viewFractions =
    offsetFromViewStart zoom (viewStartFromOffset zoom offset + viewFractions / zoom)


{-| Move the view by a fraction of its own width (negative is leftwards).
-}
panBy : Float -> State -> State
panBy viewFractions state =
    { state | offset = panOffset state.zoom state.offset viewFractions }


{-| Zoom by `factor`, keeping the genome position at `anchor` (a fraction of
the visible width, 0 = left edge) under the same point of the view.
-}
zoomAround : Float -> Float -> State -> State
zoomAround factor anchor state =
    let
        zoom = state.zoom
        newZoom = Basics.clamp 1 maxZoom (zoom * factor)
        anchorPos = viewStartFromOffset zoom state.offset + anchor / zoom
    in
    { state
        | zoom = newZoom
        , offset = offsetFromViewStart newZoom (anchorPos - anchor / newZoom)
    }


{-| If the gene is outside the visible part of the map, centre the view on it.
-}
scrollToGene : List ContigInfo -> EMapperGene -> State -> State
scrollToGene contigs gene state =
    let
        zoom = state.zoom
        globalMax = toFloat (maxPos contigs)
        viewStart = viewStartFromOffset zoom state.offset
        geneStart = toFloat gene.start / globalMax
        geneEnd = toFloat gene.end / globalMax
    in
    if zoom <= 1 || (geneStart >= viewStart && geneEnd <= viewStart + 1 / zoom) then
        state
    else
        { state | offset = offsetFromViewStart zoom ((geneStart + geneEnd) / 2 - 0.5 / zoom) }


{-| The visible range, in bp, of a map whose longest contig is `globalMax` long.
-}
visibleRange : Int -> State -> ( Int, Int )
visibleRange globalMax state =
    let
        viewStart = viewStartFromOffset state.zoom state.offset
    in
    ( round (viewStart * toFloat globalMax)
    , Basics.min globalMax (round ((viewStart + 1 / state.zoom) * toFloat globalMax))
    )


{-| Where `pageX` falls within the drawn map, as a fraction of its width.
The SVG keeps its aspect ratio, so on screens wider than the map it is drawn
centred inside a larger element.
-}
pointerFraction : Browser.Dom.Element -> Float -> Float
pointerFraction el pageX =
    let
        width = el.element.width
        drawnWidth = Basics.min width layout.mapWidth
        left = el.element.x + (width - drawnWidth) / 2
    in
    if drawnWidth <= 0 then
        0.5
    else
        Basics.clamp 0 1 ((pageX - left) / drawnWidth)


-- EVENT DECODERS


{-| Ctrl/Cmd + wheel (which is also what trackpad pinches send) zooms around
the pointer; horizontal or Shift + wheel pans. A plain vertical wheel is left
alone so that it still scrolls the page.
-}
wheelDecoder : Float -> D.Decoder ( Msg, Bool )
wheelDecoder zoom =
    let
        toPixels mode delta =
            case mode of
                1 -> delta * 16
                2 -> delta * 800
                _ -> delta

        fromEvent dx dy mode zoomKey shift pageX =
            let
                pxX = toPixels mode dx
                pxY = toPixels mode dy
                horizontal =
                    if shift && pxX == 0 then pxY
                    else if abs pxX > abs pxY then pxX
                    else 0
            in
            if zoomKey then
                D.succeed ( WheelZoom (Basics.clamp 0.5 2 (e ^ (-pxY * 0.005))) pageX, True )
            else if zoom > 1 && horizontal /= 0 then
                D.succeed ( WheelPan horizontal, True )
            else
                D.fail "not a map gesture"
    in
    D.map6 fromEvent
        (D.field "deltaX" D.float)
        (D.field "deltaY" D.float)
        (D.field "deltaMode" D.int)
        (D.map2 (||) (D.field "ctrlKey" D.bool) (D.field "metaKey" D.bool))
        (D.field "shiftKey" D.bool)
        (D.field "pageX" D.float)
        |> D.andThen identity


keyDecoder : Config msg -> D.Decoder ( msg, Bool )
keyDecoder config =
    let
        fromKey key modified =
            if modified then
                -- Leave Ctrl/Cmd/Alt shortcuts (e.g. browser zoom) alone
                D.fail "modified key"
            else
                case key of
                    "ArrowLeft" -> D.succeed (config.toMsg ScrollLeft)
                    "ArrowRight" -> D.succeed (config.toMsg ScrollRight)
                    "+" -> D.succeed (config.toMsg ZoomIn)
                    "=" -> D.succeed (config.toMsg ZoomIn)
                    "-" -> D.succeed (config.toMsg ZoomOut)
                    "0" -> D.succeed (config.toMsg ZoomReset)
                    "n" -> D.succeed (config.onSelectAdjacent 1)
                    "p" -> D.succeed (config.onSelectAdjacent (-1))
                    "Escape" -> D.succeed config.onClose
                    _ -> D.fail "not a map key"
    in
    D.map2 fromKey
        (D.field "key" D.string)
        (D.map3 (\c m a -> c || m || a)
            (D.field "ctrlKey" D.bool)
            (D.field "metaKey" D.bool)
            (D.field "altKey" D.bool)
        )
        |> D.andThen identity
        |> D.map (\msg -> ( msg, True ))


mouseDownDecoder : D.Decoder Msg
mouseDownDecoder =
    D.field "button" D.int
        |> D.andThen (\button ->
            if button == 0 then
                D.map DragStart (D.field "pageX" D.float)
            else
                D.fail "not the main button"
        )


-- CONTROLS


{-| A row of buttons joined into a single bordered control.
-}
buttonGroup : List (Html.Html msg) -> Html.Html msg
buttonGroup buttons =
    Html.div
        [ HtmlAttr.style "display" "inline-flex"
        , HtmlAttr.style "border" "1px solid #ccc"
        , HtmlAttr.style "border-radius" "4px"
        , HtmlAttr.style "overflow" "hidden"
        ]
        (List.intersperse buttonSep buttons)


buttonSep : Html.Html msg
buttonSep =
    Html.div
        [ HtmlAttr.style "width" "1px"
        , HtmlAttr.style "background" "#ddd"
        , HtmlAttr.style "align-self" "stretch"
        ]
        []


viewControls : Config msg -> State -> List ContigInfo -> Html.Html msg
viewControls config state contigs =
    let
        zoom = state.zoom
        offset = state.offset
        ( viewStartBp, viewEndBp ) = visibleRange (maxPos contigs) state
    in
    Html.div
        [ HtmlAttr.style "display" "flex"
        , HtmlAttr.style "align-items" "center"
        , HtmlAttr.style "gap" "0.25em"
        , HtmlAttr.style "margin-bottom" "0.5em"
        ]
        [ buttonGroup
            [ mapButton (config.toMsg ScrollLeft) (zoom > 1 && offset > 0) "Scroll left" "\u{25C0}" "\u{2039}"
            , mapButton (config.toMsg ZoomOut) (zoom > 1) "Zoom out" "\u{2212}" "\u{2212}"
            , Html.span
                [ HtmlAttr.style "padding" "0.3em 0.6em"
                , HtmlAttr.style "font-size" "0.85em"
                , HtmlAttr.style "color" "#555"
                , HtmlAttr.style "background" "#fafafa"
                , HtmlAttr.style "min-width" "3em"
                , HtmlAttr.style "text-align" "center"
                , HtmlAttr.style "user-select" "none"
                ]
                [ Html.text (String.fromInt (round zoom) ++ "x") ]
            , mapButton (config.toMsg ZoomIn) (zoom < maxZoom) "Zoom in" "+" "+"
            , mapButton (config.toMsg ScrollRight) (zoom > 1 && offset < 1) "Scroll right" "\u{25B6}" "\u{203A}"
            ]
        , if zoom /= 1.0 then
            Html.button
                [ HE.onClick (config.toMsg ZoomReset)
                , HtmlAttr.style "padding" "0.3em 0.7em"
                , HtmlAttr.style "font-size" "0.85em"
                , HtmlAttr.style "cursor" "pointer"
                , HtmlAttr.style "border" "1px solid #ccc"
                , HtmlAttr.style "border-radius" "4px"
                , HtmlAttr.style "background" "#fff"
                , HtmlAttr.style "color" "#555"
                , HtmlAttr.style "margin-left" "0.25em"
                ]
                [ Html.text "Reset" ]
          else
            Html.text ""
        , if zoom > 1 then
            Html.span
                [ HtmlAttr.style "font-size" "0.85em"
                , HtmlAttr.style "color" "#666"
                , HtmlAttr.style "margin-left" "0.5em"
                , HtmlAttr.attribute "aria-live" "polite"
                ]
                [ Html.text (showWithCommas viewStartBp ++ " - " ++ showWithCommas viewEndBp ++ " bp") ]
          else
            Html.text ""
        ]


{-| A toolbar button, whose label can change when it is disabled.
-}
mapButton : msg -> Bool -> String -> String -> String -> Html.Html msg
mapButton msg enabled description enabledLabel disabledLabel =
    Html.button
        [ HE.onClick msg
        , HtmlAttr.title description
        , HtmlAttr.attribute "aria-label" description
        , HtmlAttr.disabled (not enabled)
        , HtmlAttr.style "padding" "0.3em 0.7em"
        , HtmlAttr.style "font-size" "0.9em"
        , HtmlAttr.style "cursor" (if enabled then "pointer" else "default")
        , HtmlAttr.style "border" "none"
        , HtmlAttr.style "background" (if enabled then "#fff" else "#f5f5f5")
        , HtmlAttr.style "color" (if enabled then "#333" else "#bbb")
        , HtmlAttr.style "min-width" "2em"
        , HtmlAttr.style "transition" "background 0.15s"
        ]
        [ Html.text (if enabled then enabledLabel else disabledLabel) ]


viewHelp : Html.Html msg
viewHelp =
    Html.p
        [ HtmlAttr.id "genome-map-help"
        , HtmlAttr.style "font-size" "0.8em"
        , HtmlAttr.style "color" "#777"
        , HtmlAttr.style "margin" "0.3em 0 0"
        ]
        [ Html.text "Drag to pan, Ctrl + scroll or pinch to zoom. Keyboard (click the map first): "
        , Html.text "\u{2190} \u{2192} pan, + \u{2212} zoom, 0 reset, n / p next / previous gene, Esc close gene details."
        ]


-- MAP


view : Config msg -> State -> Maybe EMapperGene -> List ContigInfo -> Html.Html msg
view config state selectedGene contigs =
    let
        zoom = state.zoom
        globalMax = maxPos contigs
        mapCursor =
            case state.drag of
                Dragging _ -> "grabbing"
                _ -> if zoom > 1 then "grab" else "default"
        svgHeight = List.length contigs * (layout.rowHeight + layout.rowSpacing) + 10
        rowCentre i =
            toFloat (i * (layout.rowHeight + layout.rowSpacing)) + 5 + toFloat layout.rowHeight / 2
        -- Virtual width of the full zoomed map
        virtualWidth = layout.mapWidth * zoom
        -- viewBox: we see mapWidth worth of virtual coords, offset into the virtual space
        vbX = state.offset * (virtualWidth - layout.mapWidth)
        -- Scale maps genome position to virtual x coordinate (0 .. virtualWidth)
        scale pos = toFloat pos / toFloat globalMax * virtualWidth
        -- Show arrows only when the visible region is small enough
        visibleBp = toFloat globalMax / zoom
        useArrows = visibleBp < 500000
    in
    Html.div []
        [ Html.div
            [ HtmlAttr.class "genome-map"
            , HtmlAttr.tabindex 0
            , HtmlAttr.attribute "role" "group"
            , HtmlAttr.attribute "aria-label" "Genome map"
            , HtmlAttr.attribute "aria-describedby" "genome-map-help"
            , HE.preventDefaultOn "keydown" (keyDecoder config)
            , HtmlAttr.style "display" "flex"
            , HtmlAttr.style "border" "1px solid #ddd"
            , HtmlAttr.style "user-select" "none"
            ]
            [ -- Fixed labels column
              Svg.svg
                [ SvgAttr.width (String.fromFloat layout.labelWidth)
                , SvgAttr.height (String.fromInt svgHeight)
                , SvgAttr.viewBox ("0 0 " ++ String.fromFloat layout.labelWidth ++ " " ++ String.fromInt svgHeight)
                , SvgAttr.style "font-family: sans-serif; font-size: 11px; flex-shrink: 0;"
                ]
                (contigs
                    |> List.indexedMap (\i contig ->
                        Svg.text_
                            [ SvgAttr.x "5"
                            , SvgAttr.y (String.fromFloat (rowCentre i + 4))
                            , SvgAttr.fill "#333"
                            ]
                            [ Svg.text contig.name ]
                    )
                )
            , -- Scrollable map area
              Svg.svg
                [ SvgAttr.width (String.fromFloat layout.mapWidth)
                , SvgAttr.height (String.fromInt svgHeight)
                , SvgAttr.viewBox
                    (String.fromFloat vbX
                        ++ " 0 "
                        ++ String.fromFloat layout.mapWidth
                        ++ " " ++ String.fromInt svgHeight
                    )
                , SvgAttr.style ("font-family: sans-serif; font-size: 11px; flex-grow: 1; cursor: " ++ mapCursor ++ ";")
                , SvgAttr.id svgId
                , HE.preventDefaultOn "wheel" (D.map (Tuple.mapFirst config.toMsg) (wheelDecoder zoom))
                , HE.on "mousedown" (D.map config.toMsg mouseDownDecoder)
                ]
                (contigs
                    |> List.indexedMap (\i contig ->
                        Svg.g []
                            (backbone (scale 0) (scale contig.maxPos) (rowCentre i)
                                :: List.map (geneArrow config.onGeneClick useArrows scale (rowCentre i) selectedGene) contig.genes
                            )
                    )
                )
            ]
        , viewOverviewBar state contigs
        ]


backbone : Float -> Float -> Float -> Svg.Svg msg
backbone x1 x2 cy =
    Svg.line
        [ SvgAttr.x1 (String.fromFloat x1)
        , SvgAttr.y1 (String.fromFloat cy)
        , SvgAttr.x2 (String.fromFloat x2)
        , SvgAttr.y2 (String.fromFloat cy)
        , SvgAttr.stroke "black"
        , SvgAttr.strokeWidth "3"
        ]
        []


{-| A miniature of all contigs, highlighting the part that is visible when
zoomed in.
-}
viewOverviewBar : State -> List ContigInfo -> Html.Html msg
viewOverviewBar state contigs =
    let
        globalMax = maxPos contigs
        barWidth = layout.mapWidth
        barHeight = 6 * toFloat (List.length contigs) + 4
        viewFraction = 1 / state.zoom
        viewStart = viewStartFromOffset state.zoom state.offset
        rectW = Basics.max 4 (viewFraction * barWidth)
    in
    if state.zoom <= 1 then
        Html.text ""
    else
        Html.div
            [ HtmlAttr.style "display" "flex" ]
            [ Html.div
                [ HtmlAttr.style "width" (String.fromFloat layout.labelWidth ++ "px")
                , HtmlAttr.style "flex-shrink" "0"
                ]
                []
            , Svg.svg
                [ SvgAttr.width (String.fromFloat barWidth)
                , SvgAttr.height (String.fromFloat barHeight)
                , SvgAttr.style "display: block; margin-top: 2px;"
                ]
                (List.indexedMap
                    (\i contig ->
                        let
                            cy = toFloat i * 6 + 5
                            cWidth = toFloat contig.maxPos / toFloat globalMax * barWidth
                        in
                        Svg.line
                            [ SvgAttr.x1 "0"
                            , SvgAttr.y1 (String.fromFloat cy)
                            , SvgAttr.x2 (String.fromFloat cWidth)
                            , SvgAttr.y2 (String.fromFloat cy)
                            , SvgAttr.stroke "#bbb"
                            , SvgAttr.strokeWidth "3"
                            ]
                            []
                    )
                    contigs
                    ++ [ Svg.rect
                            [ SvgAttr.x (String.fromFloat (viewStart * barWidth))
                            , SvgAttr.y "0"
                            , SvgAttr.width (String.fromFloat rectW)
                            , SvgAttr.height (String.fromFloat barHeight)
                            , SvgAttr.fill "rgba(66, 133, 244, 0.3)"
                            , SvgAttr.stroke "#4285f4"
                            , SvgAttr.strokeWidth "1"
                            , SvgAttr.rx "2"
                            ]
                            []
                       ]
                )
            ]


{-| The genes around `gene` (10 kbp on either side), drawn at a fixed scale.
-}
viewNeighborhood : (EMapperGene -> msg) -> EMapperGene -> List EMapperGene -> Html.Html msg
viewNeighborhood onGeneClick gene allGenes =
    let
        flank = 10000
        regionStart = Basics.max 0 (Basics.min gene.start gene.end - flank)
        regionEnd = Basics.max gene.start gene.end + flank
        -- Clamp to contig extent
        contigGenes = List.filter (\g -> g.contig == gene.contig) allGenes
        contigMax = contigGenes
            |> List.map (\g -> Basics.max g.start g.end)
            |> List.maximum
            |> Maybe.withDefault regionEnd
        clampedEnd = Basics.min regionEnd contigMax
        clampedStart = Basics.max 0 regionStart
        regionLen = toFloat (clampedEnd - clampedStart)
        -- Genes overlapping the visible region
        neighborGenes = contigGenes
            |> List.filter (\g ->
                let
                    gMin = Basics.min g.start g.end
                    gMax = Basics.max g.start g.end
                in
                gMax >= clampedStart && gMin <= clampedEnd
            )
        width = layout.mapWidth + layout.labelWidth
        height = layout.rowHeight + 10
        cy = toFloat layout.rowHeight / 2 + 5
        scale pos = (toFloat pos - toFloat clampedStart) / regionLen * width
    in
    Html.div [ HtmlAttr.style "margin" "0.5em 0" ]
        [ Html.h4 [ HtmlAttr.style "margin" "0.25em 0" ]
            [ Html.text "Local neighborhood" ]
        , Svg.svg
            [ SvgAttr.width (String.fromFloat width)
            , SvgAttr.height (String.fromInt height)
            , SvgAttr.viewBox ("0 0 " ++ String.fromFloat width ++ " " ++ String.fromInt height)
            , SvgAttr.style "font-family: sans-serif; font-size: 11px; display: block;"
            ]
            (backbone (scale clampedStart) (scale clampedEnd) cy
                :: List.map (geneArrow onGeneClick True scale cy (Just gene)) neighborGenes
            )
        ]


-- GENES


rRnaColor : String -> String
rRnaColor rtype =
    case rtype of
        "16S_rRNA" -> "#d50000"
        "23S_rRNA" -> "#2962ff"
        "5S_rRNA" -> "#00c853"
        _ -> "#d50000"


geneColor : EMapperGene -> String
geneColor gene =
    case gene.geneType of
        RRna rtype -> rRnaColor rtype
        ProteinCoding -> Cog.color (Cog.primaryCategory gene)


{-| A gene drawn as an arrow pointing along its strand, or as a plain box when
zoomed out too far for the arrow heads to be seen.
-}
geneArrow : (EMapperGene -> msg) -> Bool -> (Int -> Float) -> Float -> Maybe EMapperGene -> EMapperGene -> Svg.Svg msg
geneArrow onGeneClick useArrows scale cy selectedGene gene =
    let
        x1 = scale gene.start
        x2 = scale gene.end
        geneWidth = x2 - x1
        halfHeight = 8
        color = geneColor gene
        tooltipText =
            (if String.isEmpty gene.preferredName then gene.seqid else gene.preferredName)
                ++ " [" ++ (if String.isEmpty gene.cogCategory then "-" else gene.cogCategory) ++ "]"
        isSelected = case selectedGene of
            Just sel -> sel.seqid == gene.seqid
            Nothing -> False
        strokeColor = if isSelected then "#000" else if useArrows then "#333" else color
        strokeW = if isSelected then "2" else if useArrows then "0.5" else "0.3"
        commonAttrs =
            [ SvgAttr.fill color
            , SvgAttr.stroke strokeColor
            , SvgAttr.strokeWidth strokeW
            , SvgEvents.onClick (onGeneClick gene)
            , SvgAttr.style "cursor: pointer;"
            , HtmlAttr.attribute "role" "button"
            , HtmlAttr.attribute "aria-label" (geneAccessibleLabel gene)
            , HtmlAttr.attribute "aria-pressed" (if isSelected then "true" else "false")
            ]
        title = [ Svg.title [] [ Svg.text tooltipText ] ]
    in
    if not useArrows then
        Svg.rect
            ([ SvgAttr.x (String.fromFloat x1)
            , SvgAttr.y (String.fromFloat (cy - halfHeight))
            , SvgAttr.width (String.fromFloat (Basics.max 1 geneWidth))
            , SvgAttr.height (String.fromFloat (halfHeight * 2))
            ]
            ++ commonAttrs
            )
            title
    else
        let
            arrowHeadSize = Basics.min 6 (geneWidth * 0.3)
            points =
                if gene.strand == "+" then
                    [ point x1 (cy - halfHeight)
                    , point (x2 - arrowHeadSize) (cy - halfHeight)
                    , point x2 cy
                    , point (x2 - arrowHeadSize) (cy + halfHeight)
                    , point x1 (cy + halfHeight)
                    ]
                else
                    [ point (x1 + arrowHeadSize) (cy - halfHeight)
                    , point x2 (cy - halfHeight)
                    , point x2 (cy + halfHeight)
                    , point (x1 + arrowHeadSize) (cy + halfHeight)
                    , point x1 cy
                    ]
        in
        Svg.polygon
            (SvgAttr.points (String.join " " points) :: commonAttrs)
            title


point : Float -> Float -> String
point x y =
    String.fromFloat x ++ "," ++ String.fromFloat y


{-| Screen-reader description of a gene arrow, e.g.
"dnaA, COG L (Replication, recombination and repair), contig_1 1,200 to 2,550 bp, plus strand".
-}
geneAccessibleLabel : EMapperGene -> String
geneAccessibleLabel gene =
    let
        name = if String.isEmpty gene.preferredName then gene.seqid else gene.preferredName
        kind =
            case gene.geneType of
                RRna rtype ->
                    String.replace "_" " " rtype
                ProteinCoding ->
                    if String.isEmpty gene.cogCategory then
                        "no COG category"
                    else
                        "COG " ++ gene.cogCategory ++ " (" ++ Cog.description (Cog.primaryCategory gene) ++ ")"
        strand = if gene.strand == "+" then "plus strand" else "minus strand"
    in
    String.join ", "
        [ name
        , kind
        , gene.contig ++ " " ++ showWithCommas gene.start ++ " to " ++ showWithCommas gene.end ++ " bp"
        , strand
        ]


-- LEGEND


{-| Colour key for the genes shown: the COG categories and rRNA types used.
-}
viewLegend : List EMapperGene -> Html.Html msg
viewLegend genes =
    let
        rRnaTypes =
            List.filterMap
                (\g ->
                    case g.geneType of
                        RRna rtype -> Just rtype
                        ProteinCoding -> Nothing
                )
                genes
        cogCats =
            genes
                |> List.filter (\g -> g.geneType == ProteinCoding)
                |> List.map Cog.primaryCategory
        cogItems =
            Cog.allCategories
                |> List.filter (\c -> List.member c cogCats)
                |> List.map (\cat ->
                    legendItem (Cog.color cat)
                        ((if String.isEmpty cat then "-" else cat) ++ ": " ++ Cog.description cat)
                )
        rRnaItems =
            [ "16S_rRNA", "23S_rRNA", "5S_rRNA" ]
                |> List.filter (\t -> List.member t rRnaTypes)
                |> List.map (\rtype -> legendItem (rRnaColor rtype) (String.replace "_" " " rtype))
    in
    Html.div []
        (List.filterMap legendRow [ cogItems, rRnaItems ])


legendRow : List (Html.Html msg) -> Maybe (Html.Html msg)
legendRow items =
    if List.isEmpty items then
        Nothing
    else
        Just
            (Html.div [ HtmlAttr.style "margin-top" "0.5em"
                      , HtmlAttr.style "display" "flex"
                      , HtmlAttr.style "flex-wrap" "wrap"
                      , HtmlAttr.style "gap" "0.3em 1em"
                      , HtmlAttr.style "font-size" "0.85em"
                      ]
                items
            )
