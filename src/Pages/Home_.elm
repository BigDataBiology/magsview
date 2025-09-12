module Pages.Home_ exposing (page, Model, Msg)

import View exposing (View)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Effect exposing (Effect)

import SiteMarkdown exposing (mdToHtml)
import Layouts

type alias Model = {}
type Msg = NoOp

page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = \_ -> ({}, Effect.none)
        , update = \_ _ -> ({}, Effect.none)
        , subscriptions = \_ -> Sub.none
        , view = \_ ->
            { title = "About"
            , body = [mdToHtml content]
            }
    } |> Page.withLayout (\_ -> Layouts.Main {})

content : String
content = """
## Shanghai dog gut microbiome

> _Long-read metagenomics of Shanghai pet dogs captures global pet dog gut
> microbial diversity and recovers hundreds of bacterial species as
> near-finished genomes_ by Anna Cuscó, Yiqian Duan, Fernando Gil, Alexei
> Chklovski, Nithya Kruthi, Shaojun Pan, Sofia Forslund, Susanne Lau, Ulrike
> Löber, Xing-Ming Zhao, and Luis Pedro Coelho (bioRxiv PREPRINT 2025)

Dogs are part of the family, and learning about their gut microbes can tell us
a lot about both their health and the household they live in. Most studies of
the dog gut microbiome so far have used short-read sequencing, which breaks
up genomes and misses important pieces like mobile elements, resistance genes,
and ribosomal genes.

![Shanghai dog](static/Fig1a.svg)

For this project, we used deep long-read sequencing (ONT), polished with
Illumina short reads on stool samples from 51 pet dogs in Shanghai. This gave
us **2,676 high-quality genomes** from **320 different bacterial species**. In
total, 72% of them are close to finished quality, _often better than the
available reference genomes_.

![Shanghai dog](static/Fig1e.svg)

When we compared our data to other dog microbiome datasets from around the
world, we found strong overlap (with over 90% of reads mapping to our genomes),
showing that what we see in _Shanghai dogs reflects pet dogs globally_.

In addition to the genomes, we recovered circular extrachromosomal elements and
built gene catalogs (including a catalog of putative small proteins).
"""

