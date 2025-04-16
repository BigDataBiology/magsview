module LoadData exposing (loadData)
import Csv.Decode as Decode exposing (Decoder)

import DataModel exposing (MAG)

decoder : Decoder MAG
decoder =
    Decode.into MAG
        |> Decode.pipeline (Decode.field "id" Decode.string)
        |> Decode.pipeline (Decode.field "samples_id" (Decode.string |> Decode.map (String.split ",")))
        |> Decode.pipeline (Decode.field "taxonomy" Decode.string)
        |> Decode.pipeline (Decode.field "completeness" Decode.float)
        |> Decode.pipeline (Decode.field "contamination" Decode.float)
        |> Decode.pipeline (Decode.field "#16s_rrna" Decode.int)
        |> Decode.pipeline (Decode.field "#5s_rrna" Decode.int)
        |> Decode.pipeline (Decode.field "#23s_rrna" Decode.int)
        |> Decode.pipeline (Decode.field "#trna" Decode.int)
        |> Decode.pipeline (Decode.field "nr_contigs" Decode.int)
        |> Decode.pipeline (Decode.field "nr_genes" Decode.int)
        |> Decode.pipeline (Decode.field "is_representative" (Decode.string |> Decode.map (\x -> x == "True")))
        |> Decode.pipeline (Decode.field "binning_tool" Decode.string)
        |> Decode.pipeline (Decode.field "assembly_method" Decode.string)
        |> Decode.pipeline (Decode.field "comment" Decode.string)
        |> Decode.pipeline (Decode.field "file_size" Decode.int)
        |> Decode.pipeline (Decode.field "file_sha256" Decode.string)


loadData : String -> Result Decode.Error (List MAG)
loadData csv =
    Decode.decodeCsv Decode.FieldNamesFromFirstRow decoder csv

