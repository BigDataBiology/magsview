module Downloads exposing (mkFASTALink, mkENOGLink)

{-| This module provides functions to create download links for FASTA and ENOG files based on a given MAG ID. -}

mkFASTALink : String -> String
mkFASTALink mid =
    "https://sh-dog-mags-data.big-data-biology.org/ShanghaiDogsMAGs/" ++ mid ++ ".fna.gz"

mkENOGLink : String -> String
mkENOGLink mid =
    "https://sh-dog-mags-data.big-data-biology.org/ShanghaiDogsMAGAnnotations/EMapper/" ++ mid ++ ".emapper.annotations.xz"

