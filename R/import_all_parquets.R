#' Importe tous les fichiers .parquet en un seul dataframe
#'
#' Cette fonction combine tous les fichiers .parquet présents dans le dossier spécifié
#' (par défaut, le dossier "data") en un seul dataframe. Les fichiers
#' .parquet sont lus avec la fonction read_parquet du package arrow. Le dataframe
#' résultant est filtré pour inclure uniquement les catégories "Normal", "Extended" et
#' "Collection". Une colonne supplémentaire set_number est créée en combinant les
#' colonnes number et numberVariant, et une colonne brickeconomy_link est générée en
#' utilisant le package glue pour construire les liens correspondants sur le site BrickEconomy.
#'
#' @param chemin_dossier Chemin du dossier contenant les fichiers .parquet. Par défaut : "data".
#' @return Un dataframe contenant toutes les données combinées.
#' @importFrom dplyr filter mutate
#' @importFrom purrr map_dfr
#' @importFrom arrow read_parquet
#' @importFrom glue glue
#' @export
import_all_parquets <- function(chemin_dossier = "data"){
  
  parquet_files <- list.files(chemin_dossier, pattern = "\\.parquet$", full.names = TRUE)
  df_sets <- map_dfr(parquet_files, ~ read_parquet(.x))
  df_sets <- df_sets |>
    filter(category %in% c("Normal", "Extended", "Collection")) |>
    mutate(set_number = glue("{number}-{numberVariant}")) |>
    mutate(
      brickeconomy_link = glue("https://www.brickeconomy.com/set/{set_number}/")
    )
  return(df_sets)
}
