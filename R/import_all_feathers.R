#' Importe tous les fichiers Feather disponibles dans le dossier par défaut
#'
#' Cette fonction importe tous les fichiers Feather (.feather) disponibles dans le
#' dossier spécifié par défaut (par défaut, le dossier "data") et les regroupe tous
#' dans un seul dataframe. Assurez-vous que les fichiers Feather sont présents dans
#' le dossier spécifié.
#'
#' @param chemin_data Chemin du dossier contenant les fichiers Feather. Par défaut : "data".
#' @return Un dataframe regroupant toutes les données des fichiers Feather.
#' @importFrom purrr map_dfr
#' @export
import_all_feathers <- function(chemin_data = "data"){
  feather_files <- list.files(chemin_data, pattern = "\\.feather$", full.names = TRUE)
  df <- map_dfr(feather_files, ~ feather::read_feather(.x))
  return(df)
}
