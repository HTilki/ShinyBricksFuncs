#' Nettoie une colonne de chaînes de caractères représentant des prix
#'
#' Cette fonction enlève le symbole €, remplace les virgules par des points,
#' supprime les espaces inutiles, remplace les valeurs "Unknown" par NA, et
#' transforme la colonne en type double. 
#' 
#' @param column Colonne de chaînes de caractères représentant des prix.
#' @return Colonne nettoyée avec les prix transformés en double.
#' @importFrom stringr str_remove str_replace str_trim
#' @export
price_cleaner <- function(column){
  suppressWarnings({
    clean_column = str_remove(column, "€") |> 
      str_replace(",", ".") |> 
      str_trim() |> 
      str_replace("Unknown",  NA_character_) |> 
      as.double()
  })
  return(clean_column)
}
