#' Fait la requête et extrait les informations pour obtenir un dictionnaire
#'
#' Cette fonction fait la requête sur le site BrickEconomy en utilisant le lien
#' fourni, puis extrait les informations nécessaires pour obtenir un dictionnaire
#' avec les noms de variable et les valeurs associées. Elle dépend des autres fonctions
#' telles que `make_request`, `get_keys`, `get_values`, et `create_dict`.
#'
#' @param link Lien vers le site BrickEconomy à requêter.
#' @param max_retry Nombre maximum de tentatives de requête en cas d'échec.
#' @param rowid Identifiant de ligne pour un suivi clair des liens scrapés.
#' @return Un tibble représentant le dictionnaire avec les noms de variable et les valeurs.
#' @import httr2
#' @import rvest
#' @import janitor
#' @import dplyr
#' @export
extract_values <- function(link, max_retry, rowid) {
  response <- make_request(link, max_retry, rowid)
  keys <- get_keys(response)
  values <- get_values(response)
  dict <- create_dict(keys, values)
  return(dict)
}
#' @seealso \code{\link{make_request}}, \code{\link{get_keys}}, \code{\link{get_values}}, \code{\link{create_dict}}