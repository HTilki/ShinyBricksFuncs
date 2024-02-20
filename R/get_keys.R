#' Récupère les noms des données scrapées
#'
#' Cette fonction extrait les noms des données scrapées à partir de la réponse
#' de la requête HTTP.
#' 
#' @param response Objet de réponse de la requête HTTP.
#' @return Vecteur de chaînes de caractères contenant les noms des données.
#' @importFrom httr2 resp_body_html
#' @importFrom rvest html_elements html_element html_text
#' @export
get_keys <- function(response) {
  tryCatch(
    {
      keys <- response |>
        resp_body_html() |>
        html_elements("body") |>
        html_element("#ContentPlaceHolder1_UpdatePanelSetPricing") |>
        html_elements("div.row.rowlist") |>
        html_elements("div.col-xs-5.text-muted") |>
        html_text()
      return(keys)
    },
    error = function(e) {
      message("Attention, la page du set LEGO sélectionné n'existe pas.")
      return(NA)
    }
  )
}
