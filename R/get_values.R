#' Récupère les valeurs scrapées
#'
#' Cette fonction extrait les valeurs scrapées à partir de la réponse de la requête HTTP.
#'
#' @param response Objet de réponse de la requête HTTP.
#' @return Vecteur de chaînes de caractères contenant les valeurs scrapées.
#' @importFrom httr2 resp_body_html
#' @importFrom rvest html_elements html_element html_text
#' @export
get_values <- function(response) {
  tryCatch(
    {
      values <- response |>
        resp_body_html() |>
        html_elements("body") |>
        html_element("#ContentPlaceHolder1_UpdatePanelSetPricing") |>
        html_elements("div.row.rowlist") |>
        html_elements("div.col-xs-7") |>
        html_text()
      return(values)
    },
    error = function(e) {
      message("Conséquence : Erreur lors de l'exécution de la récupération des valeurs dans la page.")
      return(NA)
    }
  )
}
