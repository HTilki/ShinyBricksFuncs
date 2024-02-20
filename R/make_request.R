#' Effectue une requête sur le site BrickEconomy
#'
#' Cette fonction effectue une requête sur le site BrickEconomy en utilisant le lien
#' fourni. Elle prend en charge des options telles que le nombre maximum de tentatives
#' de requête en cas d'échec (`max_retry`) et l'identifiant de ligne (`rowid`) pour
#' un suivi clair des liens scrapés. Assurez-vous d'avoir préalablement enregistré
#' la clé API, le nom d'utilisateur et le mot de passe dans les options.
#'
#' @param link Lien vers le site BrickEconomy à requêter.
#' @param max_retry Nombre maximum de tentatives de requête en cas d'échec.
#' @param rowid Identifiant de ligne pour un suivi clair des liens scrapés.
#' @return Objet de réponse de la requête HTTP.
#' @importFrom httr2 request req_headers req_throttle req_retry req_perform
#' @export
make_request <- function(link, max_retry, rowid) {
  response <- request(link) |>
    req_headers(
      "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
      "Accept-Language" = "fr,fr-FR;q=0.8,en-US;q=0.5,en;q=0.3",
      "User-Agent" = "a/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/119.0"
    ) |>
    req_throttle(rate = 60 / 60) |>
    req_retry(max_tries = max_retry) |>
    req_perform()
  print(paste0("Lien n°", rowid, " scrapé."))
  return(response)
}