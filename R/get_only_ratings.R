#' Récupère uniquement les notes pour un ensemble
#'
#' Cette fonction utilise l'API de Brickset pour récupérer les notes spécifiques
#' d'un ensemble identifié par son setID. Les notes incluent la moyenne générale,
#' la moyenne des pièces, l'expérience de construction, la jouabilité et la valeur
#' pour l'argent. Assurez-vous d'avoir préalablement enregistré la clé API, le
#' nom d'utilisateur et le mot de passe dans les options.
#'
#' @param setID Identifiant de l'ensemble pour lequel récupérer les notes.
#' @param reviewCount Nombre total d'avis disponibles pour cet ensemble.
#' @return Un tableau contenant les moyennes des notes spécifiques.
#' @importFrom dplyr select summarise_all
#' @importFrom brickset getReviews
#' @export
get_only_ratings <- function(setID, reviewCount) {
  if (reviewCount > 0) {
    return(
      getReviews(setID) |>
        select(overall, parts, buildingExperience, playability, valueForMoney) |>
        summarise_all(mean)
    )
  } else {
    return(NULL)
  }
}