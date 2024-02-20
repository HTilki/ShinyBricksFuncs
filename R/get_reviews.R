#' Récupère tous les avis pour un ensemble
#'
#' Cette fonction utilise l'API de Brickset pour récupérer tous les avis
#' d'un ensemble identifié par son setID. Assurez-vous d'avoir préalablement
#' enregistré la clé API, le nom d'utilisateur et le mot de passe dans les options.
#'
#' @param setID Identifiant de l'ensemble pour lequel récupérer tous les avis.
#' @param reviewCount Nombre total d'avis disponibles pour cet ensemble.
#' @return Un tableau contenant tous les avis pour l'ensemble spécifié.
#' @importFrom brickset getReviews
#' @export
get_reviews <- function(setID, reviewCount){
  if (reviewCount > 0) {
    return(
      getReviews(setID)
    )
  } else {
    return(NULL)
  }
}