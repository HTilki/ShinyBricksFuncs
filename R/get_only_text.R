#' Récupère uniquement les avis textuels pour un ensemble
#'
#' Cette fonction utilise l'API de Brickset pour récupérer les avis textuels
#' d'un ensemble identifié par son setID. Les informations incluses sont
#' l'auteur, la date de publication, le titre, le texte de l'avis et le format
#' HTML de l'avis. Assurez-vous d'avoir préalablement enregistré la clé API, le
#' nom d'utilisateur et le mot de passe dans les options.
#'
#' @param setID Identifiant de l'ensemble pour lequel récupérer les avis textuels.
#' @param reviewCount Nombre total d'avis disponibles pour cet ensemble.
#' @return Un tableau contenant les avis textuels.
#' @importFrom dplyr select
#' @importFrom brickset getReviews
#' @export
get_only_text <- function(setID, reviewCount){
  if (reviewCount > 0) {
    return(
      getReviews(setID) |>
        select(author, datePosted, title, review, HTML) 
    )
  } else {
    return(NULL)
  }
}