#' Obtient les instructions d'assemblage pour un set donné.
#'
#' Cette fonction récupère les instructions d'assemblage pour un ensemble donné à partir de l'API Brickset.
#' 
#' @param ID L'identifiant de l'ensemble pour lequel vous souhaitez obtenir les instructions d'assemblage.
#' @return Une liste d'URLs vers les instructions d'assemblage au format HTML.
#' @importFrom brickset getInstructions
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate select pull
#' @export
#'
#' @examples
#' get_set_instructions("ID_du_set")
#'
#' @references
#' Pour plus d'informations sur l'API Brickset : \url{https://brickset.com/tools/webservices/v3}.
#'
get_set_instructions <- function(ID){
  create_html_URL <- function(URL, description) {
    return(glue::glue("<a href='{URL}'>{description},</a>"))
  }
  instructions <- tryCatch(brickset::getInstructions(setID = ID) |>
                             tibble::as_tibble(),
                           error = function(e) {
                             return(
                               "Veuillez renseigner une clé pour utiliser l'API brickset afin d'avoir accès aux notices et instructions d'assemblage."
                             )
                           }
  )
  if ("tbl_df" %in% class(instructions)){
    if (sum(dim(instructions)) == 0){
      return("Aucune instructions d'assemblage a été trouvé pour ce set.")
    } else {
      return(
        instructions |>
          dplyr::mutate(htmlURL = create_html_URL(URL, description)) |>
          dplyr::select(htmlURL) |>
          dplyr::pull()
      )
    }
  } else {
    return(instructions)
  }
}
