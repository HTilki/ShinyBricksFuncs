#' Récupère tous les avis pour tous les ensembles présents dans le dataframe
#'
#' Cette fonction prend un dataframe contenant des informations sur plusieurs
#' ensembles et permet de récupérer tous les avis, soit les notes, les avis textuels,
#' ou tous les types d'avis, pour chaque ensemble. Le type d'avis est spécifié
#' par le paramètre 'type' (choisir parmi 'ratings', 'text', 'all'). Assurez-vous
#' d'avoir préalablement enregistré la clé API, le nom d'utilisateur et le mot
#' de passe dans les options.
#'
#' @param df_sets Dataframe contenant des informations sur les ensembles.
#' @param type Type d'avis à récupérer ('ratings', 'text', 'all').
#' @return Un dataframe élargi avec les avis spécifiés pour chaque ensemble.
#' @seealso \code{\link{get_only_ratings}}, \code{\link{get_only_text}}, \code{\link{get_reviews}}
#' @importFrom dplyr rowwise mutate 
#' @importFrom tidyr unnest_wider 
#' @export
get_all_reviews <- function(df_sets, type){
  if (type == "ratings"){
    return (
      df_sets |>
        rowwise() |> 
        mutate(ratings = list(get_only_ratings(setID, reviewCount))) |> 
        unnest_wider(ratings)
    )
  } else if (type == "text"){
    return (
      df_sets |>
        rowwise() |>
        filter(reviewCount > 0) |>
        select(setID) |> 
        mutate(text_reviews = list(get_only_text(setID, reviewCount))) |> 
        unnest_wider(text_reviews) |> 
        unnest_longer(c(author, datePosted, title, review, HTML))
    )
  } else if (type == "all"){
    return (
      df_sets |>
        rowwise() |> 
        mutate(reviews = list(get_reviews(setID, reviewCount))) |> 
        unnest_wider(reviews) |> 
        unnest_longer(c(author, 
                        datePosted, 
                        title, 
                        review, 
                        HTML, 
                        overall, 
                        parts, 
                        buildingExperience,
                        playability,
                        valueForMoney))
    )
  } else {print("Choississez un type parmis les suivants : 'ratings', 'text', 'all'.")}
}
