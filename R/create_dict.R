#' Crée un dictionnaire à la manière d'un dict Python
#'
#' Cette fonction crée un dictionnaire en mettant en relation les keys et values.
#'
#' @param keys Vecteur de chaînes de caractères représentant les clés du dictionnaire.
#' @param values Vecteur de valeurs associées aux clés du dictionnaire.
#' @return Un tibble représentant le dictionnaire avec les clés et les valeurs.
#' @importFrom janitor clean_names
#' @importFrom dplyr rename mutate select
#' @importFrom tibble as_tibble
#' @export
create_dict <- function(keys, values) {
  tryCatch(
    {
      dict <- data.frame(matrix(values, ncol = length(keys), byrow = TRUE))
      colnames(dict) <- keys
      dict <- dict |>
        clean_names() |>
        as_tibble()
      
      if("value" %in% colnames(dict)){
        dict <- dict |> rename(value_new = value)
      } else {
        dict <- dict |> mutate(value_new = NA)
      }
      if ("value_2" %in% colnames(dict)) {
        dict <- dict |> rename(value_used = value_2)
      } else {
        dict <- dict |> mutate(value_used = NA)
      }
      if ("retail_price" %in% colnames(dict)) {
        dict <- dict |> rename(retail_price = retail_price)
      } else {
        dict <- dict |> mutate(retail_price = NA)
      }
      if (!"range" %in% colnames(dict)) {
        dict <- dict |> mutate(range = NA)
      }
      
      return(dict |> select(retail_price, value_new, value_used, range))
    },
    error = function(e) {
      message("Erreur lors de l'exécution de la création du dictionnaire : ", conditionMessage(e))
      return(NA)
    }
  )
}
