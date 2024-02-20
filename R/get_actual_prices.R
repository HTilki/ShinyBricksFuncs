#' Effectue le scraping d'informations sur les prix de revente
#'
#' Cette fonction effectue un scraping d'informations sur les prix de revente
#' des ensembles contenus dans le dataframe fourni. Les informations incluent
#' le prix de détail, le prix en neuf, le prix en occasion et la fourchette de
#' prix. La fonction utilise les liens vers BrickEconomy pour récupérer ces
#' informations. Assurez-vous d'avoir préalablement enregistré la clé API,
#' le nom d'utilisateur et le mot de passe dans les options.
#'
#' @param df_sets Dataframe contenant des informations sur les ensembles.
#' @return Un dataframe élargi avec les informations de prix de revente.
#' @seealso \code{\link{extract_values}}, \code{\link{price_cleaner}}
#' @importFrom tibble rowid_to_column
#' @importFrom dplyr select rowwise mutate 
#' @importFrom tidyr unnest
#' @export
get_actual_prices <- function(df_sets){
  return(
    df_sets |>
      filter(released == TRUE) |>
      rowid_to_column() |>
      select(set_number, brickeconomy_link, rowid) |>
      rowwise() |>
      mutate(
        values = extract_values(brickeconomy_link, 5, rowid),
        last_scraped = format(Sys.time())
      ) |> 
      unnest(cols = c(values)) |> 
      select(
        set_number,
        brickeconomy_link,
        last_scraped, 
        retail_price,
        value_new,
        value_used,
        range
      ) |> 
      mutate(
        retail_price = price_cleaner(retail_price),
        value_new = price_cleaner(value_new),
        value_used = price_cleaner(value_used)
      )
  )
}
