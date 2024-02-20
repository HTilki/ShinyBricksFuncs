#' Joint le dataframe des sets avec le dataframe des prix scrapé des sets
#'
#' Cette fonction effectue une jointure interne (inner join) entre le dataframe des sets
#' (`df_sets`) et le dataframe des prix des sets (`df_scrap`). La jointure est réalisée
#' en utilisant la colonne "set_number" du dataframe des sets et la colonne "brickeconomy_link"
#' du dataframe des prix des sets. Les enregistrements sans prix de détail sont supprimés.
#' La fonction ajoute également des colonnes supplémentaires, telles que "diff_new_retail"
#' (rapport entre la valeur en neuf et le prix de détail), "diff_used_retail" (rapport entre
#' la valeur en occasion et le prix de détail), et "ppp" (prix par pièce).
#'
#' @param df_sets Dataframe contenant des informations sur les ensembles.
#' @param df_scrap Dataframe contenant des informations sur les prix des ensembles.
#' @return Un dataframe résultant de la jointure avec les colonnes supplémentaires.
#' @importFrom dplyr inner_join join_by mutate
#' @importFrom tidyr drop_na
#' @export
lego_assembler <- function(df_sets, df_scrap){
  df_prices <- inner_join(df_sets, 
                          df_scrap, 
                          by = join_by(set_number, brickeconomy_link)) |>
    drop_na(retail_price) |> 
    mutate(
      diff_new_retail = value_new / retail_price,
      diff_used_retail = value_used / retail_price,
      ppp = pieces / retail_price
    )
}
