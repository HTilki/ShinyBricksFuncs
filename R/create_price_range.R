#' Crée une variable de plage de prix à partir du prix de vente au détail.
#'
#' Cette fonction prend un dataframe contenant les prix de vente au détail et crée une nouvelle variable indiquant la plage de prix à laquelle chaque prix appartient.
#'
#' @param df_prices Le dataframe contenant les prix de vente au détail.
#' @return Le dataframe avec une nouvelle variable 'price_range' indiquant la plage de prix pour chaque prix de vente au détail.
#' @importFrom dplyr mutate case_when between
#' @export
#'
#' @examples
#' df <- tibble::tibble(retail_price = c(3.99, 15.50, 30.25, 99.99))
#' create_price_range(df)
#'
create_price_range <- function(df_prices){
  return(
    df_prices |>
      dplyr::mutate(
        price_range = dplyr::case_when(
          dplyr::between(retail_price, 0, 4.99) ~ "0.00€-4.99€",
          dplyr::between(retail_price, 5, 9.99) ~ "5.00€-9.99€",
          dplyr::between(retail_price, 10, 19.99) ~ "10.00€-19.99€",
          dplyr::between(retail_price, 20, 29.99) ~ "20.00€-29.99€",
          dplyr::between(retail_price, 30, 39.99) ~ "30.00€-39.99€",
          dplyr::between(retail_price, 40, 59.99) ~ "40.00€-59.99€",
          dplyr::between(retail_price, 60, 99.99) ~ "60.00€-99.99€",
          dplyr::between(retail_price, 100, 199.99) ~ "100.00€-199.99€",
          dplyr::between(retail_price, 200, 999.99) ~ "100.00€-999.99€"
        )
      )
  )
}
