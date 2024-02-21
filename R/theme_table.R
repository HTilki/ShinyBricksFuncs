#' Tableau interactif avec filtre par thème
#'
#' Cette fonction crée un tableau interactif avec un filtre par thème à partir du dataframe
#' fourni. Elle utilise le package gt pour générer une table HTML interactive avec des
#' fonctionnalités telles que le filtrage, le tri et la mise en forme des données. La table
#' affiche les informations spécifiques sur les sets LEGO en fonction du thème choisi.
#'
#' @param df_prices Le dataframe contenant les informations sur les prix des sets LEGO.
#' @param theme Le thème à filtrer dans le tableau.
#' @importFrom dplyr filter select
#' @import gt
#' @export
theme_table <- function(df_prices, theme){
  return(
    df_prices |>
      filter(theme %in% c(theme)) |> 
      select(
        name,
        set_number,
        year,
        theme,
        imageURL,
        retail_price,
        value_new,
        value_used,
        brickeconomy_link,
        pieces,
        minifigs,
        agerange_min, 
        rating,
        diff_new_retail,
        diff_used_retail
      ) |> 
      gt() |>
      fmt_url(columns = brickeconomy_link) |> 
      fmt_integer(pieces, pattern = "{x} p.") |>
      fmt_integer(agerange_min, pattern = "{x}+") |>
      fmt_currency(
        columns = c(retail_price, value_new, value_used),
        currency = "euro", placement = "right", incl_space = TRUE) |>
      fmt_number(columns = c(diff_new_retail, diff_used_retail)) |> 
      data_color(retail_price) |> 
      cols_merge(
        columns = c(value_new, diff_new_retail),
        pattern = "<<{1} << <br> (x {2})>>>>",
      ) |> 
      cols_merge(
        columns = c(value_used, diff_used_retail),
        pattern = "<<{1} << <br> (x {2})>>>>",
      ) |> 
      gt_img_rows(columns = imageURL, img_source = "web", height = 40) |>
      gt_fa_rating(rating) |> 
      gt_badge(theme) |> 
      sub_missing(
        columns = -c(diff_new_retail, diff_used_retail),
        rows = everything(),
        missing_text = "---"
      ) |> 
      tab_style(
        locations = cells_body(columns = name),
        style = cell_text(weight = "bold")
      ) |>
      tab_style(
        locations = cells_body(columns = year),
        style = cell_text(style = "italic")
      ) |>
      tab_style(
        locations = cells_body(columns = year),
        style = cell_text(v_align = "middle")
      ) |>
      tab_style(
        locations = cells_body(columns = set_number),
        style = cell_text(decorate = "underline")
      ) |>
      tab_header(
        title = md("📋 **Nombre de sets ![](https://upload.wikimedia.org/wikipedia/commons/2/24/LEGO_logo.svg){width=15 height=15} STAR WARS.**"),
      ) |> 
      cols_width(
        name~px(180),
        set_number~px(160),
        imageURL~px(180),
        brickeconomy_link~px(350),
        pieces~px(180),
        minifigs~px(200),
        agerange_min~px(180),
        rating~px(180)
      ) |> 
      cols_align(align = "center") |> 
      opt_interactive(
        use_filters = TRUE,
        use_compact_mode = TRUE,
        use_text_wrapping = FALSE
      ) |>
      cols_label(
        name = html(fontawesome::fa("rectangle-list"), "Nom du Set"),
        set_number = html(fontawesome::fa("tag"), "Numéro du Set"),
        year = html(fontawesome::fa("calendar-days"), "Année"),
        theme = html(fontawesome::fa("empire"), "Thème"),
        retail_price = html(fontawesome::fa("euro-sign"), "Prix"),
        imageURL = html(fontawesome::fa("image"), "Image"),
        pieces = html(fontawesome::fa("puzzle-piece"), "Nombre de pièces"),
        brickeconomy_link = html(fontawesome::fa("arrow-right-to-bracket"), "Lien"),
        minifigs = html(fontawesome::fa("children"), "Nombre de figurines"),
        agerange_min = html(fontawesome::fa("clipboard-user"), "Tranche d'âge"),
        rating = html(fontawesome::fa("ranking-star"), "Notation"),
      ) |>
      tab_source_note(
        source_note = md("- Données issues de l'`API` [brickset](https://brickset.com/).")
      )
  )
}