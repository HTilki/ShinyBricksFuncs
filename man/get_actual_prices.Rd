% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_actual_prices.R
\name{get_actual_prices}
\alias{get_actual_prices}
\title{Effectue le scraping d'informations sur les prix de revente}
\usage{
get_actual_prices(df_sets)
}
\arguments{
\item{df_sets}{Dataframe contenant des informations sur les ensembles.}
}
\value{
Un dataframe élargi avec les informations de prix de revente.
}
\description{
Cette fonction effectue un scraping d'informations sur les prix de revente
des ensembles contenus dans le dataframe fourni. Les informations incluent
le prix de détail, le prix en neuf, le prix en occasion et la fourchette de
prix. La fonction utilise les liens vers BrickEconomy pour récupérer ces
informations. Assurez-vous d'avoir préalablement enregistré la clé API,
le nom d'utilisateur et le mot de passe dans les options.
}
\seealso{
\code{\link{extract_values}}, \code{\link{price_cleaner}}
}
