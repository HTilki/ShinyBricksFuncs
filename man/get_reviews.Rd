% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_reviews.R
\name{get_reviews}
\alias{get_reviews}
\title{Récupère tous les avis pour un ensemble}
\usage{
get_reviews(setID, reviewCount)
}
\arguments{
\item{setID}{Identifiant de l'ensemble pour lequel récupérer tous les avis.}

\item{reviewCount}{Nombre total d'avis disponibles pour cet ensemble.}
}
\value{
Un tableau contenant tous les avis pour l'ensemble spécifié.
}
\description{
Cette fonction utilise l'API de Brickset pour récupérer tous les avis
d'un ensemble identifié par son setID. Assurez-vous d'avoir préalablement
enregistré la clé API, le nom d'utilisateur et le mot de passe dans les options.
}
