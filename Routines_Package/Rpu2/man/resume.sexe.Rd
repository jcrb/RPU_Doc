% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rapport.R
\name{resume.sexe}
\alias{resume.sexe}
\title{analyse un vecteur formé d'une suite de H, F, ou I}
\usage{
summary.sexe(vx)
}
\arguments{
\item{vx}{vecteur de Char (sexe)}
}
\value{
vecteur nommé: "N", "n.na", "n.rens", "p.rens", "n.hommes", "n.femmes", "p.hommes", "p.femmes",
         "sex.ratio", "tx.masculinité"
}
\description{
retourne: le nombre d'éléments du vcteur (NA inclus), le nombre de NA, nombre et pourcentage de valeurs renseignées,
nombre et pourcentage d'hommes et de femmes, sex ratio et taux de masculinité.
}

