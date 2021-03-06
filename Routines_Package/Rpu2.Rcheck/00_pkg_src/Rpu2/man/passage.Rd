% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rapport.R
\name{passage}
\alias{passage}
\title{Horaires de passages}
\usage{
passage(he, horaire = "nuit")
}
\arguments{
\item{he}{vecteur time de type hms}

\item{horaire}{= 'nuit', 'nuit profonde', 'jour'}
}
\value{
un vecteur avec 2 éléments: le nombre de passages et le pourcentage en
       fonction de la période (jour, nuit)
}
\note{
necessite lubridate. Prend en compte toutes les heures et pas seulement celles
     comprises entre 0 et 72h (voir passage2)
}
\examples{
e <- datetime(dx$ENTREE); he <- horaire(e); nuit <- passage(he, "nuit")
}
\seealso{
horaire
}

