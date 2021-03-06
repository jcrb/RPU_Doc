% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rapport.R
\name{week.rpu}
\alias{week.rpu}
\title{Calcule le nombre de RPU par mois}
\usage{
week.rpu(dx)
}
\arguments{
\item{dx}{un dataframe de type RPU. Doit comporter au moins une colonne ENTREE}
}
\value{
un vecteur du nombre de RPU par mois
}
\description{
Calcule le nombre de RPU par mois de tous les ES présents dans le dataframe
}
\details{
Nécessite Lubridate. dx peut regroupper tous les ES ou ne converner qu'un ES Particulier.
}
\examples{
s <- week.rpu(dx)
tot <- sum(s) # nombre total de RPU
p = s/tot # \% de RPU par semaine
summary(p)

}

