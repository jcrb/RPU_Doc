% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/Rapport.R
\name{completude}
\alias{completude}
\title{taux de completude global.}
\usage{
completude(dx, calcul = "percent", tri = FALSE)
}
\arguments{
\item{dx}{Un dataframe}

\item{calcul}{2 options "percent" (défaut) ou "somme". Somme = nb de réponses
non nulles. Percent = \% de réponses non nulles.}

\item{tri}{si tri = TRUE (defaut) les colonnes sont triées par ordre croissant.}
}
\value{
vecteur des taux de complétude
}
\description{
Pour chacune des rubriques RPU calcule le taux de réponse (complétude)
}
\details{
todo
}
\author{
JcB 2013-02-01
}
\seealso{
Other RPU: \code{\link{radar.completude}}
}
\keyword{complétude}
