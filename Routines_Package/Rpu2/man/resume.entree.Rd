% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rapport.R
\name{resume.entree}
\alias{resume.entree}
\title{analyse du vecteur ENTREE ou SORTIE}
\usage{
summary.entree(vx)
}
\arguments{
\item{vx}{vecteur de Date ou de DateTime}
}
\value{
vecteur nommé: "n", "n.na", "n.rens", "p.rens", "min", "max", "range"
}
\description{
analyse du vecteur ENTREE ou SORTIE
}
\note{
min et max ne s'affichent pas sous forme de date. Que donne hms
}
\examples{
summary.entree(as.Date(pop75$ENTREE))
}

