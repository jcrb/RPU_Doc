% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rapport.R
\name{pdsa}
\alias{pdsa}
\title{Determine si on est en horaire de PDS.}
\usage{
pdsa(dx)
}
\arguments{
\item{dx}{vecteur date/heure au format YYYY-MM-DD HH:MM:SS}
}
\value{
un vecteur de factor NPDS, PDSS, PDSW
}
\description{
Détermine si on est en horaire de PDS de WE (PDSWE) ou de semaine (PDSS) 
ou hors horaire de PDS (NPDS)
à partir d'une date.
}
\details{
REM sur xps les jours commencent par une minuscule alors que sur le Mac c'est une majuscule ?
}
\examples{
x <- "2009-09-02 12:23:33"; weekdays(as.Date(x)); pds(x) # NPDS
}

