% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rapport.R
\name{completude.time}
\alias{completude.time}
\title{Pour un etablissement donne, calcule le aux de completude par mois, semaine, jours}
\usage{
completude.time(dx, finess,  time = "month")
}
\arguments{
\item{dx}{un dataframe de type RPU}

\item{finess}{établissement concerné ('Wis', 'Hag', 'Sav', ...)}

\item{time}{factor de découpage}
}
\details{
Au départ on dispose d'un dataframe de type RPU. Ce dataframe est splité en sous groupes sur une base temporelle (mois,
jour, semaine...). Sur chacun des sous-groupes on applique la fonction "completude". Retourne un dataframe
où chaque ligne correspond à une période et chaque colonne à un élément du RPU.
Utilise "ddply" qui fonctionne comme tapply mais s'applique à un DF au lieu d'un vecteur et retourne un DF.
TODO: exension à plusieurs établissements simultannéent; limitation à certaines colonnes.
}
\examples{
load("~/Documents/Resural/Stat Resural/RPU_2014/rpu2015d0112_provisoire.Rda")
       # old
       sav <- d15[d15$FINESS == "Sav",] # Saverne 2015
       t3 <- ddply(sav, .(month(as.Date(sav$ENTREE))), completude) # completude par mois
       
       # new
       library(xts)
       t3 <- completude.time(d15, "Sav", "day")
       a <- seq(as.Date("2015-01-01"), length.out = nrow(t3), by = 1)
       x <- xts(t3, order.by = a)
       plot(x[, "DP"], main = "CH Saverne - DIAGNOSTIC PRINCIPAL", ylab = "\\\% de complétude")
       
       # TODO: tableau de complétude par mois et par Finess:
       t3 <- ddply(dx, .(dx$FINESS, month(as.Date(dx$ENTREE))), completude)
       # Application: rpu2014/Analyse/Completude/Analyse_completude
}

