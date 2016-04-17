# Fonctions obsolètes et remplacées

is.present.at <- function(dp, heure = "15:00:00"){
    # présent à 15 heures
    # limite <- hms(heure) # pour incrémenter d'une heure: hms("15:00:00") + as.period(dhours(1))
    # np <- dp$he < limite & dp$hs > limite
    
    # modifiée le 17/4/2016. Meilleure prise en compte des patients présents depuis plus de 24 heures     #et des tranches horaires proches de 0h. Voir durée_passage.Rmd paragraphe horaire de présence        # dans HET
    # présent à 15 heures
    d <- paste(as.Date(dp$ENTREE), heure)
    limite1 <- ymd_hms(d) # pour incrémenter d'une heure: hms("15:00:00") + as.period(dhours(1))
    d <- paste(as.Date(dp$SORTIE), heure)
    limite2 <- ymd_hms(d)
    np <- ymd_hms(dp$ENTREE) < limite1 & ymd_hms(dp$SORTIE) > limite1 | ymd_hms(dp$ENTREE) < limite2 & ymd_hms(dp$SORTIE) > limite2
    np
}

#------------------------------------------------------------------
#
#   is.present.at
#
#------------------------------------------------------------------
#' @title Un patient est-il présent à une heure donnée ?
#' @description Crée le vecteur des personnes présentes à une heure donnée
#' @usage is.present.at((dp, heure = "15:00:00"))
#' @param dp dataframe de type duree_passage
#' @param heure heure au format HH:MM:SS. C'es l'heure à laquelle on veut mesurer les passages
#' @return np vecteur de boolean: TRUE si présent à l'heure analysee et FALSE sinon
#' @examples dp <- df.duree.pas(dx)
#'           dp$present.a.15h <- is.present.at(dp)
#'           # nombre moyen de patients présents à 15h tous les jours
#'           n.p15 <- tapply(dp$present.a.15h, yday(as.Date(dp$ENTREE)), sum)
#'           summary(n.p15)
#'           sd(n.p15)
#'           # transformation en xts
#'           xts.p15 <- xts(n.p15, order.by = unique(as.Date(dp$ENTREE)))
#'           plot(xts.p15, ylab = "Nombre de patients à 15h", main = "Nombre de patients présents à 15 heures")
#'           lines(rollmean(x = xts.p15, k = 7), col = "red", lwd = 2)
#'           
#'           # à 2h du matin
#'           dp$present.a.2h <- is.present.at(dp, "02:00:00")
#'           n.p2 <- tapply(dp$present.a.2h, yday(as.Date(dp$ENTREE)), sum)
#'           summary(n.p2)
#'           xts.p2 <- xts(n.p2, order.by = unique(as.Date(dp$ENTREE)))
#'           plot(xts.p2, ylab = "Nombre de patients présents", main = "Nombre de patients présents à 2 heures du matin")
#'           lines(rollmean(x = xts.p2, k = 7), col = "red", lwd = 2)
#'           # pour les données de 2015, noter le pic à 2 heures du matin
#'           
#'           # à 8 heures
#'           present.a.8h <- is.present.at(dp, "08:00:00")
#'           n.p8 <- tapply(present.a.8h, yday(as.Date(dp$ENTREE)), sum)
#'           summary(n.p8)
#'           xts.p8 <- xts(n.p8, order.by = unique(as.Date(dp$ENTREE)))
#'           plot(xts.p8, ylab = "Nombre de patients présents", main = "Nombre de patients présents à 8 heures du matin")
#'           lines(rollmean(x = xts.p8, k = 7), col = "red", lwd = 2)
#' @export
#' 
is.present.at <- function(dp, heure = "15:00:00"){
    # présent à 15 heures
    limite <- hms(heure) # pour incrémenter d'une heure: hms("15:00:00") + as.period(dhours(1))
    np <- dp$he < limite & dp$hs > limite
    
    return(np)
}