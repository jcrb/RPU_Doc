# mois.R - routines pour mois.Rmd
# 
# Nécessite lubridatey

#=======================================
#
# nb.jours.mois
#
#=======================================
#'@title Pour une année donnée, calcule le nombre de jours de chaque mois 
#'@parm anc annéec oirante
#'@return un vecteur de 12 nombres
#'@usage nb.jours.mois(2016)
#' #[1] 31 29 31 30 31 30 31 31 30 31 30 31
#' 
nb.jours.mois <- function(anc){
    debut <- as.Date(paste0(anc, "-01-01"))
    fin <- as.Date(paste0(anc + 1, "-01-01"))
    n.j <- as.integer(diff(seq(debut, fin, by = "month")))
    n.j
}

#=======================================
#
# standard.month
#
#=======================================
#'@title standardise le nombre de RPU sur un mois standard de 30 jours
#'@param vx vecteur de 12 mois nombre de RPU par mois
#'@param anc année courante
#'@return nb de RPU par mois de 30 jour
#'@usage # nombre de RPU par mois
#'       t <- tapply(as.Date(dx$ENTREE), month(as.Date(dx$ENTREE), label = TRUE), length)
#'       # standardisation
#'       standard.month(t, 2016)
#'       
standard.month <- function(vx, anc){
    n.j <- nb.jours.mois(anc)
    l <- length(vx[!is.na(vx)]) # nb de mois non nuls depuis le début de l'année
    vx.corrige <- vx[1:l] * 30 / n.j[1:l]
    vx.corrige
}