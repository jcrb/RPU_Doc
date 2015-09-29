# ====================================
# 
#      Routines générales pour RPU
#      
# ====================================


#===============================================
#
# Formate un nombre à imprimer
#
#===============================================
#' @author JcB - 2015-03-12
#' @title formate un nombre décimal
#' @description formate un nombre en ajoutant un espace pour les milliers
#'                                           une virgule décimale
#'                                           pas de notation scientifique
#'                                           deux chiffres significatifs
#' @param x un nombre décimal                                         
#' @usage format.n(7890.14) -> "7 890,14"
#' @export

format.n <- function(x){
    return(format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE, digits = 2))
}      

#===============================================
#
# Taux complétude RPU
#
#===============================================

#'@title taux de complétude global. 
#'@description Pour chacune des rubriques RPU calcule le taux de réponse (complétude)
#'@details todo
#'@author JcB 2013-02-01
#'@keywords complétude
#'@family RPU
#'@param dx Un dataframe
#'@param calcul 2 options "percent" (défaut) ou "somme". Somme = nb de réponses
#'        non nulles. Percent = % de réponses non nulles.
#'@param tri si tri = TRUE (defaut) les colonnes sont triées par ordre croissant.
#'@return vecteur des taux de complétude
#'@example todo
#'@export

completude <- function(dx, calcul = "percent", tri = FALSE){
    # calcul du % ou de la somme
    percent <- function(x){round(100 * mean(!is.na(x)),2)}
    somme <- function(x){sum(!is.na(x))}
    "%!in%" <- function(x, y) x[!x %in% y]
    
    if(calcul == "percent")
        fun <- percent
    else
        fun <- somme
    
    # complétude brute. Des corrections sont nécessaires pour DESTINATION
    completude <- apply(dx, 2, fun)
    
    # correction pour Destination et Orientation
    # Les items DESTINATION et ORIENTATION ne sappliquent quaux patients hspitalisés. 
    # On appelle hospitalisation les RPU pour lequels la rubrique MODE_SORTIE = MUTATION ou TRANSFERT. 
    # Pour les sorties à domicile, ces rubriques ne peuvent pas être complétées ce qui entraine 
    # une sous estimation importante du taux de complétude pour ces deux rubriques. 
    # On ne retient donc que le sous ensemble des patients hospitalisés pour lesquels les rubriques 
    # DESTINATION et ORIENTATION doivent être renseignées.
    hosp <- dx[dx$MODE_SORTIE %in% c("Mutation","Transfert"), c("DESTINATION", "ORIENTATION")]
    completude.hosp <- apply(hosp, 2, fun)
    completude['ORIENTATION'] <- completude.hosp['ORIENTATION']
    completude['DESTINATION'] <- completude.hosp['DESTINATION']
    
    # Correction pour DP. Cette rubrique ne peut pas être remplie dans le cas où ORIENTATION =
    # FUGUE, PSA, SCAM, REO
    # exemple d'utilisation de NOT IN
    dp <- dx[!(dx$ORIENTATION %in% c("FUGUE","PSA","SCAM","REO")), "DP"]
    # completude['DP'] <- mean(!is.na(dx$DP)) * 100 # erreur remplacer !is.na(dx$DP) par !is.na(dp$DP)
    completude['DP'] <- fun(dp)
    
    # réorganise les données dans l'ordre de la FEDORU
    completude <- reorder.vector.fedoru(completude)
    # completude <- completude[-c(1,7)]
    if (tri == TRUE)
        completude <- sort(completude) # tableau trié
    return(completude) 
}
