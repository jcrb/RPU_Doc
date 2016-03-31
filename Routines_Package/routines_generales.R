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
#' @usage format.n(7890.14) -> "7 890,14"
#' @export

format.n <- function(x){
    return(format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE, digits = 2))
}      