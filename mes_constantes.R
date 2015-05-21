# CONSTANTES (in RPU_doc)
#========================

pop.als.2010.totale <- 1115226 + 765634
pop_als_2010_municipale <- 1095905 + 749782
pop.67.2010.municipale <- 1095905
pop.68.2010.municipale <- 749782

superficie.alsace <- 8323 # km carrrés
superficie.67 <- 4798
superficie.68 <- 3525

pop.als.tot.2014 <- 1868773 # chiffre insee au 1/1/2014 (http://www.insee.fr/fr/themes/tableau.asp?reg_id=15&ref_id=poptc02104)

# Autre source
#Nombre de résidents dans la région. Estimation au 1er janvier 2014: 1868773
#http://www.insee.fr/fr/themes/detail.asp?ref_id=estim-pop&reg_id=99 <- ce fichier donne la pop par région et déprtement depuis 1975

dens.als.2014 <- pop.als.tot.2014 / superficie.alsace

pop0<-21655
pop1_75<-1677958
pop75<-146074
pop1_15<-309641
pop15_75<-1368317
pop75_85<-108426
pop85<-37647

# nombre de passages aux urgences en 2013 - chiffres SAE
passages.2013.SAE <- 493321
# nombre de RPU en 2013
n.rpu.2013 <- 340338

hop.short <- c("Wis","Hag","Sav","Hus","Odi","Sel","Col","Geb","Mul","3Fr","Alk","Dia")

hop.long <- c("CH Wissembourg","CH Haguenau","CH Saverne","HUS","CL Ste Odile","CH Séletat","CH Colmar","CH Guebwiller",
              "CH Mulhouse", "CL 3 Frontières","CH Alkirch","Diaconat-Fonderie")

rpu.names <- c("Entrée","Sexe","Age","Commune","ZIP","Provenance","PEC Transport",
               "Mode Transport","Mode entrée","CCMU","Motif","DP","Sortie", "Mode sortie","Orientation","Destination")

week.short <- c("Dim","Lun","Mar","Mer","Jeu","Ven","Sam")

week.long <- c("Dimanche","Lundi","Mardi","Mercredi","Jeudi","Vendredi","Samedi")

french.short.week <- c("Lun","Mar","Mer","Jeu","Ven","Sam","Dim")

french.long.week <- c("Lundi","Mardi","Mercredi","Jeudi","Vendredi","Samedi","Dimanche")

mois.short <- c("Jan","Fev","Mar","Avr","Mai","Jun","Jui","Aou","Sep","Oct","Nov","Dec")

mois.long <- c("Janvier","Février","Mars","Avril","Mai","Juin","Juillet","Août","Septembre","Octobre","Novembre","Décembre")

#=========================================================================================
#
#   population
#
#'@name population
#'@description retourne la population d'une entité géographique selon l'année
#'@param place = Alsace, 67, 68
#'@param year = 2014
#'@example population("Alsace", 2014)
#'
population <- function(place, year){
    if(year == "2014"){
        if(place == "Alsace") return(1868773)
        if(place == 67) return(1110416)
        if(place == 68) return(758357)
    }
    return(NA)
}

#=========================================================================================
#
#   densite
#
#'@name densite
#'@description retourne la densité de la population sur une surface donnée
#'@param place: Alsace, 67, 68
#'@param year: 2014
#'
densite <- function(place, year, quoi = NULL){
    # surfaces régionales
    surf.alsace <- 8320
    surf.67 <- 4798
    surf.68 <- 3525
    if(place == "Alsace") s = surf.alsace else
    if(place == 67) s = surf.67 else
    if(place == 68) s = surf.68 else
    
    # calcul de la densité
    return(population(place, year) / s)
}

#=========================================================================================
#
#    is.cpals
#
#'@ est-ce un code postal alsacien ?
#'@param cp code postal
#'@return TRUE ou FALSE
#'@usage is.cpals(as.factor("67550")), is.cpals("56000")
#'
is.cpals <- function(cp){
    ifelse(as.character(cp) >  "66999" & as.character(cp) < "69000", TRUE, FALSE)
}

