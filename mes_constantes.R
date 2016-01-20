# CONSTANTES (in RPU_doc)
#========================

# Géographie
# ==========

superficie.alsace <- 8323 # km carrrés
superficie.67 <- 4798
superficie.68 <- 3525

# Population
# ==========

# 2010
# ----
pop.als.2010.totale <- 1115226 + 765634
pop_als_2010_municipale <- 1095905 + 749782
pop.67.2010.municipale <- 1095905
pop.68.2010.municipale <- 749782

# 2014
# ----
# chiffre insee au 1/1/2014 (http://www.insee.fr/fr/themes/tableau.asp?reg_id=15&ref_id=poptc02104)
pop.als.tot.2014 <- 1868773 
pop.67.2014 <- 1110416
pop.68.2014 <- 758357

pop75.als.2014 <- 155281 # 75 ans et plus

# 2015
#-----
# Les populations légales millésimées 2012 ont été en vigueur le 1er janvier 2015. 
# Elles ont pour date de référence statistique le 1er janvier 2012.

pop.als.tot.2015 <- 1124434 + 771668
pop.67.2015 <- 1124434 # Population totale (Population municipale : 1 104 667)
pop.67.2015 <- 771668 # Population totale (Population municipale : 755 202)

# 2016
#-----
# Les populations légales 2013 entrent en vigueur le 1er janvier 2016.
# Elles sont disponibles pour les circonscriptions administratives existant au 1er janvier 2015 
# dans leurs limites territoriales à cette date.

pop.als.tot.2016 <- 1128825 + 774976
pop.67.2016 <- 1128825 # Population totale (Population municipale : 1 109 460)
pop.67.2016 <- 774976 # Population totale (Population municipale : 758 723)

# Autre source
# ------------
# Nombre de résidents dans la région. Estimation au 1er janvier 2014: 1868773
# http://www.insee.fr/fr/themes/detail.asp?ref_id=estim-pop&reg_id=99 <- ce fichier donne la pop par région et département depuis 1975

pop0<-21655
pop1_75<-1677958
pop75<-146074
pop1_15<-309641
pop15_75<-1368317
pop75_85<-108426
pop85<-37647

# Densité
# ========

dens.als.2014 <- pop.als.tot.2014 / superficie.alsace

# Chiffres SAE
# ============

# nombre de passages aux urgences en 2013 - chiffres SAE
# ------------------------------------------------------
passages.2013.SAE <- 493321

# [SAE 2014](http://www.data.drees.sante.gouv.fr/ReportFolders/reportFolders.aspx)
passages.2014.SAE <- 521129
passages.2014.SAE.nhc <- 32177
passages.2014.SAE.htp <- 75226
passages.2014.SAE.hus <- 32177 + 75226
passages.2014.SAE.hag <- 44380
passages.2014.SAE.sav <- 27889
passages.2014.SAE.wis <- 12709
passages.2014.SAE.sel <- 30438
passages.2014.SAE.odi <- 27655
passages.2014.SAE.ane <- 16558
passages.2014.SAE.dts <- 12350
passages.2014.SAE.dia <- 29368
passages.2014.SAE.ros <- NA
passages.2014.SAE.alk <- 16295
passages.2014.SAE.tan <- 15496
passages.2014.SAE.mul <- 14201 + 62722
passages.2014.SAE.has <- 14201
passages.2014.SAE.emr <- 62722
passages.2014.SAE.col <- 47529 + 18816
passages.2014.SAE.lpr <- 47529 # pasteur
passages.2014.SAE.prc <- 18816 # parc
passages.2014.SAE.geb <- 21124
passages.2014.SAE.c3f <- 16196


# nombre de RPU 

n.rpu.2013 <- 340338
n.rpu.2013.wis <- 12646
n.rpu.2013.hag <- 34414
n.rpu.2013.sav <- NA
n.rpu.2013.hus <- 37018
n.rpu.2013.odi <- 25963
n.rpu.2013.ane <- NA
n.rpu.2013.dts <- NA
n.rpu.2013.sel <- 29534
n.rpu.2013.col <- 64758
n.rpu.2013.geb <- 15103
n.rpu.2013.mul <- 56195
n.rpu.2013.alk <- 7126
n.rpu.2013.tan <- NA
n.rpu.2013.c3f <- 15688
n.rpu.2013.dia <- 29469
n.rpu.2013.ros <- NA

# valeur pour 2014
# n.rpu.2014 <- 416733
# n.rpu.2014.wis <- 12158
# n.rpu.2014.hag <- 39938
# n.rpu.2014.sav <- 29445
# n.rpu.2014.hus <- 61793
# n.rpu.2014.odi <- 24956
# n.rpu.2014.ane <- 7418
# n.rpu.2014.dts <- 3910
# n.rpu.2014.sel <- 28828
# n.rpu.2014.col <- 67378
# n.rpu.2014.geb <- 16024
# n.rpu.2014.mul <- 59471
# n.rpu.2014.alk <- 12660
# n.rpu.2014.tan <- NA
# n.rpu.2014.c3f <- 16134
# n.rpu.2014.dia <- 29410
# n.rpu.2014.ros <- 7210

# valeurs au 31 juillet 2014
n.rpu.2014 <- 228413
n.rpu.2014.wis <- 7585
n.rpu.2014.hag <- 21794
n.rpu.2014.sav <- 17398
n.rpu.2014.hus <- 26235
n.rpu.2014.odi <- 14873
n.rpu.2014.ane <- 2655
n.rpu.2014.dts <- 102
n.rpu.2014.sel <- 16990
n.rpu.2014.col <- 39293
n.rpu.2014.geb <- 9441
n.rpu.2014.mul <- 34261
n.rpu.2014.alk <- 7369
n.rpu.2014.tan <- NA
n.rpu.2014.c3f <- 9358
n.rpu.2014.dia <- 17261
n.rpu.2014.ros <- 3798



# Constantes RESURAL
# ==================

hop.short <- c("Wis","Hag","Sav","Hus","Odi","Sel","Col","Geb","Mul","3Fr","Alk","Dia","Ros")

hop.long <- c("CH Wissembourg","CH Haguenau","CH Saverne","HUS","CL Ste Odile","CH Séletat","CH Colmar","CH Guebwiller",
              "CH Mulhouse", "CL 3 Frontières","CH Alkirch","Diaconat-Fonderie","Diaconat-Roosvelt")

rpu.names <- c("Entrée","Sexe","Age","Commune","ZIP","Provenance","PEC Transport",
               "Mode Transport","Mode entrée","CCMU","Motif","DP","Sortie", "Mode sortie","Orientation","Destination")

week.short <- c("Dim","Lun","Mar","Mer","Jeu","Ven","Sam")

week.long <- c("Dimanche","Lundi","Mardi","Mercredi","Jeudi","Vendredi","Samedi")

french.short.week <- c("Lun","Mar","Mer","Jeu","Ven","Sam","Dim")

french.long.week <- c("Lundi","Mardi","Mercredi","Jeudi","Vendredi","Samedi","Dimanche")

mois.short <- c("Jan","Fev","Mar","Avr","Mai","Jun","Jui","Aou","Sep","Oct","Nov","Dec")

mois.long <- c("Janvier","Février","Mars","Avril","Mai","Juin","Juillet","Août","Septembre","Octobre","Novembre","Décembre")

#==============================
#							  =
#   population                =
#                             =
#==============================
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

#==============================
#							  =
#   densite                   =
#                             =
#==============================
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

#==============================
#							  =
#   is.cpals                  =
#                             =
#==============================
#'@ est-ce un code postal alsacien ?
#'@param cp code postal
#'@return TRUE ou FALSE
#'@usage is.cpals(as.factor("67550")), is.cpals("56000")
#'

is.cpals <- function(cp){
    ifelse(as.character(cp) >  "66999" & as.character(cp) < "69000", TRUE, FALSE)
}

