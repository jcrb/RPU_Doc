#===============================================
#
# Formate un nombre à imprimer
#
#===============================================
#' @title formate un nombre
#' @description formate un nombre en ajoutant un espace pour les milliers
#'                                           une virgule décimale
#'                                           pas de notation scientifique
#'                                           deux chiffres significatifs
#' @usage format.n(x)  
#' @param x un nombre entier ou décimal                                        
#' @examples format.n(7890.14) -> "7 890,14"
#' @export

format.n <- function(x){
    return(format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE, digits = 2))
}

#===============================================
#
# Taux completude RPU
#
#===============================================
#' @title taux de completude global. 
#' @description Pour chacune des rubriques RPU calcule le taux de réponse (complétude)
#' @usage completude(dx, calcul = "percent", tri = FALSE)
#' @param dx Un dataframe
#' @param calcul 2 options "percent" (défaut) ou "somme". Somme = nb de réponses
#'        non nulles. Percent = \% de réponses non nulles.
#' @param tri si tri = TRUE (defaut) les colonnes sont triées par ordre croissant.
#' @details todo
#' @author JcB 2013-02-01
#' @keywords complétude
#' @family RPU
#' @return vecteur des taux de complétude
#' @export

completude <- function(dx, calcul = "percent", tri = FALSE){
    # calcul du % ou de la somme
    percent <- function(x){round(100 * mean(!is.na(x)),2)}
    somme <- function(x){sum(!is.na(x))}
    "%!in%" <- function(x, y) x[!x %in% y]
    
    if(calcul == "percent")
        fun <- percent
    else
        fun <- somme
    
    #' complétude brute. Des corrections sont nécessaires pour DESTINATION
    completude <- apply(dx, 2, fun)
    
    #' correction pour Destination et Orientation
    #' Les items DESTINATION et ORIENTATION ne s'appliquent qu'aux patients hspitalisés. 
    #' On appelle hospitalisation les RPU pour lequels la rubrique MODE_SORTIE = MUTATION ou TRANSFERT. 
    #' Pour les sorties à domicile, ces rubriques ne peuvent pas être complétées ce qui entraine 
    #' une sous estimation importante du taux de complétude pour ces deux rubriques. 
    #' On ne retient donc que le sous ensemble des patients hospitalisés pour lesquels les rubriques 
    #' DESTINATION et ORIENTATION doivent être renseignées.
    hosp <- dx[dx$MODE_SORTIE %in% c("Mutation","Transfert"), c("DESTINATION", "ORIENTATION")]
    completude.hosp <- apply(hosp, 2, fun)
    completude['ORIENTATION'] <- completude.hosp['ORIENTATION']
    completude['DESTINATION'] <- completude.hosp['DESTINATION']
    
    #' Correction pour DP. Cette rubrique ne peut pas être remplie dans le cas où ORIENTATION =
    #' FUGUE, PSA, SCAM, REO
    # exemple d'utilisation de NOT IN
    dp <- dx[!(dx$ORIENTATION %in% c("FUGUE","PSA","SCAM","REO")), "DP"]
    # completude['DP'] <- mean(!is.na(dx$DP)) * 100 # erreur remplacer !is.na(dx$DP) par !is.na(dp$DP)
    completude['DP'] <- fun(dp)
    
    # réorganise les données dans l'ordre de la FEDORU
    completude <- reorder.vector.fedoru(completude)
    #' completude <- completude[-c(1,7)]
    if (tri == TRUE)
        completude <- sort(completude) # tableau trié
    return(completude) 
}

#===============================================
#
# diagramme en étoile de la complétude
#
#===============================================
#' @title dessine un graphe en etoile
#' @description  dessine un graphe en étoile à partir des données retournées par "completude"
#' @usage radar.completude(completude, finess = NULL, titre = NULL)
#'@author JcB 2013-02-01
#'@keywords spider, diagramme étoile
#'@family RPU
#'@param completude taux de completude global calculé par la fonction completude
#'@param finess character: nom de l'établissement. NULL (defaut) => tout le datafame
#'@return diagramme en étoile
#'@examples radar.completude(completude(dx))
#'@examples radar.completude(completude(dwis), "Wissembourg")
#'@export

radar.completude <- function(completude, finess = NULL, titre = NULL){
    # library("openintro")
    # library("plotrix")
    par(cex.axis = 0.8, cex.lab = 0.8) #' taille des caractères
    #' diagramme en étoile: réglage de la distance de la légende 
    #' par rapport à l'extrémité du trait. Toutes des distances sont fixées à 1.24 puis
    #' certaines sont augmentées ou diminuée en fonction de leur taille.
    prop <- rep(1.24, length(completude))
    prop[1] <- 1.1
    prop[2] <- 1.1
    prop[10] <- 1.27
    prop[19] <- 1.1
    
    if(is.null(finess))
        main = "Radar de complétude régional (%)"
    else
        main = paste0(finess, " - Radar de complétude (%)")
    
    radial.plot(completude, rp.type="p", 
                radial.lim=c(0,100), 
                radial.labels=c("0","20%","40%","60%","80%",""),
                poly.col = fadeColor("khaki",fade = "A0"),  #' line.col="khaki",
                start = 1.57, 
                clockwise = TRUE, 
                line.col = "red",
                #line.col = c(rep("yellow",3), rep("green", 4), rep("red", 9),rep("blue", 3))
                labels = names(completude), 
                cex.axis = 0.6,
                label.prop = prop, # positionne individuellement chaque label
                mar = c(3,0,3,0),
                show.grid.labels = 1, #' N = 4
                main = main,
                #boxed.labels = FALSE,
                boxed.radial = FALSE
    )
    # par()
}

#===============================================
#
# Synthèse Taux complétude RPU
#
#===============================================
#'@title Calcule le tableau des taux de completude de l'ensemble des Finess.
#'@name synthese.completude
#'@description A partir du dataframe initial (dx) calcule le tableau des taux de complétude
#' de l'ensemble des Finess présents dans dx.
#' @usage synthese.completude(dx)
#' @param dx dataframe de type RPU
#' @details
#'Le tableau comporte en ordonnée le
#' nom des établissements, en abcisse les différents items du RPU et à l'intersection
#' ligne/colonne la complétude correspondante. dx peut comprter un ou plusieurs Finess
#' et concerner une période variable (semaine, mois, année...)
#' Nécessite la librairie plyr pour la fonction ddply()
#'@return un dataframe
#'@examples synthese.completude(dx)
#' synthese.completude(dx[dx$FINESS == "Hag",]) pour un seul établissement
#'@export
#'
synthese.completude <- function(dx){
    b <- ddply(dx, .(FINESS), completude)
    rownames(b) <- levels(factor(dx$FINESS))
    return(b)
}

#===============================================
#
# completude.time
#
#===============================================
#' @title Pour un etablissement donne, calcule le aux de completude par mois, semaine, jours
# 
#' @details Au départ on dispose d'un dataframe de type RPU. Ce dataframe est splité en sous groupes sur une base temporelle (mois,
#' jour, semaine...). Sur chacun des sous-groupes on applique la fonction "completude". Retourne un dataframe
#' où chaque ligne correspond à une période et chaque colonne à un élément du RPU.
#' Utilise "ddply" qui fonctionne comme tapply mais s'applique à un DF au lieu d'un vecteur et retourne un DF.
#' TODO: exension à plusieurs établissements simultannéent; limitation à certaines colonnes.
#' @usage completude.time(dx, finess,  time = "month")
#' @param dx un dataframe de type RPU
#' @param finess établissement concerné ('Wis', 'Hag', 'Sav', ...)
#' @param time factor de découpage
#' @examples load("~/Documents/Resural/Stat Resural/RPU_2014/rpu2015d0112_provisoire.Rda")
#'        # old
#'        sav <- d15[d15$FINESS == "Sav",] # Saverne 2015
#'        t3 <- ddply(sav, .(month(as.Date(sav$ENTREE))), completude) # completude par mois
#'        
#'        # new
#'        library(xts)
#'        t3 <- completude.time(d15, "Sav", "day")
#'        a <- seq(as.Date("2015-01-01"), length.out = nrow(t3), by = 1)
#'        x <- xts(t3, order.by = a)
#'        plot(x[, "DP"], main = "CH Saverne - DIAGNOSTIC PRINCIPAL", ylab = "\% de complétude")
#'        
#'        # TODO: tableau de complétude par mois et par Finess:
#'        t3 <- ddply(dx, .(dx$FINESS, month(as.Date(dx$ENTREE))), completude)
#'        # Application: rpu2014/Analyse/Completude/Analyse_completude
#' @export

completude.time <- function(dx, finess,  time = "month"){
    library(lubridate)
    library(plyr)
    
    df <- dx[dx$FINESS == finess,] # Saverne 2015
    
    if(time == "month") t <- ddply(df, .(month(as.Date(df$ENTREE))), completude) # completude par période
    if(time == "day")   t <- ddply(df, .(yday(as.Date(df$ENTREE))), completude)
    if(time == "wday")  t <- ddply(df, .(wday(as.Date(df$ENTREE))), completude)
    if(time == "year")  t <- ddply(df, .(year(as.Date(df$ENTREE))), completude)
    if(time == "week")  t <- ddply(df, .(week(as.Date(df$ENTREE))), completude)
    
    return(t)
}

#===============================================
#
# Ordonner les colonnes du dataframe
#
#===============================================
#' @title  Reordonne les colonnes du dataframe RPU dans l'ordre defini par la FEDORU.
#' @description Permet une meilleure cohérence du diagramme en étoile
#' @usage reorder.dataframe.fedoru(dx)
#' @param dx un dataframe de type RPU
#' @export
reorder.dataframe.fedoru <- function(dx){
    dx <- dx[, c("FINESS","id","EXTRACT","CODE_POSTAL","COMMUNE","NAISSANCE",
                 "SEXE","ENTREE","MODE_ENTREE","PROVENANCE","TRANSPORT","TRANSPORT_PEC",
                 "SORTIE","MODE_SORTIE","DESTINATION","ORIENTATION","MOTIF",
                 "GRAVITE","DP")]
    return(dx)
}

#===============================================
#
# Ordonner les variable d'un vecteur
#
#===============================================
#' @title Réordonne les colonnes pour être contorme à l'ordre RPU
#' @description On part d'un vecteur contenant les intitulés du RPU et on le réordonne pour que
#' les intitulés doient mis dans l'ordre du rapport FEDORU (proposition de GillesFaugeras)
#' @usage reorder.vector.fedoru(dx)
#' @param dx un dataframe du typr RPU
#' @export
#' @return un dataframe
#' 
reorder.vector.fedoru <- function(dx){
    dx <- dx[c("FINESS","id","EXTRACT","CODE_POSTAL","COMMUNE","NAISSANCE",
               "SEXE","ENTREE","MODE_ENTREE","PROVENANCE","TRANSPORT","TRANSPORT_PEC",
               "SORTIE","MODE_SORTIE","DESTINATION","ORIENTATION","MOTIF",
               "GRAVITE","DP")]
    # Changer l'intitulé des colonnes
    names(dx) <- c("FINESS","ID","EXTRACT","CODE POSTAL","COMMUNE","NAISSANCE",
                   "SEXE","DATE D'ENTREE","MODE D'ENTREE","PROVENANCE","TRANSPORT","TRANSPORT PEC",
                   "DATE DE SORTIE","MODE DE SORTIE","DESTINATION","ORIENTATION","MOTIF DE RECOURS",
                   "CCMU","DP")
    return(dx)
}

#===============================================
#
# teste.radar
#
#===============================================
#' @title  data pour créer automatiquement un radar RPU et faire des test
#' @export
#' @examples teste.radar()
#' @export
#' 
teste.radar <- function(){
    n <- c("FINESS","id","EXTRACT","CODE_POSTAL","COMMUNE","NAISSANCE", "SEXE","ENTREE","MODE_ENTREE","PROVENANCE","TRANSPORT",
           "TRANSPORT_PEC","SORTIE","MODE_SORTIE","DESTINATION","ORIENTATION","MOTIF","GRAVITE","DP")
    a <- rep(80, length(n))
    names(a) <- n
    a <- reorder.vector.fedoru(a)
    radar.completude(a, finess = "Test - 1er trimestre 2015")
    legend(-150, -120, legend = c("complétude régionale"), lty = 1, lwd = 3, col = "blue", bty = "n", cex = 0.8)
    legend(90, -120, legend = c("complétude locale"), lty = 1, lwd = 3, col = "red", bty = "n", cex = 0.8)
    
}

#===============================================
#
# count.CIM10
#
#===============================================
#' @title Combien de codes CIM10
#' @description examine un vecteur de caractères et compte le nombre de mots compatibles avec un code CIM10
#' NA n'est pas compté comme un code CIM10
#' @author JcB
#' @param vx un vecteur de character
#' @return n nombre de codes CIM1
#' @examples count.CIM10(dx[dx$FINESS == "Col", "MOTIF"])
#'  @export
#'
count.CIM10 <- function(vx){
    Encoding(vx) <- "latin1" # suprime les caractères bloquants pour grep. Il s'agit de Colmar avec des caractères window du type \x9
    n <- grep("^[A-Z][0-9][0-9]", vx, value = TRUE) # n contient les codes compatibles CIM10
    return(length(n))
}          

#===============================================
#
# passage
#
#===============================================
#
#'@title Horaires de passages
#'@name passage
#'@param he vecteur time de type hms
#'@param horaire = 'nuit', 'nuit profonde', 'jour'
#'@note necessite lubridate. Prend en compte toutes les heures et pas seulement celles
#'      comprises entre 0 et 72h (voir passage2)
#'@return un vecteur avec 2 éléments: le nombre de passages et le pourcentage en
#'        fonction de la période (jour, nuit)
#'@seealso horaire
#'@examples e <- datetime(dx$ENTREE); he <- horaire(e); nuit <- passage(he, "nuit")
#'@export
#'
passage <- function(he, horaire = "nuit"){
    if(horaire == "nuit")
        nuit <- he[he > hms("19:59:59") | he < hms("08:00:00")]
    else if(horaire == "nuit profonde")
        nuit <- he[he < hms("08:00:00")]
    else if(horaire == "jour")
        nuit <- he[he > hms("07:59:59") & he < hms("20:00:00")]
    
    n.passages <- length(nuit) # passages 20:00 - 7:59
    p.passages <- n.passages / length(he)
    return(c(n.passages, p.passages))
}

#===============================================
#
# horaire
#
#===============================================
#
#'@title extrait l'heure d'une date AAAA-MM-DD HH:MM:SS
#'@description extrait l'heure d'une date AAAA-MM-DD HH:MM:SS
#'@usage horaire(date)
#'@param date une date ou un vecteur au format DATE
#'@return un vecteur d'heures au format HH:MM:SS
#'@examples e <- datetime(dx$ENTREE); he <- horaire(e)
#'@export
#'
horaire <- function(date){
    library(lubridate)
    return(hms(substr(date, 12, 20)))
}

# solution avec POSIXt
horaire2 <- function(date){
    return(paste(as.POSIXlt(date)$hour, as.POSIXlt(date)$min, as.POSIXlt(date)$sec, sep=":"))
}

# solution avec strsplit
# examples: d <- horaire3(d14$ENTREE)
horaire3 <- function(date){
    return(hms(strsplit(date, " ")[[1]][2]))
    # return(ymd(strsplit(date, " ")[[1]][1])) retourne la date
}

#===============================================
#
# datetime
#
#===============================================
#'@title met une string date au format YYYY-MM-DD HH:MM:SS
#'@description met une string date au format YYYY-MM-DD HH:MM:SS
#'@usage datetime(date)
#'@param date une chaine de caractère de type Date
#'@return un vecteur date time (lubridate)
#'@note nécessite lubridate
#'@examples Transforme des rubriques ENTREE et SORTIE en objet datetime
#'@examples e <- datetime(dx$ENTREE)
#'@seealso horaire, passage.nuit
#'@export
#'
datetime <- function(date){
    return(ymd_hms(date))
}

#===============================================
#
# pdsa
#
#===============================================
#' @title Determine si on est en horaire de PDS.
#' @description Détermine si on est en horaire de PDS de WE (PDSWE) ou de semaine (PDSS) 
#' ou hors horaire de PDS (NPDS)
#' à partir d'une date.
#' @usage pdsa(dx)
#' @param dx vecteur date/heure au format YYYY-MM-DD HH:MM:SS
#' @return un vecteur de factor NPDS, PDSS, PDSW
#' @examples x <- "2009-09-02 12:23:33"; weekdays(as.Date(x)); pds(x) # NPDS
#' @examples pds(c("2015-05-23 02:23:33", "2015-05-24 02:23:33", "2015-05-25 02:23:33", 
#'              "2015-05-26 02:23:33", "2015-05-25 12:23:33", "2015-05-25 22:23:33"))
#'        # [1] "NPDS"  "PDSWE" "PDSWE" "PDSS"  "NPDS"  "PDSS" 
#' @examples Test Wissembourg sur une semaine:
#'        wis <- d14[d14$FINESS == "Wis" & 
#'                                  as.Date(d14$ENTREE) >= "2014-12-03" & 
#'                                  as.Date(d14$ENTREE) <= "2014-12-09", 
#'                                  c("ENTREE","FINESS")]
#'        wis$jour <- weekdays(as.Date(wis$ENTREE))
#'        wis$heure <- horaire(wis$ENTREE)
#'        wis$pdsa <- pds(wis$ENTREE)
#'        table(wis$pds)
#'        
#'        NPDS  PDSS PDSWE 
#'         136    35    52 
#' @export
#' @details REM sur xps les jours commencent par une minuscule alors que sur le Mac c'est une majuscule ?
pdsa <- function(dx){
    # j <- as.Date(dx)
    j <- tolower(weekdays(as.Date(dx)))
    h <- horaire(dx)
    
    # un vecteur vide
    temp <- rep("NPDS", length(dx))
    
    # jour non renseigés
    temp[is.na(j)] = NA
    
    # Horaires de PDS le WE
    temp[j =="dimanche" | 
             j =="samedi" & h > hms("11:59:59") & h <= hms("23:59:59") |
             j =="lundi" & h < hms("08:00:00")] = "PDSWE"
    
    # horaires de PDS en semaine
    temp[j %in% c("mardi","mercredi","jeudi","vendredi") &
             (h > hms("19:59:59") | h < hms("08:00:00"))]= "PDSS"
    temp[j == "lundi" & h > hms("19:59:59")]= "PDSS"
    temp[j == "samedi" & h < hms("08:00:00")]= "PDSS"
    
    return(temp)
}

#===============================================
#
# tab.completude
#
#===============================================
#' @title tableau de completude par jour
#' @description faire un tableau de complétude par jour pendant une période donnée
#' Permet de suivre les taux de complétude pour une structure et par période
#' @usage tab.completude(dx, d1, d2, finess = NULL)
#'@param dx dataframe de type RPU
#'@param d1 date de début
#'@param d2 date de fin
#'@param finess =  NULL ou un des finess abrégés autorisés. Si NULL, dx doit être spécifique
#'                 d'un établissement.
#'@examples hus <- d15[d15$FINESS == hus,]
#'       d1 <- as.Date("2015-01-01")
#'       d2 <- as.Date("2015-01-31")
#'       t <- tab.completude(hus, d1, d2)
#'       plot(t[,"DATE DE SORTIE"], type = "l", main = "Mode de sortie", ylab = "Taux de completude")
#'       t.zoo <- zoo(t) # nécessite la librairie zoo
#'       plot(xts(t.zoo$DP, order.by = as.Date(rownames(t.zoo))), las = 2, 
#'              main = "Diagnostic principal", ylab = "Taux de completude", cex.axis = 0.8)
#'      boxplot(t, las = 2, cex.axis = 0.8, ylab = "\% de completude", main = "Complétude RPU")
#' @export



tab.completude <- function(dx, d1, d2, finess = NULL){
    periode <- seq(as.Date (d1), as.Date(d2), 1)
    n <- length(periode)
    if(!is.null(finess)){
        dx <- dx[dx$FINESS == finess,]
    }
    tab <- completude(dx[as.Date(dx$ENTREE) == d1,])
    for(i in 2:n){
        j <- dx[as.Date(dx$ENTREE) == periode[i],]
        k <- completude(j)
        tab <- rbind(tab, k)
    }
    #tab <- data.frame(tab)
    rownames(tab) <- as.character(periode)
    return(tab)
}


#===============================================
#
# passages2 (nombre de passages)
#
#===============================================

#' @title Nombre de RPU sur une plage horaire donnee
#' @description Détermine le nombre de RPU sur une plage horaire donnée et le pourcentage par rapport
#' au nombre total de passages contenus dans vx.
#' @usage passages2(vx, h1, h2 = NULL)
#' @param vx vecteur de type datetime (dx$ENTREE, dx$SORTIE par exemple). Transformé par ymd_hms
#'           Transform dates stored as character or numeric vectors to POSIXct objects
#' @param h1 char heure de début ou période: 'nuit', nuit_profonde', 'jour', 'pds', 
#'                                            'soir', '08:00:00'
#' @param h2 char heure de fin. h2 doit être > h1
#' @author jcb
#' @details nécessite lubridate library(lubridate)
#' @examples n.passages.nuit <- passages2(pop18$ENTREE, "nuit"); n.passages.nuit[1]; n.passages.nuit[2]
#' @return 2 objets: nombre de RPU et pourcentage
#' @export
#' 
passages2 <- function(vx, h1, h2 = NULL){
    e <- ymd_hms(vx) # vecteur des entrées
    he <- hms(substr(e, 12, 20)) # on ne conserve que la partie horaire
    n.passages <- length(he) # nb de passages
    
    if(h1 == "nuit"){
        # nombre de passages dont l’admission s’est effectuée sur la période [20h00 - 7h59] 
        n <- he[he > hms("19:59:59") | he < hms("08:00:00")] # passages 20:00 - 7:59
    }
    else if(h1 == "nuit_profonde"){
        #nombre de passages dont l’admission s’est effectuée sur la période [00h00 - 7h59]
        n  <- he[he < hms("08:00:00")]
    }
    else if(h1 == "jour"){
        #nombre de passages dont l’admission s’est effectuée sur la période [08h00 - 19h59]
        n <- he[he > hms("07:59:59") & he < hms("20:00:00")] # passages 7:59 - 20:00
    }
    else if(h1 == "soir"){
        #nombre de passages dont l’admission s’est effectuée sur la période [20h00 - 0:00]
        n <- he[he > hms("19:59:59") & he <= hms("23:59:59")] # passages 7:59 - 20:00
    }
    else if(!is.null(h2)){
        # nombre de passages dont l’admission s’est effectuée sur la période [h1 - h2] 
        n <- he[he > hms(h1) & he < hms(h2)]
    }
    
    n <- length(n)
    p <- n/n.passages
    
    return(c(n, p))
}

#===============================================
#
# duree.passage2
#
#===============================================
#' @title Calcul de la duree de passage
#' @description todo
#' @usage duree.passage2(dx, h1 = 0, h2 = 4320, hors_uhcd = TRUE)
#' @param dx dataframe RPU
#' @param h1 durée minimale en minutes (par défaut > 0)
#' @param h2 durée maximale en minutes (par défaut 4320 = 72 heures)
#' @param hors_uhcd si TRUE (défaut) on retire les engegistrements où ORIENTATION = UHCD
#' @return dataframe à 4 colonnes: entree, sortie, mode_sortie, duree (en mn),
#'                                 he (heure d'entrée), hs (heure de sortie)
#' @export
#' 
duree.passage2 <- function(dx, h1 = 0, h2 = 4320, hors_uhcd = TRUE){
    # On forme un dataframe avec les heures d'entrées et de sortie auxquelle on rajoute 
    # pour certains calculs: MODE_SORTIE et ORIENTATION (Uhcd)
    # dataframe entrées-sorties, mode de sortie, orientation
    passages <- dx[, c("ENTREE", "SORTIE", "MODE_SORTIE", "ORIENTATION")] 
    # on ne conserve que les couples ENTREE-SORTIE complets
    passages <- passages[complete.cases(passages[, c("ENTREE", "SORTIE")]),] 
    n.passages <- nrow(passages)
    e <- ymd_hms(passages$ENTREE) # vecteur des entrées
    s <- ymd_hms(passages$SORTIE)
    # ON AJOUTE UNE COLONNE DUREE
    passages$duree <- as.numeric(difftime(s, e, units = "mins")) # vecteur des durées de passage en minutes
    # horaires seuls. Il faut isoler les heures de la date
    passages$he <- hms(substr(e, 12, 20)) # heures d'entrée
    passages$hs <- hms(substr(s, 12, 20)) # heures de sortie
    # on ne garde que les passages dont la durées > 0 et < ou = 72 heures
    passages <- passages[passages$duree > 0 & passages$duree < 3 * 24 * 60 + 1,]
    # passages hors UHCD
    if(hors_uhcd == TRUE){
        # un peu compliqué mais il faut éliminer les NA dans Orientation sinon
        # les résultats sont faux
        passages$ORIENTATION <- as.character(passages$ORIENTATION)
        passages$ORIENTATION[is.na(passages$ORIENTATION)] <- "na"
        passages <- passages[as.character(passages$ORIENTATION) != "UHCD",]
        # on remet tout en état
        passages$ORIENTATION[passages$ORIENTATION == "na"] <- NA
    }
    
    return(passages)
}

#===============================================
#
# resume.duree.passage
#
#===============================================
#' @title Resume de la Duree de passage.
#' @description Résumé de dp. dp est produit par duree.passages2 et se présente sous forme d'un 
#' data.frame à 4 colonnes
#' @usage summary.duree.passage(dp)
#' @name summary.duree.passage
#' @description analyse de la colonne durée 
#' @param dp un objet de type duree.passage2
#' @return - nb de durées
#'         - min durée
#'         - max durée
#'         - durée moyenne
#'         - durée médiane
#'         - écart-type
#'         - 1er quartile
#'         - 3ème quartile
#' @export
#' 
resume.duree.passage <- function(dp){
    n <- nrow(dp) # nb de valeurs
    s <- summary(dp$duree) # summary de la colonne durée
    sd <- sd(dp$duree)
    
    a <- c(n, s["Min."], s["Max."], s["Mean"], s["Median"], sd, s["1st Qu."], s["3rd Qu."])
    
    names(a) <- c("n", "min", "max", "mean", "median", "sd", "q1", "q3")
    return(a)
}

#===============================================
#
# resume.passages
#
#===============================================
#' @title analyse un objet de type duree.passage2
#' @description analyse un objet de type duree.passage2
#' @usage summary.passages(dp)
#' @param dp un objet de type duree.passage2. Correspond à un dataframe d'éléments du RPU dont
#'        la rurée de passage est conforme cad non nulle et inférieure à 72 heures
#'        
#' @return n.conforme               NB de durées conformes (>0 mn et < 72 heures)
#'         duree.moyenne.passage    durée moyenne d'un passage en minutes
#'         duree.mediane.passage    durée médiane d'un passage en minutes
#'         duree.moyenne.passage.dom    durée moyenne d'un passage en minutes si retour dom
#'         duree.mediane.passage.dom    durée médiane d'un passage en minutes
#'         duree.moyenne.passage.hosp    durée moyenne d'un passage en minutes si hospit.
#'         duree.mediane.passage.hosp    durée médiane d'un passage en minutes
#'         n.passage4               nombre de passages de moins de 4 heures
#'         n.hosp.passage4          nombre de passages de moins de 4 heures suivi d'hospitalisation
#'         n.domicile               nombre de retours à domicile
#'         n.dom.passage4           nombre de passages de moins de 4 heures suivi d'un retour à domicile
#'         n.dom                    nombre de retours à domicile
#' @export
#' 
resume.passages <- function(dp){
    # dp <- duree.passage2(dx)
    
    tmax <- 4 * 60 + 1 # < 4 heures
    
    n.conforme <- nrow(dp)
    duree.moyenne.passage <- mean(dp$duree, na.rm = TRUE)
    duree.mediane.passage <- median(dp$duree, na.rm = TRUE)
    # durée de passage moyenne si retour à domicile
    duree.moyenne.passage.dom <- mean(dp$duree[dp$MODE_SORTIE == "Domicile"], na.rm = TRUE)
    duree.mediane.passage.dom <- median(dp$duree[dp$MODE_SORTIE == "Domicile"], na.rm = TRUE)
    # durée de passage moyenne si hospitalisation
    duree.moyenne.passage.hosp <- mean(dp$duree[dp$MODE_SORTIE %in% c("Mutation","Transfert")], na.rm = TRUE)
    duree.mediane.passage.hosp <- median(dp$duree[dp$MODE_SORTIE %in% c("Mutation","Transfert")], na.rm = TRUE)
    
    s.mode.sortie <- summary(as.factor(dp$MODE_SORTIE))
    
    n.passage4 <- length(dp$duree[dp$duree < tmax]) #nb passages < 4h
    
    # Nombre de RPU avec une heure de sortie conforme (]0-72h[ lors d'un retour au domicile:
    n.dom <- s.mode.sortie["Domicile"]
    # Nombre de RPU avec une heure de sortie conforme (]0-72h[ lors d'une hospitalisation
    # post-urgences:
    n.hosp <- s.mode.sortie["Mutation"] + s.mode.sortie["Transfert"] 
    
    n.transfert <- s.mode.sortie["Transfert"]   # nb de transfert
    n.mutation <- s.mode.sortie["Mutation"]     # Nombre de mutation interne
    n.deces <- s.mode.sortie["Décès"]           # nombre de décès
    
    n.hosp.passage4 <- length(dp$duree[dp$duree < tmax & 
                                           dp$MODE_SORTIE %in% c("Mutation","Transfert")]) #nb passages < 4h et hospitalisation
    
    n.dom.passage4 <- length(dp$duree[dp$duree < tmax & 
                                          dp$MODE_SORTIE == "Domicile"]) #nb passages < 4h et retourà domicile
    
    a <- c(n.conforme, duree.moyenne.passage, duree.mediane.passage, duree.moyenne.passage.dom,
           duree.mediane.passage.dom, duree.moyenne.passage.hosp, duree.mediane.passage.hosp,
           n.passage4, n.hosp.passage4,
           n.dom.passage4, n.dom, n.hosp, n.transfert, n.mutation, n.deces)
    
    names(a) <- c("n.conforme", "duree.moyenne.passage", "duree.mediane.passage",
                  "duree.moyenne.passage.dom", "duree.mediane.passage.dom",
                  "duree.moyenne.passage.hosp", "duree.mediane.passage.hosp",
                  "n.passage4",
                  "n.hosp.passage4", "n.dom.passage4",  "n.dom", "n.hosp", "n.transfert", 
                  "n.mutation", "n.deces")
    
    return(a)
}


#===============================================
#
# resume.sexe
#
#===============================================
#' @title analyse un vecteur formé d'une suite de H, F, ou I
#' @description retourne: le nombre d'éléments du vcteur (NA inclus), le nombre de NA, nombre et pourcentage de valeurs renseignées,
#' nombre et pourcentage d'hommes et de femmes, sex ratio et taux de masculinité.
#' @usage summary.sexe(vx)
#' @param vx vecteur de Char (sexe)
#' @return vecteur nommé: "N", "n.na", "n.rens", "p.rens", "n.hommes", "n.femmes", "p.hommes", "p.femmes",
#'          "sex.ratio", "tx.masculinité"
#' @export
#' 
resume.sexe <- function(vx){
    sexe <- table(as.factor(vx))
    n <- length(vx) # nb de valeurs
    n.na <- sum(is.na(vx)) # nb de valeurs non renseignées
    p.na <- mean(is.na(vx)) # % de valeurs non renseignées
    n.rens <- sum(!is.na(vx)) # nb de valeurs renseignées
    p.rens <- mean(!is.na(vx)) # % de valeurs renseignées
    
    p.femme <- sexe['F']*100/(sexe['F'] + sexe['M']) # % de femmes
    p.homme <- sexe['M']*100/(sexe['F'] + sexe['M']) # % d'hommes
    sex.ratio <- sexe['M'] / sexe['F'] # sex ratio
    n.hommes <- sexe['M']
    n.femmes <- sexe['F']
    tx.masculinite <- n.hommes / (n.hommes + n.femmes)
    
    a <- c(n, n.na, n.rens, p.rens, n.hommes, n.femmes, p.homme, p.femme, sex.ratio, tx.masculinite)
    names(a) <- c("N", "n.na", "n.rens", "p.rens", "n.hommes", "n.femmes", "p.hommes", "p.femmes",
                  "sex.ratio", "tx.masculinité")
    return(a)
}

#===============================================
#
# resume.motif
#
#===============================================
#' @title analyse un vecteur de MOTIF
#' @description retourne: le nombre d'éléments du vecteur (NA inclus), le nombre de NA, nombre et pourcentage de valeurs renseignées,
#' @usage summary.motif(vx)
#' @param vx vecteur de Char (motif)
#' @return vecteur nommé:  "n.na", "p.na", "n.rens", "p.rens"
#' @export
#' 
resume.motif <- function(vx){
    motif <- table(as.factor(vx))
    
    n.na <- sum(is.na(vx)) # nb de valeurs non renseignées
    p.na <- mean(is.na(vx)) # % de valeurs non renseignées
    n.rens <- sum(!is.na(vx)) # nb de valeurs renseignées
    p.rens <- mean(!is.na(vx)) # % de valeurs renseignées
    
    a <- c(n, n.na, n.rens, p.rens)
    names(a) <- c("N", "n.na", "n.rens", "p.rens")
    
    return(a)
    
}

#===============================================
#
# resume.entree
#
#===============================================
#' @title analyse du vecteur ENTREE ou SORTIE
#' @description analyse du vecteur ENTREE ou SORTIE
#' @usage summary.entree(vx)
#' @param vx vecteur de Date ou de DateTime
#' @examples summary.entree(as.Date(pop75$ENTREE))
#' @return vecteur nommé: "n", "n.na", "n.rens", "p.rens", "min", "max", "range"
#' @note min et max ne s'affichent pas sous forme de date. Que donne hms
#' @export
#' 
resume.entree <- function(vx){
    n <- length(vx) # nb de valeurs
    n.na <- sum(is.na(vx)) # nb de valeurs non renseignées
    p.na <- mean(is.na(vx)) # % de valeurs non renseignées
    n.rens <- sum(!is.na(vx)) # nb de valeurs renseignées
    p.rens <- mean(!is.na(vx)) # % de valeurs renseignées
    
    a <- c(n, n.na, n.rens, p.rens, min(as.Date(vx)), max(as.Date(vx)), max(vx) - min(vx))
    names(a) <- c("n", "n.na", "n.rens", "p.rens", "min", "max", "range")
    
    return(a)
}

#===============================================
#
# resume.transport
#
#===============================================
#' @title analyse du vecteur TRANSPORT
#' @description analyse du vecteur TRANSPORT
#' @usage summary.transport(vx)
#' @param vx vecteur de Factor
#' @return "n", "n.na", "p.na", "n.rens", "p.rens", "n.fo", "n.heli", "n.perso", "n.smur",
#'          "n.vsav", "n.ambu", "p.fo", "p.heli", "p.perso", "p.smur", "p.vsav", "p.ambu"
#' @examples summary.transport(pop75$TRANSPORT)
#' @export

resume.transport <- function(vx){
    n <- length(vx) # nb de valeurs
    n.na <- sum(is.na(vx)) # nb de valeurs non renseignées
    p.na <- mean(is.na(vx)) # % de valeurs non renseignées
    n.rens <- sum(!is.na(vx)) # nb de valeurs renseignées
    p.rens <- mean(!is.na(vx)) # % de valeurs renseignées
    s <- summary(as.factor(vx))
    
    n.perso <- s['PERSO']
    n.smur <- s['SMUR']
    n.vsav <- s['VSAB']
    n.ambu <- s['AMBU']
    n.fo <- s['FO']
    n.heli <- s['HELI']
    
    p.perso <- n.perso / n.rens
    p.smur <- n.smur / n.rens
    p.vsav <- n.vsav / n.rens
    p.ambu <- n.ambu / n.rens
    p.fo <- n.fo / n.rens
    p.heli <- n.heli / n.rens
    
    a <- c(n, n.na, p.na, n.rens, p.rens, n.fo, n.heli, n.perso, n.smur, n.vsav, n.ambu,
           p.fo, p.heli, p.perso, p.smur, p.vsav, p.ambu)
    
    names(a) <- c("n", "n.na", "p.na", "n.rens", "p.rens", "n.fo", "n.heli", "n.perso", "n.smur",
                  "n.vsav", "n.ambu", "p.fo", "p.heli", "p.perso", "p.smur", "p.vsav", "p.ambu")
    
    return(a)
}

#===============================================
#
# resume.ccmu
#
#===============================================
#' @title Resume du vecteur vx des CCMU
#' @description résumé du vecteur vx des CCMU
#' @usage summary.ccmu(vx)
#' @param vx vecteur de factor CCMU
#' @examples summary.ccmu(dx$GRAVITE)
#' @return "n", "n.na", "p.na", "n.rens", "p.rens", "n.ccmu1", "n.ccmu2", "n.ccmu3", 
#' "n.ccmu4", "n.ccmu5", "n.ccmup", "n.ccmud", "p.ccmu1", "p.ccmu2", "p.ccmu3", "p.ccmu4", "p.ccmu5", "p.ccmup", "p.ccmud")
#' @export
#' 
resume.ccmu <- function(vx){
    n <- length(vx) # nb de valeurs
    n.na <- sum(is.na(vx)) # nb de valeurs non renseignées
    p.na <- mean(is.na(vx)) # % de valeurs non renseignées
    n.rens <- sum(!is.na(vx)) # nb de valeurs renseignées
    p.rens <- mean(!is.na(vx)) # % de valeurs renseignées
    s <- summary(factor(vx))
    
    n.ccmu1 <- s['1']
    n.ccmu2 <- s['2']
    n.ccmu3 <- s['3']
    n.ccmu4 <- s['4']
    n.ccmu5 <- s['5']
    n.ccmup <- s['P']
    n.ccmud <- s['D']
    
    p.ccmu1 <- n.ccmu1/n.rens
    p.ccmu2 <- n.ccmu2/n.rens
    p.ccmu3 <- n.ccmu3/n.rens
    p.ccmu4 <- n.ccmu4/n.rens
    p.ccmu5 <- n.ccmu5/n.rens
    p.ccmup <- n.ccmup/n.rens
    p.ccmud <- n.ccmud/n.rens
    
    a <- c(n, n.na, p.na, n.rens, p.rens, n.ccmu1, n.ccmu2, n.ccmu3, n.ccmu4, n.ccmu5, 
           n.ccmup, n.ccmud, p.ccmu1, p.ccmu2, p.ccmu3, p.ccmu4, p.ccmu5, p.ccmup,p.ccmud)
    
    names(a) <- c("n", "n.na", "p.na", "n.rens", "p.rens", "n.ccmu1", "n.ccmu2", "n.ccmu3", 
                  "n.ccmu4", "n.ccmu5", "n.ccmup", "n.ccmud", "p.ccmu1", "p.ccmu2", "p.ccmu3", 
                  "p.ccmu4", "p.ccmu5", "p.ccmup", "p.ccmud")
    
    return(a)
}

#===============================================
#
# resume.dateheure
#
#===============================================
#' @title Resume du vecteur des ENTREE ou SORTIE
#' @description résumé du vecteur vx des ENTREE ou SORTIE
#' @usage summary.dateheure(vx)
#' @param vx vecteur ENTREE ou SORTIE
#' @examples summary.ccmu(dx$SORTIE)
#' @return "n", "n.na", "p.na", "n.rens", "p.rens"
#' @export
#' 
resume.dateheure <- function(vx){
    n <- length(vx) # nb de valeurs
    n.na <- sum(is.na(vx)) # nb de valeurs non renseignées
    p.na <- mean(is.na(vx)) # % de valeurs non renseignées
    n.rens <- sum(!is.na(vx)) # nb de valeurs renseignées
    p.rens <- mean(!is.na(vx)) # % de valeurs renseignées
    s <- summary(factor(vx))
    
    a <- c(n, n.na, p.na, n.rens, p.rens)
    names(a) <- c("n", "n.na", "p.na", "n.rens", "p.rens")
    
    return(a)
}

#===============================================
#
# resume.mode.sortie
#
#===============================================
#' @title Resume du vecteur vx des MODE_SORTIE
#' @description résumé du vecteur vx des MODE_SORTIE
#' @usage summary.mode.sortie(vx)
#' @param vx vecteur char MODE_SORTIE
#' @examples summary.mode.sortie(dx$MODE_SORTIE)
#' @return "n", "n.na", "p.na", "n.rens", "p.rens", 
#' "n.dom", "n.hosp", "n.transfert", "n.mutation", "n.deces", "p.dom", "p.hosp", "p.transfert", "p.mutation", "p.deces")
#' @export
#' 
resume.mode.sortie <- function(vx){
    n <- length(vx) # nb de valeurs
    n.na <- sum(is.na(vx)) # nb de valeurs non renseignées
    p.na <- mean(is.na(vx)) # % de valeurs non renseignées
    n.rens <- sum(!is.na(vx)) # nb de valeurs renseignées
    p.rens <- mean(!is.na(vx)) # % de valeurs renseignées
    s <- summary(as.factor(vx))
    
    n.dom <- s["Domicile"]
    n.hosp <- s["Mutation"] + s["Transfert"] 
    n.transfert <- s["Transfert"]   # nb de transfert
    n.mutation <- s["Mutation"]     # Nombre de mutation interne
    n.deces <- s["Décès"]           # nombre de décès
    
    p.dom <- n.dom / n.rens
    p.transfert <- n.transfert / n.rens
    p.mutation <- n.mutation / n.rens
    p.hosp <- p.transfert + p.mutation
    p.deces <- n.deces / n.rens
    
    a <- c(n, n.na, p.na, n.rens, p.rens, n.dom, n.hosp, n.transfert, n.mutation, n.deces,
           p.dom, p.hosp, p.transfert, p.mutation, p.deces)
    names(a) <- c("n", "n.na", "p.na", "n.rens", "p.rens", 
                  "n.dom", "n.hosp", "n.transfert", "n.mutation", "n.deces",
                  "p.dom", "p.hosp", "p.transfert", "p.mutation", "p.deces")
    
    return(a)
}

#===============================================
#
# resume.dp
#
#===============================================
#' @title Resume du vecteur DP (diagnostic principal)
#' @description résumé du vecteur vx des DP (diagnostic principal)
#' @usage summary.dp(vx)
#' @param vx vecteur char DP
#' @examples summary.dp(dx$DP)
#' @return "n", "n.na", "p.na", "n.rens", "p.rens"
#' @export
#' 
resume.dp <- function(vx){
    n <- length(vx) # nb de valeurs
    n.na <- sum(is.na(vx)) # nb de valeurs non renseignées
    p.na <- mean(is.na(vx)) # % de valeurs non renseignées
    n.rens <- sum(!is.na(vx)) # nb de valeurs renseignées
    p.rens <- mean(!is.na(vx)) # % de valeurs renseignées
    
    a <- c(n, n.na, p.na, n.rens, p.rens)
    names(a) <- c("n", "n.na", "p.na", "n.rens", "p.rens")
    
    return(a)
}

#===============================================
#
# resume.age
#
#===============================================
#' @title Resume du vecteur des AGE
#' @description résumé du vecteur vx des AGE
#' @usage summary.age(vx)
#' @param vx vecteur char AGE
#' @examples summary.dp(dx$AGE)
#' @return "n", "n.na", "p.na", "n.rens", "p.rens","n.inf1an", "n.inf15ans", "n.inf18ans", "n.75ans", "n.85ans", "n.90ans",
#' "p.inf1an", "p.inf15ans", "p.inf18ans", "p.75ans", "p.85ans", "p.90ans",
#' "mean.age", "sd.age", "median.age", "min.age", "max.age", "q1", "q3")
#' @export
#' 
resume.age <- function(vx){
    n <- length(vx) # nb de valeurs
    n.na <- sum(is.na(vx)) # nb de valeurs non renseignées
    p.na <- mean(is.na(vx)) # % de valeurs non renseignées
    n.rens <- sum(!is.na(vx)) # nb de valeurs renseignées
    p.rens <- mean(!is.na(vx)) # % de valeurs renseignées
    s.age <- summary(vx)
    # summary
    s <- summary(vx)
    sd <- sd(vx, na.rm = TRUE)
    # age sans les NA
    n.inf1an <- sum(vx < 1, na.rm = TRUE) #nb de moins d'un an
    p.inf1an <- mean(vx < 1, na.rm = TRUE)
    
    n.inf15an <- sum(vx < 15, na.rm = TRUE) #nb de moins de 15 ans
    p.inf15an <- mean(vx < 15, na.rm = TRUE)
    
    n.inf18an <- sum(vx < 18, na.rm = TRUE)
    p.inf18an <- mean(vx < 18, na.rm = TRUE)
    
    n.75ans <- sum(vx > 74, na.rm = TRUE)  #nb de 75 ans et plus
    p.75ans <- mean(vx > 74, na.rm = TRUE)
    
    n.85ans <- sum(vx > 84, na.rm = TRUE)  #nb de 85 ans et plus
    p.85ans <- mean(vx > 84, na.rm = TRUE)
    
    n.90ans <- sum(vx > 89, na.rm = TRUE)  #nb de 90 ans et plus
    p.90ans <- mean(vx > 89, na.rm = TRUE)
    
    a <- c(n, n.na, p.na, n.rens, p.rens, n.inf1an, n.inf15an, n.inf18an, n.75ans, n.85ans, n.90ans,
           p.inf1an, p.inf15an, p.inf18an, p.75ans, p.85ans, p.90ans,
           s['Mean'], sd, s['Median'], s['Min.'], s['Max.'], s['1st Qu.'], s['3rd Qu.'])
    
    names(a) <- c("n", "n.na", "p.na", "n.rens", "p.rens","n.inf1an", "n.inf15ans", "n.inf18ans",
                  "n.75ans", "n.85ans", "n.90ans",
                  "p.inf1an", "p.inf15ans", "p.inf18ans", "p.75ans", "p.85ans", "p.90ans",
                  "mean.age", "sd.age", "median.age", "min.age", "max.age", "q1", "q3")
    
    return(a)
}

#===============================================
#
# sresume.age.sexe
#
#===============================================
#' @title résumé des vecteurs AGE et SEXE
#' @description résumé des vecteurs AGE et SEXE
#' @usage summary.age.sexe(dx)
#' @param dx dataframe RPU
#' @examples summary.age.sexe(dx)
#' @return moyenne, écart-type, médiane par sexe
#' @export
#' 
resume.age.sexe <- function(dx){
    
    sd <- tapply(dx$AGE, dx$SEXE, sd, na.rm = TRUE)
    sd.age.h <- sd[['M']]
    sd.age.f <- sd[['F']]
    
    s <- tapply(dx$AGE, dx$SEXE, summary)
    
    mean.age.h <- s[["M"]]["Mean"] # age moyen des hommes
    mean.age.f <- s[["F"]]["Mean"] # age moyen des femmes
    
    median.age.h <- s[["M"]]["Median"] # age médian des hommes
    median.age.f <- s[["F"]]["Median"] # age médian des femmes
    
    a <- c(mean.age.h, sd.age.h, mean.age.f, sd.age.f, median.age.h, median.age.f)
    
    names(a) <- c("mean.age.h", "sd.age.h", "mean.age.f", "sd.age.f", "median.age.h", 
                  "median.age.f")
    
    return(a)
}

#===============================================
#
# pyramide.age
#
#===============================================
#' @title pyramide des ages
#' @description pyramide des ages
#' @usage pyramide.age(dx, cut = 5, gap = 1, cex = 0.8,col.h = "light green", col.f = "khaki1")
#' @param dx datafrae RPU ou DF à 2 colonnes: AGE et SEXE
#' @param cut intervalles. Par défaut tranche d'age de 5 ans, borne sup exclue: [0-5[ ans
#' @param col.h couleur pour les hommes
#' @param col.f couleur pour les femmes
#' @param gap largeur de la colonne age (N = 1, varie de 0 à ...)
#' @details pyramid nécessite epicalc, pyramid.plot nécessite plotrix
#' @export

pyramide.age <- function(dx, cut = 5, gap = 1, cex = 0.8,col.h = "light green", col.f = "khaki1"){
    # découpage du vecteur AGE en classes
    max = max(dx$AGE, na.rm = TRUE) 
    min = min(dx$AGE, na.rm = TRUE)
    a <- cut(dx$AGE, seq(from = min, to = max, by = cut), include.lowest = TRUE, right = FALSE)
    # division en 2 classes
    h <- as.vector(100 * table(a[dx$SEXE == "M"])/n.rpu)
    f <- as.vector(100 * table(a[dx$SEXE == "F"])/n.rpu)
    # graphe
    p <- pyramid.plot(h,f,
                      labels = names(table(a)), 
                      top.labels = c("Hommes", "Age", "Femmes"), 
                      main = "Pyramide des ages", 
                      lxcol = col.h, rxcol = col.f,
                      labelcex = cex,
                      gap = gap
    )
    return(p)
}

#===============================================
#
# tarru
#
#===============================================
#' @title Taux de Recours Regional aux Urgences
#' @description Les RPU générés par les habitants de la région sont comptés à partir du vecteur des codes postaux.
#'              Le rapport est calculé en divisant le nombre de RPU régionaux par la population de la région.
#' @usage tarru(cp, pop.region, rpu.region)
#' @param pop.region population régionale de référence
#' @param cp vecteur des codes postaux. Détermine le nb de RPU générés par des Alsaciens
#' @return un pourcentage
#' @examples pop.region <- pop.als.tot.2014 <- 1868773
#'           tarru(dx$CODE_POSTAL, pop.als.tot.2014)
#' @export

tarru <- function(cp, pop.region, rpu.region){
    rpu.region <- sum(sapply(cp, is.cpals))
    tarru <- rpu.region * 100 / pop.region
    return(tarru)
}

#===============================================
#
# summary.wday
#
#===============================================
#' @title Nombre de RPU par jour de semaine
#' @description à partir du vecteur vx des ENTREE, retourne le nombre de RPU
#'                  pour chaque jour de la semaine
#' @usage summary.wday(vx)
#' @param vx vecteur datetime
#' @return vecteur nommé commençant le lundi
#' @examples summary.wday(dx$ENTREE)
#' @details La semaine américaine est modifiée pour correspondre à la semaine française commençant un lundi.
#' @export
#' 
summary.wday <- function(vx){
    a <- tapply(as.Date(vx), wday(as.Date(vx), label = TRUE), length)
    names(a) <- c("Dim","Lun","Mar","Mer","Jeu","Ven","Sam")
    b <- a[2:7]
    a <- c(b, a[1])
    return(a)
}

#===============================================
#
# summary.cp
#
#===============================================
#' @title resume du vecteur CODE_POSTAL (cp)
#' @description résumé du vecteur vx des CODE_POSTAL (cp)
#' @usage summary.cp(vx)
#' @param vx vecteur char CODE_POSTAL
#' @details NECESSITE LA BIBLIOTHEQUE RPU_Doc/mes.constantes
#' @examples summary.cp(dx$CODE_POSTAL)
#' @return - nb de CP renseignés
#'          - nb de résidents alsaciens
#'          - nb d'étrangers
#' @export
#' 
summary.cp <- function(vx){
    n <- length(vx) # nb de valeurs
    n.na <- sum(is.na(vx)) # nb de valeurs non renseignées
    p.na <- mean(is.na(vx)) # % de valeurs non renseignées
    n.rens <- sum(!is.na(vx)) # nb de valeurs renseignées
    p.rens <- mean(!is.na(vx)) # % de valeurs renseignées
    
    n.residents <- sum(sapply(vx, is.cpals))
    n.etrangers <- n.rens - n.residents
    
    a <- c(n, n.na, p.na, n.rens, p.rens,n.residents, n.etrangers)
    names(a) <- c("n", "n.na", "p.na", "n.rens", "p.rens", "n.residents", "n.etrangers")
    
    return(a)
}

#===============================================
#
# analyse_type_etablissement
#
#===============================================
#' @title Analyse etablissement
#' @description fournit une liste d'indicateur à partir des données d'un établissement
#'              ou d'un groupe d'établissements. Voir rapport 2014: Analyse par type d'étblissement
#' @usage analyse_type_etablissement(es)
#' @param es dataframe RPU (es = établissement de santé)
#' @examples # es non SAMU, siège de SMUR
#'           # es <- dx[dx$FINESS %in% c("Wis","Hag","Sav","Sel","Col"),]
#'           # analyse_type_etablissement(es)
#' @return "n.passages", "n.age.ren", "n.inf1an", "n.inf15ans", "n.75ans", "n.cp.rens",
#' "n.etrangers", "n.lun", "n.mar", "n.mer", "n.jeu", "n.ven", "n.sam", "n.dim", 
#' "n.nuit", "n.pds", "n.h.rens", "n.trans.rens", "n.fo",
#' "n.heli", "n.perso", "n.smur", "n.vsav", "n.ambu", "n.ccmu.rens", "n.ccmu1", 
#' "n.ccmu2", "n.ccmu3", "n.ccmu4",
#' "n.ccmu5", "n.ccmuP", "n.ccmuD", "n.ccmu45", "n.sorties.conf", "mean.passage", 
#' "median.passage", "n.passage4", "n.hosp.passage4", "n.dom.passage4", "n.dom", 
#' "n.hosp", "n.transfert", "n.deces", "n.mode.sortie",
#' "n.mutation2"
#' @export
#' 
analyse_type_etablissement <- function(es){
    # nombre de passages déclarés
    n.passages <- nrow(es)
    
    s <- summary.age(es$AGE)# summary
    # Nombre de RPU avec un âge renseigné
    n.age.ren <- s["n.rens"]
    n.inf1an <- s["n.inf1an"]
    n.inf15ans <- s["n.inf15ans"]
    n.75ans <- s["n.75ans"]
    
    s <- summary.cp(es$CODE_POSTAL)
    # Nombre de RPU avec un code postal renseigné
    n.cp.rens <- s["n.rens"]
    # Nombre ne veant pas de la région
    n.etrangers <- s["n.etrangers"]
    
    # par jour de semaine
    s <- summary.wday(es$ENTREE)
    n.lun <- s[1]
    n.mar <- s[2]
    n.mer <- s[3]
    n.jeu <- s[4]
    n.ven <- s[5]
    n.sam <- s[6]
    n.dim <- s[7]
    
    # passages de nuit
    n.nuit <- passage(horaire(es$ENTREE), "nuit")[1]
    # p.nuit <- passage(horaire(es$ENTREE), "nuit")[2]
    
    # passage en PDS
    t <- table(pdsa(es$ENTREE))
    n.pds <- t["PDSS"] + t["PDSWE"]
    
    #Nombre de RPU avec une date et heure d'entrée renseignées
    s <- summary.dateheure(es$ENTREE)
    n.h.rens <- s["n.rens"]
    
    # nombre avec moyen de transport renseigné
    s <- summary.transport(es$TRANSPORT)
    n.trans.rens <- s["n.rens"]
    n.fo <- s["n.fo"]
    n.heli <- s["n.heli"]
    n.perso <- s["n.perso"]
    n.smur <- s["n.smur"]
    n.vsav <- s["n.vsav"]
    n.ambu <- s["n.ambu"]
    
    # nombre avec CCMU renseigné
    s <- summary.ccmu(es$GRAVITE)
    n.ccmu.rens <- s["n.rens"]
    n.ccmu1 <- s["n.ccmu1"]
    n.ccmu2 <- s["n.ccmu2"]
    n.ccmu3 <- s["n.ccmu3"]
    n.ccmu4 <- s["n.ccmu4"]
    n.ccmu5 <- s["n.ccmu5"]
    n.ccmuP <- s["n.ccmup"]
    n.ccmuD <- s["n.ccmud"]
    n.ccmu45 <- n.ccmu4 + n.ccmu5
    
    # nombre de sorties conformes
    s <- summary.passages(duree.passage2(es))
    n.sorties.conf <- s["n.conforme"]
    mean.passage <- s["duree.moyenne.passage"]
    median.passage <- s["duree.mediane.passage"]
    
    n.passage4 <- s["n.passage4"] # nb de passages de moins de 4 heures
    n.hosp.passage4 <- s["n.hosp.passage4"] # nb de passages de moins de 4 heures suivi hospit.
    n.dom.passage4 <- s["n.dom.passage4"] # nb de passages de moins de 4 heures suivi retour dom.
    n.dom  <- s["n.dom"] # nb total de retour à domicile
    n.hosp  <- s["n.hosp"]
    n.transfert  <- s["n.transfert"]
    n.deces <- s["n.deces"]
    
    # Nombre de RPU avec un mode de sortie renseigné
    s <- summary.mode.sortie(es$MODE_SORTIE)
    n.mode.sortie <- s["n.rens"]
    n.dom2 <- s["n.dom"]
    n.transfert2 <- s["n.transfert"]
    n.mutation2 <- s["n.mutation"]
    n.deces2 <- s["n.deces"]
    n.hosp2 <- s["n.hosp"]
    
    a <- c(n.passages, n.age.ren, n.inf1an, n.inf15ans, n.75ans, n.cp.rens, n.etrangers, n.lun,
           n.mar, n.mer, n.jeu, n.ven, n.sam, n.dim, n.nuit, n.pds, n.h.rens, n.trans.rens, n.fo,
           n.heli, n.perso, n.smur, n.vsav, n.ambu, n.ccmu.rens, n.ccmu1, n.ccmu2, n.ccmu3, n.ccmu4,
           n.ccmu5, n.ccmuP, n.ccmuD, n.ccmu45, n.sorties.conf, mean.passage, median.passage,
           n.passage4, n.hosp.passage4, n.dom.passage4, n.dom, n.hosp, n.transfert, n.deces, n.mode.sortie,
           n.mutation2)
    
    names(a) <- c("n.passages", "n.age.ren", "n.inf1an", "n.inf15ans", "n.75ans", "n.cp.rens",
                  "n.etrangers", "n.lun", "n.mar", "n.mer", "n.jeu", "n.ven", "n.sam", "n.dim", 
                  "n.nuit", "n.pds", "n.h.rens", "n.trans.rens", "n.fo",
                  "n.heli", "n.perso", "n.smur", "n.vsav", "n.ambu", "n.ccmu.rens", "n.ccmu1", 
                  "n.ccmu2", "n.ccmu3", "n.ccmu4",
                  "n.ccmu5", "n.ccmuP", "n.ccmuD", "n.ccmu45", "n.sorties.conf", "mean.passage", 
                  "median.passage", "n.passage4", "n.hosp.passage4", "n.dom.passage4", "n.dom", 
                  "n.hosp", "n.transfert", "n.deces", "n.mode.sortie",
                  "n.mutation2")
    
    return(a)
}

#===============================================
#
# summary.destination
#
#===============================================
#' @title Resume de la DESTINATION
#' @description résumé du vecteur vx des DESTINATION. En cas d'hospitalisation, il y a quatre destinations possibles:
#' MCO, SSR, SLD et PSY. En ca  de sortie au domicile: HAD et Structure médico-sociale (EHPAD)
#' @usage summary.destination(dx, correction = TRUE)
#' @param dx dataframe RPU
#' @param correction = TRUE: on ne retient que les destinations 
#'                           correspondant à une hospitalisation
#' @return "n", "n.na", "p.na", "n.rens", "p.rens"
#' @details MANQUE LE SUMMARY DU VECTEUR.
#' @export

summary.destination <- function(dx, correction = TRUE){
    if(correction == TRUE){
        vx <- dx$DESTINATION[dx$MODE_SORTIE %in% c("Mutation","Transfert")]
    }
    else{
        vx <- dx$DESTINATION
    }
    n <- length(vx) # nb de valeurs
    n.na <- sum(is.na(vx)) # nb de valeurs non renseignées
    p.na <- mean(is.na(vx)) # % de valeurs non renseignées
    n.rens <- sum(!is.na(vx)) # nb de valeurs renseignées
    p.rens <- mean(!is.na(vx)) # % de valeurs renseignées
    
    a <- c(n, n.na, p.na, n.rens, p.rens)
    names(a) <- c("n", "n.na", "p.na", "n.rens", "p.rens")
    
    return(a)
}

#===============================================
#
# summary.orientation
#
#===============================================
#' @title Resume de ORIENTATION
#' @description résumé du vecteur vx des ORIENTATION
#' @usage summary.orientation(dx, correction = TRUE)
#' @param dx dataframe RPU
#' @param correction = TRUE: on ne retient que les orientation 
#'                           correspondant à une hospitalisation
#' @return "n", "n.na", "p.na", "n.rens", "p.rens",
#' "n.chir", "n.med", "n.obst", "n.si", "n.sc", "n.rea", "n.uhcd", "n.ho", "n.hdt", 
#' "n.reo", "n.scam", "n.psa",
#' "p.chir", "p.med", "p.obst", "p.si", "p.sc", "p.rea", "p.uhcd", "p.ho", "p.hdt", 
#' "p.reo", "p.scam", "p.psa"
#' @export

summary.orientation <- function(dx, correction = TRUE){
    if(correction == TRUE){
        vx <- dx$ORIENTATION[dx$MODE_SORTIE %in% c("Mutation","Transfert")]
    }
    else{
        vx <- dx$ORIENTATION
    }
    n <- length(vx) # nb de valeurs
    n.na <- sum(is.na(vx)) # nb de valeurs non renseignées
    p.na <- mean(is.na(vx)) # % de valeurs non renseignées
    n.rens <- sum(!is.na(vx)) # nb de valeurs renseignées
    p.rens <- mean(!is.na(vx)) # % de valeurs renseignées
    
    s <- table(vx)
    
    # hospitalisés
    n.chir <- s['CHIR']
    n.med <- s['MED']
    n.obst <- s['OBST']
    n.si <- s['SI']
    n.sc <- s['SC']
    n.rea <- s['REA']
    n.uhcd <- s['UHCD']
    n.ho <- s['HO']
    n.hdt <- s['HDT']
    # non hospitalisés
    n.reo <- s['REO']
    n.scam <- s['SCAM']
    n.psa <- s['PSA']
    
    p.chir <- n.chir / n.rens
    p.med <- n.med / n.rens
    p.obst <- n.obst / n.rens
    p.si <- n.si / n.rens
    p.sc <- n.sc / n.rens
    p.rea <- n.rea / n.rens
    p.uhcd <- n.uhcd / n.rens
    p.ho <- n.ho / n.rens
    p.hdt <- n.hdt / n.rens
    p.reo <- n.reo / n.rens
    p.scam <- n.scam / n.rens
    p.psa <- n.psa / n.rens
    
    a <- c(n, n.na, p.na, n.rens, p.rens,
           n.chir, n.med, n.obst, n.si, n.sc, n.rea, n.uhcd, n.ho, n.hdt, n.reo, n.scam, n.psa,
           p.chir, p.med, p.obst, p.si, p.sc, p.rea, p.uhcd, p.ho, p.hdt, p.reo, p.scam, p.psa)
    
    names(a) <- c("n", "n.na", "p.na", "n.rens", "p.rens",
                  "n.chir", "n.med", "n.obst", "n.si", "n.sc", "n.rea", "n.uhcd", "n.ho", "n.hdt", 
                  "n.reo", "n.scam", "n.psa",
                  "p.chir", "p.med", "p.obst", "p.si", "p.sc", "p.rea", "p.uhcd", "p.ho", "p.hdt", 
                  "p.reo", "p.scam", "p.psa")
    
    return(a)
}

#===============================================
#
# evolution
#
#===============================================
#' @title Evolution d'une annee sur l'autre
#' @description calcule l'évolution entre 2 chiffres
#' @usage evolution(a, b)
#' @param a chiffre de l'année courante
#' @param b chiffre de l'année précédente
#' @return pourcentage d'augmentation ou de diminution
#' @examples evolution(n.rpu, n.rpu.2013)
#' @export

evolution <- function(a, b){
    return((a - b)/b)
}


#===============================================
#
# mn2h
#
#===============================================
#' @title transforme des minutes en heure/mn
#' @description transforme des minutes en heure/mn
#' @usage mn2h(x)
#' @param x integer = nombre de minutes
#' @return char
#' @export
#' 

mn2h <- function(x){
    h <- floor(x/60)
    mn <- round((x/60 - h) * 60, 0)
    a <- paste0(h, "h", mn)
    return(a)
    
}

#===============================================
#
# resume.rpu
#
#===============================================
#' @title calcule le nombre de RPU par SU, territoire de sante et
#'              departement.
#'@description calcule le nombre de RPU par SU, territoire de santé et
#'              département à partir d'un dataframe RPU. Deux colonnes sont
#'              indispensables: ENTREE et FINESS
#' @usage summary.rpu(dx)           
#'@author JcB - 2015-08-24
#'@source summary_rpu.R
#'@details v1.0 24/08/2015
#'@param dx un dataframe RPU ou un dataframe réduit à 2 colonnes: ENTREE et
#'          FINESS
#'@return un objet "list"
#'        n nombre total de RPU
#'        n.tx  total RPU du territoire x
#'        n.67  total pour le 67
#'        n.68  total pour 68
#'        n.xxx total pour le Finess xxx
#'        p.tx  % pour territoire x
#'@examples s <- summary.rpu(d15); s[1]; s$debut; s$n
#'@export

resume.rpu <- function(dx){
    
    debut <- min(as.Date(dx$ENTREE))
    fin <- max(as.Date(dx$ENTREE))
    
    t <- tapply(as.Date(dx$ENTREE), dx$FINESS, length)
    n <- sum(t)
    n.t1 <- sum(t['Hag'], t['Sav'], t['Wis'])
    n.hus <- sum(t['Hus'], t['HTP'], t['NHC'])
    n.t2 <- sum(n.hus, t['Ane'], t['Odi'], t['Dts'])
    n.t3 <- sum(t['Sel'], t['Col'], t['Geb'])
    n.t4 <- sum(t['Alk'], t['Tan'], t['Mul'], t['3Fr'], t['Dia'], t['Ros'],
                na.rm = TRUE)
    n.67 <- n.t1 + n.t2 + t['Sel']
    n.68 <- n - n.67
    
    p.t1 <- n.t1 / n
    p.t2 <- n.t2 / n
    p.t3 <- n.t3 / n
    p.t4 <- n.t4 / n
    
    n.wis <- t['Wis']
    n.hag <- t['Hag']
    n.sav <- t['Sav']
    n.ane <- t['Ane']
    n.odi <- t['Odi']
    n.dts <- t['Dts']
    n.sel <- t['Sel']
    n.col <- t['Col']
    n.geb <- t['Geb']
    n.alk <- t['Alk']
    n.tan <- t['Tan']
    n.mul <- t['Mul']
    n.dia <- t['Dia']
    n.ros <- t['Ros']
    n.3fr <- t['3Fr']
    
    # a est un objet composite formé de dates et de chiffres => on ne peut pas
    # en faire un vecteur car tous les éléments d'un vecteur doivent appartenir
    # au même type.
    a <- list(debut, fin, n, n.t1, n.t2, n.t3, n.t4, n.67, n.68, n.wis, n.hag, n.sav, n.ane,
              n.hus, n.odi, n.sel, n.col, n.geb, n.alk, n.tan, n.mul, n.dia,
              n.ros, n.3fr,
              p.t1, p.t2, p.t3, p.t4)
    
    names(a) <- c("debut", "fin", "n", "n.t1", "n.t2", "n.t3", "n.t4", "n.67", "n.68",
                  "n.wis", "n.hag", "n.sav", "n.ane", "n.hus", "n.odi",
                  "n.sel", "n.col", "n.geb", "n.alk", "n.tan", "n.mul",
                  "n.dia", "n.ros", "n.3fr",
                  "p.t1", "p.t2", "p.t3", "p.t4")
    
    return(a)
}

##############################################################################################

#    IMPRESSION DE TABLEAU

##############################################################################################

#===============================================
#
# print.table.rpu
#
#===============================================
#' @title Imprime une table avec xtable.
#' @description imprime une table avec xtable. Par défaut l'environnement est du type latex, le
#'              séparateur de milliers est l'espace et la virgule décimale
#' @usage print.table.rpu(t, caption = "", type = "latex", ref = "")             
#' @param t un objet de type table
#' @param caption une légende. Mettre c("légende", "sommaire") si nécessaire
#' @param type "latex" ou "html"
#' @param ref référence du tableau (latex)
#' 
#' @examples   print.table.rpu(t)
#'          print.table.rpu(t, "table de test")
#'          print.table.rpu(t, "table de test", "html")
#' @export

print.table.rpu <- function(t, caption = "", type = "latex", ref = ""){
    print.xtable(xtable(t, caption = caption), 
                 type = type, 
                 format.args=list(big.mark = " ", decimal.mark = ","), 
                 label = ref, 
                 comment = FALSE)
}

#===============================================
#
# print.summary.rpu
#
#===============================================
#' 
#' @title Imprime un summary.rpu
#' @description imprime un objet de type summary.rpu, en ligne eou en colonne (défaut) avec xtable.
#' @usage print.summary.rpu(x, sens = "colonne", cnames = NULL, rnames = NULL,
#'                          caption = "", type = "latex", ref = "")
#' @param x un vecteur nommé
#' @param sens 'colonne' = vertical, 'ligne' = horizontal
#' @param cnames  noms des colonnes
#' @param rnames  noms des lignes
#' @examples x <- ummary.wday(es$ENTREE))
#'        print.summary.rpu(x, cnames = c("Jour","n"), caption = "Nombre de RPU par jour de semaine")
#' @export
#' 
print.summary.rpu <- function(x, sens = "colonne", cnames = NULL, rnames = NULL, caption = "", type = "latex", ref = ""){
    y <- names(x)
    z <- as.numeric(x)
    p <- z/sum(z) # pourcentage
    if(sens == "colonne"){
        t <- cbind(y, format.n(z), round(p*100,2))
    }else{
        t <- rbind(y, format.n(z))
    }
    if(!is.null(cnames)) colnames(t) <- cnames
    if(!is.null(rnames)) rownames(t) <- rnames
    
    print.xtable(xtable(t, caption = caption), 
                 type = type, 
                 format.args=list(big.mark = " ", decimal.mark = ","), 
                 label = ref, 
                 comment = FALSE)
    
}

#===============================================
#
# factor2table
#
#===============================================
#' @title crée une table à 2 colonnes
#' @description crée une table à 2 colonnes: fréquence et pourcentage
#' @usage factor2table(vx, pc = TRUE)
#' @param vx un vecteur de facteurs ou d'entiers
#' @param pc si TRUE crée une colonne de \%
#' @return une table
#' @examples a <- c(1,2,3,4,5,5,5,5,1,1,2); factor2table(a); print.table.rpu(a)
#'      #        Fréq.     %
#'      #      1     3 27.27
#'      #      2     2 18.18
#'      #      3     1  9.09
#'      #      4     1  9.09
#'      #      5     4 36.36
#'      #     
#'      #     factor2table(pop18$GRAVITE, TRUE)
#' @export
#' 
factor2table <- function(vx, pc = TRUE){
    if(class(vx) == "factor")
        t <- table(factor(vx), dnn = "Fréq.")
    else t <- table(vx, dnn = "Fréq.")
    if(pc == "TRUE"){
        t2 <- prop.table(t)
        t <- cbind(t, round(t2 * 100, 2))
        colnames(t) <- c("Fréq.", "%")
    }
    return(t)
}

#===============================================
#
# passages.en.moins.de.4h
#
#===============================================
#' @title Analyse les passages de moins de 4 heures.
#' @description analyse les durée de passage de moins de 4 heures par rapport
#'   aux durées de passage conformes (c'est à dire de mons de 72 heures).
#' @name  passages.en.moins.de.4h
#' @param dx un dataframe de type RPU
#' @section Warning:
#' Cette fonction n'est pas terminée.
#' @return n.so.conforme.dom, n.duree.passage.inf4h.dom, p.passages.en.moins.de.4h.dom, n.so.conforme.hosp,
#' n.duree.passage.inf4h.hosp, p.duree.passage.inf4h.hosp
#' @export
#' 
passages.en.moins.de.4h <- function(dx){
    so <- dx[!is.na(dx$SORTIE), c("ENTREE","SORTIE", "MODE_SORTIE")]
    e <- ymd_hms(so$ENTREE)
    s <- ymd_hms(so$SORTIE)
    so$duree <- as.numeric(difftime(s, e, units = "mins"))
    # sortie conforme = > 0 et < 72h
    so <- so[so$duree > 0,]
    so <- so[so$duree < 60*24*3,]
    # heure de sortie conforme et retour à domicile
    so.conforme.dom <- so[!is.na(so$MODE_SORTIE) & so$MODE_SORTIE == "Domicile",]
    n.so.conforme.dom <- nrow(so.conforme.dom)
    # durée de passage < 4h et retour à domicile
    duree.passage.inf4h.dom <- so[!is.na(so$MODE_SORTIE) & so$MODE_SORTIE == "Domicile" & so$duree < 4*60,]
    n.duree.passage.inf4h.dom <- nrow(duree.passage.inf4h.dom)
    # pourcentage
    p.passages.en.moins.de.4h.dom <- n.duree.passage.inf4h.dom / n.so.conforme.dom
    
    # heure de sortie conforme et hospitalisation
    so.conforme.hosp <- so[!is.na(so$MODE_SORTIE) & so$MODE_SORTIE %in% c("Transfert", "Mutation") ,]
    n.so.conforme.hosp <- nrow(so.conforme.hosp)
    
    # durée de passage < 4h et hospitalisation
    duree.passage.inf4h.hosp <- so[!is.na(so$MODE_SORTIE) & so$MODE_SORTIE %in% c("Transfert", "Mutation") & so$duree < 4*60,]
    n.duree.passage.inf4h.hosp <- nrow(duree.passage.inf4h.hosp)
    
    # pourcentage de passages de moins de 4h suivis d'hospitalisation
    p.duree.passage.inf4h.hosp <- n.duree.passage.inf4h.hosp / n.so.conforme.hosp
    
    a <- c(n.so.conforme.dom, n.duree.passage.inf4h.dom, p.passages.en.moins.de.4h.dom, n.so.conforme.hosp,
           n.duree.passage.inf4h.hosp, p.duree.passage.inf4h.hosp)
    
    return(a)
}

#===========================================================================
#
# copyrigth
#
#===========================================================================
#'@title copyrigth
#'@author JcB
#'@description Place un copyright Resural sur un graphique. 
#'Par défaut la phrase est inscrite verticalement sur le bord droit de l'image
#'@usage copyright(an ="2013-2015",side=4,line=-1,cex=0.8, titre = "Resural")
#'@param an (str) année du copyright (par défaut 2013)
#'@param side coté de l'écriture (défaut = 4)
#'@param line distance par rapport au bord. Défaut=-1, immédiatement à l'intérieur du cadre
#'@param titre
#'@param cex taille du texte (défaut 0.8)
#'@return "© 2012 Resural"
#'@export
#'
copyright <- function(an ="2013-2015",side=4,line=-1,cex=0.8, titre = "Resural"){
    titre<-paste("©", an, titre, sep=" ")
    mtext(titre,side=side,line=line,cex=cex)
}

#===========================================================================
#
# Nombre de RPU par mois
#
#===========================================================================
#' @title Nombre de RPU par mois
#' @description Calcule le nombre de RPU par mois entre deux dates sous forme brute
#' ou corrigée en mois constants de 30 jours.
#' @usage rpu.par.mois(dx, standard = FALSE)
#' @param dx dataframe (au minimum la colonne ENTREE)
#' @param standard (boolean) si true retourne par mois corrigés de 30j sinon le nombre brut de RPU
#' @return un vecteur nommé: nom du mois, nb de RPU
#' @examples tc1 <- rpu.par.mois(d15, FALSE)
#' tc2 <- rpu.par.mois(d15, TRUE)
#' a <- rbind(tc1, tc2)
#' par(mar=c(5.1, 4.1, 8.1, 2), xpd=TRUE)
#' barplot(a, beside = TRUE, cex.names = 0.8)
#' legend("topleft", inset = c(0, -0.1), legend = c("Brut","Standardisé"), bty = "n", col = c("black","gray80"), pch = 15)
#' @export
#' 
rpu.par.mois <- function(dx, standard = FALSE){
    t <- tapply(as.Date(dx$ENTREE), months(as.Date(dx$ENTREE)), length)
    # remet les mois par ordre chronologique
    t <- t[c("janvier","février","mars", "avril","mai","juin","juillet", "août", "septembre", "octobre", "novembre", "décembre")]
    if(standard == TRUE){
        # nb de jours dans le mois (la séquence doit inclure le mois suivant. 
        # SOURCE: https://stat.ethz.ch/pipermail/r-help/2007-August/138116.html)
        min <- year(min(as.Date(dx$ENTREE)))
        max <- min + 1
        d1 <- paste0(min, "-01-01")
        d2 <- paste0(max, "-01-01")
        n.j <- as.integer(diff(seq(as.Date(d1), as.Date(d2), by = "month")))
        # nb de RPU par mois constant de 30 jours
        t <- t * 30 / n.j
    }
    return(t)
}

#=======================================
#
# rpu.par.jour
#
#=======================================
#' @title Nombre de RPU par jour et par FINESS
#' @description retourne une table contenant le nombre de RPU par jour et par FINESS
#' @usage rpu.par.jour(dx)
#' @param dx un dataframe de type rpu ayant un minimum 2 colonnes ENTREE et FINESS
#' @examples rpu.par.jour(d04)
#' @export
#'
#           3Fr Alk Ane Col Dia Dts Geb Hag Hus Mul Odi Ros Sav Sel Wis
# 2015-01-01  48  51   0 190  59  29  52 129 306 220  15   9  83  85  28
# 2015-01-02  45  52   0 210 102  27  43 118 292 200  10  28  94  81  30
# 2015-01-03  31  43   0 203  85  30  64 135 325   0   2  13 106 103  42
rpu.par.jour <- function(dx){
    return(table(as.Date(dx$ENTREE), dx$FINESS))
}

#=======================================
#
# finess2territoires
#
#=======================================
#' @title réorganiser les FINESS par territoires de santé
#' @usage finess2territoires(finess)
#' @param finess code finess de létablissement
#' @examples dx$FINESS <- finess2territoires(dx)
#' @export
#'
finess2territoires <- function(finess){
    finess <- factor(finess, levels = c('Wis','Hag','Sav','Hus','Ane','Odi','Dts','Sel','Col','Geb','Mul', 'Alk','Dia','Ros','3Fr'))
    return(finess)
}


#=======================================
#
# add.territoire
#
#=======================================
#' @title Crée une colonne TERRITOIRE
#' @description Ajoute une colonne TERRITOIRE à un dataframe qui contient une colonne FINESS
#' @usage add.territoire(dx)
#' @param dx un dataframe ayant une colonne FINESS renseignée
#' @return un dataframe 
#' @export
#'
add.territoire <- function(dx){
    dx$TERRITOIRE[dx$FINESS %in% c("Wis","Sav","Hag")] <- "T1"
    dx$TERRITOIRE[dx$FINESS %in% c("Hus","Odi","Ane","Dts")] <- "T2"
    dx$TERRITOIRE[dx$FINESS %in% c("Sel","Col","Geb")] <- "T3"
    dx$TERRITOIRE[dx$FINESS %in% c("Mul","3Fr","Alk","Ros","Dia")] <- "T4"
    return(dx)
}

#=======================================
#
# rpu.par.jour2
#
#=======================================
#' @title A partir d'un vecteur de dates, calcule le nombre de RPU par jour
#' @usage rpu.par.jour(d, roll = 7)
#' @param d vecteur de dates compatible avec le format Date
#' @param roll: nb de jours pour la moyenne lissée. Défaut = 7
#' @return un dataframe de 4 colonnes: date calendaire, nb de RPU du jour, le n° du jour de l'année (1 à 365), la moyennne lissée
#' @details RAJOUTER LES SOMMES   CUMuLEES. Nécessite xts, lubridate
#' @examples p2013 <- rpu.par.jour(j2013$ENTREE)
#'        plot(p2013$V2, type="l") # les RPU
#'        lines(p2013$V3, p2013$V4) # moyenne mobile
#' @export

rpu.par.jour2 <- function(d, roll = 7){
    # janvier 2013
    t <- tapply(as.Date(d), as.Date(d), length)
    df <- as.data.frame(cbind(names(t), as.numeric(t)), stringsAsFactors = FALSE)
    df$V1 <- as.Date(df$V1) # col. date
    df$V2 <- as.numeric(df$V2) # nb de RPU
    df$V3 <- yday(df$V1) # date du jour en n° du jour dans l'année
    df$V4 <- rollmean(df$V2, 7, fill = NA) # moyenne mobile sur 7 jours. rollmean crée un vecteur plus petit. Pour obtenir un vecteur de la même longueur, on remplace les valeurs manquantes par NA
    df$V5 <- df$V2 - df$V4 # pour CUSUM
    return(df)
}

#=======================================
#
# plot.xts2
#
#=======================================
#' @title plot.xts en couleur
#' @description La méthode plot.xts comprte un bug qui empêche l'affichage de courbes en couleur. Cette version corrige le bug.
#' @usage plot.xts2(x, y = NULL, type = "l", auto.grid = TRUE, major.ticks = "auto", minor.ticks = TRUE, major.format = TRUE, 
#' bar.col = "grey", candle.col = "white", ann = TRUE, axes = TRUE, col = "black", ...) 
#' @author Roman Luštrik (http://stackoverflow.com/users/322912/roman-lu%c5%a1trik)
#' @source http://stackoverflow.com/questions/9017070/set-the-color-in-plot-xts
#' @export
#'
plot.xts2 <- function (x, y = NULL, type = "l", auto.grid = TRUE, major.ticks = "auto", 
                       minor.ticks = TRUE, major.format = TRUE, bar.col = "grey", 
                       candle.col = "white", ann = TRUE, axes = TRUE, col = "black", ...) 
{
    series.title <- deparse(substitute(x))
    ep <- axTicksByTime(x, major.ticks, format = major.format)
    otype <- type
    if (xts:::is.OHLC(x) && type %in% c("candles", "bars")) {
        x <- x[, xts:::has.OHLC(x, TRUE)]
        xycoords <- list(x = .index(x), y = seq(min(x), max(x), 
                                                length.out = NROW(x)))
        type <- "n"
    }
    else {
        if (NCOL(x) > 1) 
            warning("only the univariate series will be plotted")
        if (is.null(y)) 
            xycoords <- xy.coords(.index(x), x[, 1])
    }
    plot(xycoords$x, xycoords$y, type = type, axes = FALSE, ann = FALSE, 
         col = col, ...)
    if (auto.grid) {
        abline(v = xycoords$x[ep], col = "grey", lty = 4)
        grid(NA, NULL)
    }
    if (xts:::is.OHLC(x) && otype == "candles") 
        plot.ohlc.candles(x, bar.col = bar.col, candle.col = candle.col, 
                          ...)
    dots <- list(...)
    if (axes) {
        if (minor.ticks) 
            axis(1, at = xycoords$x, labels = FALSE, col = "#BBBBBB", 
                 ...)
        axis(1, at = xycoords$x[ep], labels = names(ep), las = 1, 
             lwd = 1, mgp = c(3, 2, 0), ...)
        axis(2, ...)
    }
    box()
    if (!"main" %in% names(dots)) 
        title(main = series.title)
    do.call("title", list(...))
    assign(".plot.xts", recordPlot(), .GlobalEnv)
}

#=======================================
#
# rpu2xts
#
#=======================================
#' @title Transforme RPU eb XTS
#' @description A partir du fichier habituel des RPU retourne un objet xts ayant autant de
#'colonnes qu'il y a de SU dans d plus 2 colonnes supplémentaires:
#'- date de type 'Date' qui sert d'index à xts
#'- total nombre total de RPU par jour
#' @usage rpu2xts(dx)
#' @param dx un datafrale de type RPU comportant au moins une colonne ENTREE
#' @return un dataframe avec une colonne 'total'
#' @examples ts <- rpu2xts(d0106p); plot(ts$total);lines(rollapply(ts$total, 7, mean), col="red")
#' @export
#'
rpu2xts <- function(d){
    library(xts)
    t <- table(as.Date(d$ENTREE), d$FINESS)
    date <- rownames(t)
    a <- as.data.frame.matrix(t)
    a <- cbind(a, date)
    a$date <- as.Date(a$date)
    a$total <- rowSums(a[,1:14])
    ts <- xts(a, order.by = a$date)
    ts
}

#===========================================================================
#
# Nombre de RPU par semaine 
#
#===========================================================================
#' @title Calcule le nombre de RPU par mois
#' @description Calcule le nombre de RPU par mois de tous les ES présents dans le dataframe
#' @usage week.rpu(dx)
#' @param dx un dataframe de type RPU. Doit comporter au moins une colonne ENTREE
#' @details Nécessite Lubridate. dx peut regroupper tous les ES ou ne converner qu'un ES Particulier.
#' @return un vecteur du nombre de RPU par mois
#' @examples 
#' s <- week.rpu(dx)
#' tot <- sum(s) # nombre total de RPU
#' p = s/tot # % de RPU par semaine
#' summary(p)
#' 
#' @export
#' 
week.rpu <- function(dx){
    s <- tapply(as.Date(dx$ENTREE), week(as.Date(dx$ENTREE)), length)
    return(s)
}

#===========================================================================
#
# Variation du nombre de RPU par semaines
#
#===========================================================================
#' @title Variation du nombre de RPU par semaine
#' @description Variation du nombre de RPU par semaine
#' @usage week.variations(vx, last = FALSE)
#' @param vx vecteur du nombre de RPU pr semaine (voir week.rpu)
#' @param last boolean Si TRUE, on élimine la dernière semaine qui est souvent incomplète. FALSE par défaut.
#' @return un vecteur d'entiers positifs ou négatifs
#' @examples 
#' # d3 <- week.rpu(dx[dx$FINESS == "3Fr",])
#' # v <- week.variations(d3)
#' @export
#' 
week.variations <- function(vx, last = FALSE){
    # calcul de la différence d'une semaine à 'autre
    x <- diff(vx)
    # x compte une unité de moins que vx. Le 1er chiffre de d3 correspond à la semaine 2
    # ajout de 0 en tête du vecteur pour remplacer la première semaine
    x <- c(0, x)
    # pour supprimer la denière semaine qui est souvent incomplète (option)
    if(last == TRUE)
        x <-x[-length(x)]
    return(x)
}

#===========================================================================
#
# Représentation graphique des variations hebdomadaires
#
#===========================================================================
#' @title Variation du nombre de RPU par semaine
#' @description Variation du nombre de RPU par semaine
#' @usage barplot.week.variations()
#' @param x vecteur du nombre de RPU pr semaine (voir week.rpu)
#' @param coltitre bool, si TRUE la valeur de la barre est inscrite au dessus ou en dessous
#' @param colmoins couleur des barres négatives. Red par défaut
#' @param colplus couleur des barres positives. Blue par défaut
#' @param xlab nom pour l'axe des X. 'Semaines' par défaut
#' @param cex.names échelle pour le titre des barres (n° de la semaine). 0.8 par défaut
#' @param cex.col échelle pour les valeurs des colonnes. Utile que si coltitre = TRUE. Défaut 0.8
#' @param dx écart entre le sommet de la barre et l'affichage de sa valeur. Utile que si coltitre = TRUE. Défaut 3.
#' @param ... autres paramètres pour boxplot
#' @return le vecteur des abcisses des colonnes
#' @examples v <- week.variations(dx[dx$FINESS == "3Fr",])
#' barplot.week.variations(v[-length(v)], las = 2, main = "test", ylim = c(min(v[-length(v)])-10, max(v[-length(v)])+10), 
#' ylab = "Variations hebdomadaires")
#' 
#' ###
#' v <- week.variations(week.rpu(dx[dx$FINESS == "Col",]))
#' barplot.week.variations(v[-length(v)], las = 2, main = "CH Colmar - 2015", 
#' ylim = c(min(v[-length(v)])-10, max(v[-length(v)])+10), ylab = "Variations hebdomadaires", dx = 5)
#' 
#' @export
barplot.week.variations <- function(x, coltitre = TRUE, colmoins = "red", colplus = "blue", xlab = "Semaines", 
                                    cex.names = 0.8, cex.col =  0.8, dx = 3, ...){
    # barplot sauf la dernière semaine qui est souvent incomplète
    b <- barplot(x, col = ifelse(x > 0, colplus, colmoins), names.arg = 1:length(x), cex.names = cex.names,  xlab = xlab, ...)
    if(coltitre == TRUE)
        text(b, ifelse(x > 0, x + dx,  x - dx), x, cex = cex.col)
}

#===============================================
#
# p.isna
#
#===============================================
# pour mesurer le pourcentage de non réponses
#
#' @title Pourcentage de NA
#' @description Pourcentage de NA dans un vecteur
#' @usage p.isna(x)
#' @param x un vecteur quelconque
#' @return un pourcentage
#' 
p.isna <- function(x){return(mean(is.na(x)))}

#===============================================
#
# n.isna
#
#===============================================
# pour mesurer le pourcentage de non réponses
#
#' @title Nombre de NA
#' @description Nombre de NA dans un vecteur
#' @usage n.isna(x)
#' @param x un vecteur quelconque
#' @return en entier
#' 
n.isna <- function(x){return(sum(is.na(x)))}

#===============================================
#
# df.duree.pas
#
#===============================================
#' @title Dataframe Durée de passage
#' @description fabrique à partir d'un dataframe de type RPU, un dataframe de type duree_passage comportant les colonnes suivantes:
#' date/heure d'ebtree, date/heure de sortie, durée de passage (en minutes par défaut), l'heure d'entrée (HMS), l'heure de sortie
#' @usage df.duree.pas(dx, unit = "mins", mintime = 0, maxtime = 3)
#' @param dx un dataframe de type RPU
#' @param unit unité de temps. Défaut = mins
#' @param mintime défaut = 0. Durée de passage minimale
#' @param maxtime défaut = 3 (72 heures). Durée de passage maximale
#' @return dataframe de type duree_passage
#' @examples df <- df.duree.pas(dx)
df.duree.pas <- function(dx, unit = "mins", mintime = 0, maxtime = 3){
    pas <- dx[, c("ENTREE", "SORTIE", "MODE_SORTIE", "ORIENTATION", "AGE")]
    
    # on ne conserve que les couples complets
    pas2 <- pas[complete.cases(pas[, c("ENTREE", "SORTIE")]),]
    
    # calcul de la rurée de passage
    e <- ymd_hms(pas2$ENTREE)
    s <- ymd_hms(pas2$SORTIE)
    pas2$duree <- as.numeric(difftime(s, e, units = unit))
    
    # on ne garde que les passages dont la durées > 0 et < ou = 72 heures
    pas3 <- pas2[pas2$duree > mintime & pas2$duree < maxtime * 24 * 60 + 1,]
    
    # mémorise les heures d'entrée et de sortie
    pas3$he <- horaire(pas3$ENTREE)
    pas3$hs <- horaire(pas3$SORTIE)
    
    return(pas3)
    
}

#===============================================
#
# is.present.at
#
#===============================================
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
#' 
is.present.at <- function(dp, heure = "15:00:00"){
    # présent à 15 heures
    limite <- hms(heure) # pour incrémenter d'une heure: hms("15:00:00") + as.period(dhours(1))
    np <- dp$he < limite & dp$hs > limite
    
    return(np)
}
