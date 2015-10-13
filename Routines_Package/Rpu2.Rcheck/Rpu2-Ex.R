pkgname <- "Rpu2"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "Rpu2-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('Rpu2')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("analyse_type_etablissement")
### * analyse_type_etablissement

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: analyse_type_etablissement
### Title: Analyse etablissement
### Aliases: analyse_type_etablissement

### ** Examples

# es non SAMU, siège de SMUR
         es <- dx[dx$FINESS %in% c("Wis","Hag","Sav","Sel","Col"),]
         analyse_type_etablissement(es)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("analyse_type_etablissement", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("completude.time")
### * completude.time

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: completude.time
### Title: Pour un etablissement donne, calcule le aux de completude par
###   mois, semaine, jours
### Aliases: completude.time

### ** Examples

load("~/Documents/Resural/Stat Resural/RPU_2014/rpu2015d0112_provisoire.Rda")
       # old
       sav <- d15[d15$FINESS == "Sav",] # Saverne 2015
       t3 <- ddply(sav, .(month(as.Date(sav$ENTREE))), completude) # completude par mois

       # new
       library(xts)
       t3 <- completude.time(d15, "Sav", "day")
       a <- seq(as.Date("2015-01-01"), length.out = nrow(t3), by = 1)
       x <- xts(t3, order.by = a)
       plot(x[, "DP"], main = "CH Saverne - DIAGNOSTIC PRINCIPAL", ylab = "% de complétude")

       # TODO: tableau de complétude par mois et par Finess:
       t3 <- ddply(dx, .(dx$FINESS, month(as.Date(dx$ENTREE))), completude)
       # Application: rpu2014/Analyse/Completude/Analyse_completude



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("completude.time", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("count.CIM10")
### * count.CIM10

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: count.CIM10
### Title: Combien de codes CIM10
### Aliases: count.CIM10

### ** Examples

count.CIM10(dx[dx$FINESS == "Col", "MOTIF"])



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("count.CIM10", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("datetime")
### * datetime

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: datetime
### Title: met une string date au format YYYY-MM-DD HH:MM:SS
### Aliases: datetime

### ** Examples

Transforme des rubriques ENTREE et SORTIE en objet datetime



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("datetime", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("evolution")
### * evolution

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: evolution
### Title: Evolution d'une annee sur l'autre
### Aliases: evolution

### ** Examples

evolution(n.rpu, n.rpu.2013)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("evolution", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("finess2territoires")
### * finess2territoires

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: finess2territoires
### Title: r<c3><a9>organiser les FINESS par territoires de sant<c3><a9>
### Aliases: finess2territoires

### ** Examples

dx$FINESS <- finess2territoires(dx)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("finess2territoires", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("format.n")
### * format.n

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: format.n
### Title: formate un nombre
### Aliases: format.n

### ** Examples

format.n(7890.14) -> "7 890,14"



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("format.n", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("horaire")
### * horaire

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: horaire
### Title: extrait l'heure d'une date AAAA-MM-DD HH:MM:SS
### Aliases: horaire

### ** Examples

e <- datetime(dx$ENTREE); he <- horaire(e)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("horaire", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("passage")
### * passage

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: passage
### Title: Horaires de passages
### Aliases: passage

### ** Examples

e <- datetime(dx$ENTREE); he <- horaire(e); nuit <- passage(he, "nuit")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("passage", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("passages2")
### * passages2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: passages2
### Title: Nombre de RPU sur une plage horaire donnee
### Aliases: passages2

### ** Examples

n.passages.nuit <- passages2(pop18$ENTREE, "nuit"); n.passages.nuit[1]; n.passages.nuit[2]



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("passages2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pdsa")
### * pdsa

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pdsa
### Title: Determine si on est en horaire de PDS.
### Aliases: pdsa

### ** Examples

x <- "2009-09-02 12:23:33"; weekdays(as.Date(x)); pds(x) # NPDS



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pdsa", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("print.summary.rpu")
### * print.summary.rpu

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: print.summary.rpu
### Title: Imprime un summary.rpu
### Aliases: print.summary.rpu

### ** Examples

x <- ummary.wday(es$ENTREE))
       print.summary.rpu(x, cnames = c("Jour","n"), caption = "Nombre de RPU par jour de semaine")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("print.summary.rpu", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("print.table.rpu")
### * print.table.rpu

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: print.table.rpu
### Title: Imprime une table avec xtable.
### Aliases: print.table.rpu

### ** Examples

print.table.rpu(t)
         print.table.rpu(t, "table de test")
         print.table.rpu(t, "table de test", "html")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("print.table.rpu", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("radar.completude")
### * radar.completude

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: radar.completude
### Title: dessine un graphe en etoile
### Aliases: radar.completude
### Keywords: diagramme spider, étoile

### ** Examples

radar.completude(completude(dx))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("radar.completude", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rpu.par.jour")
### * rpu.par.jour

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rpu.par.jour
### Title: Nombre de RPU par jour et par FINESS
### Aliases: rpu.par.jour

### ** Examples

rpu.par.jour(d04)
p2013 <- rpu.par.jour(j2013$ENTREE)
       plot(p2013$V2, type="l") # les RPU
       lines(p2013$V3, p2013$V4) # moyenne mobile



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rpu.par.jour", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rpu.par.mois")
### * rpu.par.mois

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rpu.par.mois
### Title: Nombre de RPU par mois
### Aliases: rpu.par.mois

### ** Examples

tc1 <- rpu.par.mois(d15, FALSE)
tc2 <- rpu.par.mois(d15, TRUE)
a <- rbind(tc1, tc2)
par(mar=c(5.1, 4.1, 8.1, 2), xpd=TRUE)
barplot(a, beside = TRUE, cex.names = 0.8)
legend("topleft", inset = c(0, -0.1), legend = c("Brut","Standardisé"), bty = "n", col = c("black","gray80"), pch = 15)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rpu.par.mois", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("rpu2xts")
### * rpu2xts

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rpu2xts
### Title: Transforme RPU eb XTS
### Aliases: rpu2xts

### ** Examples

ts <- rpu2xts(d0106p); plot(ts$total);lines(rollapply(ts$total, 7, mean), col="red")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rpu2xts", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary.age")
### * summary.age

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.age
### Title: Resume du vecteur des AGE
### Aliases: summary.age

### ** Examples

summary.dp(dx$AGE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary.age", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary.age.sexe")
### * summary.age.sexe

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.age.sexe
### Title: r<c3><a9>sum<c3><a9> des vecteurs AGE et SEXE
### Aliases: summary.age.sexe

### ** Examples

summary.age.sexe(dx)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary.age.sexe", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary.ccmu")
### * summary.ccmu

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.ccmu
### Title: Resume du vecteur vx des CCMU
### Aliases: summary.ccmu

### ** Examples

summary.ccmu(dx$GRAVITE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary.ccmu", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary.cp")
### * summary.cp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.cp
### Title: resume du vecteur CODE_POSTAL (cp)
### Aliases: summary.cp

### ** Examples

summary.cp(dx$CODE_POSTAL)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary.cp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary.dateheure")
### * summary.dateheure

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.dateheure
### Title: Resume du vecteur des ENTREE ou SORTIE
### Aliases: summary.dateheure

### ** Examples

summary.ccmu(dx$SORTIE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary.dateheure", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary.dp")
### * summary.dp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.dp
### Title: Resume du vecteur DP (diagnostic principal)
### Aliases: summary.dp

### ** Examples

summary.dp(dx$DP)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary.dp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary.entree")
### * summary.entree

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.entree
### Title: analyse du vecteur ENTREE ou SORTIE
### Aliases: summary.entree

### ** Examples

summary.entree(as.Date(pop75$ENTREE))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary.entree", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary.mode.sortie")
### * summary.mode.sortie

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.mode.sortie
### Title: Resume du vecteur vx des MODE_SORTIE
### Aliases: summary.mode.sortie

### ** Examples

summary.mode.sortie(dx$MODE_SORTIE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary.mode.sortie", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary.rpu")
### * summary.rpu

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.rpu
### Title: calcule le nombre de RPU par SU, territoire de sante et
###   departement.
### Aliases: summary.rpu

### ** Examples

s <- summary.rpu(d15); s[1]; s$debut; s$n



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary.rpu", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary.transport")
### * summary.transport

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.transport
### Title: analyse du vecteur TRANSPORT
### Aliases: summary.transport

### ** Examples

summary.transport(pop75$TRANSPORT)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary.transport", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary.wday")
### * summary.wday

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.wday
### Title: Nombre de RPU par jour de semaine
### Aliases: summary.wday

### ** Examples

summary.wday(dx$ENTREE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary.wday", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("synthese.completude")
### * synthese.completude

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: synthese.completude
### Title: Calcule le tableau des taux de completude de l'ensemble des
###   Finess.
### Aliases: synthese.completude

### ** Examples

synthese.completude(dx)
synthese.completude(dx[dx$FINESS == "Hag",]) pour un seul établissement



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("synthese.completude", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tab.completude")
### * tab.completude

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tab.completude
### Title: tableau de completude par jour
### Aliases: tab.completude

### ** Examples

hus <- d15[d15$FINESS == hus,]
      d1 <- as.Date("2015-01-01")
      d2 <- as.Date("2015-01-31")
      t <- tab.completude(hus, d1, d2)
      plot(t[,"DATE DE SORTIE"], type = "l", main = "Mode de sortie", ylab = "Taux de completude")
      t.zoo <- zoo(t) # nécessite la librairie zoo
      plot(xts(t.zoo$DP, order.by = as.Date(rownames(t.zoo))), las = 2,
             main = "Diagnostic principal", ylab = "Taux de completude", cex.axis = 0.8)
     boxplot(t, las = 2, cex.axis = 0.8, ylab = "% de completude", main = "Complétude RPU")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tab.completude", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tarru")
### * tarru

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tarru
### Title: Taux de Recours Regional aux Urgences
### Aliases: tarru

### ** Examples

pop.region <- pop.als.tot.2014 <- 1868773
          tarru(dx$CODE_POSTAL, pop.als.tot.2014)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tarru", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("teste.radar")
### * teste.radar

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: teste.radar
### Title: data pour cr<c3><a9>er automatiquement un radar RPU et faire des
###   test
### Aliases: teste.radar

### ** Examples

teste.radar()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("teste.radar", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
