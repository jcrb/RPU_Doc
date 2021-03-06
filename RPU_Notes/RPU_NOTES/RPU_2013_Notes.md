# R - Notes techniques
JcB  
28/01/2015  
Notes techniques
========================================================

- Notes Github
- Notes Knitr
- Notes Pandoc
- Notes Latex
- Notes R et RStudio

Notes Github
============

git push origin master:

- git push: on pousse le dépot git
- origin: dans le dépot d'origine
- master: branche master

Si on travaille toujours sur le dépot master de la branche origin, un simple push suffit.

Notes [Knitr](http://rmarkdown.rstudio.com/)
============================================

Pour inclure des options de __Knitr__ dans un document RStudio:

```
# ```{r setup, include=FALSE} 
knitr::opts_chunk$set(echo = TRUE) # si one ne met pas library(knitr)
set_alias(w = "fig.width", h = "fig.height")
```
ou

``` 
# ```{r Declarations, echo=FALSE, include=FALSE}
library(knitr)
# set global chunk options
opts_chunk$set(echo = FALSE, cache=TRUE, warning=FALSE, tidy=FALSE, fig.width=8, fig.height=6)
```


Notes Pandoc
============

Transformer un fichier __md__ en __pdf__:
-----------------------------------------
FILE<-"EqulibreAB"
system(paste("pandoc -o ", FILE, ".pdf ", FILE, ".md", sep=""))


Notes Latex
===========

source: Arnaud Gazagnes, Latex pour le prof. de Maths

Ecriture d'un nombre et virgule
-------------------------------
#### La commande \\np (pp 58)

Package _numprint_

\\Sexpr{\\np{3,14}} permet d'écrire la valeur sans qu'apparaisse un blanc entre 3 et 14. 


#### La commande \\DecimalMathComma (pp 58)
Mettre cette expression dans le préambule. Ne fonctionne qu'avec l'option _francais_ du package _babel_.

#### Symbole %

Dans un commentaire comme dans xtable utiliser un double échappement:
```{}
\\%
```



R - RStudio
================

- Tools/project options/Sweave weave Rnw file using *choisir* **knitr**
- symbole plus ou moins: $\pm \Sexpr{x}$ (ne pas oublier les parenthèses)

Utilisation du nom de l'ordinateur pour définir un chemin d'accès:
------------------------------------------------------------------
```{}
if(as.character(Sys.info()["nodename"]) == "MacBook-Air-de-JCB.local")
  file.reg <- "~/Documents/FEDORU/Codes_regroupement_ORUMIP/Regroupements ORUMiP Thésaurus SFMU.csv" else
  file.reg <- "~/Documents/Resural/FEDORU/Codes_regroupement_ORUMIP/Regroupement_ORUMIP/Regroupements ORUMiP Thésaurus SFMU.csv"
reg <- read.csv(file.reg, skip = 1)
```

Référencer le travail
---------------------

> citation('lubridate')

To cite lubridate in publications use:

  Garrett Grolemund, Hadley Wickham (2011). Dates and Times Made Easy with lubridate. Journal of Statistical Software, 40(3),
  1-25. URL http://www.jstatsoft.org/v40/i03/.

A BibTeX entry for LaTeX users is

  @Article{,
    title = {Dates and Times Made Easy with {lubridate}},
    author = {Garrett Grolemund and Hadley Wickham},
    journal = {Journal of Statistical Software},
    year = {2011},
    volume = {40},
    number = {3},
    pages = {1--25},
    url = {http://www.jstatsoft.org/v40/i03/},
  }

> citation()

To cite R in publications use:

  R Core Team (2014). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna,
  Austria. URL http://www.R-project.org/.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {R: A Language and Environment for Statistical Computing},
    author = {{R Core Team}},
    organization = {R Foundation for Statistical Computing},
    address = {Vienna, Austria},
    year = {2014},
    url = {http://www.R-project.org/},
  }

We have invested a lot of time and effort in creating R, please cite it when using it for data analysis. See also
‘citation("pkgname")’ for citing R packages.

Librairies
----------
#### gdata
gdata est utile pour la méthode drop.levels() qui supprime les levels inutiles:
(ref: http://rwiki.sciviews.org/doku.phpéid=tips:data-manip:drop_unused_levels)

#### RCurl
Permet de lire des données via HTTP. Avant de l'installer il faut installer la librairie *libcurl4-openssl-dev*:  
sudo apt-get install libcurl4-openssl-dev  
ref: http://www.omegahat.org/RCurl/FAQ.html

#### Chunks
- pour supprimer les messages inutiles *{r message=FALSE}*
- pour supprimer les warnings: *{r warning=FALSE}*
- pour obtenir l'équivalent de Sepr: in the source code I have calculated 2 + 2 = 4. Ref: Getting Started with R Markdown, knitr, and Rstudio, http://jeromyanglim.blogspot.com.au/2012/05/getting-started-with-r-markdown-knitr.html


#### A NE FAIRE QU'UNE FOIS AU DEBUT DE LA SESSION:

- rm(list=ls(all=TRUE)) pour vider complètement la mémoire

- loadhistory(file = ".Rhistory")
- timestamp()
- A FAIRE SI ON CHANGE DE WORKING DIRECTORY
- sauvegarde: savehistory(file = ".Rhistory")

#### Légendes graphes:
- Pour que les légendes de l'axe des Y soient perpendiculaires a ce dernier, rajouter *las = 1*
- Pour que les légendes de l'axe des X soient perpendiculaires a ce dernier, rajouter *las = 2*
- Pour que les légendes soient perpendiculaires aux 2 axes, rajouter *las = 3*
- Par défaut *las = 0*

- pour ne pas encadrer les légendes: bty = "n" (bty = box type)

#### L'utilisation de la méthode *SweaveInput* 
- provoque un erreur si le fichier à inclure comporte des caractéres accenués (méme enregistrés en UTF8)
- lui préférer
<<child = 'mon_fichier.Rnw'>>=
@

Création d'un tableau avec *cbind* et une matrice (et xtable pour pdf): voir exhaustivité des données

### Tables

#### Tables trop larges

Les tables trop larges pour tenir sur une largeur de page, on la scinde en deux (pas trouvé d'autre solution). Exemple dans sau_rpu.Rnw.

#### Transformer une *table* en *dataframe*:

as.data.frame.matrix(ma_table)

#### Modifier l'entête de colonne d'un tableau

Losqu'un tableau n'a qu'une seule colonne, R l'intitule *c* (count) par défaut. Pour changer l'intitulé ajouter *dnn = "%"*:  
tcna<-round(prop.table(table(c, dnn="%"))*100,3)

#### Renomer les lignes et/ou les colonnes

- rownames(a) <- c("un", "deux")
- colnames(a) <- c("un", "deux")

#### tapply et xtable

Affichage avec formatage des chiffres:

```{}
t<-tapply(as.Date(d1$ENTREE),d1$FINESS,length)
t2 <- data.frame(as.character(row.names(t)), as.numeric(t))
names(t2) <- c("Etablissement", "RPU")
print(xtable(t2, caption=c("Passages par service d'urgence","Nombre de passages par service d'urgence"),label="fig:passage_su",align=c("r","c","r"), display = c("s","s","f"), digits = 0), format.args = list(big.mark = " ", decimal.mark = ","))
```
Formatage simple:
```{}
print(xtable(x),format.args = list(big.mark = " ", decimal.mark = ","))
```


On divise la colonne AGE en 3 groupes puis on calcule l'effectif de chaque groupe avec tapply. Pour obtenir un affichage latex avec xtable, il faut transformer le vecteur en dataframe. On obtient un affichage en colonne. Pour obtenir l'affichage en ligne, utiliser la transposée t(t).  

tranche_age<-cut(d1$AGE,breaks=c(-1,15,75,max(d1$AGE,na.rm=T)),labels=c("15 ans et moins","16 à 74 ans","75 ans et plus"))  
t <- tapply(d1$presence,tranche_age,mean,na.rm=TRUE)  
t  
xtable(as.data.frame(t))  
xtable(as.data.frame(t(t)), caption=c("Durée de passage (mn) en fonction de l'age","Durée de passage et age"), label="tab:age_dp") 

Note that latex.environments = "" avoids the default option of tables being centered, which in this example would lead
to the caption not being directly under the table. 
Most importantly, table.placement = NULL is required to ensure that the default table placement options [ht] being inserted after
\begin{margintable}.
This is necessary because the margintable environment does not allow placement options like [ht]
.
> library(xtable)
> x <- matrix(rnorm(6), ncol = 2)
> x.small <- xtable(x, label ='tabsmall', caption ='A margin table')
> print(x.small,floating.environment='margintable',latex.environments = "", table.placement = NULL)

#### xprop.table(x) [mes_fonctions.R]  

Affiche un vecteur de FACTOR sur deux lignes: n et %  

xprop.table(table(tranche_age))  
a
  15 ans et moins 16 à 74 ans 75 ans et plus  
n        74739.00   215694.00       49895.00  
%           21.96       63.38          14.66  

#### sélectionner des variables avec TRUE et FALSE

- v<-c("ajax","troie","voiture")
- x<-nchar(v)<6
- x
  [1]  TRUE  TRUE FALSE
- v
  [1] "ajax"    "troie"   "voiture"
- v[x]
  [1] "ajax"  "troie"
- v[!x]
  [1] "voiture"

#### Version:

```{}
sessionInfo()
toLatex(sessionInfo())
```
Voir aussi les aides suivantes:
- person
- citation
- bibentry
- citEntry
- toBibtex(object, ...)
- toLatex(object, ...)

#### Copyright

```{}
 mtext("© RESURAL 2013",cex=0.6,side=4,line=-1,adj=0.1)
```
####Informations de session

sessionInfo()
toLatex(sessionInfo(), locale = FALSE)

#### xtable

Crée des tables en latex:  
<<label=tab2,echo=FALSE,results='asis'>>=  
Attention pour results mettre *asis* (et non pas *tex* qui correspond à sweawe)  

xtable(c, caption = c("intitulé long", "intitulé court"), label = "tab1",align="|l|r|r|l|r|",digits = c(0,0,2,0,0), table.placement = "ht",caption.placement = "top")  

table \ref{tab1}, page \pageref{tab1}.

*xtable* génère une erreur si l'intitulé d'une colonne est *<na>*. Dans ce cas il faut transformer les <na> en "NA" et refaire le tableau (voir sortie.rnw par exemple).

Pour supprimer les commantaires commenant par %: print(xtable, comment = FALSE) [stackoverflow](http://stackoverflow.com/questions/24400308/how-to-remove-the-lines-in-xtable-table-output-by-knitr)


#### Pour présenter en latex un tableau type *summary*:

stargazer(as.data.frame(a2[1:length(a)]),summary=TRUE,digit.separator=" ",digits=2,median=TRUE,iqr=TRUE)

stargazer(as.data.frame(data$total),summary=TRUE,digit.separator=" ",digits=2, median=TRUE, iqr=TRUE, title ="Totalité des passages: résumé des principales caractéristiques")

#### Séparateur de milliers:

\np{x} génère une erreur si x est en notation scientifique

#### Se débarasser des NA

Procéder en 2 temsps:
- créer un vecteur de *logical* avec *!is.na*
- extraire les valeurs qui ne sont pas de NA à l'aide du vecteur précédent
exemple:
```{}
vector<-c(1,NA,2)
vector
select<-!is.na(vector)
vector[select]
mean(vector)
mean(vector[select])
```
#### couleur

The argument col will set the colours, you could use this in conjunction with an ifelse statement  col<-
plot(x,y,xlab="PC1",ylab="PC2", col = ifelse(x < 0,'red','green'), pch = 19 )

#### Dates

Transformer une date *R* en date *française*:  
date<-"2013-02-05"
\Sexpr{format(date,format="%d %B %Y")}

#### tab1

Nécessite le package **epicalc**
- a<-tab1(factor(d1$MODE_ENTREE),sort.group = "decreasing", main="Origine des patients (en %)",ylab="Pourcentage",bar.values = "percent")

- est un objet composé d'un graphique et d'un tableau
- l'intitulé est accessible par a$first.line<-"Origine des patients"
- le tableau est accessible via a$output.table. Permet d'utiliser xtable(a$output.table).
- valeurs individuelles: a$output.table[1,1]
- noms des lignes: row.names(a$output.table)
- nom des colonnes: colnames(a$output.table)
- franciser le nom des colonnes: colnames(a$output.table)<-c("Fréquence","Pourcentage","Pourcentage cumul.")
- sort.group = "decreasing", affiche les colonnes par ordre décroissant.

Pour imprimer avec xtable, utiliser tab1$output.table :

```{}
a <- tab1(heure.AVC, missing=FALSE,main="Heures d'admission des AVC", ylab="Fréquence")
colnames(a$output.table)<-c("Fréquence","Pourcentage","Pourcentage cumul.")
print(xtable(a$output.table,  caption=c(paste0("Horaires de passages des AVC en ", anc, "."),"Horaire de passage des AVC"),label="fig:passage_avc"), format.args = list(big.mark = " ", decimal.mark = ","))
```

#### Supprimer la notation scientifique

Par défaut R transforme tout nombre supérieur à 10^5 en notation scientifique. Voir **?option** puis *scipen* pour explication (et http://yihui.name/knitr/demo/output/).
ex: options(scipen = 6, digits = 2) Accepté par knitr et Sweave. A mettre en début de programme.

#### Mettre un espace comme séparateur de milliers
A mettre en début de document. Source: http://stackoverflow.com/questions/18965637/set-global-thousand-separator-on-knitr
knit_hooks$set(inline = function(x) {
  prettyNum(x, big.mark=" ")
})

#### tapply et xtable

```{}
t <- round(tapply(d1$presence,d1$GRAVITE,mean,na.rm=TRUE),2)
p <- round(prop.table(t)*100,2)
r <- rbind(t,p)
rownames(r) <- c("mn","%")
xtable(r, caption=c("Titre long","Titre court"), label = "duree_gravite")
ou
x <- xtable(r, caption=c("Durée de présence et gravité","Durée de présence et gravité"), label = "duree_gravite")
print.xtable {x} permet d'utiliser d'autres arguments notamment longtable.

```

#### afficher un tableau en Latex
\begin{figure}[ht!]
 \centering
  <<duree_gravite2,echo=FALSE,fig.width=8>>=
  boxplot(d1$presence ~ d1$GRAVITE,,outline=F,main="Durée de présence selon la gravité",ylab="durée de présence",xlab="Gravité en unités CCMU")
  @
 \caption{Durée de passage en fonction de l'âge}
 \label{toucan}
\end{figure}

La figure~\ref{toucan} (pp.\pageref{toucan}) montre une photographie de toucan. Le tilde (\~) dans l'exemple ci-dessus est un symbole spécial en Latex. Il représente un espace insécable. Il est utile ici parce qu'il garde « figure » et le numéro quel qu'il soit auquel \\ref se rapporte comme un tout, et ne les coupera pas sur une ligne ou une page lors de la production du document.

#### tracer une ligne joignant les sommets des barres du barplot

NOTE TECHNIQUE: tracer une ligne joignant les sommets des barres du barplot. On utilise lines avec les valeurs suivantes:
- x = abcisse des colonnes. Elles sont contenues dans l'objet barplot. On peut les recueillir eplicitement par la fonction *str* (str(x)).
- y = ordonnées des barres, récupérées avec la fonction *table* qui agglomère les données par mois
Voir aussi: http://www.ats.ucla.edu/stat/r/faq/barplotplus.htm

#### Renommer des levels prédéfini
Notamment pour franciser certains tableau:
```
RPU.jour.semaine <- wday(d1$ENTREE,label=T)
levels(RPU.jour.semaine) <- week.short

On réordonne les jours pour que la semaines commence le lundi. Source: http://www.r-bloggers.com/reorder-factor-levels-2/
RPU.jour.semaine  <- factor(RPU.jour.semaine, levels(RPU.jour.semaine)[c(2:7,1)])

table(RPU.jour.semaine)
```
wday retourne le nom du jour en anglais. _week.short_ contient la même liste en français. Par défaut le premier jour de la semaine est le dimanche = 1. En réordonnant les niveaux on le place en dernier.

#### notes sur les spatialPolygons
> str(poly)
Formal class 'SpatialPolygons' [package "sp"] with 4 slots
  ..@ polygons   :List of 1
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 1 10
  .. .. .. .. .. .. ..@ area   : num 1
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 0.5 0.5 1.5 1.5 0.5
9.5 10.5 10.5 9.5 9.5
  .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. ..$ : chr [1:5] "s1" "s1" "s1" "s1" ...
  .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 1 10
  .. .. .. ..@ ID       : chr "g1"
  .. .. .. ..@ area     : num 1
  ..@ plotOrder  : int 1
  ..@ bbox       : num [1:2, 1:2] 0.5 9.5 1.5 10.5
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:2] "r1" "r2"
  .. .. ..$ : chr [1:2] "min" "max"
  ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slots
  .. .. ..@ projargs: chr NA

 I'll explain, stepping through the structure breadth first, and backwards:

 @proj4string is the coordinate reference system slot
 @bbox is the bounding box slot
 @plotOrder is the order to plot the polygons
 @polygons is the list of Polygons objects.

 @polygons[[1]] is the first (and in this case, only) feature. It is
an object of class 'Polygons' (non-spatial, since there's no
@proj4string in this part of the structure).

 @polygons[[1]]@Polygons is the 'Polygons' slot of class 'Polygons',
and is a list of rings that make up the feature.

 @polygons[[1]]@Polygons[[1]] is an object of class 'Polygon'.

 @polygons[[1]]@Polygons[[1]]@coords is the coordinates of the Polygon:

> poly@polygons[[1]]@Polygons[[1]]@coords
     x    y
s1 0.5  9.5
s1 0.5 10.5
s1 1.5 10.5
s1 1.5  9.5
s1 0.5  9.5


Outils de présentation
======================
Sont également utilisables pour le site internet de Resural:
- Interactive presentation with slidify and googleVis: comment faire un diaporama en R en incorporant des graphiques interactifs présentés via googlevis et les incorporer dans son site http://feedproxy.google.com/~r/RBloggers/~3/4w4oN8fMOa4/?utm_source=feedburner&utm_medium=email

Analyse de la variance (one-way)
================================

Comparaisons de plusieurs moyennes. Le principe est de comparer une variable _qualitative_ par une variable _quantitatives_ possédant _plusieurs_ niveaux. Par exemple on veut comparer l'age moyen des patients consultants aux urgences (_variable qualitative_) selon le territoire de santé (_variable quantitative à 4 niveaux_).

```
tapply(d1$AGE,d1$secteur,mean, na.rm=TRUE)
tapply(d1$AGE,d1$secteur,sd, na.rm=TRUE)
tapply(d1$AGE,d1$secteur,median, na.rm=TRUE)
boxplot(d1$AGE ~ d1$secteur, ylab="Age", xlab="Territoitre de santé", main="Age des consultants selon le territoire de santé")
age <- d1$AGE
territoire <- d1$secteur
mod <- aov(age ~ territoire)
mod
summary(mod)
plotmeans(age ~ territoire, ylab="Age moyen",  p=0.9999999, xlab="Territoire de santé", main = "Age moyen selon le territoire de santé\n(avec intervalle de confiance à 99%)")
TukeyHSD(mod)
par(las=2)
par(mar=c(5,8,4,2))
plot(TukeyHSD(mod))

tuk <- glht(mod, linfct=mcp(territoire="Tukey"))
par(las=1)
plot(cld(tuk, levels=0.05), col="lightgray")

Impression de qualité:


```
L'intervalle de confiance a été augmenté à  p=0.9999999 pour qu'il soit visible. A p = 0.95 on ne les voit âs car inférieurs à 1 et des messages d'avertissement sont générés. La méthode __plotCI__ fait la même chose (voir notamment le premier exemple de cette méthode).
```
Explications:
- les 3 premières lignes explorent le pb: on calcule la moyenne, l'écart-type et le boxplot pour chaque secteur (territoire de santé)
- les 3 lignes suivantes constituent l'Anova proprement dite: anova <- aov(var.quantitative ~ var.qualitative)
- la ligne _plotmeans() trace un graphique avec les moyennes des groupes et l'écart-type. La taille des groupes étant très imprtante, les SD sont très petits.
- TukeyHSD calcule les différence par groupe pris 2 à 2 et dessine le graphe associé.

ref: R in action pp 225-230

Boxplot
=======
extrait de Uffler2014. Dessine une boxplot et rajoute les moyennes sous forme de points et de textes:
```{}
means <- tapply(motivation, CSP, mean, na.rm=TRUE)
means
boxplot(motivation ~ CSP, xlab="CSP", ylab="Score de motivation", main="Motivation selon la CSP", col="antiquewhite")
points(1:length(means), means, pch = 23, cex = 0.75, bg = "red")
text(1:length(means) , means + 10, labels = formatC(means, format = "f", digits = 1),pos = 1, cex = 0.9, col = "blue")
```

Pour supprimer la notation scientifique:
------------------------------------------
options(scipen = 6, digits = 2)


Pour imposer un péparateur de milliers:
-----------------------------------------
knit_hooks$set(inline = function(x) {
  prettyNum(x, big.mark=" ")
})

Barchart
--------
[Bar charts with percentage labels but counts on the y axis](http://socialdatablog.com/bar-charts-with-percentage-labels-but-counts-on-the-y-axis/)

library(ggplot2)
library(scales)
perbar=function(xx){
q=ggplot(data=data.frame(xx),aes(x=xx))+
geom_bar(aes(y = (..count..)),fill="orange")+
geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="bin",colour="darkgreen")
q
}
perbar(mtcars$cyl)
perbar(mtcars$wt)+coord_flip()

Histogramme de qualité
----------------------
```{}
heure.AVC <- hour(AVC$ENTREE)
hist(heure.AVC, breaks=24, xlab="Heure de la journée", main="Répartition des AVC dans la journée", ylab="Fréquence", col="cornflowerblue", border="white")
```


Tracer 2 histogrammes en miroir
-------------------------------
[source](http://www.r-bloggers.com/making-back-to-back-histograms/)
```{}
df = data.frame(x = rnorm(100), x2 = rnorm(100, mean=2))
h1 = hist(df$x, plot=FALSE)
h2 = hist(df$x2, plot=FALSE)
h2$counts = - h2$counts
hmax = max(h1$counts)
hmin = min(h2$counts)
X = c(h1$breaks, h2$breaks)
xmax = max(X)
xmin = min(X)
plot(h1, ylim=c(hmin, hmax), col="green", xlim=c(xmin, xmax))
lines(h2, col="blue")

Refusal of Care in the Prehospital Setting
```

Packages installés
------------------

source: http://www.ats.ucla.edu/stat/r/faq/packages.htm

Liste de tous les packages installés sur la machine:

```{}
a <- installed.packages()
a[1:5, 1]
```
Uniquement la liste des noms
```{}
a <- .packages(all.available = TRUE)
save(a, file = "my_RPackages.R")
rm(a)
load("my_RPackages.R")

```

Pour installer des packages:
```{}
install.packages(a)
```
Tous les packages disponibles:
```{}
a <- available.packages()
```

#### Franciser et réorganiser la semaine
---------------------------------------

La se
```{}
library("gdata")
w<-wday(AVC$ENTREE,label=TRUE)
levels(w) <- week.short
w <- reorder(w,new.order = french.short.week)
wt <- table(w, dnn = "Nombre d'AVC selon le jour de la semaine")
pwt <- round( prop.table(table(w, dnn = "Pourcentage d'AVC selon le jour de la semaine"))*100,2)
a <- rbind(wt, pwt)
rownames(a) <- c("fréquence", "p.cent")
xtable(a, caption = c("Distribution des AVC en fonction du jour de la semaine. La fréquence quotidienne théorique est de 14.28 p.cent d'AVC par jour. Les AvC sont plus fréquents en début de semainet plus rares en fin de semaine.","AVC selon le jour de la semaine"), label = "tab:avc_jour")
```

# radial.plot
=============

```{}
radial.plot(1-a,rp.type="p",radial.pos=NULL,labels= names(a),line.col=fadeBlue,poly.col=fadeBlue,radial.lim=c(0,1),main= paste0("Taux de complétude des RPU transmis en ",anc), mar=c(2,2,5,2))
```
Le paramètre __mar__ permet de faire de la place pour le titre et les labels.

Dataframe
=========

Réordonner les colonnes
-----------------------

Il suffit de recréer le DF en modifiant l'ordre des colonnes
```{}
df[,c(1,2,3,4)]

df2[,c(1,3,2,4)]

pour sauvegarder: write.csv(df2, file="somedf.csv")
```
[source](http://stackoverflow.com/questions/5620885/how-does-one-reorder-columns-in-r)

Remarques sur "format"
======================
```{}
a <- 123456.789
a
format(a)
# nombre de chiffres significatifs
format(a, digits = 20)
# nombre MINIMUM de chiffres après la virgule
format(a, nsmall = 3)
format(a, nsmall = 3, big.mark = ' ')
format(a, nsmall = 3, big.mark = ' ', decimal.mark = ",")

```

graphe avec 2 axes y pour les passages et le taux d'hospitalisation. 
====================
```{}
# premier graphique
plot(d.xts$passages, minor.ticks = FALSE, main = "")
# permet de dessiner un second graphique avec ses propres paramètres
par(new = T) 
# second graphique
plot(d.xts$taux, axes = F, ylim = c(0, 100),  col = "blue", main="")
axis(4, ylim = c(0, 100),  col = "blue" ) # utilise l'axe de droite. Prévoir plus de marge
```

Ajouter une légende à un graphique
----------------------------------
```{}
legend("topleft", legend = c("Passages","Taux d'hospitalisation"), col = c("black", "blue"), lty = 1, bty = "n")
```

