Notes techniques
========================================================
Table des matières
------------------
1. notes techniques
2. cartographie
3. population


Notes techniques
================
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
#### Transformer une *table* en *dataframe*:

as.data.frame.matrix(ma_table)

#### Modifier l'entête de colonne d'un tableau
Losqu'un tableau n'a qu'une seule colonne, R l'intitule *c* (count) par défaut. Pour changer l'intitulé ajouter *dnn = "%"*:  
tcna<-round(prop.table(table(c,dnn="%"))*100,3)

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

```r
sessionInfo()
```

```
## R version 3.0.2 (2013-09-25)
## Platform: x86_64-pc-linux-gnu (64-bit)
## 
## locale:
##  [1] LC_CTYPE=fr_FR.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=fr_FR.UTF-8        LC_COLLATE=fr_FR.UTF-8    
##  [5] LC_MONETARY=fr_FR.UTF-8    LC_MESSAGES=fr_FR.UTF-8   
##  [7] LC_PAPER=fr_FR.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=fr_FR.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] knitr_1.5
## 
## loaded via a namespace (and not attached):
## [1] evaluate_0.5.1 formatR_0.10   stringr_0.6.2  tools_3.0.2
```

```r
toLatex(sessionInfo())
```

```
## \begin{itemize}\raggedright
##   \item R version 3.0.2 (2013-09-25), \verb|x86_64-pc-linux-gnu|
##   \item Locale: \verb|LC_CTYPE=fr_FR.UTF-8|, \verb|LC_NUMERIC=C|, \verb|LC_TIME=fr_FR.UTF-8|, \verb|LC_COLLATE=fr_FR.UTF-8|, \verb|LC_MONETARY=fr_FR.UTF-8|, \verb|LC_MESSAGES=fr_FR.UTF-8|, \verb|LC_PAPER=fr_FR.UTF-8|, \verb|LC_NAME=C|, \verb|LC_ADDRESS=C|, \verb|LC_TELEPHONE=C|, \verb|LC_MEASUREMENT=fr_FR.UTF-8|, \verb|LC_IDENTIFICATION=C|
##   \item Base packages: base, datasets, graphics, grDevices,
##     methods, stats, utils
##   \item Other packages: knitr~1.5
##   \item Loaded via a namespace (and not attached): evaluate~0.5.1,
##     formatR~0.10, stringr~0.6.2, tools~3.0.2
## \end{itemize}
```

Voir aussi les aides suivantes:
- person
- citation
- bibentry
- citEntry
- toBibtex(object, ...)
- toLatex(object, ...)

#### Copyright

```r
mtext("© RESURAL 2013", cex = 0.6, side = 4, line = -1, adj = 0.1)
```

```
## Error: plot.new has not been called yet
```

####Informations de session
sessionInfo()
toLatex(sessionInfo(), locale = FALSE)

#### xtable
Crée des tables en latex:  
<<label=tab2,echo=FALSE,results='asis'>>=  
Attention pour results mettre *asis* (et non pas *tex* qui correspond à sweawe)  

xtable(c, caption = "Structures hospitalières participantes en 2013", label = "tab1",align="|l|r|r|l|r|",digits = c(0,0,2,0,0), table.placement = "ht",caption.placement = "top")  

table \ref{tab1}, page \pageref{tab1}.

*xtable* génère une erreur si l'intitulé d'une colonne est *<na>*. Dans ce cas il faut transfoermer les <na> en "NA" et refaire le tableau (voir sortie.rnw par exemple).


#### Pour présenter en latex un tableau type *summary*:
stargazer(as.data.frame(a2[1:length(a)]),summary=TRUE,digit.separator=" ",digits=2,median=TRUE,iqr=TRUE)

#### Séparateur de milliers:
\np{x} génère une erreur si x est en notation scientifique

#### Se débarasser des NA
Procéder en 2 temsps:
- créer un vecteur de *logical* avec *!is.na*
- extraire les valeurs qui ne sont pas de NA à l'aide du vecteur précédent
exemple:

```r
vector <- c(1, NA, 2)
vector
```

```
## [1]  1 NA  2
```

```r
select <- !is.na(vector)
vector[select]
```

```
## [1] 1 2
```

```r
mean(vector)
```

```
## [1] NA
```

```r
mean(vector[select])
```

```
## [1] 1.5
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
- a<-tab1(factor(d1$MODE_ENTREE),main="Origine des patients (en %)",ylab="Pourcentage",bar.values = "percent")
- est un objet composé d'un graphique et d'un tableau
- l'intitulé est accessible par a$first.line<-"Origine des patients"
- le tableau est accessible via a$output.table. Permet d'utiliser xtable(a$output.table).
- valeurs individuelles: a$output.table[1,1]
- noms des lignes: row.names(a$output.table)
- nom des colonnes: colnames(a$output.table)
- franciser le nom des colonnes: colnames(a$output.table)<-c("Fréquence","Pourcentage","Pourcentage cumul.")

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



