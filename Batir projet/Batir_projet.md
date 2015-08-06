Batir un projet
========================================================

Pour commencer un projet de recherche, créer un *dépot* pour ce projet dans gitHub. Un projet qui dérive de façon sustantielle d'un travail antérieur peuvent débuter comme un *clone* de ce travail. Une nouvelle direction à partir d'un projet existant peut comencer comme une branche de ce projet et être secondairement fusionné ou totalement scindé.

Chaque dépot est organisé comme un package R, avec les *répertoires* de base:
- R/ fournit les fonctions R utilisées pour les analyses
- src/ fpournit le code source en C des fonctions R lorsqu'elles sont trop lentes
- man/ contient la documentaion générée automatiquement par ROxygen pour les fonctions précédentes.
- data/ contient les fichiers de données. Le gestionnaire de version n'est pas l'idéal pour les fichiers volumineux. A la place un fichier README contient les metadata et les liens vers les données stockées sur un autre serveur.
- inst/examples/ contient les investigations sous la forme de fichiers *knitR* permettant de mélanger des commentaires et des séquences d'instruction. Les séquences trop longues sont transformées en fonctions stockées dans R/ où elles sont documentées.
- inst/doc les exemples matures sont stockés là.
- DESCRIPTION: fichier de type metadata donnant des inforations comme l'auteur ou les dépendances
- RE

Le paquet est installable directement à partir de gitHub.

Dynamic document with R and knitr
=================================

Chapitre 9 - Cross references (References croisees)
---------------------------------------------------
custom = personalise
knit = tricot
chunks = morceau, fragment (chunk of wood: un morceau de bois )

<< mon-theme, eval=FALSE >>=
library("ggplot2")
theme(legend.text = element_text(size = 12, angle = 45)) + theme(legend.position = "bottom")
@

On peut reutiliser theme comme un sous programme:
<<>>=
library("ggplot2")
qplot(carat, price, data = diamonds, color = cut) + "mon-theme"
@



