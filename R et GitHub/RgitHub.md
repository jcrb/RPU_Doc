R et Github
============

Comment faire un *push* vers GitHub ?
-------------------------------------
situation:  
On dispose d'un projet R avec controle de version par git. Le projet est a jour localement grace à *commit*.   
On veut *pusher* vers le dépot github mais le push de RStudio génère une erreur (cas de la version beta 0.98).  
Question: comment faire un push manuel ?

1. en mode console se positionner dans le dossier *.git* du projet

```{}
 cd ~/Documents/Resural/"Stat Resural"/RPU2013/.git
```

2. faire un push

```{}
 git push origin master
```

Le programme demande le code d'identification puis le mot de passe et c'est tout!

