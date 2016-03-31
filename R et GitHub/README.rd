RPU 2013
========================================================

Transférer un projet existant vers gitHub
-----------------------------------------
Ceete procédure s'applique lorsqu'un projet RStudio existe déjà en local et que l'on souhaite l'utiliser pour créer un nouveau dépot sur gitHub.

1. créer un nouveau dépot sur gitHub:
  - cliquer sur la tête du chat (en haut et à gauche) pour se mettre sur la page principale
  - dans le cartouche "your repositories" cliquer sur le bouton vert **new repositories**
  - donner un nom au nouveau dépot et alider
  - sur la page qui apparait, ignorer le paragraphe qui commence par *create* et s'intéresser à celui commençant par **push an existing repository from the command line**
  - copier les 2 instructions *git remote add origin https://github.com/jcrb/RPU2013.git* et *git push -u origin master*

2. ouvrir une console et se déplacer sur le dossier qui sera transféré:
  - jcb@mint1:~$ cd ~/Documents/Resural/"Stat Resural"/RPU2013

3. coller les 2 instructions précédantes: 
  - jcb@mint1:~/Documents/Resural/Stat Resural/RPU2013$ git remote add origin https://github.com/jcrb/RPU2013.git
  - jcb@mint1:~/Documents/Resural/Stat Resural/RPU2013$ git push -u origin master
4. un warning apparait sigalant que github ne trouve pas le trousseau des clés de cryptage et donc demande un login et un mot de passe:
  - WARNING: gnome-keyring:: couldn't connect to: /home/jcb/.cache/keyring-ny7iNK/pkcs11: No such file or directory
  - Username for 'https://github.com': jcrb
  - Password for 'https://jcrb@github.com': 
5. puis le transfert se fait (long)
  - Counting objects: 65, done.
  - Delta compression using up to 4 threads.
  - Compressing objects: 100% (64/64), done.
  - Writing objects: 100% (65/65), 38.25 MiB | 99 KiB/s, done.
  - Total 65 (delta 14), reused 0 (delta 0)
  - To https://github.com/jcrb/RPU2013.git
  - * [new branch]      master -> master
  - Branch master set up to track remote branch master from origin.



