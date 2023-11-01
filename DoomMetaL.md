# DoomMetaL

## Introduction générale
    Ce projet consiste à faire une version du jeu vidéo Doom (1993), ce jeu ainsi que Wolfenstein 3D (1992) sont les jeux qui utilisent des techniques de moteur 3D révolutionnaire a cette époque qui s'appellent, Raycasting pour Wolfenstein 3D, et Binary Space Partitioning pour Doom.

    Le Raycasting consiste a calculer le distance entre la position du joueur est tous les murs dans son champ de vision, il suffit de prendre le milieu de l'écran, et ensuite on envoi un rayon depuis la position du joueur et on regarde a quelle distance ce rayon touche un mur, 
    puis on peux afficher le mur dont la hauteur dépend de la distance calculée, plus le mur est loins, plus la hauteur sera petite, ce qui rendra une illusion de 3D.

    Binary Space Partition (BSP) est plus complexe mais plus rapide et efficace comparé au Raycasting, on commence par prendre notre terrain de jeu et a l'aide de tous les murs sur ce terrain on le divise en plusieurs sous-zones, en divisant récursivement le terrain en deux en formant en même temps un arbre binaire avec les deux cotés du terrain, cette étape est nommé "partitionnement" (partitioning), elle est faite que au lancement du jeu et non pas a chaque cycle d'affichage. 
    Maintenant a chaque affichage du terrain, on applique une recherche binaire pour trouver sur quel sommet de l'arbre se trouve le joueur, puis on itere parmis ces fils pour trouver les sous-zones les plus proches qu'on dessine en utilisant un peu de Raycasting.

    On souhaite faire ce projet en Ocaml

## Objectifs
    L'objectif principal initial est de réussir à faire le moteur 3D Raycasting qui est plus simple, et ensuite on va l'améliorer en Binary Space Partitioning 

## Testabilité
    Voici les points sur lesquelles on peut écrire des tests:
        - deplacement - on peux faire un robot à qui on dit d'avancer un peu et on test sur sa position finale est au bon endroit
        - colision - on dit à ce même robot d'avancer dans un mur et on teste si il depasse le mur ou pas 
        - tests sur les fonctions du calcul de distance des rayons, etc

## Calendrier
    - création du terrain de jeu : mi décembre 
    - création du Raycasting, avec affichage des rayons sur l'affichage 2D: fin decembre
    - affichage en 2D, vu vol d’oiseau : debut janvier
    - affichage 3D: mi janvier
    - amélioration de l'affichage avec les textures: fevrier - debut mars
    - amelioration du deplacement du joueur: fin mars-debut avril
    - ajout et affichage des ennemies : avril
    - HUD (barre de vie, arme, munition, etc...) - avril
    - ajout des portes ou autre contenu - fin avril - debut mai
    - IA des enemies : fin avril
    - ajout d'un menu principal avec selection des differents niveaux : mai
    - ajout des contrôles avec une mannette: mai
    

## Références

https://guy-grave.developpez.com/tutoriels/jeux/doom-wolfenstein-raycasting/

Exemple du Raycasting en video: https://youtu.be/igKRMz--BuM