# Projet_OCaml_Decodeur
Projet final de l'unité d'enseignement de programmation fonctionnelle durant ma première année à l'ENSIIE. 

Note reçue pour le projet : 19.5/20

## Pour lancer le décodeur/encodeur

Le fichier projet.ml comprend toutes les fonctions demandées du projet et la dernière version de la fonction generate_program

Le fichier tests.ml comprend quelques tests sur les différents prototypes des fonctions generate_program. J'aurais aimé rajouter des tests unitaires à toutes mes fonctions, mais je n'ai pas pu par manque de temps...

Le dossier exemple_prog contient des fichiers en .prog pour tester le projet en phase 1 et 2.
Le dossier exemple_msg contient des fichiers en .msg (.txt) pour tester le projet en phase 3.

Le dossier continent aussi le rapport en pdf.

 - Pour compiler : 
```bash
ocamlc -o exec projet.ml
```
 - Pour tester le décodage (exemple) :
```bash
./exec 2 ./Exemples/exemples_prog/bordel.prog 
```
 - Pour tester l'encodage (exemple) : 
```bash
./exec 3 ./Exemples/exemples_msg/lalaoublablabla.msg 
```


# Contenu et détails du projet

L'objectif de ce projet était de créer un petit décodeur de "langage extraterrestre" en OCaml, et par la suite construire le procédé inverse.
Il était dit explicitement dans le sujet : "Afin de décoder cette communication, il est du ressort de la terre de fournir un ruban infini et de mettre au point une machine permettant d’exécuter les instructions en fonction de leur sémantique afin d’écrire le message sur le ruban."

L'objectif a été de construire cette "machine".

## Décodage de notre langage extraterrestre

Les messages à décoder étaient de la forme (par exemple) : 
```
#affiche "hello"
W(a);W(o);L;F(2,[W(l);L]);W(e);L;W(h);
```
Où :
- W(c) : "écrire" le caractère c.
- R : déplacement de la t^ete du ruban d'un caractère vers la droite.
- L : déplacement de la t^ete du ruban d'un caractère vers la droite.
- F(n,[li]) : répéter n fois la liste d'instuctions li.
- C(n) : applique un encodage de César de pas n au message déjà décrypté.
- D(a) : efface le caractère a dans le message déjà décrypté.
- I : inverse le contenu actuel du ruban et replace le curseur.

L'implémentation en OCaml de ce décodeur est brièvement détaillée directement dans le fichier .ml, et davantage développée dans le rapport pdf.

## Encodage de messages

Maintenant, l'objectif est de construire le procédé inverse. C'est, pour être honnête, la seule fonction "difficile" et intéressante du projet. La grande majorité du temps de travail de ce projet est passée dedans.

Le but est de transformer ceci :
```
lala ou blablabla
```
en :
```
F(2,[W(l);R;W(a);R;]);W( );R;W(o);R;W(u);R;W( );R;F(3,[W(b);R;W(l);R;W(a);R;]);
```

Le gros du travail est dans la fonction "generate_program". Son implémentation, ainsi que sa complexité, sont largement développées dans le rapport pdf.
