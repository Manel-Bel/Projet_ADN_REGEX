# Projet PF5

> ## _**Projet pour l'université de paris cité réalisé en binôme pour l'année 2023-2024**_

## Installation d'`opam`

Pour commencer, installez le gestionnaire de paquets [`opam`](https://opam.ocaml.org/) en suivant les instructions données [ici](https://opam.ocaml.org/doc/Install.html).

## Installation des paquets

Placez-vous dans le répertoire cloné.  
De là, exécutez les commandes suivantes, qui créent un switch `opam` local en y installant les paquets nécessaires :

```
opam update
opam switch create . 4.14.1 -y --deps-only
eval $(opam env) #to update the current shell environment
```

## Compilation

Pour compiler le projet, exécutez la commande `make`.

## Toplevel

Afin de vous-même tester et déboguer, vous pouvez utiliser le toplevel `utop` qui a été installé.  
Pour le lancer, exécutez la commande `make top`.

## Tests

Pour lancer tous les tests disponibles, exécutez `make test`.  
Pour tester seulement les fonctions de l'exercice _i_, exécutez `make test-i`.
