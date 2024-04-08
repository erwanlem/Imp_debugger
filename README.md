# Débogueur pour le langage Imp

## Travaux effectués

Les fonctionnalités implémentées sont les suivantes:
- Commandes du débogueur:
    - `next` : **pas en avant**
    - `undo` : **retour arrière**
    - `break n` **breakpoint** ligne n
    - `so` : **step over** (passage d'un bloc)
- **suivi des tableaux en mémoire**
- améliorations du langage
    - **inférence de type**
- affichage console
    - option affichage standard
    - option affichage avec *ncurses*

Le rapport du travail effectué est disponible dans le répertoire `report`.


## Utilisation

> Compilation :
>   dune build
>
> Exécution :
>   ./impcat.exe tests/min.imp
>
> Nettoyage :
>   dune clean



## Documentation:
**Ocaml Format**
https://hal.science/hal-01503081/file/format-unraveled.pdf

**Ocaml garbage collector**
https://gallium.inria.fr/~doligez/publications/cuoq-doligez-mlw-2008.pdf
