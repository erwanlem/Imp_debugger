# Débogueur pour le langage Imp

## Travaux effectués

Les fonctionnalités implémentées sont les suivantes:
- fonctionnement général du débogueur avec
    - **pas en avant** (`step`)
    - **retour arrière** (`step_back`)
    - **breakpoint**
    - **passage d'un bloc** (`step_over`)
    - **suivi mémoire** des tableaux
- améliorations du langage
    - **inférence de type**
- affichage console
    - option affichage standard
    - option affichage avec *ncurses*



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