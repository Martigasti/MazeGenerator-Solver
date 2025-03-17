type dir = N | S | E | O ;; (* pour faciliter les directions (Nord, Sud, ... *)
val reset: string ;;
val colorWhite: string ;;
val colorGrey: string ;;
val colorRed: string ;;
val colorGreen: string ;;
val colorBlue: string ;;


(* renvoie l'erreur @err *)
val general_error : exn -> unit ;;

(* Donne les caractères ANSI representant la couleur *)
val color_char: char -> string ;;

(* Affichage de char array array en couleur *)
val affiche_2D_array_couleurs : char array array -> unit ;;

(* Affiche un char array array *)
val affiche_2D_array : char array array -> unit ;;

(* Affiche un fichier avec son nom *)
val print_with_filename : string -> unit ;;

(* Affiche la grille "binaire" sous format souhaité en placant le START et le END *)
val affichage: int array array -> int -> int -> unit ;;