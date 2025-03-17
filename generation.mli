type dir = N | S | E | O ;; (* pour faciliter les directions (Nord, Sud, ... *)

(* Renvoie la valeur reprsenté par la direction *)
val dir_valeur: dir -> int ;;

(* Renvoie la valeur de la direction sur l'axe horizontale *)
val dx: dir -> int ;;

(* Renvoie la valeur de la direction sur l'axe verticale *)
val dy: dir -> int ;;

(* Renvoie la direction opposee *)
val opposee: dir -> dir ;;

(* Renvoie l'array de directions melangé *)
val shuffle: dir array -> dir array ;;

(* Algo de Backstage pour creer le laby.
* La fonction utilise le backtracking pour creer le laby, 
* mais egalement les operateurs binaires pour l'affichage  *)
val backstage_generation : int -> int -> int array array ;;
