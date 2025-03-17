let largeur = 6 (* c'est juste pour un test. Après ca sera donné en argument *) 
let hauteur = 6;; (* idem *)
let seed = 42;; (* idem *)

Random.init seed;; (* initialisation par le seed *)

let grille = Array.make_matrix hauteur largeur 0;; (* pour creer la matrice vide *)

type dir = N | S | E | O;; (* pour faciliter les directions (Nord, Sud, ... *)

let dir_valeur = function
  N -> 1 | S -> 2 | E -> 4 | O -> 8
;;

let dx = function 
  E -> 1 | O -> -1 | N-> 0 | S-> 0
;;

let dy = function 
  E -> 0 | O -> 0 | N-> -1 | S-> 1
;;

let opposee = function 
  E -> O | O -> E | N-> S | S-> N
;;


(* Affichage *)
let affichage_stylee grille = 

  (* Fonction pour afficher le mur d'en haut *)
	let rec mur_haut_aux bis = 
		if bis < largeur then begin
			Printf.printf "+-";
			mur_haut_aux (bis+1)
		end 
	in
	(* On affiche un + à la fin *)
	mur_haut_aux 0; Printf.printf "+\n";



  Array.iteri (fun y ligne ->
    print_char '|'; (* Mur première ligne de tout à gauche *)
    Array.iteri (fun x case -> 
        Printf.printf (if (case land dir_valeur E) <> 0 then "  " else " |" ) ) ligne;
    print_newline ();
   print_char '+'; (* Mur deuxième ligne de tout à gauche *)
    Array.iteri (fun x case -> 
        Printf.printf (if (case land dir_valeur S) <> 0 then " +" else "-+")
    ) ligne;
	print_newline ();
  ) grille
;;

(* J'utilise l'algo de Backstage pour creer le laby.
* Tu peux regarder sur internet comment creer avec Backstage.
* C'est très simple à implementer, vu que j'ai déjà fait un algo pareil pour un councours (en python) *)
(* D'ailleurs on peut utiliser DFS pour la recherche de chemin ! Il existe mieux comme algo, mais ca tu le sais déjà ;) *)
let rec dfs_generation cx cy grille =

  (* On shuffle l'array *)
  let directions = Array.map (fun value -> (value, Random.int 3)) [|N; S; E; O|] in 
    Array.sort (fun (_, sort1) (_, sort2) -> compare sort1 sort2) directions;
  let directions = Array.map (fun (value, _) -> value) directions in



  Array.iter (fun direction ->
    let nx = cx + dx direction in (* Pour savoir de quelle mur parle-t-on (en bas) *)
    let ny = cy + dy direction in (* Pour savoir de quelle mur parle-t-on (à droite) *)
    
    if (ny >= 0) && (ny < hauteur) 
    && (nx >= 0) && (nx < largeur) 
    && grille.(ny).(nx) = 0 then begin

      grille.(cy).(cx) <- grille.(cy).(cx) lor (dir_valeur direction);
      grille.(ny).(nx) <- grille.(ny).(nx) lor (dir_valeur (opposee direction));

      (* affichage_stylee grille ; (* Cette ligne sert à afficher la grille à chaque pas *) *)
      dfs_generation nx ny grille
    end
  ) directions
;;

let affiche_2D_array array = 
   Array.iter (fun arr -> Array.iter (fun c -> Printf.printf "%d " c) arr; Printf.printf "\n" ) array; Printf.printf "\n"
;;
let () = dfs_generation 0 0 grille;
		 affiche_2D_array grille;
		 print_newline();
		 affichage_stylee grille;
