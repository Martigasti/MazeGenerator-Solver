type dir = N | S | E | O;; (* pour faciliter les directions (Nord, Sud, ... *)

let dir_valeur = function
  N -> 1 | S -> 2 | E -> 4 | O -> 8
;;

let dx = function 
  E -> 1 | O -> -1 | _-> 0
;;

let dy = function 
  N -> -1 | S -> 1 | _-> 0 
;;

let opposee = function 
  E -> O | O -> E | N -> S | S -> N
;;


let shuffle dirs =
  let directions = 
    Array.map (fun value -> (value, Random.int 3)
    ) dirs in 
    Array.sort (fun (_, sort1) (_, sort2) -> compare sort1 sort2
    ) directions ;

    Array.map ( fun (value, _) -> value
    ) directions
  ;;


(* Algo de Backstage pour creer le laby.
* La fonction utilise le backtracking pour creer le laby, 
* mais egalement les operateurs binaires pour l'affichage  *)
let backstage_generation hauteur largeur = 

  let grille = Array.make_matrix hauteur largeur 0 in 

	let rec backstage_generation_bis cx cy hauteur largeur grille =

	  (* On shuffle l'array qui contient les 4 coordonnées *)
	  (* De cette manière la generation depends de la seed entrée en argument*)

	  let directions = shuffle [|N; S; E; O|] in

	  Array.iter (fun direction ->
	    let nx = cx + dx direction in (* Pour savoir de quelle mur parle-t-on (en bas) *)
	    let ny = cy + dy direction in (* Pour savoir de quelle mur parle-t-on (à droite) *)
	    
	    if (ny >= 0) && (ny < hauteur) 
	    && (nx >= 0) && (nx < largeur) 
	    && grille.(ny).(nx) = 0 then begin

	      grille.(cy).(cx) <- grille.(cy).(cx) lor (dir_valeur direction);
	      grille.(ny).(nx) <- grille.(ny).(nx) lor (dir_valeur (opposee direction));

	      (* affichage grille ; (* Cette ligne servira pour à afficher la genreration de la grille pas à pas *) *)
	      backstage_generation_bis nx ny hauteur largeur grille
	    end
	  ) directions
	in

  backstage_generation_bis 0 0 hauteur largeur grille;
  grille
;;
