(* Dans ce fichier, on testera les fonctions des differents fichiers *)
(* Si vous voulez voir les tests, allez a la ligne 456 *)

(* Fonctions de generation.ml *)
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


(* FINAL fonctions de generation.ml *)
(* _______________________________________________________________________*)



(* fonctions de affichage.ml *)

let general_error err =
  failwith (Printexc.to_string err)
;;

let affiche_2D_array array_2D = 
  Array.iter (fun arr -> 
    Array.iter (fun c -> Printf.printf "%c " c) arr; 
    Printf.printf "\n" ) array_2D; 
  Printf.printf "\n"
;;

let print_with_filename filename =
  try
    (* val print_in_channel : in_chanel -> unit *)
	let rec print_in_channel file_in =
		try
			Printf.printf "%s\n" (input_line file_in); 
		    (* lit la premiere ligne à chaque appel *)
		    print_in_channel file_in
		with
		  End_of_file -> 
		      close_in file_in
		  | err -> 
			  close_in file_in; 
			  general_error err
	in
	
    let file_in = open_in filename in
    print_in_channel file_in
  with
    err -> general_error err
;;

(* Affichage *)
let affichage grille n m = 
	(* Donne les coordonnées de START et END (aléatoirement) *)
	let startEnd n m =
	   let s = (Random.int n, Random.int m) in
	   let e = (Random.int n, Random.int m) in
	   if e <> s then (s, e) else 
		   let e1, e2 = e in
		   (s, ((e1+1) mod n, e2))
	in
	let s, e = startEnd n m in
	
	let affichage_stylee grille s e = 
	  (* Fonction pour afficher le mur d'en haut *)
		let rec mur_haut_aux bis = 
			if bis < m then begin
				Printf.printf "+-";
				mur_haut_aux (bis+1)
			end 
		in
		(* On affiche un + à la fin *)
		mur_haut_aux 0; Printf.printf "+\n";


	  Array.iteri (fun x ligne ->
		print_char '|'; (* Mur première ligne de tout à gauche *)
		Array.iteri (fun y case -> 
			Printf.printf (if s = (x, y) then "S" else if e = (x, y) then "E" else " ");
			Printf.printf (if (case land dir_valeur E) <> 0 then " " else "|" ) 
		) ligne;
		print_newline ();
	   print_char '+'; (* Mur deuxième ligne de tout à gauche *)
		Array.iteri (fun y case -> 
			Printf.printf (if (case land dir_valeur S) <> 0 then " +" else "-+")
		) ligne;
		print_newline ();
	  ) grille
    in
	affichage_stylee grille s e
	
(* FINAL fonctions de affichage.ml *)
(* _______________________________________________________________________*)


(* Fonctions de grid.ml*)
let read_file_listOfStrings file_in =
  let rec loop acc =
  try
    let line = input_line file_in in
    loop (line :: acc)
  with
    End_of_file -> 
      close_in file_in; 
      List.rev acc (* accumulé à l'envers *)
  in
  loop []
;;


let string_to_2D_array filename =
  try
    (* read_file_to_2D array: in_chanel -> char array array *)
    let read_file_to_2D_array file_in =
	  let rec loop acc =
		try
		  let line = input_line file_in in
		  let char_array = Array.init (String.length line) (String.get line) in
		  loop (char_array :: acc)
		with End_of_file ->
		  close_in file_in;
		  Array.of_list (List.rev acc)
	  in
	  loop []
	in
    let in_channel = open_in filename in
    let array_2D = read_file_to_2D_array in_channel in
    close_in in_channel;
    array_2D
  with
  | Sys_error msg -> Printf.eprintf "Erreur: %s\n" msg; exit 1
;;


let est_lab lab =
  
  let height = Array.length lab in
  if height = 0 then false (* Array vide *)
  else 
  let width = Array.length lab.(0) in

  (* Sous-fonction pour vérifier la longueur uniforme des lignes *)
  let check_line_lengths () =
    Array.for_all (fun line -> Array.length line = width) lab
  in

  (* Sous-sous-fonction pour vérifier les bords horizontales *)
  let est_bord_horizontal i j =
    let char = lab.(i).(j) in
    if j mod 2 = 0 then char = '+' 
    else char = '-'
  in

  (* Sous-sous-fonction pour vérifier les bords verticales *)
  let est_bord_vertical i j =
    let char = lab.(i).(j) in
    if i mod 2 = 0 then char = '+' 
    else char = '|'
  in

  (* Sous-fonction pour vérifier les bords *)
  let check_bords () =
    let rec check_ligne i j =
      if j >= width then true (* On a parcouru toutes les colonnes *)
      else if not (est_bord_horizontal i j) then false
      else check_ligne i (j + 1)
    in
    let rec check_colonne i j =
      if i >= height then true (* On a parcouru toutes les lignes *)
      else if not (est_bord_vertical i j) then false
      else check_colonne (i + 1) j
    in
    check_ligne 0 0 && check_ligne (height - 1) 0 && check_colonne 0 0 && check_colonne 0 (width - 1)
  in
  
 
  
  (* Sous-fonction pour vérifier s'il y a que un Start et que un End *)
  let check_es() = 
     let rec loop i j accS accE =
	    if i >= height then (accS, accE)
		else if j >= width then loop (i+1) 0 accS accE
		else 
		   match lab.(i).(j) with
		      | 'S' -> loop i (j+1) (accS+1) accE
			  | 'E' -> loop i (j+1) accS (accE+1)
			  | _ -> loop i (j+1) accS accE
	 in
     let cptS, cptE = loop 0 0 0 0 in
	 cptS = 1 && cptE = 1
  in
  
  (* Sous-fonction qui vérifie si il n'y a que des char: '|', '-', '+', 'S', 'E', ' ', '\n' *)
  let check_char() =
	  let rec loop i j =
		  if i >= height then true
		  else if j >= width then loop (i+1) 0
		  else
			  match lab.(i).(j) with
				  | '|' -> loop i (j+1)
				  | '-' -> loop i (j+1)
				  | '+' -> loop i (j+1)
				  | 'S' -> loop i (j+1)
				  | 'E' -> loop i (j+1)
				  | ' ' -> loop i (j+1)
				  | '\n' -> loop i (j+1)
				  | _ -> false
	  in
	  loop 0 0
  in
  
 (* Sous-fonction qui vérifie que le contenu de la grille soit correcte *)
  let check_contenu() = 
	  let rec loop i j =
		  if i >= height then true
		  else if j >= width then loop (i + 1) 0
		  else
			  let case = lab.(i).(j) in
			  if i mod 2 = 0 then (* lignes paires *)
				  match case with
					  | '|' -> false
					  | _ -> if j mod 2 = 0 then
							   match case with
							   | '+' -> loop i (j + 1)
							   | _ -> false
							 else match case with
							   | '+' -> false
							   | _ -> loop i (j+1)
		  	else (* lignes impaires *)
			  	if j mod 2 = 1 then (* colonnes impaires *)
				    match case with
					    | '-' -> false
					    | '+' -> false
					    | '|' -> false
					    | _ -> loop i (j + 1)
				  else 
					  match case with
					    | '-' -> false
					    | '+' -> false
					    | _ -> loop i (j + 1)
	    in
	    loop 0 0 
  in
    
  check_line_lengths() && check_bords() && check_es() && check_char() && check_contenu()
;;


(* FINAL fonctions de grid.ml*)
(* _______________________________________________________________________*)


(* Fonctions de solve.ml *)
let estLibreE grille x y =
  grille.(x).(y+1) = ' ' || grille.(x).(y+1) = 'E'
;;

let estLibreO grille x y = 
  grille.(x).(y-1) = ' ' || grille.(x).(y-1) = 'E'
;;

let estLibreN grille x y = 
  grille.(x-1).(y) = ' ' || grille.(x-1).(y) = 'E'
;;

let estLibreS grille x y = 
  grille.(x+1).(y) = ' ' || grille.(x+1).(y) = 'E'
;;

let coordsS grille = 
	let height = Array.length grille in
	let width = Array.length grille.(0) in
	let rec loop i j x y =
		if i >= height then (x,y)
		else if j >= width then loop (i+1) 0 x y
		else
			match grille.(i).(j) with
				| 'S' -> loop i (j+1) i j
				| _ -> loop i (j+1) x y
	in
	loop 0 0 (-1) (-1)
;;

let rec dfs_solution grille x y =
		match grille.(x).(y) with
		 'E' -> true (* la fin *)
		| ' ' | 'S' -> 
		grille.(x).(y) <- '.'; (* On marque *)

		(* Check les directions possibles à prendre *)
		(* S'il ne peut plus bouger il teste un autre chemin *)
		if estLibreN grille x y && dfs_solution grille (x-1) y then true
		else if estLibreS grille x y && dfs_solution grille (x+1) y then true
		else if estLibreE grille x y && dfs_solution grille x (y+1) then true
		else if estLibreO grille x y && dfs_solution grille x (y-1) then true

		else begin
		  (* Backtrack si pas de chemins trouvé *)
		  (* en reaffichant en blanc (espace) la case *)
		  grille.(x).(y) <- ' '; 
		  false
		end
	  | _ -> false (* en cas de mur ou case deja visite *) 


let dfs_solve grille =
	if est_lab grille then begin
		let startX, startY = coordsS grille in
		if dfs_solution grille startX startY then begin 
			grille.(startX).(startY) <- 'S';
			affiche_2D_array grille 
		end else
			failwith "Solution non trouvee"
	end else failwith "La grille n'est pas un labyrinthe"

(* Affichage couleurs *)
let reset = "\x1b[0m"
let colorWhite = "\x1b[87;97m"
let colorGrey = "\x1b[90;100m"
let colorRed = "\x1b[5;37;41m"
let colorGreen = "\x1b[32;42m"
let colorBlue = "\x1b[36;46m"

let color_char c =
  match c with
  | ' ' -> colorWhite
  | '-' | '|' | '+' -> colorGrey
  | 'S' | 'E' -> colorRed
  | '.' -> colorGreen
  | '#' -> colorBlue
  | _ -> ""
  
(* Affichage de couleur *)
let affiche_2D_array_couleurs array_2D =
  Array.iter (fun arr ->
      Array.iter (fun c ->
          Printf.printf "%s%c %s" (color_char c) c reset
        ) arr;
      Printf.printf "\n"
    ) array_2D;
  Printf.printf "\n"
;;


let rec dfs_solution_couleurs grille x y =
		match grille.(x).(y) with
		 'E' -> true (* la fin *)
		| ' ' | 'S' -> 
		grille.(x).(y) <- '.'; (* On marque *)

		(* Check les directions possibles à prendre *)
		(* S'il ne peut plus bouger il teste un autre chemin *)
		if estLibreN grille x y && dfs_solution_couleurs grille (x-1) y then true
		else if estLibreS grille x y && dfs_solution_couleurs grille (x+1) y then true
		else if estLibreE grille x y && dfs_solution_couleurs grille x (y+1) then true
		else if estLibreO grille x y && dfs_solution_couleurs grille x (y-1) then true

		else begin
		  (* Backtrack si pas de chemins trouvé *)
		  (* en reaffichant en blanc (espace) la case *)
		  grille.(x).(y) <- '#'; 
		  false
		end
	  | _ -> false (* en cas de mur ou case deja visite *) 



let dfs_solve_couleurs grille =
	if est_lab grille then begin
		let startX, startY = coordsS grille in
		if dfs_solution_couleurs grille startX startY then begin 
			grille.(startX).(startY) <- 'S';
			affiche_2D_array_couleurs grille 
		end else
			failwith "Solution non trouvee"
	end else failwith "La grille n'est pas un labyrinthe"

	
(* FINAL fonctions de solve.ml *)
(* _____________________________________________________________________________ *)



(* Fonction extra pour tests *) 

let affiche_2D_intarray array_2D = 
  Array.iter (fun arr -> 
    Array.iter (fun i -> Printf.printf "%d " i) arr; 
    Printf.printf "\n" ) array_2D; 
  Printf.printf "\n"
;;

(* ______________________________________________________________________________ *)

let() =
	
	(* On crée des char array array pour tester *)
	let grille1 = [|
		[| '+'; '-'; '+'; '-'; '+' |];
		[| '|'; 'E'; ' '; ' '; '|' |];
		[| '+'; '-'; '+'; ' '; '+' |];
		[| '|'; ' '; ' '; 'S'; '|' |];
		[| '+'; '-'; '+'; '-'; '+' |];
		|] 
	in
	
	let grille2 = [|
		[| '+'; '-'; '+'; '-'; '+' |];
		[| '|'; ' '; ' '; 'E'; '|' |];
		[| '+'; 'S'; '+'; '-'; '+' |];
		[| '|'; ' '; ' '; '-'; '|' |];
		[| '+'; '-'; '+'; '-'; '+' |];
		|] 
	in
	
	let grille3 = [|
		[| '+'; '-'; '+'; '-'; '+' |];
		[| '|'; ' '; ' '; 'E'; '|'; '|' |];
		[| '+'; 'S'; '+'; '-'; '+' |];
		[| '|'; ' '; ' '; '-'; '|' |];
		[| '+'; '-'; '+'; '-'; '+' |];
		|] 
	in
	
	let grille4 = [|
		[| '+'; '-'; '+'; '-'; '+' |];
		[| '|'; 'E'; ' '; ' '; '|' |];
		[| '+'; '-'; '+'; '-'; '+' |];
		[| '|'; ' '; ' '; ' '; '|' |];
		[| '+'; '-'; '+'; '-'; '+' |];
		|] 
	in
	
	let grille5 = [|
		[| '+'; '-'; '+'; '-'; '+' |];
		[| '|'; 'S'; ' '; ' '; '|' |];
		[| '+'; '-'; '+'; ' '; '+' |];
		[| '|'; 'a'; ' '; 'E'; '|' |];
		[| '+'; '-'; '+'; '-'; '+' |];
		|] 
	in
	
	let grille6 = [|
		[| '+'; '-'; '+'; '-'; '+' |];
		[| '|'; 'E'; ' '; ' '; ' ' |];
		[| '+'; '-'; '+'; '-'; '+' |];
		[| '|'; ' '; ' '; 'S'; '|' |];
		[| '+'; '-'; '+'; '-'; '+' |];
		|] 
	in
	
	let grille7 = [|
		[| '+'; '-'; '+'; '-'; '+' |];
		[| '|'; 'E'; ' '; ' '; '|' |];
		[| '+'; '-'; '+'; ' '; '+' |];
		[| '|'; ' '; ' '; ' '; '|' |];
		[| '+'; '-'; '+'; ' '; '+' |];
		[| '|'; 'S'; ' '; ' '; '|' |];
		[| '+'; '-'; '+'; '-'; '+' |];
		|] 
	in
	
	let grille8 = [|
		[| '+'; '-'; '+'; '-'; '+'; '-'; '+'; '-'; '+'; '-'; '+' |];
		[| '|'; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; '|' |];
		[| '+'; '-'; '+'; ' '; '+'; ' '; '+'; '-'; '+'; '-'; '+' |];
		[| '|'; ' '; ' '; ' '; '|'; ' '; ' '; ' '; ' '; ' '; '|' |];
		[| '+'; '-'; '+'; ' '; '+'; '-'; '+'; ' '; '+'; '-'; '+' |];
		[| '|'; ' '; ' '; ' '; '|'; 'S'; ' '; ' '; '|'; ' '; '|' |];
		[| '+'; '-'; '+'; ' '; '+'; '-'; '+'; '-'; '+'; '-'; '+' |];
		[| '|'; ' '; ' '; ' '; ' '; ' '; '|'; ' '; ' '; ' '; '|' |];
		[| '+'; ' '; '+'; '-'; '+'; ' '; '+'; '-'; '+'; '-'; '+' |];
		[| '|'; ' '; '|'; ' '; ' '; ' '; ' '; ' '; ' '; ' '; '|' |];
		[| '+'; '-'; '+'; ' '; '+'; '-'; '+'; '-'; '+'; '-'; '+' |];
		[| '|'; 'E'; ' '; ' '; ' '; ' '; '|'; ' '; ' '; ' '; '|' |];
		[| '+'; '-'; '+'; '-'; '+'; '-'; '+'; '-'; '+'; '-'; '+' |];
		|] 
	in
	Printf.printf "grille1:\n";
	affiche_2D_array grille1;
	Printf.printf "grille2:\n";
	affiche_2D_array grille2;
	Printf.printf "grille3:\n";
	affiche_2D_array grille3;
	Printf.printf "grille4:\n";
	affiche_2D_array grille4;
	Printf.printf "grille5:\n";
	affiche_2D_array grille5;
	Printf.printf "grille6:\n";
	affiche_2D_array grille6;
	Printf.printf "grille7:\n";
	affiche_2D_array grille7;
	Printf.printf "grille8:\n";
	affiche_2D_array grille8;
	Printf.printf "____________________________________________________________________________\n\n";
	
	Printf.printf "Tests de fonctions de grid.ml\n\n";
		
	Printf.printf "Tests de val est_lab : char array array -> bool\n";
	Printf.printf "est_lab grille1 doit afficher true\nElle affiche: %b\n" (est_lab grille1);
	Printf.printf "est_lab grille2 doit afficher false\nElle affiche: %b\n" (est_lab grille2);
	Printf.printf "est_lab grille3 doit afficher false\nElle affiche: %b\n" (est_lab grille3);
	Printf.printf "est_lab grille4 doit afficher false\nElle affiche: %b\n" (est_lab grille4);
	Printf.printf "est_lab grille5 doit afficher false\nElle affiche: %b\n" (est_lab grille5);
	Printf.printf "est_lab grille6 doit afficher false\nElle affiche: %b\n" (est_lab grille6);
	Printf.printf "est_lab grille7 doit afficher true\nElle affiche: %b\n\n" (est_lab grille7);
	Printf.printf "est_lab grille8 doit afficher true\nElle affiche: %b\n\n" (est_lab grille8);
	Printf.printf "____________________________________________________________________________\n\n";
	
    Printf.printf "Tests de fonctions de generation.ml\n\n";
	
	Printf.printf "Tests de val dir_valeur : dir -> int\n";
	Printf.printf "dir_valeur N doit renvoyer 1\nElle renvoie %d\n" (dir_valeur N);
	Printf.printf "dir_valeur S doit renvoyer 2\nElle renvoie %d\n" (dir_valeur S);
	Printf.printf "dir_valeur E doit renvoyer 4\nElle renvoie %d\n" (dir_valeur E);
	Printf.printf "dir_valeur O doit renvoyer 8\nElle renvoie %d\n\n" (dir_valeur O);
	
	Printf.printf "Tests de val val backstage_generation : int -> int -> int array array\n";
	Printf.printf "Int array array 2x2:\n";
	let gen2x2 = backstage_generation 2 2 in 
	affiche_2D_intarray gen2x2;
	Printf.printf "Ce int array array 2x2 crée la grille suivante:\n";
	affichage gen2x2 2 2;
	Printf.printf "Int array array 5x5:\n";
	let gen5x5 = backstage_generation 5 5 in 
	affiche_2D_intarray gen5x5;
	Printf.printf "Ce int array array 5x5 crée la grille suivante:\n";
	affichage gen5x5 5 5;
	Printf.printf "Int array array 8x8:\n";
	let gen8x8 = backstage_generation 8 8 in 
	affiche_2D_intarray gen8x8;
	Printf.printf "Ce int array array 8x8 crée la grille suivante:\n";
	affichage gen8x8 8 8;
	Printf.printf "____________________________________________________________________________\n\n";
	
	Printf.printf "Tests de fonctions de solve.ml\n\n";
	
	Printf.printf "Tests de val coordsS : char array array -> int * int\n";
	let x1, y1 = coordsS grille1 in
	let x2, y2 = coordsS grille2 in
	let x4, y4 = coordsS grille4 in
	let x7, y7 = coordsS grille7 in
	Printf.printf "coordsS grille1 doit afficher (3,3)\nElle affiche: (%d,%d)\n" x1 y1;
	Printf.printf "coordsS grille2 doit afficher (2,1)\nElle affiche: (%d,%d)\n" x2 y2;
	Printf.printf "coordsS grille4 doit afficher (-1,-1)\nElle affiche: (%d,%d)\n" x4 y4;
	Printf.printf "coordsS grille7 doit afficher (5,1)\nElle affiche: (%d,%d)\n\n" x7 y7;
	
	Printf.printf "Tests des fonctions val estLibreX : char array array -> int -> int -> bool\n";
	Printf.printf "estLibreE grille1 1 1 doit renvoyer true\nElle renvoie %b\n" (estLibreE grille1 1 1);
	Printf.printf "estLibreE grille1 1 4 doit renvoyer false\nElle renvoie %b\n" (estLibreE grille1 1 3);
	Printf.printf "estLibreO grille1 1 3 doit renvoyer true\nElle renvoie %b\n" (estLibreO grille1 1 3);
	Printf.printf "estLibreO grille1 3 1 doit renvoyer false\nElle renvoie %b\n" (estLibreO grille1 3 1);
	Printf.printf "estLibreN grille1 2 3 doit renvoyer true\nElle renvoie %b\n" (estLibreN grille1 2 3);
	Printf.printf "estLibreN grille1 3 1 doit renvoyer false\nElle renvoie %b\n" (estLibreN grille1 3 1);
	Printf.printf "estLibreS grille1 1 3 doit renvoyer true\nElle renvoie %b\n" (estLibreS grille1 1 3);
	Printf.printf "estLibreS grille1 3 1 doit renvoyer false\nElle renvoie %b\n\n" (estLibreS grille1 3 1);
	
	Printf.printf "Tests de val dfs_solve : char array array -> unit\n";
	Printf.printf "Solution grille1:\n";
	dfs_solve grille1;
	Printf.printf "Solution grille7:\n";
	dfs_solve grille7;
	Printf.printf "Solution grille8:\n";
	dfs_solve grille8;
	
	(* On réinitialise les grilles pour la solution couleur *)
	let grille1 = [|
		[| '+'; '-'; '+'; '-'; '+' |];
		[| '|'; 'E'; ' '; ' '; '|' |];
		[| '+'; '-'; '+'; ' '; '+' |];
		[| '|'; ' '; ' '; 'S'; '|' |];
		[| '+'; '-'; '+'; '-'; '+' |];
		|] 
	in
	
	let grille7 = [|
		[| '+'; '-'; '+'; '-'; '+' |];
		[| '|'; 'E'; ' '; ' '; '|' |];
		[| '+'; '-'; '+'; ' '; '+' |];
		[| '|'; ' '; ' '; ' '; '|' |];
		[| '+'; '-'; '+'; ' '; '+' |];
		[| '|'; 'S'; ' '; ' '; '|' |];
		[| '+'; '-'; '+'; '-'; '+' |];
		|] 
	in 
	
	let grille8 = [|
		[| '+'; '-'; '+'; '-'; '+'; '-'; '+'; '-'; '+'; '-'; '+' |];
		[| '|'; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; '|' |];
		[| '+'; '-'; '+'; ' '; '+'; ' '; '+'; '-'; '+'; '-'; '+' |];
		[| '|'; ' '; ' '; ' '; '|'; ' '; ' '; ' '; ' '; ' '; '|' |];
		[| '+'; '-'; '+'; ' '; '+'; '-'; '+'; ' '; '+'; '-'; '+' |];
		[| '|'; ' '; ' '; ' '; '|'; 'S'; ' '; ' '; '|'; ' '; '|' |];
		[| '+'; '-'; '+'; ' '; '+'; '-'; '+'; '-'; '+'; '-'; '+' |];
		[| '|'; ' '; ' '; ' '; ' '; ' '; '|'; ' '; ' '; ' '; '|' |];
		[| '+'; ' '; '+'; '-'; '+'; ' '; '+'; '-'; '+'; '-'; '+' |];
		[| '|'; ' '; '|'; ' '; ' '; ' '; ' '; ' '; ' '; ' '; '|' |];
		[| '+'; '-'; '+'; ' '; '+'; '-'; '+'; '-'; '+'; '-'; '+' |];
		[| '|'; 'E'; ' '; ' '; ' '; ' '; '|'; ' '; ' '; ' '; '|' |];
		[| '+'; '-'; '+'; '-'; '+'; '-'; '+'; '-'; '+'; '-'; '+' |];
		|] 
	in
	
	Printf.printf "Tests de val dfs_solve_couleurs : char array array -> unit\n";
	Printf.printf "Solution couleur grille1:\n";
	dfs_solve_couleurs grille1;
	Printf.printf "Solution couleurs grille7:\n";
	dfs_solve_couleurs grille7;
	Printf.printf "Solution couleurs grille8:\n";
	dfs_solve_couleurs grille8;


;;

