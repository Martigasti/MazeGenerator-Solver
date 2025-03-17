open Generation

type dir = N | S | E | O ;; (* pour faciliter les directions (Nord, Sud, ... *)

let general_error err =
  failwith (Printexc.to_string err)
;;

let reset = "\x1b[0m" ;;
let colorWhite = "\x1b[87;97m" ;;
let colorGrey = "\x1b[90;100m" ;;
let colorRed = "\x1b[5;37;41m" ;;
let colorGreen = "\x1b[32;42m" ;;
let colorBlue = "\x1b[36;46m" ;;

let color_char c =
  match c with
  | ' ' -> colorWhite
  | '-' | '|' | '+' -> colorGrey
  | 'S' | 'E' -> colorRed
  | '.' -> colorGreen
  | '#' -> colorBlue
  | _ -> ""
;;

let affiche_2D_array_couleurs array_2D =
	Printf.printf "\x1b[H";
  Array.iter (fun arr ->
      Array.iter (fun c ->
          Printf.printf "%s%c %s" (color_char c) c reset
        ) arr;
      Printf.printf "\n"
    ) array_2D;
  Printf.printf "\n"
;;

let affiche_2D_array array_2D = 
  Array.iter (fun arr -> 
    Array.iter (fun c -> Printf.printf "%c" c) arr; 
    Printf.printf "\n" ) array_2D; 
  Printf.printf "\n"
;;

let print_with_filename filename =
  try
	let rec print_in_channel file_in =
		try
			Printf.printf "%s\n" (input_line file_in); 
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