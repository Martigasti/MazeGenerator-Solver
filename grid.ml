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
