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
;;

let dfs_solve grille =
	if Grid.est_lab grille then begin
		let startX, startY = coordsS grille in
		if dfs_solution grille startX startY then begin 
			grille.(startX).(startY) <- 'S';
			Affichage.affiche_2D_array grille 
		end else
			failwith "Solution non trouvee"
	end else failwith "La grille n'est pas un labyrinthe"
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
;;

let dfs_solve_couleurs grille =
	if Grid.est_lab grille then begin
		let startX, startY = coordsS grille in
		if dfs_solution_couleurs grille startX startY then begin 
			grille.(startX).(startY) <- 'S';
			Affichage.affiche_2D_array_couleurs grille 
		end else
			failwith "Solution non trouvee"
	end else failwith "La grille n'est pas un labyrinthe"
;;

let dfs_solve_track grille =
	if Grid.est_lab grille then begin
		let startX, startY = coordsS grille in
			Printf.printf "\x1b[2J";

			let rec dfs_solve_track_aux grille x y =
				Affichage.affiche_2D_array_couleurs grille;
				Unix.sleepf(0.03);

				match grille.(x).(y) with
				 'E' -> true (* la fin *)
				| ' ' | 'S' -> 
				grille.(x).(y) <- '.'; (* On marque *)
				
				Affichage.affiche_2D_array_couleurs grille;
				Unix.sleepf(0.03);
				
				(* Check les directions possibles à prendre *)
				(* S'il ne peut plus bouger il teste un autre chemin *)
				if estLibreN grille x y && dfs_solve_track_aux grille (x-1) y then true
				else if estLibreS grille x y && dfs_solve_track_aux grille (x+1) y then true
				else if estLibreE grille x y && dfs_solve_track_aux grille x (y+1) then true
				else if estLibreO grille x y && dfs_solve_track_aux grille x (y-1) then true

				else begin
				  (* Backtrack si pas de chemins trouvé *)
				  (* en reaffichant en bleu la case *)
				  grille.(x).(y) <- '#'; 
				  Affichage.affiche_2D_array_couleurs grille;
					Unix.sleepf(0.02);
				  false
				end
			  | _ -> 
			  false (* en cas de mur ou case deja visite *) 
				in

		if dfs_solve_track_aux grille startX startY then begin 
			grille.(startX).(startY) <- 'S';
			Affichage.affiche_2D_array_couleurs grille 
		end else
			failwith "Solution non trouvee"
	end else failwith "La grille n'est pas un labyrinthe"
;;
