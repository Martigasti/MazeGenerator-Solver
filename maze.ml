(* --help *)
let display_help_message () : unit =
	Printf.printf "Options de Maze.exe:\n";

	Printf.printf ("\n\t\x1b[1mmaze.exe print <fichier.laby> :\x1b[22m 
    Lit le labyrinthe se trouvant dans le fichier <fichier.laby> .
    L’affiche dans la console\n")
  ;
  Printf.printf ("\n\t\x1b[1mmaze.exe print --color <fichier.laby> :\x1b[22m 
    Lit le labyrinthe se trouvant dans le fichier <fichier.laby>.
    L'affiche dans la console avec l'affichage amélioré\n");

	Printf.printf ("\n\t\x1b[1mmaze.exe solve <fichier.laby> : \x1b[22m
    Lit le labyrinthe se trouvant dans le fichier <fichier.laby>.
    L’affiche dans la console en mettant en évidence le chemin du départ vers l’arrivée\n")
  ;
  Printf.printf ("\n\t\x1b[1mmaze.exe solve --color <fichier.laby> : \x1b[22m
    Lit le labyrinthe se trouvant dans le fichier fichier.laby.
    L’affiche dans la console en mettant en évidence le chemin du départ vers l’arrivée en couleurs\n")
  ;
  Printf.printf ("\n\t\x1b[1mmaze.exe solve --track <fichier.laby> : \x1b[22m
    Lit le labyrinthe se trouvant dans le fichier fichier.laby.
    L’affiche dans la console à chaque pas de l'algorithme de solution.\n")
  ;
	Printf.printf ("\n\t\x1b[1mmaze.exe random <n> <m> [r] : \x1b[22m
    Génère un labyrinthe aléatoire de hauteur <n> et de largeur <m>.
    Utilise éventuellement l’entier positif [r] comme graine initiale 
    pour le générateur de nombres aléatoires.\n\n")
;;

(* ****************|  Ligne de commande  |******************* *)

match Array.to_list Sys.argv with
  [ _; "--help" ] -> 
    display_help_message()

  | [ _; "print"; file ] -> 
    Affichage.print_with_filename file

  (* Affichage amélioré du labyrinthe *)
  | [ _; "print"; "--color"; file ] -> 
    let lab = Grid.string_to_2D_array file in
    Affichage.affiche_2D_array_couleurs lab

  (* Solution normale*)
  | [ _; "solve"; file ] -> 
    let lab = Grid.string_to_2D_array file in
    Solve.dfs_solve lab
  
  (* Solution avec affichage amélioré *)
  | [ _; "solve"; "--color"; file ] -> 
    let lab = Grid.string_to_2D_array file in
    Solve.dfs_solve_couleurs lab
  
  | [ _; "solve"; "--track"; file ] -> 
    let lab = Grid.string_to_2D_array file in
    Solve.dfs_solve_track lab
  
  | [_; "random"; n; m] | [_; "random"; n; m; _] ->
    let n = int_of_string n in
    let m = int_of_string m in
    let r =
      match Array.length Sys.argv with
      | 5 -> int_of_string Sys.argv.(4)
      | _ -> int_of_float (Unix.time ())
    in
      Random.init r; (* initialisation par le seed *)
      let grille = Generation.backstage_generation n m 
      in Affichage.affichage grille n m

  | _ -> 
  failwith "ligne de commande invalide"
;;

