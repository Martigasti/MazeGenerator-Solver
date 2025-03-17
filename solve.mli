(* Renvoie les coords de S sur une grille *)
val coordsS : char array array -> int * int

(* Renvoie true si le Est est libre *)
val estLibreE : char array array -> int -> int -> bool

(* Renvoie true si le Ouest est libre *)
val estLibreO : char array array -> int -> int -> bool

(* Renvoie true si le Nord est libre *)
val estLibreN : char array array -> int -> int -> bool

(* Renvoie true si le Sud est libre *)
val estLibreS : char array array -> int -> int -> bool

(* algo utilsant le backtracking pour trouvé le chemin *)
(* il renvoie vrai si trouvé (il y a toujours une solution donc toujours vrai) *)
val dfs_solution : char array array -> int -> int -> bool

(* Meme fontion mais affiche x pour le mauvais chemain emprenté *)
val dfs_solution_couleurs : char array array -> int -> int -> bool

(* Resoud et affiche la solution du labyrinth *)
val dfs_solve : char array array -> unit

(* Meme fonction mais en couleurs *)
val dfs_solve_couleurs : char array array -> unit

(* Meme fonction mais en utilisant dfs_solution_track *)
val dfs_solve_track : char array array -> unit
