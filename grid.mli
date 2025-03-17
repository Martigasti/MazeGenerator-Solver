(*
* @c_in: in_chanel -> fichier ouvert en lecture 
* @return : string list -> une liste de strings (cf. les lignes du @c_in)
*)
val read_file_listOfStrings : in_channel -> string list


(* Renvoie le nom du fichier de la forme d'un char array array *)
val string_to_2D_array: string -> char array array


(* Vérifie si un Array 2D est un labyrinthe *)
val est_lab : char array array -> bool
