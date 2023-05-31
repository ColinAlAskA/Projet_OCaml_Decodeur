
type instruction = (* representation d'une instruction reçu *)
  | Left (* Deplacement du curseur vers la gauche *)
  | Right (* Deplacement du curseur vers la droite *)
  | Write of char (* ecriture du caractere sur le ruban *)
  | Repeat of (int * instruction list) (* Repeter n fois la liste d'instructions *)
  
type program = instruction list ;;(* Un programme est simplement une liste d'instruction *)

let string_to_char_list str =
  let len = String.length str in
  let rec aux i acc =
    if i < 0 then acc
    else aux (i-1) (str.[i] :: acc)
  in aux (len-1) [];;






(* Prototype naïf *)

let generate_program_naif msg =
  let rec aux = function
    | [] -> []
    | t::q -> Write(t)::Right:: aux q
  in aux msg;;

generate_program_naif (string_to_char_list "Bonjour");;

generate_program_naif (string_to_char_list "lalalala");;

generate_program_naif (string_to_char_list "lala ou blablabla");;

generate_program_naif (string_to_char_list "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
);;

generate_program_naif (string_to_char_list 
"Frere Jacques
Frere Jacques
Dormez vous?
Dormez vous?
Sonnez les matines
Sonnez les matines
Ding ding dong
Ding ding dong
");;

generate_program_naif (string_to_char_list 
"Il etait un petit homme
Pirouette, cacahuete
Il etait un petit homme
Qui avait une drole de maison
Qui avait une drole de maison
Sa maison est en carton
Pirouette, cacahuete
Sa maison est en carton
Les escaliers sont en papier
Les escaliers sont en papier
Si vous voulez y monter
Pirouette, cacahuete
Si vous voulez y monter
Vous vous casserez le bout du nez
Vous vous casserez le bout du nez
Le facteur y est monte
Pirouette, cacahuete
Le facteur y est monte
Il s'est casse le bout du nez
Il s'est casse le bout du nez
On lui a raccommode
Pirouette, cacahuete
On lui a raccommode
Avec du joli fil dore
Avec du joli fil dore
Le beau fil, il s'est casse
Pirouette, cacahuete
Le beau fil, il s'est casse
Le bout du nez s'est envole
Le bout du nez s'est envole
Un avion a reaction
Pirouette, cacahuete
Un avion a reaction
A rattrape le bout du nez
A rattrape le bout du nez
Mon histoire est terminee
Pirouette, cacahuete
Mon histoire est terminee
Messieurs, Mesdames, applaudissez
Messieurs, Mesdames, applaudissez
");;







(* Prototype 1 *)

let rec meme_mot_au_debut mot msg = 
  match (mot, msg) with
  | [], _ -> true
  | _, [] -> false
  | a::q1, b::q2 -> if a = b then meme_mot_au_debut q1 q2 else false ;;

let rec enleve_premiers_termes n l =
  match (n,l) with
  | 0, _ -> l
  | _, t::q -> enleve_premiers_termes (n-1) q
  | _, [] -> failwith "la liste est trop petite";;

let rec ecrire_mot = function
  | [] -> []
  | t::q -> Write(t)::Right:: ecrire_mot q ;;

let generate_program_1 message =
  let rec aux prog count mot msg =
    match msg with
    | [] -> 
			if count >= 2 then List.rev (Repeat(count, ecrire_mot mot)::
			Repeat(List.length mot,[Left]) :: prog)
			else List.rev prog
    | t :: q -> 
      if count = 0 then aux (Right::Write(t)::prog) 1 (mot @ [t]) q
      else if meme_mot_au_debut mot msg then
        let new_msg = enleve_premiers_termes (List.length mot) msg in
        aux prog (count + 1) mot new_msg
        else 
          if count >= 2 then 
            aux (Right::Write(t)::Repeat(count, ecrire_mot mot)::
            Repeat(List.length mot,[Left]) :: prog) 1 [t] q 
          else aux (Right::Write(t)::prog) 1 (mot @ [t]) q
  in
  aux [] 0 [] message ;;


generate_program_1 (string_to_char_list "Bonjour");;

generate_program_1 (string_to_char_list "lalalala");;

generate_program_1 (string_to_char_list "lala ou blablabla");;

generate_program_1 (string_to_char_list "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
);;

generate_program_1 (string_to_char_list 
"Frere Jacques
Frere Jacques
Dormez vous?
Dormez vous?
Sonnez les matines
Sonnez les matines
Ding ding dong
Ding ding dong
");;

generate_program_1 (string_to_char_list 
"Il etait un petit homme
Pirouette, cacahuete
Il etait un petit homme
Qui avait une drole de maison
Qui avait une drole de maison
Sa maison est en carton
Pirouette, cacahuete
Sa maison est en carton
Les escaliers sont en papier
Les escaliers sont en papier
Si vous voulez y monter
Pirouette, cacahuete
Si vous voulez y monter
Vous vous casserez le bout du nez
Vous vous casserez le bout du nez
Le facteur y est monte
Pirouette, cacahuete
Le facteur y est monte
Il s'est casse le bout du nez
Il s'est casse le bout du nez
On lui a raccommode
Pirouette, cacahuete
On lui a raccommode
Avec du joli fil dore
Avec du joli fil dore
Le beau fil, il s'est casse
Pirouette, cacahuete
Le beau fil, il s'est casse
Le bout du nez s'est envole
Le bout du nez s'est envole
Un avion a reaction
Pirouette, cacahuete
Un avion a reaction
A rattrape le bout du nez
A rattrape le bout du nez
Mon histoire est terminee
Pirouette, cacahuete
Mon histoire est terminee
Messieurs, Mesdames, applaudissez
Messieurs, Mesdames, applaudissez
");;





(* Prototype 2 *)

let rec prefixe n l =
	if n = 0 then []
	else
		match l with
		| [] -> []
		| t::q -> t :: prefixe (n-1) q ;;

let double_prefixe n l =
	let rec aux m l = 
			if m = 0 then ([], prefixe n l)
			else
				match l with
				| [] -> ([],[])
				| t::q -> let (l1,l2) = aux (m-1) q in (t::l1, l2)
	in
	if List.length l < 2*n then ([],[])
	else aux n l;;

let reviens_2_fois n l =
	let rec aux n =
		if n = 0 then []
		else (double_prefixe n l) :: aux (n-1)
	in
	let f (x, y) acc =
		if x <> [] && y <> [] && x = y then x :: acc
		else acc
	in List.fold_right f (aux n) [];;

let ll = reviens_2_fois 10 ['l';'a';'l';'a';'o';'l';'a';'l';'a';'o'] ;;
let aaa = reviens_2_fois 200 (string_to_char_list "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
") ;;


let tous_egaux ll =
	let l = List.concat ll in
	match l with
		|[] -> true
		|t::q -> List.fold_left (fun b x -> b && x = t) true q ;;

tous_egaux ll;;
tous_egaux aaa;;

let rec enleve_premiers_termes n l =
	if n = 0 then l
	else
		match l with
		| [] -> failwith "la liste est trop petite, ce cas n'est pas sense arriver"
		| t::q -> enleve_premiers_termes (n-1) q ;;

let nombre_repetitions mot n_mot msg =
	let rec aux msg =
		let pref = prefixe n_mot msg in
		if pref <> mot then 0
		else 1 + aux (enleve_premiers_termes n_mot msg)
	in aux msg;;

let generate_program_2 message =
  let n_max = (List.length message) /2 in
  let rec aux prog msg =
    match msg with
    | [] -> List.rev prog
    | c::q -> 
      let mot_repeat = reviens_2_fois n_max msg in
      match mot_repeat with
      | [] -> aux (Right::Write(c)::prog) q
      | mot::q2 ->
				let n_mot = List.length mot in
				let nb_rep = nombre_repetitions mot n_mot msg in
				let new_msg = enleve_premiers_termes (nb_rep*n_mot) msg in
				aux (Repeat(nb_rep, aux [] mot) :: prog) new_msg
  in
  aux [] message;;

generate_program_2 (string_to_char_list "Bonjour");;

generate_program_2 (string_to_char_list "lalalala");;

generate_program_2 (string_to_char_list "lala ou blablabla");;

generate_program_2 (string_to_char_list "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
);;

generate_program_2 (string_to_char_list 
"Frere Jacques
Frere Jacques
Dormez vous?
Dormez vous?
Sonnez les matines
Sonnez les matines
Ding ding dong
Ding ding dong
");;

generate_program_2 (string_to_char_list 
"Il etait un petit homme
Pirouette, cacahuete
Il etait un petit homme
Qui avait une drole de maison
Qui avait une drole de maison
Sa maison est en carton
Pirouette, cacahuete
Sa maison est en carton
Les escaliers sont en papier
Les escaliers sont en papier
Si vous voulez y monter
Pirouette, cacahuete
Si vous voulez y monter
Vous vous casserez le bout du nez
Vous vous casserez le bout du nez
Le facteur y est monte
Pirouette, cacahuete
Le facteur y est monte
Il s'est casse le bout du nez
Il s'est casse le bout du nez
On lui a raccommode
Pirouette, cacahuete
On lui a raccommode
Avec du joli fil dore
Avec du joli fil dore
Le beau fil, il s'est casse
Pirouette, cacahuete
Le beau fil, il s'est casse
Le bout du nez s'est envole
Le bout du nez s'est envole
Un avion a reaction
Pirouette, cacahuete
Un avion a reaction
A rattrape le bout du nez
A rattrape le bout du nez
Mon histoire est terminee
Pirouette, cacahuete
Mon histoire est terminee
Messieurs, Mesdames, applaudissez
Messieurs, Mesdames, applaudissez
");;






(* Prototype final *)

let generate_program message =
  let n_max = (List.length message) /2 in
  let rec aux prog msg =
    match msg with
    | [] -> List.rev prog
    | c::q -> 
      let mot_repeat = reviens_2_fois n_max msg in
      match mot_repeat with
      | [] -> aux (Right::Write(c)::prog) q
      | mot::q2 ->
				if tous_egaux mot_repeat then
					let lettre = List.hd mot in
					let nb_rep = nombre_repetitions [lettre] 1 msg in
					let new_msg = enleve_premiers_termes nb_rep msg in
					aux (Repeat(nb_rep, aux [] [lettre]) :: prog) new_msg
				else
					let n_mot = List.length mot in
					let nb_rep = nombre_repetitions mot n_mot msg in
					let new_msg = enleve_premiers_termes (nb_rep*n_mot) msg in
					aux (Repeat(nb_rep, aux [] mot) :: prog) new_msg
  in
  aux [] message;;

generate_program (string_to_char_list "Bonjour");;

generate_program (string_to_char_list "lalalala");;

generate_program (string_to_char_list "lala ou blablabla");;

generate_program (string_to_char_list "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
);;

generate_program (string_to_char_list 
"Frere Jacques
Frere Jacques
Dormez vous?
Dormez vous?
Sonnez les matines
Sonnez les matines
Ding ding dong
Ding ding dong
");;

generate_program (string_to_char_list 
"Il etait un petit homme
Pirouette, cacahuete
Il etait un petit homme
Qui avait une drole de maison
Qui avait une drole de maison
Sa maison est en carton
Pirouette, cacahuete
Sa maison est en carton
Les escaliers sont en papier
Les escaliers sont en papier
Si vous voulez y monter
Pirouette, cacahuete
Si vous voulez y monter
Vous vous casserez le bout du nez
Vous vous casserez le bout du nez
Le facteur y est monte
Pirouette, cacahuete
Le facteur y est monte
Il s'est casse le bout du nez
Il s'est casse le bout du nez
On lui a raccommode
Pirouette, cacahuete
On lui a raccommode
Avec du joli fil dore
Avec du joli fil dore
Le beau fil, il s'est casse
Pirouette, cacahuete
Le beau fil, il s'est casse
Le bout du nez s'est envole
Le bout du nez s'est envole
Un avion a reaction
Pirouette, cacahuete
Un avion a reaction
A rattrape le bout du nez
A rattrape le bout du nez
Mon histoire est terminee
Pirouette, cacahuete
Mon histoire est terminee
Messieurs, Mesdames, applaudissez
Messieurs, Mesdames, applaudissez
");;









