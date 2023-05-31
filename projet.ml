
(** 
    Cette partie du programme NE DOIT EN AUCUN CAS être modifiée sous peine
    de voir votre code ne pas passer les tests 
*)
type instruction = (* représentation d'une instruction reçu *)
  | Left (* Déplacement du curseur vers la gauche *)
  | Right (* Déplacement du curseur vers la droite *)
  | Write of char (* Écriture du caractère sur le ruban *)
  | Repeat of (int * instruction list) (* Repeter n fois la liste d'instructions *)
  | Caesar of int (* Caesar n à partir de la phase 3Aiens *)
  | Delete of char (* Delete(a) suppression du caractère a du message à partir de la phase 3Aiens *)
  | Invert (* Invert : retournement du message  à partir de la phase 3Aiens *)
  
type program = instruction list (* Un programme est simplement une liste d'instruction *)

(**
   VOUS N'AVEZ PAS BESOIN DE LIRE OU DE COMPRENDRE CETTE PARTIE DU CODE (LES SIX FONCTIONS SUIVANTES). 
   ELLE EST VOLONTAIREMENT NON COMMENTÉE ET RÉDIGÉE DE MANIÈRE PEU CLAIRE 
 *)
let rec analyse_comment fd =
  let c = Scanf.bscanf fd "%c" (fun c -> c) in
  if c = '\n'
  then ()
  else analyse_comment fd
             
let rec analyse_program_aux =
  fun fd lvl  ->
  try 
    let c = Scanf.bscanf fd "%c" (fun a  -> a) in
    if c = '#'
    then
      let _ = analyse_comment fd in
      analyse_program_aux fd lvl
    else if c = ';' || c = '\n' || c = ' '
    then analyse_program_aux fd lvl
    else if c = ']'
    then
      if lvl > 0
      then []
      else raise (Invalid_argument "Error on char ]")
    else if c = 'W'
      then
        let i= Write(Scanf.bscanf fd "(%c)" (fun a -> a)) in
        let li = analyse_program_aux fd lvl in
        i::li
    else if c = 'C'
      then
        let i= Caesar(Scanf.bscanf fd "(%d)" (fun a -> a)) in
        let li = analyse_program_aux fd lvl in
        i::li
    else if c = 'D'
    then
      let a = Scanf.bscanf fd "(%c)" (fun a -> a) in 
      let i= Delete(a) in
      let li = analyse_program_aux fd lvl in
      i::li
    else if c = 'R'
    then let li = analyse_program_aux fd lvl in
         Right::li
    else if c = 'I'
    then Invert::analyse_program_aux fd lvl
    else if c = 'L'
    then Left::analyse_program_aux fd lvl
    else if c = 'F'
    then
      let n = Scanf.bscanf fd "(%d,[" (fun n -> n) in
      let l = analyse_program_aux fd (lvl + 1) in
      let c = Scanf.bscanf fd "%c" (fun a -> a) in
      if c <> ')' then raise (Invalid_argument ("Error found '"^String.make 1 c^"' instead of ')'"))
      else
        let li = analyse_program_aux fd lvl in
        Repeat(n,l)::li
    else
      let _ = Format.printf  "ERROR '%c' (%d)@." c (Char.code c) in
      assert false
  with End_of_file -> []

let rec read_file_aux =
  fun acc fd ->
  try
    let c = Scanf.bscanf fd "%c" (fun x -> x) in
    read_file_aux (c::acc) fd
  with End_of_file -> acc

let read_file file =
  try
    if Sys.is_directory file
    then raise (Invalid_argument "is a directory")
    else
      let fd = Scanf.Scanning.open_in file in 
      List.rev (read_file_aux [] fd)
  with exc ->
    Format.fprintf Format.err_formatter "Problème à l'ouverture du fichier %s (exception %s)@." file (Printexc.to_string exc);
    exit 127

                    
let rec fprintf_instruction fmt i =
  match i with
  | Write c -> Format.fprintf fmt "W(%c)" c
  | Right -> Format.fprintf fmt "R"
  | Left -> Format.fprintf fmt "L"
  | Repeat(n,li) ->
     Format.fprintf fmt "F(%d,[%a])" n (fun fmt li -> List.iter (fun i -> Format.fprintf fmt "%a;" fprintf_instruction i) li) li
  | Caesar n -> Format.fprintf fmt "C(%d)" n
  | Delete(a) -> Format.fprintf fmt "D(%c)" a
  | Invert -> Format.fprintf fmt "I"
            
let fprintf_program fmt l =
  List.iter (fun i -> Format.fprintf fmt "%a;" fprintf_instruction i) l

(*** 
     Retour au côté clair
*)


(* 
   [print_program] : program -> unit 
   affiche un [program] sur la sortie standard de l'executable
 *)  
let print_program p = Format.printf "%a" fprintf_program p

(*
  [analyse_program] : unit -> program
  Lit un programme 1Aien ou 2Aien, l'analyse et le retourne sous forme d'une valeur de type [program]
 *)                    
let analyse_program file =
  try
    if Sys.is_directory file
    then raise (Invalid_argument "is a directory")
    else
      let fd = Scanf.Scanning.open_in file in 
      analyse_program_aux fd 0
  with exc ->
    Format.fprintf Format.err_formatter "Problème à l'ouverture du fichier %s (exception %s)@." file (Printexc.to_string exc);
    exit 127









    
(** Votre code doit commencer à partir de ce point.

    NE MODIFIER RIEN AU DESSUS DE CE POINT 
*)

(* Définition de notre type ruban *)
type ruban = {left: char list; right: char list;};;

(* type : ruban -> ruban = <fun> 
   @requires : Rien
   @ensures : déplacement de la tête du ruban d'un caractère vers la gauche
   @raises : Rien
   *)
let go_left r =
	match r.left with
	| [] -> {left = []; right = '\000'::r.right}  (* '\000' est le caractère nul en OCaml, cela évite des espaces indésirables *)
	| t::q -> {left = q; right = t::r.right};;


(* type : ruban -> ruban = <fun> 
   @requires : Rien
   @ensures : déplacement de la tête du ruban d'un caractère vers la droite
   @raises : Rien
   *)
let go_right r =
	match r.right with
	| [] -> {left = '\000'::r.left; right = []}
	| t::q -> {left = t::r.left; right = q};;


(* type : char -> ruban -> ruban = <fun> 
   @requires : Le caractère c sera un caractère affichable de l'ASCII non étendu (en particulier, pas d'accent).
   @ensures : écriture du caractère c sur le ruban r sans déplacer la tête de lecture
   @raises : Rien
   *)
let write c r =
    match r.right with
    | [] -> {left = r.left; right = [c]}
    | t::q -> {left = r.left; right = c::q};;


(* type : ('a -> 'b -> 'a) -> int -> 'b list -> 'a -> 'a = <fun> 
   @requires : Rien
   @ensures : répète n fois la liste d’instructions l sur le ruban r
   @raises : Rien
   *)
let rec repeat f_execute n l r =
  let liste_execution l r =   (* Renvoie un nouveau ruban en effectuant la liste d'instructions l sur le ruban r *)
    let f_aux r instru = f_execute r instru in
    List.fold_left f_aux r l
  in
  if n>0 then
    let r = liste_execution l r in  (* l'application de la liste d'instructions l sur le ruban se fait ici *)
    repeat f_execute (n-1) l r  (* applique 1 seule fois la liste l au ruban r à chaque appel récursif *)
  else r ;;


(* type : (char -> char) -> ruban -> ruban = <fun>
   @requires : Rien
   @ensures : fait un List.map de la fonction f sur les deux listes du ruban r
   @raises : Rien
   *)
let map_ruban f r = {left = List.map f r.left; right = List.map f r.right};;

(* type : int -> ruban -> ruban = <fun> 
   @requires : Rien (n peut être positif comme négatif)
   @ensures : applique un encodage de Cesar de pas n au message deja decrypte dans le ruban r
   @raises : Rien
   *)
let caesar n r =
  (* La fonction auxiliaire renvoie le nouveau caractère correspondant au codage Cesar de pas n du caractère c *)
  let aux decalage c =  (* decalage est une entier qui permettra de se ramener entre -26 et 26 pour appliquer le codage Cesar *)
    let code = Char.code c in   (* on récupère le code ASCII de c (un entier) *)
    let i = (code - decalage + n) mod 26 in (* on applique le codage Cesar (donc modulo 26)*)
    let new_code = i + decalage in  (* on récupère le nouveau code ASCII du caractère c *)
    if i >= 0 then Char.chr new_code  (* Si i était positif, le new_code est bien le nouveau code ASCII, on récupère donc la lettre correspondant *)
    else Char.chr (new_code + 26) (* Si i était négatif, il faut rajouter 26 pour obtenir le bon code ASCII car, dans l'idée, -1 mod 26 = -1 (et non 25) *)
  in
  let applique_caractere c =
    if ('A'<= c && c <= 'Z') then aux 65 c  (* Si c est une majuscule, decalage = Char.code 'A' = 65 *)
    else if ('a' <= c && c <= 'z') then aux 97 c  (* Si c est une minuscule, decalage = Char.code 'a' = 97 *)
    else c
  in
  map_ruban applique_caractere r ;;   (* Avec notre fonction map_ruban, on applique ce codage césar à tous les caractères du ruban *)


(* type : char -> ruban -> ruban = <fun> 
   @requires : Rien
   @ensures : efface le caratere c dans le message deja decrypte dans le ruban r
   @raises : Rien
   *)
let delete c r =
  (* f_supp sera une fonction auxiliaire qui va garder à l'aide d'un List.filter les caractères d'une liste qui sint différents de c *)
  let f_supp = List.filter (fun x -> x <> c) in
  {left = f_supp r.left; right = f_supp r.right} ;; (* On applique f_supp aux listes gauche et droite *)


(* type : ruban -> ruban = <fun>
   @requires : Rien
   @ensures : On inverse les listes gauches et droites. Ceci a pour effet d'inverse le contenu actuel du ruban et de correctement replacer le curseur
   @raises : Rien
   *)
let invert r = {left = r.right; right = r.left};;


(* type : ruban -> instruction -> ruban = <fun> 
   @requires : Rien
   @ensures : execute l'instruction donnée en argument sur le ruban r. Elle retourne le ruban ainsi modifié
   @raises : Rien
   *)
let rec execute_instruction r = function
  | Left -> go_left r
  | Right -> go_right r
  | Write(c) -> write c r
  | Repeat(n,l) -> repeat execute_instruction n l r
  | Caesar(n) -> caesar n r
  | Delete(c) -> delete c r
  | Invert -> invert r ;;


(* type : program -> ruban = <fun> 
   @requires : Rien
   @ensures : retourne le ruban qui a suivi toutes les instructions du programme p.
   @raises : Rien
   *)
let execute_program p =
  let r = {left = []; right = []} in  (* On part du ruban vide *)
  let rec execute_aux r = function
    | [] -> r   (* On retourne le ruban final *)
    | t::q -> let r = execute_instruction r t in execute_aux r q  (* On execute l'instruction t sur le ruban r et on continue alors l'execution des autres instructions q*)
  in execute_aux r p ;;


(* type : ('a -> char -> 'a) -> 'a -> ruban -> 'a = <fun> 
   @requires : Rien
   @ensures : parcourt le ruban de gauche à droite en appliquant la fonction f.
   @raises : Rien
   *)
let fold_ruban f v0 r =
  let acc2 = List.fold_right (fun e acc -> f acc e) r.left v0 in  (* L'accumulateur initial est v0 *)
  List.fold_left f acc2 r.right ;;  (* Ici, l'accumulateur initial correspond à la valeur déjà obtenue par le précédent List.fold_right *)






(* type : int -> 'a list -> 'a list = <fun>
   @requires : Rien
   @ensures : détaillé dans le rapport
   @raises : Rien
   *)
let rec prefixe n l =
  if n = 0 then []
  else
    match l with
    | [] -> []
    | t::q -> t :: prefixe (n-1) q ;;


(* type : int -> 'a list -> 'a list * 'a list = <fun> 
   @requires : Rien
   @ensures : détaillé dans le rapport
   @raises : Rien
   *)
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


(* type : int -> 'a list -> 'a list list = <fun>
   @requires : Rien
   @ensures : détaillé dans le rapport
   @raises : Rien
   *)
let reviens_2_fois n l =
  let rec aux n =
    if n = 0 then []
    else (double_prefixe n l) :: aux (n-1)
  in
  let f (x, y) acc =
    if x <> [] && y <> [] && x = y then x :: acc
    else acc
  in List.fold_right f (aux n) [];;


(* type : 'a list list -> bool = <fun>
   @requires : Rien
   @ensures : détaillé dans le rapport
   @raises : Rien
   *)
let tous_egaux ll =
  let l = List.concat ll in
  match l with
    |[] -> true
    |t::q -> List.fold_left (fun b x -> b && x = t) true q ;;


(* type : int -> 'a list -> 'a list = <fun>
   @requires : Rien
   @ensures : détaillé dans le rapport
   @raises : la liste l doit être de taille au moins n
   *)
let rec enleve_premiers_termes n l =
  if n = 0 then l
  else
    match l with
    | [] -> failwith "la liste est trop petite, ce cas n'est pas sensé arriver"
    | t::q -> enleve_premiers_termes (n-1) q ;;


(* type : 'a list -> int -> 'a list -> int = <fun> 
   @requires : Rien
   @ensures : détaillé dans le rapport
   @raises : Rien
   *)
let nombre_repetitions mot n_mot msg =
  let rec aux msg =
    let pref = prefixe n_mot msg in
    if pref <> mot then 0
    else 1 + aux (enleve_premiers_termes n_mot msg)
  in aux msg;;


(* type : char list -> instruction list = <fun> 
   @requires : Rien
   @ensures : détaillé dans le rapport
   @raises : Rien
   *)
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
   
  

(** Votre code doit s'arreter à partir de ce point.

    NE MODIFIER RIEN EN DESSOUS DE CE POINT 
 *)











                         
                         
let die i = 
  let _ = Format.fprintf Format.err_formatter "Usage: %s <1|2|3> <file>@." Sys.argv.(0) in
  exit i
  
let main phase file =
  if phase = "1" || phase = "2"
  then
    let li = analyse_program file in
    let rub = execute_program li in
    let _ = fold_ruban (fun _ c -> Format.printf "%c" c) () rub in
    Format.printf "@."
  else if phase = "3"
  then
    let msg = read_file file in
    let p = generate_program msg in
    print_program p
  else die 1


let _ =
  if Array.length Sys.argv = 3 
  then
    main Sys.argv.(1) Sys.argv.(2)
  else die 2
