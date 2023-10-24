#load "graphics.cma";;
open Graphics;;

open_graph ":0";;
resize_window 800 800;;
let tailleCase = 40;;
let nombreCases = 800/40;;
type direction = Haut | Bas | Gauche | Droite | Rien;;
exception Stop;;
let grey = rgb 58 70 76;;
(* 
Z ou z -> haut
Q ou q -> gauche 
S ou s -> bas
D ou d -> droite *)

let direction_of_char c = match c with
		| 'z' -> Haut
		| 'Z' -> Haut
		| 'q' -> Gauche
		| 'Q' -> Gauche
		| 's' -> Bas
		| 'S' -> Bas
		| 'd' -> Droite
		| 'D' -> Droite
		| _ -> Rien;;

let string_of_direction d = match d with 
|Haut -> "Haut"
|Bas -> "Bas"
|Gauche -> "Gauche"
|Droite-> "Droite"
|Rien-> "Rien";;

let affiche_map () = 
	set_color white;
	fill_rect 0 0 800 800;
	set_color black;
	for i = 1 to nombreCases do
		begin 
			moveto (i*tailleCase) 0;
			lineto (i* tailleCase) 1000;
			moveto 0 (i* tailleCase);
			lineto 1000 (i* tailleCase);
		end
	done
;;	
	
let colore_case (x, y) color = 
	set_color color;
	fill_rect (x*tailleCase) (y*tailleCase) tailleCase tailleCase;;
	
let rec affiche_snake snake = match snake with 
|t::q -> colore_case t grey; affiche_snake q
|[] -> ();;

let affiche_fruit (x, y) = colore_case (x, y) red;;

let verification_fruit (x, y) snake = 
	List.for_all (fun (xS, yS) -> (x <> xS && y <> yS)) snake;;

let rec choisi_fruit snake = 
	let x = Random.int nombreCases in 
	let y = Random.int nombreCases in 
	if not (verification_fruit (x, y) snake) then choisi_fruit snake
	else (x, y);;

let int_of_direction (x, y) direction = match direction with 
|Droite -> (x+1, y)
|Gauche -> (x-1, y)
|Haut -> (x, y+1)
|Bas -> (x, y-1)
|_ -> (x, y);;

let direction_opposee (x, y) direction = match direction with 
|Droite -> (x-1, y) 
|Gauche -> (x+1, y)
|Haut -> (x, y-1)
|Bas -> (x, y+1)
|_ -> (x, y);;

let direction_opposee2 d = match d with
|Gauche -> Droite
|Droite -> Gauche
|Haut -> Bas 
|Bas -> Haut
|_ -> Rien ;;

let determine_direction_queue (xF, yF) (xFF, yFF) = 
	let (xEcart, yEcart) = (xF-xFF, yF-yFF) in match xEcart, yEcart with
	| (0, 1) -> Bas 
	| (0, -1) -> Haut
	| (1, 0) -> Droite
	| (-1, 0) -> Gauche 
	| _ -> failwith "problème de queue";;

let collision_murs (x, y) = (x < 0 || x > nombreCases) || (y < 0 || y > nombreCases);;

let rec entremelement snake (x, y) = verification_fruit (x, y) snake;; 

let directionSnakeValide snake (x, y) =
	not ((collision_murs (x, y))) || (entremelement snake (x, y));;

let agrandir_snake_queue snake directionActuelle = (* snake est une référence *)
	if List.length !snake = 1 then 
		let hd = List.hd !snake in 
		snake := List.rev ( (int_of_direction hd (direction_opposee2 directionActuelle))::!snake)
	else
		let (xF, yF) = List.nth !snake (List.length !snake - 2) in
		let (xFF, yFF) = List.nth !snake (List.length !snake - 1) in 
		let directionQueue = determine_direction_queue (xF, yF) (xFF, yFF) in 
		let coordNouvelleQueue = int_of_direction (xFF, yFF) directionQueue in 
		if (not (collision_murs coordNouvelleQueue)) then 
			snake := List.rev (coordNouvelleQueue::(List.rev !snake))
		else failwith "on ne peut pas agrandir le snake";;
	

let main = 
		let score = ref 0 in 
		let snake = ref [(nombreCases/2, nombreCases/2)] in 
		let toucheAppuyee = ref false in
		let touche = ref ' ' in
		let directionActuelle = ref Droite in
		let fruit = ref (2, 2) in 
		let attente = 0.2 in (* correspond à 1s *)
		let time = ref (Sys.time ()) in
		try 
		begin 
			while true do

				while Sys.time() -. !time < attente do (* attente *) 
					toucheAppuyee := key_pressed(); (* vérification du changement de direction *)
					if !toucheAppuyee then 
						begin
							touche := read_key();
							let d = direction_of_char !touche in
							if d <> Rien then directionActuelle := d;
							toucheAppuyee := false

						end;
					done; 
				time := Sys.time();

				auto_synchronize false;

				(* prochaine case *)
				let (x, y) = int_of_direction (List.hd !snake) !directionActuelle in

				if !fruit = (x, y) then   (* vérification du fruit *)
					begin 
						incr score;
						agrandir_snake_queue snake !directionActuelle;
						fruit := choisi_fruit !snake;
					end;
				
				if directionSnakeValide !snake (x, y) then (* avancement du snake *)
					begin 
						snake := (x, y)::!snake;
						snake := List.rev(List.tl (List.rev !snake));
					end
				else raise Stop;

				(* affichage *)
				affiche_map ();
				affiche_snake !snake;
				affiche_fruit !fruit;

				synchronize ()
			done
		end	
	with Stop -> print_int (!score);;