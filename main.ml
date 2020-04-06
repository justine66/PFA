open Graphics
open Ig
open Affichage
open P1
open Str

let bouton () = 
	 set_color blue;
	 set_font "-bitstream-bitstream charter-bold-r-normal--0-0-0-0-p-0-ascii-0";
     fill_rect 250 320 155 25;
     moveto 270 325;
     set_color white;
     draw_string "Nouvelle partie";
     set_color blue;
     fill_rect 250 250 155 25;
     set_color white;
     moveto 255 255;
     draw_string "Charger une partie";;

let charge_grille cpt = 
	let charge_file = open_in ("sauvegarde.txt") in
	let rec aux cf x =
		if x != cpt then 
			try
				print_string (input_line cf);
				aux cf (x+1)
			with
				|End_of_file -> close_in charge_file;
		else 
			begin
				let sp = Str.split (Str.regexp "/") (input_line cf) in
				match sp with
				|[] -> draw_string "Oh bah ça marche pas !!! "
				|z::s -> (match s with
							|y::t -> (match t with
								|x::[] -> P1.start x y z;
										Affichage.main ();
								|[] -> draw_string "pas assez d'arguments !!!!"
								|_ -> draw_string "trop d'arguments !!!!")
							|_ -> draw_string "y a un pb !!!!")

			end
	in aux charge_file 0;;
	
		 
let rec zone_click x =
	let attend = wait_next_event [Button_down] in
	let abscisse = attend.mouse_x and ordonnee = attend.mouse_y  in
	let rec zc cpt =
		if cpt == -1 then zone_click x (* permet de recommencer si on a pas cliqué au bon endroit*)
		else if abscisse < 245  && abscisse > 225 && ordonnee < 420-(25*cpt) && ordonnee > 400-(25*cpt)
			then charge_grille cpt
			(*then begin moveto 50 50; draw_string "test" end*)
			else zc (cpt-1)
	in zc x;;
	
let w_charge () = 
		let charge_file = open_in ("sauvegarde.txt") in
		let cpt = ref 0 in
		clear_graph ();
		draw_image (Ig.init_image "m.ppm") 0 0;
		let rec charge_aux x y cf =
			try
				Printf.printf "charge";
				let sp = Str.split (Str.regexp "/") (input_line cf) in
				match sp with 
				|[] -> ()
				|hd::tl -> (set_color blue;
					fill_rect 225 y 20 20;
					moveto x y;
					set_color white;
					draw_string hd ;
					charge_aux x (y-25) cf;
					cpt := !cpt +1;)
			with
				|End_of_file -> close_in charge_file;
		in charge_aux 250 400 charge_file;
		zone_click !cpt
		 ;;
		 
let random_charge () =
	let r = Random.int 244 in
	let a = "grid"^(string_of_int r)^".txt" in
	let b = "solution"^(string_of_int r)^".txt" in
	
	Printf.printf "%s\n" a;
	P1.start a b "_.txt";
	Affichage.main ();;
			 
let rec click () =
    let attend = wait_next_event [Button_down] in
	let abscisse = attend.mouse_x and ordonnee = attend.mouse_y  in
	if abscisse < 405 && abscisse > 250 && ordonnee < 345 && ordonnee > 320  then random_charge ()
	else if abscisse < 405 && abscisse > 250 && ordonnee < 280 && ordonnee > 250 then w_charge ()
		else click ();;
		 
let () =
Printf.printf "bonjours\n";
	open_graph " 900x600";
	draw_image (Ig.init_image "m.ppm") 0 0;
	moveto 200 500;

	set_color red;
	draw_string "Bienvenue dans Sudoku"; 
	bouton ();
	click ();
	
	ignore(read_key ())
