open Graphics
open Ig
open Affichage

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
			with
				|End_of_file -> close_in charge_file;
		else 
			begin
				let sp = Str.split (Str.regexp "/") (input_line cf) in
				match sp with
				|[] -> draw_string "Oh bah ça marche pas !!! "
				|x::s -> match s with
							|y::[] -> P1.start x y;
									Affichage.main ();
							|_ -> draw_string "trop d'arguments"
				
			end
	in aux charge_file 0;;
	
		 
let rec zone_click x =
	let attend = wait_next_event [Button_down] in
	let abscisse = attend.mouse_x and ordonnee = attend.mouse_y  in
	let rec zc cpt =
		if cpt == -1 then zone_click x 
		else if abscisse < 245  && abscisse > 225 && ordonnee < 420-(25*cpt) && ordonnee > 400-(25*cpt)  
			then charge_grille cpt
			else zc (cpt-1)
	in zc x;;
	
let w_charge () = 
		let charge_file = open_in ("sauvegarde.txt") in
		let cpt = ref 0 in
		clear_graph ();
		draw_image (Ig.init_image "m.ppm") 0 0;
		let rec charge_aux x y cf =
			try
				let sp = Str.split (Str.regexp "/") (input_line cf) in
				set_color blue;
				fill_rect 225 y 20 20;
				moveto x y;
				set_color white;
				draw_string (List.hd sp);
				charge_aux x (y-25) cf;
				cpt := !cpt +1;
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
	P1.start a b;
	Affichage.main ();;
			 
let rec click () =
    let attend = wait_next_event [Button_down] in
	let abscisse = attend.mouse_x and ordonnee = attend.mouse_y  in
	if abscisse < 405 && abscisse > 250 && ordonnee < 345 && ordonnee > 320  then random_charge ()
	else if abscisse < 405 && abscisse > 250 && ordonnee < 280 && ordonnee > 250 then w_charge ()
		else click ();;
		 
let () =
Printf.printf "bonjour\n";
	open_graph " 900x600";
	draw_image (Ig.init_image "m.ppm") 0 0;
	moveto 200 500;

	set_color red;
	draw_string "Bienvenue dans Sudoku"; 
	bouton ();
	click ();
	
	ignore(read_key ())
