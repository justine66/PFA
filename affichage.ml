open Graphics
open Ig
open P1

let affichageVerticale ()= 

	for j = 0 to 3 do
		moveto (29+j*180) (30);
		lineto (29+j*180) (570);
		moveto (31+j*180) (30);
		lineto (31+j*180) (570);
	done;
	
	for i = 0 to 9 do
		moveto (30+i*60) (30);
		lineto (30+i*60) (570);
	done
	
	
	
let affichageHorizontale ()= 

	for i = 0 to 3 do
		moveto (30) (29+i*180);
		lineto (570) (29+i*180);
		moveto (30) (31+i*180);
		lineto (570) (31+i*180);
	done;

	for j = 0 to 9 do
		moveto (30) (30+j*60);
		lineto (570) (30+j*60);
	done
	
(* moment où on a galéré !!!
let affichageChiffres g =
	let rec aChiffres_rec g i j x y = 
		match i with
		|8 -> (match j with
			|8 -> if g.(i).(j) == '0' then () else moveto 550 50; draw_char g.(i).(j)
			|j when j >= 0 && j < 8 -> Printf.printf "%c, %d, %d dernier chiffre **** \n" g.(i).(j) i j; if g.(i).(j) == '0' then aChiffres_rec g i (j+1) (x+60) y else moveto x y; 
																			 draw_char g.(i).(j);
																			 aChiffres_rec g i (j+1) (x+60) y
			|_ -> assert false)
		|i when i >= 0 && i < 8 -> (match j with
			|8 -> Printf.printf "%c, %d, %d dernier chiffre  &&&&&\n" g.(i).(j) i j; if g.(i).(j) == '0' then aChiffres_rec g (i+1) 0 50 (y-60) else moveto x y; 
																			 draw_char g.(i).(j);
																			 aChiffres_rec g (i+1) 0 50 (y-60)
			|j when j >= 0 && j < 8 -> Printf.printf "%c, %d, %d dernier chiffre @@@@@@\n" g.(i).(j) i j; if grille.(i).(j) == '0' then aChiffres_rec g i (j+1) (x+60) y else moveto x y; 
																				  draw_char g.(i).(j);
																				  aChiffres_rec g i (j+1) (x+60) y
			|_ -> assert false)
		|_ -> assert false
	in aChiffres_rec g 0 0 50 550;;*)
  	
(*let affichageChiffres g =
	let rec aChiffres_rec g i j x y = 
		match i with
		|8 -> (match j with
			|8 -> if g.(i).(j).valeur == '0' then () else moveto (x+60) y; draw_char g.(i).(j).valeur
			|j when j >= 0 && j < 8 -> if g.(i).(j).valeur =='0' then begin aChiffres_rec g i (j+1) (x+60) y end else begin moveto (x+60) y; draw_char g.(i).(j).valeur; aChiffres_rec g i (j+1) (x+60) y end 
			|_ -> assert false)
		|i when i >= 0 && i < 8 -> (match j with
			|8 -> if g.(i).(j).valeur == '0' then begin aChiffres_rec g (i+1) 0 0 (y-60) end else begin moveto (x+60) y; draw_char g.(i).(j).valeur; aChiffres_rec g (i+1) 0 0 (y-60) end
			|j when j >= 0 && j < 8 -> if g.(i).(j).valeur == '0' then begin aChiffres_rec g i (j+1) (x+60) y end else begin moveto (x+60) y; draw_char g.(i).(j).valeur; aChiffres_rec g i (j+1) (x+60) y end
			|_ -> assert false)
		|_ -> assert false
	in aChiffres_rec g 0 0 (-5) 532;; 	*)
	
let affichageChiffres g =
	let rec aChiffres_rec g i j = 
		match i with
		|8 -> (match j with
			|8 -> if g.(i).(j).valeur != '0' then Printf.printf "%c" g.(i).(j).valeur; moveto (j*60+60) (600-(i*60+60)); draw_char g.(i).(j).valeur
			|j when j >= 0 && j < 8 -> if g.(i).(j).valeur =='0' then begin aChiffres_rec g i (j+1) end else begin Printf.printf "%c" g.(i).(j).valeur; moveto (j*60+60) (600-(i*60+60)); draw_char g.(i).(j).valeur; aChiffres_rec g i (j+1) end 
			|_ -> assert false)
		|i when i >= 0 && i < 8 -> (match j with
			|8 -> if g.(i).(j).valeur == '0' then begin aChiffres_rec g (i+1) 0 end else begin Printf.printf "%c" g.(i).(j).valeur; moveto (j*60+60) (600-(i*60+60)); draw_char g.(i).(j).valeur; aChiffres_rec g (i+1) 0 end
			|j when j >= 0 && j < 8 -> if g.(i).(j).valeur == '0' then begin aChiffres_rec g i (j+1) end else begin Printf.printf "%c" g.(i).(j).valeur; moveto (j*60+60) (600-(i*60+60)); draw_char g.(i).(j).valeur; aChiffres_rec g i (j+1) end
			|_ -> assert false)
		|_ -> assert false
	in aChiffres_rec g 0 0;; 	
 
let test ()= 
	set_color red;
	affichageVerticale ();
    affichageHorizontale ();
    (*set_font "-*-fixed-bold-*-*-*-18-120-*-*-*-*-*-*";*)
    set_font "-bitstream-bitstream charter-bold-r-normal--0-0-0-0-p-0-ascii-0";
    affichageChiffres grille
    
(*let trouverDansGrille x y =
	Printf.print*)
    
let mettreChiffre x y key = 
	
	moveto x y;
	draw_char key
    
let rec saisiChiffres () = 
	set_color green;
	let attend = wait_next_event [Button_down] in
	let abscisse = attend.mouse_x and ordonnee = attend.mouse_y and touche = read_key() in
	(*let touche = attend.read_key() in*)
	(*let touche = wait_next_event [Key_pressed] in*)
	mettreChiffre abscisse ordonnee touche;
	saisiChiffres ()
	
	
let () = 
	P1.grillePourAffichage ();
	P1.affichage grille;
	open_graph " 600x600";
	draw_image (Ig.init_image "galaxy.ppm") 0 0;
	test ();
	saisiChiffres ();
	ignore(read_key ())
	(*while key_pressed()=false do test () done*)
	

