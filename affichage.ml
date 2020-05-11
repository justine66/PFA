open Graphics
open Ig
open P1

let affichageVerticale ()= 

	for j = 0 to 3 do
		Graphics.moveto (29+j*180) (30);
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

	
let affichageChiffres g =
	let rec aChiffres_rec g i j = 
		match i with
		|8 -> (match j with
			|8 -> if g.(i).(j).valeur != '0' then begin moveto (j*60+50) (600-(i*60+50)); if g.(i).(j).modifiable == false then
																							begin
																							set_color red;
																							draw_char g.(i).(j).valeur
																							end
																						else
																							begin 
																							set_color green;
																							draw_char g.(i).(j).valeur
																							end
																						end
			|j when j >= 0 && j < 8 -> if g.(i).(j).valeur =='0' then begin aChiffres_rec g i (j+1) end else begin moveto (j*60+55) (600-(i*60+70)); if g.(i).(j).modifiable == false then
																							begin
																							set_color red;
																							draw_char g.(i).(j).valeur;
																							aChiffres_rec g i (j+1)
																							end
																						else
																							begin 
																							set_color green;
																							draw_char g.(i).(j).valeur;
																							aChiffres_rec g i (j+1)
																							end
																						end
										
			|_ -> assert false)
		|i when i >= 0 && i < 8 -> (match j with
			|8 -> if g.(i).(j).valeur == '0' then begin aChiffres_rec g (i+1) 0 end else begin moveto (j*60+55) (600-(i*60+70)); if g.(i).(j).modifiable == false then
																							begin
																							set_color red;
																							draw_char g.(i).(j).valeur;
																							aChiffres_rec g (i+1) 0
																							end
																						else
																							begin 
																							set_color green;
																							draw_char g.(i).(j).valeur;
																							aChiffres_rec g (i+1) 0
																							end
																						end
			|j when j >= 0 && j < 8 -> if g.(i).(j).valeur == '0' then begin aChiffres_rec g i (j+1) end else begin moveto (j*60+55) (600-(i*60+70)); if g.(i).(j).modifiable == false then
																							begin
																							set_color red;
																							draw_char g.(i).(j).valeur;
																							aChiffres_rec g i (j+1)
																							end
																						else
																							begin 
																							set_color green;
																							draw_char g.(i).(j).valeur;
																							aChiffres_rec g i (j+1)
																							end
																						end
			|_ -> assert false)
		|_ -> assert false
	in aChiffres_rec g 0 0;; 

let setChiffre ()= 
    set_color red;
    affichageVerticale ();
    affichageHorizontale ();
    (*set_font "-*-fixed-bold-*-*-*-18-120-*-*-*-*-*-*";*)
    set_font "-bitstream-bitstream charter-bold-r-normal--0-0-0-0-p-0-ascii-0";
    affichageChiffres grille		
 
let regles () = (*permet d'afficher les regles et les commandes*)
	set_color black;
	set_font "-bitstream-bitstream charter-bold-r-normal--0-0-0-0-p-0-ascii-0";
	moveto 640 550;
	draw_string "Les Regles : ";
	moveto (650) (530);
	set_font "-bitstream-bitstream charter-medium-r-normal--0-0-0-0-p-0-ascii-0";
	draw_string "Remplir les cases vides avec";  
	moveto (650) (510);
	draw_string "les chiffres de 1 a 9, de telle"; 
	moveto (650) (490);
	draw_string "sorte qu'ils n'apparaissent";
	moveto (650) (470);
	draw_string "qu'une fois par ligne, ";
	moveto (650) (450);
	draw_string "par colonne et par carre";
	moveto (650) (430);
	draw_string "de 3x3 cases."; 
	moveto (640) (400);
	set_font "-bitstream-bitstream charter-bold-r-normal--0-0-0-0-p-0-ascii-0";
	draw_string "Les Commandes : ";
	moveto (650) (380);
	set_font "-bitstream-bitstream charter-medium-r-normal--0-0-0-0-p-0-ascii-0";
	draw_string "q: exit";
	moveto (650) (360);
	set_font "-bitstream-bitstream charter-medium-r-normal--0-0-0-0-p-0-ascii-0";
	draw_string "pour effacer un chiffre cliquez";
	moveto (650) (340);
	set_font "-bitstream-bitstream charter-medium-r-normal--0-0-0-0-p-0-ascii-0";
	draw_string "sur la case et tapez 0"

			
let rec boutons () =
 (* cree le bouton pour afficher les regles et les commandes*)
	set_color blue;
	moveto 650 400;
    fill_rect 650 300 105 25;
    moveto 655 300;
    set_color white;
    draw_string "Commandes";
    (* cree le bouton pour sauvegarder*) 
	set_color blue;
	moveto 650 300;
    fill_rect 650 250 105 25;
    moveto 655 250;
    set_color white;
    draw_string "Sauvegarder";
(* cree le bouton pour verifier une case*)
    set_color blue;
	moveto 650 200;
    fill_rect 650 200 105 25;
    moveto 655 200;
    set_color white;
    draw_string "check case";
    set_color blue;

	moveto 650 200;
    fill_rect 650 150 105 25;
    moveto 655 150;
    set_color white;
    draw_string "recommencer";
	 
    let attend = wait_next_event [Button_down] in
	let abscisse = attend.mouse_x and ordonnee = attend.mouse_y  in
	 	if abscisse < 756 && abscisse > 650 && ordonnee < 275 && ordonnee > 250 then
			 begin
			 P1.save [];
		     end

     	else if abscisse < 756 && abscisse > 650 && ordonnee < 325 && ordonnee > 300 then
			 begin
		     regles();
		     end

			 else if abscisse < 756 && abscisse > 650 && ordonnee < 225 && ordonnee > 200 then
				 begin
				 	Printf.printf "check";
				 	let attend = wait_next_event [Button_down] in
				 	let abscisse = attend.mouse_x and ordonnee = attend.mouse_y  in
				 	let a = 8-(ordonnee-30)/60 in
					let b = ((abscisse-30)/60) in
					(*Printf.printf "a: %d\n b: %d\n" a b;
					Printf.printf "grille: %c\n solution: %c\n" grille.(a).(b).valeur solution.(a).(b);*)
					if P1.check_case a b then 
						begin 
						Printf.printf "true\n";
						clear_graph ();
						draw_image (Ig.init_image "galaxy.ppm") 0 0;
						setChiffre ();
						
						set_color green;
						moveto 655 100;
						draw_string "true";
						boutons ();
						end
					else begin 
						Printf.printf "false\n";
						clear_graph ();
						draw_image (Ig.init_image "galaxy.ppm") 0 0;
						setChiffre ();
						
						set_color red;
						moveto 655 100;
						draw_string "false";
						boutons ();
						end
					
			     end
			 	else if abscisse < 756 && abscisse > 650 && ordonnee < 175 && ordonnee > 150 then
					 begin
					 	Printf.printf "recommencer";
					 	P1.recommencer ();
					 end;;
     

let mettreChiffre x y key = 
	moveto (x*60+55) (600-(70+y*60));
	let m =['1';'2';'3';'4';'5';'6';'7';'8';'9'] in
		if grille.(y).(x).modifiable != false then
			let rec modif n=
				match n with
				|hd::tl  when hd == key ->
						if grille.(y).(x).valeur == '0' then 
							begin
								draw_char key;
							grille.(y).(x) <- { modifiable = true; valeur = key};
							boutons ();
							end
						else
							begin
							grille.(y).(x) <- { modifiable = true; valeur = key};
							clear_graph ();
							draw_image (Ig.init_image "galaxy.ppm") 0 0;
							setChiffre ();
							boutons ();
							end
				|hd::tl  when hd != key -> modif tl
				|[] -> (* peut mettre un match avec la liste des commandes si il y en a plus qu'une *)
						if key == 'q' then close_graph() 
			in modif m ;;
    
let trouverDansGrille x y touche =
	let a = (x-30)/60 in
	let b = 8-((y-30)/60) in
	mettreChiffre a b touche

let finir ()= 
if P1.check () then 
	begin 
	clear_graph ();
	draw_image (Ig.init_image "galaxy.ppm") 0 0;
	P1.aff_text "Vous avez gagne" 200 500;
	end
else 
	begin 
	clear_graph ();
	draw_image (Ig.init_image "galaxy.ppm") 0 0;
	P1.aff_text "Vous avez perdu" 200 500;
	end

let contient_un_zero () =
 try
  for i = 0 to 8 do
	for j = 0 to 8 do
	   if grille.(i).(j).valeur = '0' then raise Exit;
	done
  done;
  true
 with Exit -> false;;

    
let rec saisiChiffres () = 
	set_color green;
	let attend = wait_next_event [Button_down] in
	let abscisse = attend.mouse_x and ordonnee = attend.mouse_y (*and touche = read_key()*) in
	let attend2 = wait_next_event [Key_pressed] in
	let touche = attend2.key in
	trouverDansGrille abscisse ordonnee touche;
	if contient_un_zero() then finir();
	(*mettreChiffre abscisse ordonnee touche;*)
	saisiChiffres ()


	      

	
let main() = 
	
	(*P1.affichage grille; affiche la grille dans le terminal*)
	open_graph " 900x600";
	draw_image (Ig.init_image "galaxy.ppm") 0 0;
	setChiffre ();
	boutons ();
	saisiChiffres ();
	ignore(read_key ());;
	(*while key_pressed()=false do test () done*)
