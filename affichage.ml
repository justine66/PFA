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

	
let affichageChiffres g =
	let rec aChiffres_rec g i j = 
		match i with
		|8 -> (match j with
			|8 -> if g.(i).(j).valeur != '0' then begin Printf.printf "abscisse : %d ordonne : %d = %b" j i g.(i).(j).modifiable; moveto (j*60+50) (600-(i*60+50)); draw_char g.(i).(j).valeur end
			|j when j >= 0 && j < 8 -> if g.(i).(j).valeur =='0' then begin aChiffres_rec g i (j+1) end else begin Printf.printf "abscisse : %d ordonne : %d = %b|" j i g.(i).(j).modifiable; moveto (j*60+55) (600-(i*60+70)); draw_char g.(i).(j).valeur; aChiffres_rec g i (j+1) end 
			|_ -> assert false)
		|i when i >= 0 && i < 8 -> (match j with
			|8 -> if g.(i).(j).valeur == '0' then begin aChiffres_rec g (i+1) 0 end else begin Printf.printf "abscisse : %d ordonne : %d = %b \n" j i g.(i).(j).modifiable; moveto (j*60+55) (600-(i*60+70)); draw_char g.(i).(j).valeur; aChiffres_rec g (i+1) 0 end
			|j when j >= 0 && j < 8 -> if g.(i).(j).valeur == '0' then begin aChiffres_rec g i (j+1) end else begin Printf.printf "abscisse : %d ordonne : %d = %b|" j i g.(i).(j).modifiable; moveto (j*60+55) (600-(i*60+70)); draw_char g.(i).(j).valeur; aChiffres_rec g i (j+1) end
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

let mettreChiffre x y key = 
	moveto (x*60+55) (600-(70+y*60));
	if grille.(y).(x).modifiable != false then
	begin
		grille.(y).(x) <- { modifiable = true; valeur = key};
		draw_char key;
	end
    
let trouverDansGrille x y touche =
	let a = (x-30)/60 in
	let b = 8-((y-30)/60) in
	mettreChiffre a b touche
    
let rec saisiChiffres () = 
	set_color green;
	let attend = wait_next_event [Button_down] in
	let abscisse = attend.mouse_x and ordonnee = attend.mouse_y and touche = read_key() in
	(*let touche = attend.read_key() in*)
	(*let touche = wait_next_event [Key_pressed] in*)
	trouverDansGrille abscisse ordonnee touche;
	(*mettreChiffre abscisse ordonnee touche;*)
	saisiChiffres ()
	      
let regles() = (*permet d'afficher les regles et les commandes*)
	set_color black;
	set_font "-bitstream-bitstream charter-bold-r-normal--0-0-0-0-p-0-ascii-0";
	moveto 640 400;
	draw_string "Les Regles : ";
	moveto (650) (380);
	set_font "-bitstream-bitstream charter-medium-r-normal--0-0-0-0-p-0-ascii-0";
	draw_string "Remplir les cases vides avec";  
	moveto (650) (365);
	draw_string "les chiffres de 1 a 9, de telle"; 
	moveto (650) (350);
	draw_string "sorte qu'ils n'apparaissent";
	moveto (650) (335);
	draw_string "qu'une fois par ligne, ";
	moveto (650) (320);
	draw_string "par colonne et par carre";
	moveto (650) (305);
	draw_string "de 3x3 cases."; 
	moveto (640) (280);
	set_font "-bitstream-bitstream charter-bold-r-normal--0-0-0-0-p-0-ascii-0";
	draw_string "Les Commandes : ";
	moveto (660) (260);
	set_font "-bitstream-bitstream charter-medium-r-normal--0-0-0-0-p-0-ascii-0";
	draw_string "q: exit"
			
let boutons () = (* cree le bouton pour afficher les regles et les commandes*)
	 set_color blue;
	 moveto 650 400;
     fill_rect 650 400 105 25;
     moveto 655 400;
     set_color white;
     draw_string "Commandes";
     let attend = wait_next_event [Button_down] in
	 let abscisse = attend.mouse_x and ordonnee = attend.mouse_y  in
	 if abscisse < 756 && abscisse > 650 && ordonnee < 425 && ordonnee > 400 then
	 begin
	 set_color white;
	 moveto 640 400;
     fill_rect 650 400 105 25;
     regles()
     end
	
let main() = 
	P1.grillePourAffichage ();
	(*P1.affichage grille; affiche la grille dans le terminal*)
	open_graph " 900x600";
	draw_image (Ig.init_image "galaxy.ppm") 0 0;
	setChiffre ();
	boutons ();
	saisiChiffres ();
	ignore(read_key ())
	(*while key_pressed()=false do test () done*)
