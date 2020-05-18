open Str
open Graphics


type case = { mutable modifiable : bool;
			  mutable valeur     : char}
			  
(*let grille = Array.make_matrix 9 9 (ref{modifiable = true ; valeur = '0'})*)
let grille = Array.make_matrix 9 9 {modifiable = false ; valeur = '0'}
let solution = Array.init 9 (fun i -> Array.make 9 '0')
let fichier_solution = ref "c"
let fichier_original = ref "c"
let fichier_charge = ref "c"


let rec start nom_fichier nom_soluce nom_charge = 
  
  fichier_solution := nom_soluce ;
  fichier_original := nom_fichier;
  fichier_charge := nom_charge;
  (*Printf.printf "%s" !fichier_solution;*)
  if !fichier_charge = "_.txt" then 
  	let files = open_in ("grids/"^nom_fichier) in 
  	let soluce = open_in ("solutions/"^nom_soluce) in 
  	let split () =
	    let a = input_line files in
	    let b = input_line soluce in
	    
	    for i = 0 to (String.length a)-1  do
	     if a.[i] != '0' then
				begin
					grille.(i/9).(i mod 9) <-{ modifiable = false; valeur = a.[i]};        
					solution.(i/9).(i mod 9) <- b.[i];    
				end
			else 
				begin
					grille.(i/9).(i mod 9) <-{ modifiable = true; valeur = a.[i]};        
					solution.(i/9).(i mod 9) <- b.[i];    
				end
	                         
	    done
  	in split();
  else 
  	let files = open_in ("grids/"^nom_charge) in 
  	let file = open_in ("grids/"^nom_fichier) in
  	let soluce = open_in ("solutions/"^nom_soluce) in 
	  let split () =
	    let a = input_line files in
	    let c = input_line file in
	    let b = input_line soluce in
	    
	    for i = 0 to (String.length a)-1  do
	     if a.[i] != '0' then
	     	begin
	     	if c.[i] = '0' then
		     	begin
						grille.(i/9).(i mod 9) <-{ modifiable = true; valeur = a.[i]};        
						solution.(i/9).(i mod 9) <- b.[i];    
					end
			else
				begin
					grille.(i/9).(i mod 9) <-{ modifiable = false; valeur = a.[i]};        
					solution.(i/9).(i mod 9) <- b.[i];    
				end
			end
		else 
			begin
				grille.(i/9).(i mod 9) <-{ modifiable = true; valeur = a.[i]};        
				solution.(i/9).(i mod 9) <- b.[i];    
			end              
	    done
	  in split();
  close_in files;
  close_in soluce;
  if Sys.file_exists !fichier_charge then 
	begin
		let file = open_in ("grids/"^(!fichier_charge)) in 
		
			let charge () =
				let a = input_line file in
				
				for i = 0 to (String.length a)-1  do
					 if a.[i] != '0' then
							begin
								if grille.(i/9).(i mod 9).valeur == '0' then 
									grille.(i/9).(i mod 9) <-{ modifiable = true; valeur = a.[i]};         
							end                        
				done
			in charge();
			close_in file;
		
	end;
  ;;
    

let affichage g =
 (*Printf.printf "%i" (Array.length g);*)
  for i = 0 to (Array.length g)-1  do
     for j = 0 to (Array.length g.(0))-1  do
    
      Printf.printf "%c" g.(i).(j).valeur
                    done
   
  done;;
 
        
let check () = 
	let rec check_rec  i j = 
		match i with
		|8 -> (match j with
			|8 -> if grille.(i).(j).valeur == solution.(i).(j) then true else false
			|_ -> if grille.(i).(j).valeur == solution.(i).(j) then check_rec i (j+1) else false)
		|_ -> (match j with
			|8 -> if grille.(i).(j).valeur == solution.(i).(j) then check_rec (i+1) 0 else false
			|_ -> if grille.(i).(j).valeur == solution.(i).(j) then check_rec i (j+1) else false)
	in check_rec 0 0;;

let check_case i j = if grille.(i).(j).valeur == solution.(i).(j) then true else false;;

let save_file a = 
	let file = open_out ("grids/"^a) in
	Printf.printf "\n";
	for i = 0 to (Array.length grille)-1  do
		for j = 0 to (Array.length grille.(0))-1  do
			Printf.printf "%c" grille.(i).(j).valeur;
			Printf.fprintf file "%c" grille.(i).(j).valeur; 
		done
	done;
	close_out file;;

let save_name bd =
		Printf.printf "Test\n";
		let file = open_out "sauvegarde.txt" in 
		let rec save_b bc = 
			match bc with

			|hd::tl -> Printf.fprintf file "%s\n" hd; save_b tl
			|[] -> ()
		in save_b bd;
		close_out file;;
		
let save_n a b =
	let file = open_in "sauvegarde.txt" in 
	let rec save_n_aux bb =

		try
			let c = input_line file in 
			Printf.printf "1";
			save_n_aux (c::bb); (*Printf.printf "test\n" ; if (i == (in_channel_length file)) then a::b else b*)
			
		with
			|End_of_file -> save_name ((a^".txt/"^(!fichier_solution)^"/"^(!fichier_original))::List.rev(bb));
	in save_n_aux b;
	save_file (a^".txt");
	close_in file;;

let aff_text s x y = 
	set_color white;
	moveto x y;
	set_font "-bitstream-bitstream charter-medium-r-normal--0-0-0-0-p-0-ascii-0";
	draw_string s;;

let rec concat s l m =
	match l with
	|hd::tl -> concat s tl m^s^hd
	|[]-> m
	

let rec save l = 
	
	clear_graph ();
	draw_image (Ig.init_image "galaxy.ppm") 0 0;
	
	if !fichier_charge = "_.txt" then
		begin
		aff_text "donnez un nom a votre fichier " 200 500;
		aff_text " nom : " 200 480;
		aff_text (concat "" l "") 250 480;
		let attend = wait_next_event [Key_pressed; Button_down] in
			if attend.keypressed then
			begin
				if attend.key == '\r' then
				begin
					close_graph ();
					save_n (concat "" l "") [];
				end
				else  
					begin
						let s= Char.escaped attend.key in 
						draw_char attend.key;
						save (s::l);
					end
				end
			else 
				save_n (concat "" l "") []
		end
	else save_file !fichier_charge;;

let aide () =
	let rec aide_aux x y =
		if grille.(x).(y).valeur = '0' then grille.(x).(y) <- { modifiable = false; valeur = solution.(x).(y)}
		else if y = 8 then  aide_aux (x+1) 0
			else aide_aux (x) (y+1)
	in aide_aux 0 0;;
		
	
let grillePourAffichage ()=
	start "grid2.txt" "solution0.txt" "_.txt";;
	
let main() =
	start "grid0.txt" "solution0.txt" "_.txt" ;
	(*affichage grille;*)
	Printf.printf "\n";
	(*save();*)
	(*Printf.printf "SOLUTION : %s\n" !fichier_solution;*)
	(*charge()*)

