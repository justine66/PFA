open Str


type case = { mutable modifiable : bool;
			  mutable valeur     : char}
			  
(*let grille = Array.make_matrix 9 9 (ref{modifiable = true ; valeur = '0'})*)
let grille = Array.make_matrix 9 9 {modifiable = false ; valeur = '0'}
let solution = Array.init 9 (fun i -> Array.make 9 '0')
let fichier_solution = ref "c"

let start nom_fichier nom_soluce = 
  
  fichier_solution := nom_soluce ;
  (*Printf.printf "%s" !fichier_solution;*)
  let files = open_in ("grids/"^nom_fichier) in 
  let soluce = open_in ("solutions/"^nom_soluce) in 
  let split () =
    let a = input_line files in
    let b = input_line soluce in
    
    for i = 0 to (String.length a)-1  do
      grille.(i/9).(i mod 9) <-{ modifiable = true; valeur = a.[i]};        
	  solution.(i/9).(i mod 9) <- b.[i];    
                         
    done
  in split();
  close_in files;
  close_in soluce;;
    

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
	
let save_name bd =
		(*Printf.printf "Test\n";*)
		let file = open_out "sauvegarde.txt" in 
		let rec save_b bc = 
			match bc with
			|hd::tl -> (*Printf.fprintf file "%s\n" hd;*) save_b tl
			|[] -> ()
		in save_b bd;
		close_out file;;
		
let save_n a b =
	let file = open_in "sauvegarde.txt" in 
	let rec save_n_aux bb =

		try
			let c = input_line file in 
			save_n_aux (c::bb); (*Printf.printf "test\n" ; if (i == (in_channel_length file)) then a::b else b*)
			
		with
			|End_of_file -> save_name ((a^"/"^(!fichier_solution))::List.rev(bb));
	in save_n_aux b;
	close_in file;;
			
let save () = 
		Printf.printf "donnez un nom a votre fichier\n%!";
	let a = Scanf.scanf "%s"(fun x -> x^".txt"); in
	let save_file = open_out ("grids/"^a) in
	for i = 0 to (Array.length grille)-1  do
		for j = 0 to (Array.length grille.(0))-1  do
			Printf.fprintf save_file "%c" grille.(i).(j).valeur; 
		done
	done;
	close_out save_file;
	save_n a [];;


		 

		
	
let grillePourAffichage ()=
	start "grid2.txt" "solution0.txt";;
	
let main() =
	start "grid0.txt" "solution0.txt" ;
	(*affichage grille;*)
	Printf.printf "\n";
	(*save();*)
	(*Printf.printf "SOLUTION : %s\n" !fichier_solution;*)
	(*charge()*)

