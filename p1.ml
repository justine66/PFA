let grille = Array.init 9 (fun i -> Array.make 9 '0')
let solution = Array.init 9 (fun i -> Array.make 9 '0')

let start nom_fichier nom_soluce = 
  
  let files = open_in ("grids/"^nom_fichier^".txt") in 
  let soluce = open_in ("solutions/"^nom_soluce^".txt") in 
  let split () =
    let a = input_line files in
    let b = input_line soluce in
    
    for i = 0 to (String.length a)-1  do
      grille.(i/9).(i mod 9) <- a.[i];        
	  solution.(i/9).(i mod 9) <- b.[i];    
                         
    done
  in split()
    

let affichage g =
 (*Printf.printf "%i" (Array.length g);*)
  for i = 0 to (Array.length g)-1  do
     for j = 0 to (Array.length g.(0))-1  do
    
      Printf.printf "%c" g.(i).(j)
                    done
   
  done
 
        
let check () = 
	let rec check_rec  i j = 
		match i with
		|8 -> (match j with
			|8 -> if grille.(i).(j) == solution.(i).(j) then true else false
			|_ -> if grille.(i).(j) == solution.(i).(j) then check_rec i (j+1) else false)
		|_ -> (match j with
			|8 -> if grille.(i).(j) == solution.(i).(j) then check_rec (i+1) 0 else false
			|_ -> if grille.(i).(j) == solution.(i).(j) then check_rec i (j+1) else false)
	in check_rec 0 0;;

let check_case i j = if grille.(i).(j) == solution.(i).(j) then true else false;;
		
		

let () =
	start "grid0" "solution0" ;
	affichage grille;
	Printf.printf "\n";
	Printf.printf "%b\n" (check ());


	
