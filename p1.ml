let grille = Array.init 9 (fun i -> Array.make 9 '0')

let start nom_fichier = 
  
  let files = open_in ("grids/"^nom_fichier^".txt") in 
  let split () =
    let a = input_line files in
    
    for i = 0 to (String.length a)-1  do
      grille.(i/9).(i mod 9) <- a.[i];        

                         
    done
  in split()
    

let affichage g =
 (*Printf.printf "%i" (Array.length g);*)
  for i = 0 to (Array.length g)-1  do
     for j = 0 to (Array.length g.(0))-1  do
    
      Printf.printf "%c" g.(i).(j)
                    done
   
  done
        
    

let () =
start "grid0" ;
affichage grille;
