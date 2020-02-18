open Graphics
(*** Question 1 ***)

type grille = bool array array;;

let create n = Array.init n (fun i -> Array.make n false);;

(*** Question 2 ***)

let init_center g = if ((Array.length g) mod 2 != 0 ) then g.(((Array.length g) /2)+1).(((Array.length g) /2)+1) <- true
    else g.(((Array.length g) /2)).(((Array.length g) /2)) <- true;;

let () = Random.self_init ();;

let init_aleatoire p g =
  for i = 0 to (Array.length g)-1 do
  	for j = 0 to (Array.length g.(0))-1 do
  		if (Random.int 100 < p) then g.(i).(j)<-true
  	done
  done;;
 
(*** Question 3 ***)

let affichage g = 

	for i = 0 to (Array.length g)-1  do
		for j = 0 to (Array.length g.(0))-1  do
			if (g.(i).(j) = true ) then set_color green else set_color black;
			plot (j*10+100) (i*10 +100)
		done
	done;;
  	
let test n= 	
	let g=create n in
	init_aleatoire 20 g;
	
	affichage g;;
	
let () = 
	open_graph " 600x600";
	while key_pressed()=false do test 5 done
	;; 
	
(*** Question 4 ***)

type etat = { up : bool; down: bool; right : bool; left : bool }
