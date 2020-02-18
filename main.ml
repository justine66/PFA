open Graphics
open Ig
open Affichage


let () =
Printf.printf "bonjour";
	open_graph " 600x600";
	draw_image (Ig.init_image "m.ppm") 0 0;
	moveto 0 0;
	draw_string "Bienvenue dans Sudoku"; 
	close_graph ();
	Affichage.main();
	ignore(read_key ())
