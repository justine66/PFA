open Graphics
open Ig


let camoulox () =
	open_graph " 600x600";
	draw_image (Ig.init_image "m.ppm") 0 0;
	ignore(read_key ())
