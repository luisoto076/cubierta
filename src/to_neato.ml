open Grafica
open Printf

let neato g vc =
	let s = ref "graph G {\n" in
	for i = 0 to g.orden - 1 do
		for j = i + 1 to g.orden-1 do
			if (conectados g i j) then(
				s := !s ^"\tv"^(string_of_int i)^" -- v" ^ (string_of_int j)^"\n";
			)else() 
		done
	done;
	List.iter (fun x -> s := !s ^ "v" ^ (string_of_int x) ^ " [style=filled, fillcolor=red];\n") vc;
	let gr = string_of_int (Greedy.greedy g) in
	s := (!s^"A"^(string_of_int (List.length vc))^" [style=filled, fillcolor=yellow];\nG"^gr^" [style=filled, fillcolor=blue];\n}");
	!s
	
let guarda g vc s =
	let oc = open_out s in 
	fprintf oc "%s" (neato g vc);
	
