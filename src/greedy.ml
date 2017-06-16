(**Módulo para calcular la cuvierta de vertices mediante un algoritmo glotón o greedy*)

open Grafica

let max_deg g =
	let max_v = ref (0) in
	let max_i = ref 0 in
	for i = 0 to g.orden - 1 do
		let deg = ref 0 in
		for j = 0 to g.orden - 1 do
			if(g.aristas.(i).(j)) then
				deg := !deg + 1
			else ()			
		done;
		if !max_v < !deg then(
			max_v := !deg;
			max_i := i 
		)
	done;
	!max_i
	
let elimina g md =
	for i = 0 to g.orden -1 do
		if(g.aristas.(md).(i)) then(
			g.tamano := !(g.tamano) - 1;
			g.aristas.(md).(i) <- false;
			g.aristas.(i).(md) <- false;
		)
		else()	
	done
	
(**
	calcula la cuvierta de la grafica que recive como argumento
	@param g grafica de la que se obtendra la cuvierta
	@return lista de indices de los vertices que conforman la cuvierta
*)
let greedy g = 
	let sol = ref [] in
	while !(g.tamano) <> 0 do
		let md = max_deg g in
		sol := md :: !sol;
		elimina g md;
	done;
	Printf.printf "Greedy:\n";
	List.iter  (fun x -> Printf.printf "%d %!" x) !sol;
	Printf.printf "\nl:%d\n%!" (List.length !sol);
	(List.length !sol)
