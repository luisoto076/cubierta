(**Modulo principal que implementa la heristica de aceptacion por umbrales*)
open Grafica
open Str
open Greedy
open To_neato

exception Break

type hormiga = {solucion: int list ref ; vertice: int ref; aristas : int array array; sin_cubrir: int ref}

let inicializa v = Random.init v

let init_hormigas g n =
	let h_aux = {solucion = ref []; vertice = ref (-1); aristas = [|[||]|]; sin_cubrir = ref 0} in
	let hormigas = Array.make Conf.n_hormigas h_aux in
	for i = 0 to n - 1 do
		hormigas.(i) <- {solucion = ref []; vertice = ref (-1); aristas = Array.make_matrix g.orden g.orden 0; sin_cubrir = ref !(g.tamano)}
	done;
	for i = 0 to g.orden - 1 do
		for j = 0 to g.orden - 1 do
			for k = 0 to n - 1 do
				if g.aristas.(i).(j) then
					hormigas.(k).aristas.(i).(j) <- 1
				else
					hormigas.(k).aristas.(i).(j) <- 0
			done
		done
	done;
	hormigas

let print g =
	for i = 0 to g.orden-1 do
		for j = 0 to g.orden-1 do
			if (g.aristas.(i).(j)) then
				Printf.printf ("%d %d\n%!") i j
			else ()
		done
	done

(**
	Funcion que lee el archivo que se pasa como argumento y construye solucion inicial
	con la que se trabajara la heuristica
*)	
let lee_grafica archivo =
  	let ic = open_in archivo in
  	let g = ref {aristas = [|[||]|] ; orden = 0; tamano = ref 0} in
  	try 
  		let line = input_line ic in
    	let n = int_of_string(line) in
    	g := Grafica.initgraf n;
    	let re = Str.regexp "[ ]*,[ ]*" in
    	while true do
    		let arista = Str.split re (input_line ic) in
    		let line1 = List.nth arista 0 in
    		let line2 = List.nth arista 1 in
    		let u = int_of_string line1 in
    		let v = int_of_string line2 in
    		Grafica.conecta !g u v
    	done;
    	!g
    with
    	|End_of_file -> close_in ic; !g           
  	 	|e -> close_in_noerr ic;raise e
                                    

let psi g hh k i j it =
	if (hh.(k).aristas.(i).(j) = it) then
		1
	else
		0

let eta g hh k j it =
	let total = ref 0 in
	for i = 0 to g.orden - 1 do
		total := !total + (psi g hh k i j it)
	done;
	!total
	
let tau_eta g tau hh k it =
	let tek = Array.make g.orden 0. in
	for i = 0 to g.orden - 1 do
		tek.(i) <- tau.(i) *. ((float_of_int (eta g hh k i it)) ** Conf.betha) 	
	done;
	tek

let posiciona_hormigas n hh =
	let vv = Array.make n (-1) in
	for i = 0 to Conf.n_hormigas-1 do
		let rand = ref (Random.int n) in
		while vv.(!rand) <> -1 do
			rand := Random.int n
		done;
		hh.(i).vertice := !rand;
		vv.(!rand) <- i
	done;()
	
let max_et arr =
	let max_v = ref 0. in
	let max_i = ref (-1) in
	for i = 0 to (Array.length arr)-1 do
		if !max_v < arr.(i) then(
			max_v := arr.(i);
			max_i := i
		)else ()
	done;
	!max_i
	
let actualiza_phi g h it =
	for j = 0 to g.orden - 1 do
		let aux = !(h.vertice) in
		if (h.aristas.(aux).(j) = it) then(
			h.sin_cubrir := !(h.sin_cubrir) - 1;
			h.aristas.(aux).(j) <- it + 1;
			h.aristas.(j).(aux) <- it + 1;
		)else()
	done;()
	
let siguiente_vertice g hh tau it =
	let nv = ref 0 in
	for i = 0 to Conf.n_hormigas-1 do
		if !(hh.(i).sin_cubrir) <> 0 then (
			if not(List.mem !(hh.(i).vertice) !(hh.(i).solucion)) then
				hh.(i).solucion := !(hh.(i).vertice) :: !(hh.(i).solucion)
			else ();
			actualiza_phi g hh.(i) it; 
			let q = Random.float 1. in
			let te = tau_eta g tau hh i it in 
			if q < Conf.q0 then(
				let m = max_et te in
				try
					for j = 0 to g.orden-1 do
						if m = j && not(List.mem m !(hh.(i).solucion)) then(
							nv := j;
							raise (Break)
						)else ()
					done;
				with Break -> ()
			)else(
				let sum = ref 0. in
				Array.iter (fun x -> sum := !sum +. x) te;
				try
					
						for j = 0 to g.orden-1 do
							let r = Random.float 1. in
(*							Printf.printf "%f %f %f\n%!" r (te.(j) /. !sum) !sum;*)
							if r <= (te.(j) /. !sum) && not(List.mem j !(hh.(i).solucion)) then(
								nv := j;
								raise Break
							)else()
						done
					
				with Break -> hh.(i).vertice := !nv
			)
		)else ()
	done;()
	
let actualiza_feromona_local g tau =
	for i = 0 to g.orden-1 do
		tau.(i) <- (1. -. Conf.phi) *. tau.(i) +. Conf.phi *. Conf.tau0
	done;()
	
let actualiza_feromona_global g tau v1 =
	for i = 0 to g.orden-1 do
		if List.mem i v1 then 
			tau.(i) <- (1. -. Conf.rho) *. tau.(i) +. (1. /.(float_of_int(List.length v1))) *. Conf.rho
		else
			tau.(i) <- (1. -. Conf.phi) *. tau.(i) 
	done;()

let mejor g hh =
	let min_l = ref [] in
	let min_i = ref g.orden in
	for i = 0 to Conf.n_hormigas - 1 do
		let n = (List.length !(hh.(i).solucion)) in
		if n <= !min_i then
			(min_l := !(hh.(i).solucion); min_i := n) 
		else()
	done;
	!min_l 
	
let todos_terminaron hh =
	let total = ref 0 in
	for k = 0 to Conf.n_hormigas-1 do
		total := !total + !(hh.(k).sin_cubrir)
	done;
	!total = 0


let genera_grafica n =
	let g = Grafica.initgraf n in
	let x = ref 0 in
	for i = 0 to n - 1 do
		let y = ref (Random.int n) in
		while (Grafica.conectados g !x !y) || (!x = !y) do
			y := Random.int n;
		done;
		Grafica.conecta g !x !y;
		x := !y
	done;
	for i = 1 to (5 * n) do
		let a = Random.int n in
		let b = Random.int n in
		if not (Grafica.conectados g a b) then
			Grafica.conecta g a b
		else()
	done;
	g
	
(**
funcion principal de la aplicacion
*)
let () =
	inicializa (int_of_string Sys.argv.(1));
(*	let g = lee_grafica Sys.argv.(2) in*)
	let g = genera_grafica (int_of_string Sys.argv.(2))in
	let hormigas = init_hormigas g Conf.n_hormigas in
	let tau = Array.make g.orden Conf.tau0 in
	let general = ref [] in
	let general_l = ref (g.orden) in
	for j = 1 to Conf.n_ciclos do
		Printf.printf "%d\n%!" j; 
		posiciona_hormigas g.orden hormigas;
		while not (todos_terminaron hormigas) do
			siguiente_vertice g hormigas tau j;
			actualiza_feromona_local g tau;
		done;
		let v1 = mejor g hormigas in
		if (List.length v1) < !(general_l) then(
			general_l := (List.length v1);
			Array.iter (fun x -> Printf.printf "l:%d\n" (List.length !(x.solucion))) hormigas;
			general := v1
		)else (); 
		actualiza_feromona_global g tau v1;
(*		Array.iter (fun x -> Printf.printf "%d\n" (List.length !(x.solucion))) hormigas;*)
		for k = 0 to Conf.n_hormigas-1 do
			hormigas.(k).sin_cubrir := !(g.tamano);
			hormigas.(k).solucion := []
		done;
	done;
	Printf.printf "ACO:\n%!";
	List.iter  (fun x -> Printf.printf "%d %!" x) !general;
	Printf.printf "\nl:%d\n%!" (List.length !general);
	To_neato.guarda g !general "grafica.gv";
	Greedy.greedy g
	

