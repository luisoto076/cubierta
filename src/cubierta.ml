(**Modulo principal que implementa la heristica de aceptacion por umbrales*)

open Grafica
open Greedy
open To_dot

(**Exepcion para emular la insruccion para salir de un ciclo*)
exception Break

exception OpcionIncorrecta of string

type hormiga = {solucion: int list ref ; vertice: int ref; aristas : int array array; sin_cubrir: int ref}

(**
	Inicializa el arreglo de hormigas relacionadas con la grafica que se pasa como argumento
	@param g grafica
	@param n numero de hormigas que se crearan
	@return arreglo de hormigas de tamano n
*)
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

(**
	obtiene la solucion conformada por la menor cantidad de vertices
	@param g grafica
	@param hh arreglo de hormigas
*)
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
	
(**
	indica si todas las hormigas terminaron de construir su cubierta
	@param hh arreglo de hormigas
	@return verdadero si todas las soliciones son cubiertas de la grafica
*)
let todos_terminaron hh =
	let total = ref 0 in
	for k = 0 to Conf.n_hormigas-1 do
		total := !total + !(hh.(k).sin_cubrir)
	done;
	!total = 0


	
(**
funcion principal de la aplicacion
*)
let () =
	let grafica = ref {aristas = [||]; orden = 0; tamano = ref 0} in 
	if Sys.argv.(1) = "-g" then (
		grafica := genera_grafica (int_of_string Sys.argv.(3)) (int_of_string Sys.argv.(4));
	)else (
		if Sys.argv.(1) = "-r" then (
			grafica := lee_grafica Sys.argv.(3);
		)else(
			raise (OpcionIncorrecta (Sys.argv.(3)))
		)
	);
	Random.init (int_of_string Sys.argv.(2));
	let g = !grafica in
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
			general := v1
		)else (); 
		actualiza_feromona_global g tau v1;
		for k = 0 to Conf.n_hormigas-1 do
			hormigas.(k).sin_cubrir := !(g.tamano);
			hormigas.(k).solucion := []
		done;
	done;
	Printf.printf "ACO:\n%!";
	List.iter  (fun x -> Printf.printf "%d %!" x) !general;
	Printf.printf "\nl:%d\n%!" (List.length !general);
	To_dot.guarda g !general ("grafica.gv");
	

