(**Modulo para operar las soluciones*)

open Grafica

(**funcion para inicializar el generador de numeros seudo aleatorios con la semilla*)
let inicializa v = Random.init v

type solucion = {mutable ruta: int array ; ant1: int ref ; ant2: int ref ; max_dist_path: float ; avg_path:float; fs:float ref}



(**
Funcion que inicializa la distancia promedio y distancia maxima entre dos ciudades
en la solucion
*)	
let init_max_avg g s = 
	let n = Array.length s in
	let maxs = ref 0.0 in
	let suma = ref 0.0 in
	let k = ref 0 in
	for i = 0 to n-2 do
		for j = i+1 to n-1 do
			let d = Grafica.getPeso g s.(i) s.(j) in
			if not (d = 0.0) then(
				suma:= !suma +. d;
				k:=!k+1;
				if !maxs<d then (
					maxs:=d
				)
			)else () 
		done 
	done;
	(!suma /. (float_of_int !k),!maxs)

(**funcion de costo de la heuristica*)	
let finit g s =
	let total = ref 0.0 in
	let n = Array.length s.ruta in
	let prom = s.avg_path *. float_of_int (n-1) in
	for i = 0 to n-2 do
		let peso = Grafica.getPeso g s.ruta.(i) s.ruta.(i+1) in
		if not (peso = 0.) then
			total := !total +. peso 
		else
			total:=!total +. (s.max_dist_path *. Conf.c);
	done;
	let fs = !total/.prom in
	s.fs := fs

let init g s =
	let par = init_max_avg g s in
	let sol = {ruta = s; ant1 = ref 0; ant2= ref 0; max_dist_path = snd par; avg_path = fst par; fs = ref 0.} in
	finit g sol;
	sol
	

(**funcion que calcula el "vecino" de una solucion
La funcion solo realiza un intercambio de valores y guarda los indices de los
valores intervambiados para poder volver a la solucion de que se partio
@param solucion a la que se le calculara el vecino*)
let vecino s =
	let n = (Array.length s.ruta) in
	s.ant1 := Random.int n; 
	s.ant2 := Random.int n; 
	let aux = s.ruta.(!(s.ant1)) in
	s.ruta.(!(s.ant1)) <- s.ruta.(!(s.ant2));
	s.ruta.(!(s.ant2)) <- aux
	
(**funcion que regresa el cambio hecho por vecino
Solo funciona para una anterior*)
let regresa s =
	let aux = s.ruta.(!(s.ant1)) in
	s.ruta.(!(s.ant1)) <- s.ruta.(!(s.ant2));
	s.ruta.(!(s.ant2)) <- aux
	
	
(**funcion para conseguir una permutacion aleatoria de la instancia inicializa
y usarla como solucion inicial de la heuristica*)	
let permuta s =
	for i = 0 to (Array.length s.ruta)/2 do
		vecino s
	done



(**funcion de costo de la heuristica*)	
let f g s =
	let total = ref 0.0 in
	let n = Array.length s.ruta in
	let prom = s.avg_path *. float_of_int (n-1) in
	let a1 = ref (-1.) in
	let a2 = ref (-1.) in
	let a3 = ref (-1.) in
	let a4 = ref (-1.) in
	let b1 = ref (-1.) in
	let b2 = ref (-1.) in
	let b3 = ref (-1.) in
	let b4 = ref (-1.) in
	if ((!(s.ant1) = 0) && (!(s.ant2) = (n-1)))  then(
		a1 := Grafica.getPeso g s.ruta.(!(s.ant2)) s.ruta.(!(s.ant1)+1);
		a4 := Grafica.getPeso g s.ruta.(!(s.ant1)) s.ruta.(!(s.ant2)-1);
		b2 := Grafica.getPeso g s.ruta.(!(s.ant2)) s.ruta.(!(s.ant2)-1);
		b3 := Grafica.getPeso g s.ruta.(!(s.ant1)) s.ruta.(!(s.ant1)+1)
	)else( if ((!(s.ant1) = (n-1))  &&  (!(s.ant1) = 0) )then(
			a2 := Grafica.getPeso g s.ruta.(!(s.ant2)) s.ruta.(!(s.ant1)-1);
			a3 := Grafica.getPeso g s.ruta.(!(s.ant1)) s.ruta.(!(s.ant2)+1);
			b1 := Grafica.getPeso g s.ruta.(!(s.ant2)) s.ruta.(!(s.ant2)+1);
			b4 := Grafica.getPeso g s.ruta.(!(s.ant1)) s.ruta.(!(s.ant1)-1)
		)else(
			a1 := Grafica.getPeso g s.ruta.(!(s.ant2)) s.ruta.(!(s.ant1)+1);
			a2 := Grafica.getPeso g s.ruta.(!(s.ant2)) s.ruta.(!(s.ant1)-1);
			a3 := Grafica.getPeso g s.ruta.(!(s.ant1)) s.ruta.(!(s.ant2)+1);
			a4 := Grafica.getPeso g s.ruta.(!(s.ant1)) s.ruta.(!(s.ant2)-1);
			b1 := Grafica.getPeso g s.ruta.(!(s.ant2)) s.ruta.(!(s.ant2)+1);
			b2 := Grafica.getPeso g s.ruta.(!(s.ant2)) s.ruta.(!(s.ant2)-1);
			b3 := Grafica.getPeso g s.ruta.(!(s.ant1)) s.ruta.(!(s.ant1)+1);
			b4 := Grafica.getPeso g s.ruta.(!(s.ant1)) s.ruta.(!(s.ant1)-1)
		)
	);
	if !a1 = 0. then(
		a1 := (s.max_dist_path *. Conf.c)
	)else(
		a1 := 0.
	);
	if !a2 = 0. then(
		a2 := (s.max_dist_path *. Conf.c)
	)else(
		a2 := 0.
	);
	if !a3 = 0. then(
		a3 := (s.max_dist_path *. Conf.c)
	)else(
		a3 := 0.
	);
	if !a4 = 0. then(
		a4 := (s.max_dist_path *. Conf.c)
	)else(
		a4 := 0.
	);
	if !b1 = 0. then(
		b1 := (s.max_dist_path *. Conf.c)
	)else(
		b1 := 0.
	);
	if !b2 = 0. then(
		b2 := (s.max_dist_path *. Conf.c)
	)else(
		b2 := 0.
	);
	if !b3 = 0. then(
		b3 := (s.max_dist_path *. Conf.c)
	)else(
		b3 := 0.
	);
	if !b4 = 0. then(
		b4 := (s.max_dist_path *. Conf.c)
	)else(
		b4 := 0.
	);
	let auxr = ((!a1 +. !a2 +. !a3 +. !a4)/.prom) in
	let auxs = ((!b1 +. !b2 +. !b3 +. !b4)/.prom) +. !(s.fs)in
	s.fs:= auxs +. !(s.fs) -. auxr;
	!(s.fs)


(**funcion que indica si una solucion es factible y el numero de desconexiones 
presenta*)
let factible g s =
	let n = Array.length s.ruta in
	let desc = ref 0 in
	let b = ref true in
	for i = 0 to n-2 do
		if (Grafica.conectados g s.ruta.(i) s.ruta.(i+1)) then () else (b:=false;desc:=!desc+1)
	done;
	(!desc,!b)


                   
