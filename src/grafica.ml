(**
Modulo para representar graficas de ciudades
*)

open Str

(**Exepcion para indicar errores en las operaciones de la grafica*)
exception ErrorGrafica of string

    		   
type grafica = {
				aristas: bool array array;
				orden: int;
				tamano: int ref
			   }


(**funcion que inicializa la grafica. Las graficas se representan como un arreglo de referencias
  a listas. Cada entrada del arreglo representa un verice y cada lista contiene las adyacencias
  del nodo
  @param initgraf numero de vertices de la grafica
  *)  
let initgraf n =
	let a = Array.make_matrix n n false in
	{aristas=a; orden = n; tamano = ref 0}

(**
Valida si es posible conectar dos nodos o si ya existe una arista que los une en esa direccion
@param ht hashtable que contiene vecinos
@param e el elemento a buscar
*)                   
let conectados g u v = g.aristas.(u).(v)
                
(**
conecta dos nodos de una grafica. Al conecta los nodos i y j se agrega a la entrada i del arreglo
el par (j,p) donde p es el peso de la arista
@param a verice a conectar
@param b vertice a conectar
@param c peso de la arista que conecta a a y b
*)                
let conecta g a b = let n = g.orden in

                      if (a < 0 || a > n || b < 0 || b > n)
                      	then raise(ErrorGrafica "Se ingreso un indice inexistente")
                        else
                        	let v1 = not (conectados g a b) in
                        	let v2 = not (conectados g b a) in
                        	if v1 && v2 then(
                        		g.tamano := !(g.tamano) + 1;
                        		g.aristas.(a).(b) <- true;
                        		g.aristas.(b).(a) <- true;
                        	)else
                        		raise(ErrorGrafica "Vertices ya conectados")
                            
(**
	Genera una grafica con aristas aleatorias, con la semilla y el tamanio que se pasan como
	argumento
	@param m semilla
	@param n numero de vertices de la grafica
	@return grafica construida
*)
let genera_grafica m n =
	Random.init m;
	let g = initgraf n in
	let x = ref 0 in
	for i = 0 to n - 1 do
		let y = ref (Random.int n) in
		while (!x = !y) || (conectados g !x !y)   do
			y := Random.int n;
		done;
		conecta g !x !y;
		x := !y
	done;
	for i = 1 to n + (n/4) do
		let a = Random.int n in
		let b = Random.int n in
		if not (conectados g a b) && (a <> b) then
			conecta g a b
		else()
	done;
	g


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
    	g := initgraf n;
    	let re = Str.regexp "[ ]*,[ ]*" in
    	while true do
    		let arista = Str.split re (input_line ic) in
    		let line1 = List.nth arista 0 in
    		let line2 = List.nth arista 1 in
    		let u = int_of_string line1 in
    		let v = int_of_string line2 in
    		conecta !g u v
    	done;
    	!g
    with
    	|End_of_file -> close_in ic; !g           
  	 	|e -> close_in_noerr ic;raise e

let print g =
	for i = 0 to g.orden-1 do
		for j = 0 to g.orden-1 do
			if (g.aristas.(i).(j)) then
				Printf.printf ("%d %d\n%!") i j
			else ()
		done
	done

