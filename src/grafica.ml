(**
Modulo para representar graficas de ciudades
*)

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
funcion para agregar una ciudad a la grafica
@param g grafica a la que se agregara la ciudad
@param v ciudad que se agregara
*)
(*let agrega g v = g.orden := !(g.orden) + 1; g.vertices.(!(g.orden)) <- v*)

(**
Valida si es posible conectar dos nodos o si ya existe una arista que los une en esa direccion
@param ht hashtable que contiene vecinos
@param e el elemento a buscar
*)                   
let conectados g u v = g.aristas.(u).(v)

(**
regresa el peso de la arsita entre dos nodos a partir del indice de ambos
@param g grafica
@param u verice
@param v verice
*)
let getPeso g u v = g.aristas.(u).(v)
                
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
                            

