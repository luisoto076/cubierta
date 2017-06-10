open Printf
open Grafica

(*nombre del archivo que depende de el origen y destino de la ruta a calcular*)
let file s = "grafica"^s^".svg"


(*La costante pi*)
let pi = 4.0 *. atan 1.0

(*Esqueleto que regresa una cadena para construir una linea en svg, recive las cordenadas de 
  los puntos de inicio y fin de la linea y el color
 *)
let creaLinea x1 y1 x2 y2 color = "<line x1=\""^x1^"\" y1=\""^y1^"\" x2=\""^x2^"\" y2=\""^y2^"\" stroke=\""^color^"\" stroke-width=\"2\" />\n"


(*Esqueleto que regresa una cadena para construir un circulo en svg, recive las cordenadas del
  centro y el color. El radio es fijo
 *)
let creaCirculo x y color = "<circle cx=\""^x^"\" cy=\""^y^"\" r=\"10\" stroke=\"black\" stroke-width=\"2\" fill=\""^color^"\" />\n"

(*Esqueleto que regresa una cadena para construir un componente de texto, recive las cordenadas del 
  centro del area de texto, el color del mismo y el contenido.
 *)
let creaTexto x y color txt = "<text x=\""^x^"\" y=\""^y^"\" fill=\""^color^"\" font-family=\"sans-serif\" font-size=\"9\" text-anchor=\"middle\">"^txt^"</text>\n"

(*Regresa el encabezado del svg, recive el tamaño de una lado de la imagen, la imagen siempre
  sera cuadrada*)
let encabezado h =  "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n<!DOCTYPE svg>\n<svg height=\"" ^h^ "\" width=\""^h^"\">\n"


(*función que contruye el contenido del svg a partir de la informacion de la grafica y la lista de 
  vertices que indica la ruta mas corta*)                       
let svg g l = let n = g.orden in
	let theta = (2.0*.pi)/.(float_of_int n) in
	let r = ((float_of_int n)*.(30.0+.(10.0*.(((float_of_int n)/.10.0)))))/.pi in
	let h = (2.0 *. r) +. 40.0 in
	let cadena = ref (encabezado (string_of_float h)) in
	for i = 0 to n-1 do
		let x1 = r *. (cos (theta *. (float_of_int i))) +. r +. 20.0 in
		let y1 = r *. (sin (theta *. (float_of_int i))) +. r +. 20.0 in
    	for j = 0 to n-1 do
    		if Grafica.conectados g i j then(
    			let x2 = r *. (cos (theta *. (float_of_int j))) +. r +. 20.0 in
    			let y2 = r *. (sin (theta *. (float_of_int j))) +. r +. 20.0 in
    			cadena := !cadena ^ (creaLinea (string_of_float x1) (string_of_float y1) (string_of_float x2) (string_of_float y2) "black")
    		)else()
    	done	
	done;
	for i = 0 to n-1 do
		let color = ref "" in
		if List.mem i l then color := "yellow" else color := "white";
		let x3 = r *. (cos (theta *. (float_of_int i))) +. r +. 20.0 in
		let y3 = r *. (sin (theta *. (float_of_int i))) +. r +. 20.0 in
		cadena := !cadena ^(creaCirculo (string_of_float x3) (string_of_float y3) !color)
	done;
	!cadena
               
(*funcion que guarda en el archivo indicado el svg*)
let guarda g l s = 
  (* Write message to file *)
  let oc = open_out (file s)  in    (* create or truncate file, return channel *)
  fprintf oc "%s\n</svg>" (svg g l);   (* write something *)   
  close_out oc
