(**modulo para realizar la conexion a la base de datos*)
exception E of string
open Sqlite3

 
let get o = match o with
			|Some x -> x
			|None -> raise (E "what")


(**
funcion para obtener las entradas de una tabla y aplicar una funcion a cada
fila que regresa como resultado
@param tname nombre de la tabla
@param lister funcion que se aplicara a cada entrada de la tabla
@param data estructura en que se guardan las filas
@param db base de datos   
*)
let get_rows tname lister db =
	let querty = "SELECT * FROM "^ tname ^";" in
	let code = Sqlite3.exec_not_null  db ~cb:lister querty in
	match code with
	|    Sqlite3.Rc.OK -> ();
	|    x -> raise (E (Sqlite3.Rc.to_string x))


(**
abre la conexion a la base de datos
@param db_name nombre de  la base de datos
@return la base de datos 
*)
let open_db_conection db_name =
	let db = Sqlite3.db_open db_name in
		db

(**
Cierra la conexion a la base de datos
@param db base de datos que se cerrara
*)
let close_db_conection db =
	if Sqlite3.db_close db
		then print_endline "All done.\n"
      	else print_endline "Cannot close database.\n"
      	
