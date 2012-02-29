type pieza = Empty | Rojo | Verde | Azul;;
type grua = Grua of (int * int)*pieza;;
type mundo = Mundo of grua * pieza array array;;
type accion = Arriba | Abajo | Izquierda | Derecha | Agarrar | Soltar;;
type plan = Plan of accion list;;
		
let matrix_copy matrix1 =
	let rec aux matrix1 matrix2 i =
		if i <0 then
			matrix2
		else
			(matrix2.(i) <- Array.copy matrix1.(i);
			aux matrix1 matrix2 (i-1))
in
	let matrix2 = Array.make_matrix (Array.length matrix1) (Array.length matrix1.(1)) matrix1.(0).(0) in
		aux matrix1 matrix2 ((Array.length matrix1)-1);;	

(*Mundo -> ((int * int) * pieza) * pieza array array *)
let ejecutar_accion mundo = function
	Arriba -> (match mundo with
		Mundo (Grua ((x,y),p),m) -> if x <> 0 then 
				(true, Mundo (Grua ((x-1, y),p),(matrix_copy m)))
				else(false, mundo))
	| Abajo -> (match mundo with
       		Mundo (Grua ((x,y),p),m) -> if x<>2 && ((p = Empty && m.(x).(y) = Empty) || (p <> Empty && m.(x+1).(y) = Empty)) then 
       				(true, Mundo (Grua ((x+1, y),p),(matrix_copy m)))
				else(false, mundo))
	| Izquierda -> (match mundo with
       		Mundo (Grua ((x,y),p),m) -> if (x = 0 && y <> 0) || (y<>0 && ((p <> Empty && m.(x).(y-1) = Empty) || (p = Empty && m.(x-1).(y-1) = Empty))) then 
       				(true, Mundo (Grua ((x, y-1),p),(matrix_copy m)))
				else(false, mundo))
	| Derecha -> (match mundo with
       		Mundo (Grua ((x,y),p),m) -> if (x = 0 && y <> 2) || (y<>2 && ((p <> Empty && m.(x).(y+1) = Empty) || (p = Empty && m.(x-1).(y+1) = Empty))) then 
       				(true, Mundo (Grua ((x, y+1),p),(matrix_copy m)))
				else(false, mundo))
	| Agarrar -> (match mundo with
		Mundo (Grua ((x,y),p),m) -> if m.(x).(y) <> Empty && p = Empty then 
				let n = matrix_copy m in
					n.(x).(y) <- Empty;
					(true, Mundo (Grua ((x,y),m.(x).(y)),n))
				else (false, mundo))
	| Soltar -> (match mundo with
		Mundo (Grua ((x,y),p),m) -> if m.(x).(y) = Empty && p <> Empty && (x = 2 || m.(x+1).(y) <> Empty) then 
				let n = matrix_copy m in
					n.(x).(y) <- p;
					(true, Mundo (Grua ((x,y),Empty),n))
				else (false, mundo));;

let acc = [Arriba;Abajo;Izquierda;Derecha;Agarrar;Soltar];;

let construir_plan mi mf = 
	let rec aux = function
		  (_,[],siguientes, visitados) -> aux (acc, siguientes, [], visitados)
		| ([],_::tm, siguientes,visitados) -> aux (acc,tm,siguientes,visitados)
		| (a::ta,(m,path)::tm,siguientes,visitados) -> 
			let (valido,mr) = ejecutar_accion m a in
				if valido then
				if mr = mf then
					Plan (List.rev (a::path))
				else
				if List.mem mr visitados then
					aux (ta, (m,path)::tm, siguientes, visitados)
				else
					aux (ta, (m,path)::tm, (mr,a::path)::siguientes, mr::visitados)
				else
					aux (ta, (m,path)::tm, siguientes, visitados) 
		in
			if mi = mf then
				Plan []
			else
				aux (acc, [(mi,[])],[],[mi]);;
         
let escribir_accion = function
	Arriba -> print_endline "Arriba"
	| Abajo -> print_endline "Abajo"
	| Izquierda -> print_endline "Izquierda"
	| Derecha -> print_endline "Derecha"
	| Agarrar -> print_endline "Coger"
	| Soltar -> print_endline "Soltar";;
	
let rec escribir_plan = function
	Plan [] -> ()
	| Plan (h::t) -> (escribir_accion h; 
			escribir_plan (Plan t));;                              
	                               
let mundo_of_string cad=
	let rec aux2 mundo i k=
		if i>=0 then
			let rec aux mundo j k= 
				if j >=0 then 
				match String.get cad k with
				'0' -> (mundo.(j).(i) <- Empty; aux mundo (j-1) (k-1))
				|'1' -> (mundo.(j).(i) <- Rojo; aux mundo (j-1) (k-1))
				|'2' -> (mundo.(j).(i) <- Verde; aux mundo (j-1) (k-1))
				|'3' -> (mundo.(j).(i) <- Azul; aux mundo (j-1) (k-1))
				| _ -> raise(Invalid_argument "mundo_of_string")
				else
					aux2 mundo (i-1) k
			in aux mundo ((Array.length mundo.(i))-1) k
		else mundo	
	in 
		let mundo = Array.make_matrix 3 3 Empty in
			aux2 mundo ((Array.length mundo)-1) 8;;                      
                                
let construir_mundo () =
        let mi = Mundo(Grua((0,0),Empty),(mundo_of_string Sys.argv.(1))) in
             let mf = Mundo(Grua((0,0),Empty),(mundo_of_string Sys.argv.(2))) in (mi,mf);;
             
let (mundoi,mundof) = construir_mundo ();;

let plan = construir_plan mundoi mundof;;

escribir_plan plan;;
                                               
