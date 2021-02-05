open Ast

let string_of_relop (op : opComp)  =
  match op with
    Eq -> "="
  | Neq -> "<>"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="

let rec string_of_expr (e : expr) =
  match e with
    Id x -> x
  | Cste i -> string_of_int i
  | StringCste s -> "(\"" ^ s ^ "\")"
	| Plus (g, d) -> "(" ^ string_of_expr g ^ " + " ^ string_of_expr d ^ ")"
	| Minus (g, d) -> "(" ^ string_of_expr g ^ " - " ^ string_of_expr d ^ ")"
	| Times (g, d) -> "(" ^ string_of_expr g ^ " * " ^ string_of_expr d ^ ")"
	| Div (g, d) -> "(" ^ string_of_expr g ^ " / " ^ string_of_expr d ^ ")"
	| Concat (g, d) -> "(" ^ string_of_expr g ^ " & " ^ string_of_expr d ^ ")"
	| Comp (g, opComp, d) -> "(" ^ string_of_expr g ^ " " ^ string_of_relop opComp ^ " " ^ string_of_expr d ^ ")"
  | Cast (castType, expr) -> "(as " ^ castType ^ ": " ^ string_of_expr expr ^ ")"
  | Select (expr, nom) -> "(" ^ string_of_expr expr ^ "." ^ nom ^ ")"
  | Instance (className, expr_list) -> "(new " ^ className ^ "(" ^ List.fold_left (fun acc e -> acc ^ string_of_expr e ^ ", ") "" expr_list ^ "))"
  | Message (expr, methodName, expr_list) -> "(" ^ string_of_expr expr ^ "." ^ methodName ^ "(" ^ List.fold_left (fun acc e -> acc ^ string_of_expr e ^ ", ") "" expr_list ^ "))"

let print_decl (d : decl) =
  print_string (d.declName ^ ": " ^ d.declType);
  match d.value with
    Some e -> print_string " := "; print_string (string_of_expr e)
  | _ -> ()

let rec print_instr (i : instr) =
  match i with
    Expr e ->  print_string (string_of_expr e); print_string ";";
  | Return eo -> (
    match eo with
      Some e -> print_string "return ";  print_string (string_of_expr e); print_string ";"
    | None -> ()
  )
  | Bloc b -> print_bloc b
  | Affectation (e1, e2) -> print_string (string_of_expr e1); print_string " := "; print_string (string_of_expr e2);  print_string ";";
  | Ite (e, i1, i2) -> print_string (string_of_expr e); print_instr i1; print_instr i2

and print_bloc (b : bloc) =
  match b with
    (decls, instrs) -> List.iter (fun d -> print_decl d; print_string "\n") decls; 
                       List.iter (fun i -> print_instr i; print_string "\n") instrs

let print_param (var, name, _type) =
  if (var) then print_string "var " else ();
  print_string (name ^ ": " ^ _type) 

let print_method (m : _method) =
  print_string "DEF ";

  if m.override then print_string "override " else ();
  print_string (m.name ^ " ("); List.iter (fun p -> print_param p; print_string ", ") m.params; print_string ")";
  
  (match m.returnType with
    Some _type -> print_string (" : " ^ _type)
  | None -> ());

  (match m.bodyExpr with
    Some e -> print_string (" := "); print_string (string_of_expr e); print_string "\n"
  | None -> ());

  (match m.super with
    Some (superName, el) -> print_string (" : " ^ superName ^ " ("); List.iter (fun e -> print_string (string_of_expr e); print_string ", ") el; print_endline ") {"
  | _ -> ());

  (match m.bodyBloc with
    Some b -> print_endline " is {"; print_bloc b; print_endline "}"
  | None -> ())

let print_attrMeth (am : attrMeth) =
  match am with
    Attribute d -> print_string "VAR "; print_decl d
  | Method m -> print_method m

let print_class (c : _class) =
  if (c.isClass) then (print_string "Class : ")
  else (print_string "Object : ");

  print_string (c.name ^ "(");
  (* Parametres *)
  List.iter (fun param -> print_param param; print_string ", ") c.parameters;
  print_string ")";
  (* Superclasse *)
  (match c.extends with
    Some superclass -> print_string (" extends " ^ superclass)
  | None -> ());
  
  (* Corps de la classe *)
  print_string " is {\n";
  List.iter (fun attrMeth -> print_attrMeth attrMeth; print_string "\n") c.body;
  print_string "}\n"

let print_program (ld : _class list) (b : bloc) =
  List.iter (fun d -> print_class d) ld;
  print_bloc b