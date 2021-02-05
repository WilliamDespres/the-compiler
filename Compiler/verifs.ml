open Ast
open Misc

(*
  List of things to verify in declarations:
    - Declaration of this or result are forbidden
    - Declaration type exists
    - Value (expr), when declared, has the same type as declared
*)
let decl_verif (d : decl) envName env =
  if d.declName = "result" || d.declName = "this" then raise (Declaration_InvalidType ("in " ^ envName ^ ", declaration name " ^ d.declName ^ " is not allowed"));
  if not (Hashtbl.mem env d.declType) then raise (Declaration_InvalidType ("in " ^ envName ^ ", variable " ^ d.declName ^ ", declaration type " ^ d.declType ^ " is not defined"));
  if not (Hashtbl.find env d.declType).isClass then raise (Declaration_InvalidType ("in " ^ envName ^ ", variable " ^ d.declName ^ ", declaration type " ^ d.declType ^ " is impossible because it is an object"));
  match d.value with
    Some e ->
      if not (check_same_type e d.declType envName env) then raise (Declaration_InvalidType ("in " ^ envName ^ ", variable " ^ d.declName ^ " is expecting " ^ d.declType ^ " but " ^ get_expr_type e env ^ " given"))
  | None -> ()

(*
  List of things to verify in instructions:
    - Expression: valid
    - Return expr: expr is valid (more checks are done later)
    - Bloc: verify bloc
    - Affectation: Both expressions are valids and have same type
    - Ite e i1 i2: e is valid and is an Integer, same for i1 and i2
*)
let rec instr_verif (i : instr) envName env =
  match i with
    Expr e -> expr_verif e envName env
  | Return Some e -> expr_verif e envName env (* More verifications are done independantly in return verification *)
  | Return None -> ()
  | Bloc b -> (bloc_verif b envName env)
  | Affectation (e1, e2) -> (
    expr_verif e1 envName env; expr_verif e2 envName env;
    let type1 = get_expr_type e1 env in
    let type2 = get_expr_type e2 env in
    if not (check_same_type e2 type1 envName env) then raise (Instruction_IllegalAffectation ("in " ^ envName ^ ", trying to affect " ^ type2 ^ " to " ^ type1))
  )
  | Ite (e, i1, i2) -> (
    expr_verif e envName env; instr_verif i1 envName env; instr_verif i2 envName env;
    let condType = get_expr_type e env in
    if condType <> "Integer" then raise (Instruction_InvalidCondition ("in " ^ envName ^ ", trying to do a comparison on " ^ condType ^ " but only Integers are comparable"))
  )

(*
  List of things to verify in bloc:
    - All instructions and declarations are valid
*)
and bloc_verif (b : bloc) envName env =
  match b with
    (ldecl, li) ->
      List.iter (fun decl -> decl_verif decl envName env; Hashtbl.add env decl.declName (Hashtbl.find env decl.declType)) ldecl;
      List.iter (fun instr -> instr_verif instr envName env) li;
      List.iter (fun decl -> Hashtbl.remove env decl.declName) ldecl

(*
  List of things to verify in body methods:
    - Expression
    - Bloc:
      - Declarations are valid
      - Instructions are valid
    - Returned type exists
    - Either bloc or expression as body is the same type as return type declared
*)
let meth_body_verif (m : _method) (c : _class) env =
  match m.bodyExpr with
    Some e -> expr_verif e (c.name ^ " class, " ^ m.name ^ " method") env; check_expr_return e m.returnType m.name c env
  | None -> (
    (match m.returnType with
      Some r -> Hashtbl.add env "result" (Hashtbl.find env r) (* Add pseudo variable result to env if necessary *)
    | None -> ());
    (match m.bodyBloc with
      Some (ldecl, li) ->
        List.iter (fun decl -> decl_verif decl (c.name ^ " class, " ^ m.name ^ " method") env; Hashtbl.add env decl.declName (Hashtbl.find env decl.declType)) ldecl;
        List.iter (fun instr -> instr_verif instr (c.name ^ " class, " ^ m.name ^ " method") env) li;
        check_bloc_return li m.returnType m.name c env;
        (* Clean env *)
        List.iter (fun decl -> Hashtbl.remove env decl.declName) ldecl
    | None -> assert false);
    Hashtbl.remove env "result"; (* Remove pseudo variable result to env if is has been added *)
  )

(*
  List of things to verify in body constructor:
    - Same as methods but environment is different
*)
let constr_body_verif (m : _method) (c : _class) env =
  match m.bodyBloc with
    Some (ldecl, li) ->
      List.iter (fun decl -> decl_verif decl (c.name ^ "'s constructor") env; Hashtbl.add env decl.declName (Hashtbl.find env decl.declType)) ldecl;
      List.iter (fun (var, nam, typ) -> if not var then Hashtbl.add env nam (Hashtbl.find env typ)) m.params; (* Add none var parameters in environment (they have already been checked) *)
      List.iter (fun instr -> instr_verif instr (c.name ^ "'s constructor") env) li;
      (* Clean env *)
      List.iter (fun decl -> Hashtbl.remove env decl.declName) ldecl;
      List.iter (fun (_, nam, _) -> Hashtbl.remove env nam) m.params
  | None -> ()

(*
  List of things to verify in methods:
    - Method is unique in its heritage tree
    - Returned type exists
    - Either bloc or expression as body is the same type as return type declared
    - Body is valid
    - Overrides correctly:
      - If overriding's declared, method should be in a class having super ones that already define this method
      - If not, no other method with the same name should exists in super classes
*)
let meth_verif (m : _method) (c : _class) env =
  if not m.isConstructor then (
    ignore(get_method c m.name env); (* get_method will raise an error if there is not a unique method called m.name *)
    check_method_parameters_types m c.name env;
    List.iter (fun (_, nam, typ) -> Hashtbl.add env nam (Hashtbl.find env typ)) m.params; (* Add parameters in environment (they have already been checked) *)
    meth_body_verif m c env;
    check_override_validity m c.name c env;
    List.iter (fun (_, nam, _) -> Hashtbl.remove env nam) m.params
  )
      
(*
  List of things to verify in attributes:
    - Same as declarations
*)
let attr_verif (a : decl) (c : _class) env = 
  ignore(get_attribute_in_class c a.declName); (* get_attribute will raise an error if there is not a unique attribute called a.declName *)
  decl_verif a c.name env  

(*
  List of things to verify in class body:
    - All attributes are well defined
    - All methods are well defined
*)
let class_body_verif (body : attrMeth list) (c : _class) env =
  List.iter (
    fun am ->
      match am with
        Attribute a -> attr_verif a c env
      | _ -> () (* Methods validity is done next *)
  ) body;
  List.iter (
    fun am ->
      match am with
        Method m -> meth_verif m c env
      | _ -> () (* Attributes validity has been done earlier *)
  ) body

(*
  List of things to verify in class and object:
    - Classes names are unique
    - Parameter list is valid (each type exists)
    - Classes have unique constructor, this constructor has the same name and parameters list as its class
    - If a class extends from another, verify this super class exists
    - Extends is valid
*)
let class_verif (c : _class) env =
  Hashtbl.add env "this" c; (* Add this to environment for accessing class attributes and methods *)
  if (c.isClass) then (
    check_number_of_constructors c;
    check_heritage c c.name env; (* Loop through heritage and checks everything's fine *)
    add_super c env;
    check_constructor c env; (* if class extends from another, give super class to check_constructor *)
    constr_body_verif (let c = List.hd (get_constructor c.body) in match c with Method m -> m | _ -> assert false) c env;
  ) else ( (* Object *)
    check_no_constructors c;
  );
  class_body_verif c.body c env; (* This part is common between classes and objects *)
  Hashtbl.remove env "this";
  Hashtbl.remove env "super"

(*
  Convert definition list into an hashtable (to increase computation speed)
  While converting, it verifies there is no duplicates and raise an error if there are
*)
let create_env (lc : _class list) =
  let env = Hashtbl.create (2 + List.length lc) in (* + 2 is because there are 2 default classes: Integer and String *)
  List.iter (
    fun d -> (
      if (Hashtbl.mem env d.name) then raise (Class_MultipleDefinitions d.name)
      else Hashtbl.add env d.name d
    )
  ) lc;
  add_predefined_classes env;
  List.iter (fun c -> check_class_parameters_types c env; add_var_class_parameters c) lc; (* class parameters are verified early to avoid strang errors, also, var parameters are added as class attributes *)
  env

let program_verif (lc : _class list) (b : bloc) =
  let env = create_env lc in (* Add classes and objects definitions to environment *)
  Hashtbl.iter (fun _ c -> class_verif c env) env;
  bloc_verif b "main bloc" env;
  env
