open Ast
open Print

(******************************************************************************************************************)
(*************************************************** EXCEPTIONS ***************************************************)
(******************************************************************************************************************)


(* Exceptions related to declarations *)
exception Declaration_InvalidType of string
exception Declaration_IllegalDeclarationOfKeyWord of string

(* Exceptions related to instructions *)
exception Instruction_IllegalAffectation of string
exception Instruction_InvalidCondition of string

(* Exceptions related to expressions *)
exception Expression_InvalidType of string
exception Expression_InvalidCast of string
exception Expression_UndefinedVariable of string
exception Expression_UndefinedClass of string
exception Expression_IllegalAccessToProtectedAttribute of string
exception Expression_UseOfClassAsId of string

(* Exceptions related to classes *)
exception Class_MultipleDefinitions of string
exception Class_NotUniqueConstructor of string
exception Class_InvalidParameterType of string
exception Class_InvalidExtends of string
exception Class_HeritageLoop of string
exception Class_DifferentParameterLists of string
exception Class_CallingSuperConstructorButNoSuperClass of string
exception Class_InvalidParametersForSuperConstructor of string
exception Class_NotCallingSuperConstructor of string
exception Class_DifferentNames of string
exception Class_NonExistingAttribute of string
exception Class_NonExistingMethod of string
exception Class_IllegalDeclarationOfKeyWord of string

(* Exceptions related to objects *)
exception Object_ConstructorForbidden of string

(* Exceptions related to methods *)
exception Method_InvalidCall of string
exception Method_InvalidReturnType of string
exception Method_NotOverriding of string
exception Method_MultipleDefinitions of string
exception Method_InvalidOverride of string
exception Method_MissingOverride of string
exception Method_InvalidParameterType of string

(* Exceptions related to attributes *)
exception Attribute_MultipleDefinitions of string
exception Attribute_InvalidSelection of string


(***************************************************************************************************************)
(*************************************************** GETTERS ***************************************************)
(***************************************************************************************************************)


(*
  Returns direct parent of given class
  If there's not, raise exception
*)
let get_direct_parent_class (c : _class) env =
  match c.extends with
    Some p -> (try Some (Hashtbl.find env p) with Not_found -> raise (Class_InvalidExtends (c.name ^ " extends from " ^ p ^ " which is a non-existant class")))
  | None -> None

(*
  Returns a list of constructors for the given class or object body
*)
let get_constructor (body : attrMeth list) =
  List.filter (
    fun am ->
      match am with
        Method m -> m.isConstructor
      | _ -> false
  ) body

(*
  Gets an attribute of class and raise an error if not found
  Doesn't look in parent classes
*)
let get_attribute_in_class (c : _class) attr =
  let attributes = List.filter (
    fun am ->
      match am with
        Attribute a -> a.declName = attr
      | _ -> false
  ) c.body in
  if List.length attributes = 1 then (match List.hd attributes with Attribute a -> a | _ -> assert false) (* Attribute is a declaration, we can directly return a decl *)
  else if List.length attributes = 0 then raise (Class_NonExistingAttribute ("in " ^ c.name ^ ": no match for attribute " ^ attr))
  else raise (Attribute_MultipleDefinitions ("in " ^ c.name ^ ": attribute " ^ attr ^ " defined several times"))


(*
  Gets an attribute of class and its parents classes and raise an error if not found
*)
let rec get_attribute (c : _class) attr env =
  try get_attribute_in_class c attr
  with Class_NonExistingAttribute s -> (
    try (
      let superClass = get_direct_parent_class c env in
      match superClass with
        Some s -> get_attribute s attr env
      | None -> raise (Class_NonExistingAttribute s)
    )
    with Class_InvalidExtends _ -> raise (Class_NonExistingAttribute s)
  )

(*
  Gets method of class and raise an error if not found
  Doesn't look in parent classes
*)
let get_method_in_class (c : _class) meth =
  let methods = List.filter (
    fun am ->
      match am with
        Method m -> m.name = meth && (not m.isConstructor)
      | _ -> false
  ) c.body in
  if List.length methods = 1 then (match List.hd methods with Method m -> m | _ -> assert false)
  else if List.length methods = 0 then raise (Class_NonExistingMethod ("in " ^ c.name ^ ": no match for method " ^ meth))
  else raise (Method_MultipleDefinitions ("in " ^ c.name ^ ": method " ^ meth ^ " defined several times"))

(*
  Gets method of class and its parents classes and raise an error if not found
*)
let rec get_method (c : _class) meth env =
  try get_method_in_class c meth
  with Class_NonExistingMethod s -> (
    try (
      let superClass = get_direct_parent_class c env in
      match superClass with
        Some s -> get_method s meth env
      | None -> raise (Class_NonExistingMethod s)
    )
    with Class_InvalidExtends _ -> raise (Class_NonExistingMethod s)
  )


(****************************************************************************************************************)
(*************************************************** CHECKERS ***************************************************)
(****************************************************************************************************************)


(*
  Returns true if id is in given expression and false otherwise
*)
let rec is_id_in_expr id (e : expr) =
  match e with
    Id x -> x = id
	| Plus (g, d) | Minus (g, d) | Times (g, d) | Div (g, d) | Concat (g, d) | Comp (g, _, d) -> is_id_in_expr id g || is_id_in_expr id d
  | Cast (castType, expr) -> is_id_in_expr id expr
  | Select (expr, nom) -> nom = id || is_id_in_expr id expr
  | Instance (nomClasse, expr_list) -> List.fold_left (fun acc e -> acc || is_id_in_expr id e) false expr_list
  | Message (expr, nomMethode, expr_list) -> is_id_in_expr id expr || List.fold_left (fun acc e -> acc || is_id_in_expr id e) false expr_list
  | _ -> false

(*
  Checks if casting current class to another one (parent) is possible
*)
let rec check_cast_doable current parent env =
  try (
    let c = Hashtbl.find env current in
    if current = parent then true
    else
      match c.extends with
        Some e -> if e = parent then true else check_cast_doable e parent env
      | None -> false
  )
  with Not_found -> false

(*
  List of things to verify in expressions:
    - Variables are declared (in current environment)
    - Arithmetic operations are done on integers
    - Comparison are done on integers
    - Concatenation are done on strings
    - Casts are done from child to parent existing class
    - Selection are done on existing objects or classes (so expr must return a class or an object) and attribute selected exists
    - Instanciation class name exists and given parameters list corresponds to required ones
    - Message are done on existing objects or classes (so expr must return a class or an object), method called exists for expression type and parameters corresponds to required ones
*)
let rec expr_verif (e : expr) envName env =
  match e with
    Id x -> (* Second if is trying to know if x starts with an uppercase *)
    if not (Hashtbl.mem env x) then raise (Expression_UndefinedVariable ("in " ^ envName ^ ", expression " ^ string_of_expr e ^ " undefined variable " ^ x));
    if (Hashtbl.find env x).isClass && 65 <= (int_of_char (String.get x 0)) && (int_of_char (String.get x 0)) <= 90 then raise (Expression_UseOfClassAsId ("in " ^ envName ^ ", expression " ^ string_of_expr e ^ ", " ^ x ^ " is a class, not an instance"))
  | Cste i -> ()
  | StringCste s -> ()
  | Plus (g, d) | Minus (g, d) | Times (g, d) | Div (g, d) ->
    expr_verif g envName env; expr_verif d envName env;
    let type1 = get_expr_type g env in
    let type2 = get_expr_type d env in
    if type1 <> type2 || type1 <> "Integer" then raise (Expression_InvalidType ("in " ^ envName ^ ", expression " ^ string_of_expr e ^ " arithmetic operation on " ^ type1 ^ " and " ^ type2) )
  | Concat (g, d) ->
    expr_verif g envName env; expr_verif d envName env;
    let type1 = get_expr_type g env in
    let type2 = get_expr_type d env in
    if type1 <> type2 || type1 <> "String" then raise (Expression_InvalidType ("in " ^ envName ^ ", expression " ^ string_of_expr e ^ " concatenation of " ^ type1 ^ " and " ^ type2) )
  | Comp (g, opComp, d) ->
    expr_verif g envName env; expr_verif d envName env;
    let type1 = get_expr_type g env in
    let type2 = get_expr_type d env in
    if type1 <> type2 || type1 <> "Integer" then raise (Expression_InvalidType ("in " ^ envName ^ ", expression " ^ string_of_expr e ^ " comparison of " ^ type1 ^ " and " ^ type2) )
  | Cast (castType, expr) ->
    expr_verif expr envName env;
    let type1 = get_expr_type expr env in
    if not (check_cast_doable type1 castType env) then raise (Expression_InvalidCast ("in " ^ envName ^ ", expression " ^ string_of_expr e ^ " trying to cast " ^ type1 ^ " in " ^ castType ^ " is impossible"))
  | Select (expr, name) ->
    (* As attributes are protected, we can only select an attribute in a method of its class or one of its subclasses *)
    expr_verif expr envName env;
    let type1 = get_expr_type expr env in
    if type1 = "void" then raise (Attribute_InvalidSelection ("Cannot select attribute " ^ name ^ " on type void"))
    else
    if Hashtbl.mem env "this" then ( (* If we are in a class *)
      (* Check that we are in a subclass of type1 *)
      if not (check_cast_doable (Hashtbl.find env "this").name type1 env)
      then raise (Expression_IllegalAccessToProtectedAttribute ("in " ^ envName ^ ", expression " ^ string_of_expr e ^ ": selection of protected attribute on " ^ type1 ^ " is illegal, " ^ (Hashtbl.find env "this").name ^ " is not a subclass of " ^ type1))
      else
      (try ignore(get_attribute (Hashtbl.find env type1) name env) (* Hashtbl.find must not cause any problem because expression has been verified *)
      with Class_NonExistingAttribute _ -> raise (Class_NonExistingAttribute ("in " ^ envName ^ ", expression " ^ string_of_expr e ^ ": selection on " ^ type1 ^ " impossible, attribute " ^ name ^ " does not exist")))
    )
    else (* Cannot select attribute from main block *)
    raise (Expression_IllegalAccessToProtectedAttribute ("in " ^ envName ^ ", expression " ^ string_of_expr e ^ ": selection of protected attribute on " ^ type1 ^ " is illegal from main block"))
  | Instance (className, expr_list) ->
    if not (Hashtbl.mem env className) then raise (Expression_UndefinedClass ("in " ^ envName ^ ", expression " ^ string_of_expr e ^ " undefined class " ^ className))
    else
      let c = Hashtbl.find env className in
      if c.name = "String" || c.name = "Integer" then raise (Expression_InvalidType ("in " ^ envName ^ ", expression " ^ string_of_expr e ^ " " ^ className ^ " can not be instantiated"));
      if not c.isClass then raise (Expression_InvalidType ("in " ^ envName ^ ", expression " ^ string_of_expr e ^ " " ^ className ^ " can not be instantiated because it is a static object"))
      else
        if (List.length expr_list <> List.length c.parameters)
        then raise (Method_InvalidCall ("in " ^ envName ^ ", expression " ^ string_of_expr e ^ " " ^ c.name ^ " expected " ^ string_of_int (List.length c.parameters) ^ " parameters, " ^ string_of_int (List.length expr_list) ^ " given"))
        else List.iter2 (fun expr (_, _, typ) ->
          expr_verif expr envName env;
          if not (check_same_type expr typ envName env)
          then raise (Class_InvalidParameterType ("in " ^ envName ^ ", expression " ^ string_of_expr e ^ " " ^ c.name ^ " expected type " ^ typ ^ ", " ^ get_expr_type expr env ^ " given"))
        ) expr_list c.parameters
  | Message (expr, methodName, expr_list) ->
    expr_verif expr envName env;
    let type1 = get_expr_type expr env in
    if type1 = "void" then raise (Method_InvalidCall ("Cannot call method " ^ methodName ^ " on type void"))
    else
    (try
      let meth = get_method (Hashtbl.find env type1) methodName env in
      if List.length meth.params <> List.length expr_list then raise (Method_InvalidCall ("in " ^ envName ^ ", expression " ^ string_of_expr e ^ " " ^ meth.name ^ " called with not enough parameters: expected " ^ string_of_int (List.length meth.params) ^ " parameters, " ^ string_of_int (List.length expr_list) ^ " given"));
      List.iter2 (
        fun expr (_, _, typ) -> (
          expr_verif expr envName env; (* Checks if expression if well defined *)
          if not (check_same_type expr typ envName env) then raise (Method_InvalidCall ("in " ^ envName ^ ", expression " ^ string_of_expr e ^ " " ^ " when calling " ^ methodName ^ ", " ^ typ ^ " expected but " ^ get_expr_type expr env ^ " given")) (* For each parameter, checks if types are the same *)
        )
      ) expr_list meth.params;
    with Class_NonExistingMethod _ -> raise (Class_NonExistingMethod ("in " ^ envName ^ ", expression " ^ string_of_expr e ^ " when sending message on " ^ type1 ^ " impossible, method " ^ methodName ^ " does not exist"))
    )

(*
  Get expression's type and check if it's the same as the other given type
*)
and check_same_type (e : expr) otherType envName env =
  expr_verif e envName env; check_cast_doable (get_expr_type e env) otherType env

(*
  Get expression's type according to environment
  Does't verify validity, needs to be done before
*)
and get_expr_type (e : expr) env =
  match e with
    Id x -> (Hashtbl.find env x).name
  | Cste i -> "Integer"
  | StringCste s -> "String"
	| Plus (g, d) -> "Integer"
	| Minus (g, d) -> "Integer"
	| Times (g, d) -> "Integer"
	| Div (g, d) -> "Integer"
	| Concat (g, d) -> "String"
	| Comp (g, opComp, d) -> "Integer"
  | Cast (castType, expr) -> castType
  | Select (expr, name) -> let a = get_attribute (Hashtbl.find env (get_expr_type expr env)) name env in a.declType
  | Instance (className, expr_list) -> className
  | Message (expr, methodName, expr_list) -> let m = get_method (Hashtbl.find env (get_expr_type expr env)) methodName env in (match m.returnType with Some r -> r | None -> "void")

(*
  Checks if there is an unique constructor within a class body
*)
let check_number_of_constructors (c : _class) =
  if List.length (get_constructor c.body) <> 1 then raise (Class_NotUniqueConstructor (c.name ^ " has " ^ string_of_int (List.length (get_constructor c.body)) ^ " constructor(s) but only one is expected"))

(*
  Checks if there is no constructor within an object body
*)
let check_no_constructors (c : _class) =
  if List.length (get_constructor c.body) <> 0 then raise (Object_ConstructorForbidden (c.name ^ " has " ^ string_of_int (List.length (get_constructor c.body)) ^ " constructor(s) but an object can not have constructor"))

(*
  Checks if an optional expression returned by a method body has the right type according to declared type
*)
let check_expr_return (e : expr) returnType methName (c : _class) env =
  match returnType with
    Some r ->
      if not (Hashtbl.mem env r) then raise (Method_InvalidReturnType ("in " ^ c.name ^ ", method " ^ methName ^ " is supposed to return " ^ r ^ " but this type does not exist"));
      if not (check_same_type e r ("in " ^ c.name ^ ", method " ^ methName) env) then raise (Method_InvalidReturnType ("in " ^ c.name ^ ", method " ^ methName ^ " is supposed to return " ^ r ^ " but expression has type " ^ get_expr_type e env))
  | None -> raise (Method_InvalidReturnType ("in " ^ c.name ^ ", method " ^ methName ^ " is a single expression method but does not have a return type"))

(*
  Checks if an optional block returned by a method body has the right type according to declared type
*)
let check_bloc_return (li : instr list) returnType methName (c : _class) env =
  match returnType with (* A return type is precised *)
    Some r -> (
      if not (Hashtbl.mem env r) then raise (Method_InvalidReturnType ("in " ^ c.name ^ ", method " ^ methName ^ " is supposed to return " ^ r ^ " but this type does not exist"));
      if List.length li = 0 then raise (Method_InvalidReturnType ("in " ^ c.name ^ ", method " ^ methName ^ " is supposed to return " ^ r ^ " but body is empty"));
      List.iter (
        fun i ->
          match i with
            Return e -> (
              match e with
                Some e -> if not (check_same_type e r ("in " ^ c.name ^ ", method " ^ methName) env) then raise (Method_InvalidReturnType ("in " ^ c.name ^ ", method " ^ methName ^ " is supposed to return " ^ r ^ " but " ^ string_of_expr e ^ " has type " ^ get_expr_type e env))
              | None -> () (* Returns pseudo-variable result, it's unnecessary to verify its type because result has a forced type equals to method's return type *)
            )
          | _ -> ()
      ) li
    )
  | None -> ( (* Method is void *)
    List.iter (
      fun i -> (
        match i with
          Return e -> (
            match e with
              Some e -> raise (Method_InvalidReturnType ("in " ^ c.name ^ ", method " ^ methName ^ " is supposed to be void but an expression has type " ^ get_expr_type e env))
            | None -> ()
          )
        | _ -> ()
      )
    ) li
  )

(*
  Adds var parameters in class body as attributes
*)
let add_var_class_parameters (c : _class) =
  List.iter (
    fun (var, nam, typ) ->
      if var then c.body <- (Attribute { declName = nam; declType = typ; value = None; }) :: c.body;
  ) c.parameters

(*
  Checks if all class parameters types exist and are different from current class name
*)
let check_class_parameters_types (c : _class) env =
  List.iter (
    fun (_, nam, typ) -> (
      if typ = c.name then raise (Class_InvalidParameterType ("in " ^ c.name ^ "'s header parameter " ^ nam ^ " can not be class itself"));
      if not (Hashtbl.mem env typ) then raise (Class_InvalidParameterType ("in " ^ c.name ^ "'s header parameters " ^ nam ^ " has invalid type"))
    )
  ) c.parameters

(*
  Checks if all method parameters types exist
*)
let check_method_parameters_types (m : _method) className env =
  List.iter (
    fun (_, nam, typ) -> (
      if not (Hashtbl.mem env typ) then raise (Method_InvalidParameterType ("in " ^ className ^ ", method " ^ m.name ^  ": parameter " ^ nam ^ " has invalid type"))
    )
  ) m.params

(*
  Checks all extends in an heritage and verify there's no loop, in other words, for a given class name, none of super classes extend current class' name
*)
let rec check_heritage (c : _class) name env =
  match c.extends with
    Some s ->
      if not (Hashtbl.mem env s) then raise (Class_InvalidExtends (c.name ^ " extends from " ^ s ^ " which is a non-existant class"));
      if s = "String" || s = "Integer" then raise (Class_InvalidExtends (c.name ^ " extends from " ^ s ^ " which is not derivable"));
      if not (Hashtbl.find env s).isClass then raise (Class_InvalidExtends (c.name ^ " extends from " ^ s ^ " which is an object"));
      if s = name then raise (Class_HeritageLoop (name ^ " is in its own heritage, " ^ c.name ^ " extends from " ^ name));
      check_heritage (Hashtbl.find env s) name env
  | None -> ()

(*
  Adds "super" variable to the environment
*)
let add_super (c : _class) env =
  match c.extends with
    Some s ->
      let superC = Hashtbl.find env s in
      Hashtbl.add env "super" superC;
  | None -> ()

(*
  Checks if two parameters lists correspond to each other, it means having same lengths and each parameter are identic (var, name and type)
*)
let check_constructor_parameters_lists (c : _class) pl1 pl2 =
  if List.length pl1 <> List.length pl2 then raise (Class_DifferentParameterLists ("in " ^ c.name ^ "'s head, expected " ^ string_of_int (List.length pl2) ^ " parameters, but in constructor " ^ string_of_int (List.length pl1) ^ " parameters given")); (* Different lengths *)
  List.iter2 (
    fun (var1, name1, type1) (var2, name2, type2) -> (* Compares each method's parameters *)
      if not (var1 = var2 && name1 = name2 && type1 = type2) then raise (Class_DifferentParameterLists ("in " ^ c.name ^ "'s constructor, parameter " ^ name1 ^ " is not the same as " ^ name2 ^ " declared in class header"))
  ) pl1 pl2

(*
  Checks if two given methods have the same signature (i.e. same name, parameters and returned type)
*)
let check_same_signature (meth : _method) (superMeth : _method) className =
  if meth.returnType <> superMeth.returnType then raise (Method_InvalidOverride ("in " ^ className ^ ", when overriding method " ^ meth.name ^ ": returns types are different"))
  else if List.length meth.params <> List.length superMeth.params then raise (Method_InvalidOverride ("in " ^ className ^ ", when overriding method " ^ meth.name ^ ": different number of parameters")) (* Different lengths *)
  else
    List.iter2 (
      fun (var1, name1, type1) (var2, name2, type2) -> (* Compares each method's parameters *)
        if not (var1 = var2 && type1 = type2) then raise (Method_InvalidOverride ("in " ^ className ^ ", when overriding method " ^ meth.name ^ ": expected type (" ^ type2 ^ " but " ^ type1 ^ " given"))
    ) meth.params superMeth.params

(*
  Checks if an override is valid, it means there's at least one method in parents classes with same signature (iterate through heritage) and if the keyword override was not written for a method with the same name as a super class
  className is the original class name where verification started, it is used to correctly print error
*)
let rec check_override_validity (meth : _method) className (c : _class) env =
  if meth.override then (
    let superClass = get_direct_parent_class c env in
    match superClass with
      Some s -> (
        try let superMeth = get_method s meth.name env in check_same_signature meth superMeth className
        with Class_NonExistingMethod _ -> raise (Method_NotOverriding ("in " ^ className ^ ", method " ^ meth.name ^ " has override attribute but there is no method with the same name in super class"))
      )
    | None -> raise (Method_NotOverriding ("in " ^ className ^ ", method " ^ meth.name ^ " has override attribute but there is no super class"))
  )
  else (
    let superClass = get_direct_parent_class c env in
    match superClass with
      Some s -> (
        try ignore(get_method s meth.name env); raise (Method_MissingOverride ("in " ^ className ^ ", method " ^ meth.name ^ " has not override attribute but there is a method with the same name in a super class"))
        with Class_NonExistingMethod _ -> ()
      )
    | None -> ()
  )

(*
  Checks if the constructor is valid, i.e. it has the same name as the class, and the same list of parameters
  If its class has a parent, the constructor needs to call parent's one with right parameters
  Checks that var parameters are attributes of the class
*)
let check_constructor (c : _class) env =
  let constructor = List.hd (get_constructor c.body) in (* If we're here, we know for sure there's an unique constructor *)
  match constructor with
    Method constr -> (
      if constr.name <> c.name then raise (Class_DifferentNames (c.name ^ "'s constructor has a different name from its class")); (* Constructor hasn't same as its class *)
      check_constructor_parameters_lists c constr.params c.parameters;
      let superClass = get_direct_parent_class c env in
      match superClass with
        Some s ->
          (match constr.super with (* If class has super, constructor must call parent's one with right parameters *)
            Some (name, el) -> ( (* Name is parents constructor and el is a list of expression for parent's constructor *)
              if name <> s.name then raise (Class_NotCallingSuperConstructor (c.name ^ " extends from " ^ s.name ^ " but is calling " ^ name ^ " super constructor"));
              if List.length el <> List.length s.parameters then raise (Class_InvalidParametersForSuperConstructor ("in " ^ c.name ^ "'s constructor, when calling super constructor, expected " ^ string_of_int (List.length s.parameters) ^ " parameters, " ^ string_of_int (List.length el) ^ " given"));
              List.iter (fun (_, nam, typ) -> Hashtbl.add env nam (Hashtbl.find env typ)) constr.params;
              List.iter2 (
                fun a (_, _, type1) -> (
                  expr_verif a (c.name ^ "'s class constructor") env; (* Checks if expression if well defined *)
                  if not (check_same_type a type1 (c.name ^ "'s class constructor") env) then raise (Class_InvalidParametersForSuperConstructor ("in " ^ c.name ^ "'s constructor, when calling super constructor, type " ^ type1 ^ " but " ^ get_expr_type a env ^ " given")) (* For each parameter, checks if types are the same *)
                )
              ) el s.parameters;
              List.iter (fun (_, nam, _) -> Hashtbl.remove env nam) constr.params (* Clean environment from parameters that have been added *)
            )
          | None -> raise (Class_NotCallingSuperConstructor (c.name ^ " extends from " ^ s.name ^ " but is not calling super constructor")) (* At this point we know there's a super class but its constructor has not been called *)
        )
      | None -> ( (* If there's no superClass, then we must check there's no super constructor called *)
        match constr.super with
          Some _ -> raise (Class_CallingSuperConstructorButNoSuperClass (c.name ^ "'s constructor is calling some super constructor but " ^ c.name ^ " has no super class"))
        | None -> ()
      );
      match constr.bodyBloc with
        Some b ->
          let rec check_illegal_bloc (ld, li) =
            List.iter (fun (d : decl) -> if (d.declName = "result" || d.declName = "super" || d.declName = "this") then raise (Class_IllegalDeclarationOfKeyWord ("in " ^ c.name ^ "'s constructor is using return or result keyword"))) ld;
            let rec check_illegal_instr li =
              match li with
                Return _ -> raise (Class_IllegalDeclarationOfKeyWord ("in " ^ c.name ^ "'s constructor has a return expression"))
              | Bloc b -> check_illegal_bloc b
              | Expr e -> if (is_id_in_expr "return" e || is_id_in_expr "result" e) then raise (Class_IllegalDeclarationOfKeyWord ("in " ^ c.name ^ "'s constructor is using return or result keyword"))
              | Affectation (e1, e2) -> if (is_id_in_expr "return" e1 || is_id_in_expr "result" e1 || is_id_in_expr "return" e2 || is_id_in_expr "result" e2) then raise (Class_IllegalDeclarationOfKeyWord (c.name ^ "'s constructor is using return or result keyword"))
              | Ite (e, i1, i2) -> if (is_id_in_expr "return" e || is_id_in_expr "result" e) then raise (Class_IllegalDeclarationOfKeyWord ("in " ^ c.name ^ "'s constructor is using return or result keyword")); check_illegal_instr i1; check_illegal_instr i2
            in List.iter check_illegal_instr li
          in check_illegal_bloc b
      | _ -> ()
    )
  | _ -> assert false


(**************************************************************************************************************************)
(*************************************************** PREDEFINED CLASSES ***************************************************)
(**************************************************************************************************************************)


(*
  Add predefined classes to list of definitions:
    - String and Integer are two predefined classes
    - String has two methods, print and println and one constructor
    - Integer has one method, toString and one constructor
*)
let add_predefined_classes env = (* add String and Integer classes *)
  let toString = {
    override = false;
    name = "toString";
    params = [];
    returnType = Some "String";
    bodyExpr = Some (StringCste "");
    bodyBloc = None;
    super = None;
    isConstructor = false;
  } in
  let integerConstructor = {
    override = false;
    name = "Integer";
    params = [];
    returnType = Some "Integer";
    bodyExpr = Some (Cste 0);
    bodyBloc = None;
    super = None;
    isConstructor = true;
  } in
  let integerClass = {
    name = "Integer";
    parameters = [];
    extends = None;
    body = [Method toString; Method integerConstructor];
    isClass = true;
  } in

  let print = {
    override = false;
    name = "print";
    params = [];
    returnType = None;
    bodyExpr = None;
    bodyBloc = Some ([], []);
    super = None;
    isConstructor = false;
  } in
  let println = {
    override = false;
    name = "println";
    params = [];
    returnType = None;
    bodyExpr = None;
    bodyBloc = Some ([], []);
    super = None;
    isConstructor = false;
  } in
  let stringConstructor = {
    override = false;
    name = "String";
    params = [];
    returnType = Some "String";
    bodyExpr = None;
    bodyBloc = None;
    super = None;
    isConstructor = true;
  } in
  let stringClass = {
    name = "String";
    parameters = [];
    extends = None;
    body = [Method print; Method println; Method stringConstructor];
    isClass = true;
  } in
  Hashtbl.add env integerClass.name integerClass;
  Hashtbl.add env stringClass.name stringClass;
