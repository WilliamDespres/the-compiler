open Ast
open Misc

(****************************************************************************************************************)
(*************************************************** PRE-WORK ***************************************************)
(****************************************************************************************************************)


(*
  This type is contained in a Hashtbl
  It allows, for a specific class, to know:
    - How much storage is needed for it
    - Its attributes deltas
    - Its methods deltas
    - Labels assiociated to methods
*)
type classInfo = {
  space: int;
  deltasAttr: (string, int) Hashtbl.t;
  deltasMeth: (string, int) Hashtbl.t;
  labelsMeth: (string, string) Hashtbl.t;
}

(*
  As many memory as number of attributes.
  Attributes are stored as references to other objects.
  Store attributes deltas.
  Store methods deltas.
  Generate methods labels.
*)
let compute_class_infos (c : _class) superClassName superClass =
  let nbAttr = ref 0 in
  let nbMeth = ref 0 in
  List.iter (
    fun am ->
      match am with
        Attribute _ -> nbAttr := !nbAttr + 1
      | Method m -> if not m.override then nbMeth := !nbMeth + 1 (* We will add super methods so if it's an override, it's counted in super class *)
  ) c.body;

  let attrDelta = ref 1 in (* Attribute delta number: starts at 1 because delta = 0 is reserved for virtual table *)
  let methDelta = if c.isClass then ref 1 else ref 0 in (* Method delta number: starts at 1 because delta = 0 is reserved for class constructor, if c is an object, no need to reserve delta = 0 *)

  let myDeltasAttr = Hashtbl.create (!nbAttr + superClass.space) in
  Hashtbl.iter ( (* Add deltas for super attributes, a specific attribute has same delta as in super class *)
    fun k v ->
      Hashtbl.add myDeltasAttr k v; attrDelta := !attrDelta + 1
  ) superClass.deltasAttr;
  List.iter ( (* Then, add delta for current class attributes *)
    fun am ->
      match am with
        Attribute a -> Hashtbl.add myDeltasAttr a.declName !attrDelta; attrDelta := !attrDelta + 1
      | _ -> ()
  ) c.body;

  let myDeltasMeths = Hashtbl.create (!nbMeth + (Hashtbl.length superClass.deltasMeth) - 1) in (* -1 because super constructor is not considered *)
  let myLabelsMeths = Hashtbl.create (!nbMeth + (Hashtbl.length superClass.deltasMeth) - 1) in (* -1 because super constructor is not considered *)
  Hashtbl.iter ( (* Add deltas for super methods, a specific method has same delta as in super class *)
    fun k v ->
      if k <> superClassName then (Hashtbl.add myDeltasMeths k v; Hashtbl.add myLabelsMeths k (Hashtbl.find superClass.labelsMeth k); methDelta := !methDelta + 1) (* k <> superClassName is to avoid copying super constructor *)
  ) superClass.deltasMeth;
  List.iter ( (* Then, add delta for current class methods *)
    fun am ->
      match am with
        Method m ->
        if m.isConstructor then (Hashtbl.add myDeltasMeths m.name 0; Hashtbl.add myLabelsMeths m.name (c.name ^ "_" ^ m.name))
        else if not m.override then (Hashtbl.add myDeltasMeths m.name !methDelta; Hashtbl.add myLabelsMeths m.name (c.name ^ "_" ^ m.name); methDelta := !methDelta + 1)
        else ( (* Then it overrides, meaning it has been added as super method, we just replace its label *)
          Hashtbl.replace myLabelsMeths m.name (c.name ^ "_" ^ m.name)
        )
      | _ -> ()
  ) c.body; (* + 1 just bellow is because we save one space for virtual table *)
  { space = !nbAttr + Hashtbl.length superClass.deltasAttr + 1; deltasAttr = myDeltasAttr; deltasMeth = myDeltasMeths; labelsMeth = myLabelsMeths }

(*
  classesInfo allows to know how much space is needed for any class, but it's also used to know each attribute's and method's delta, also methods labels
  classesInfo type: (string, classInfo) Hashtbl.t

  To know needed space of class C, you do: (Hashtbl.find classesInfo "C").space
  To know attribute x delta in C, you do: Hashtbl.find (Hashtbl.find classesInfo "C").deltasAttr "x"
  To know method f delta in C, you do: Hashtbl.find (Hashtbl.find classesInfo "C").deltasMeth "f"
  To know method f label in C, you do: Hashtbl.find (Hashtbl.find classesInfo "C").labelsMeth "f"

  To computed each class's needed memory, this is how we'll do:
    - We created to Hash Tables, one to store memory (space), the other (computed) to know if a class's needed space has been already computed
    - At the begining, all classes are not yet computed (except Integer and String)
    - Integer and String needed spaces to 1
    - While we haven't computed all memories spaces:
      - For all none already computed bindings in env:
        - Has it a super?:
          - Yes, super's memory needing is already known:
            - Yes, compute this class space
            - No, do nothing
          - No, compute this class space
*)
let preprocess_classes_infos env =
  let classesInfo = Hashtbl.create (Hashtbl.length env) in
  let computed = Hashtbl.create (Hashtbl.length env) in
  let nbComputed = ref 2 in

  Hashtbl.iter (fun k _ -> Hashtbl.add computed k false) env;

  (* Create the right methods deltas and labels for Integer and String *)
  let intDeltasMeth = Hashtbl.create 1 in Hashtbl.add intDeltasMeth "toString" 0;
  let intLabelsMeth = Hashtbl.create 1 in Hashtbl.add intLabelsMeth "toString" "Integer_toString";
  Hashtbl.add classesInfo "Integer" { space = 1; deltasAttr = Hashtbl.create 0; deltasMeth = intDeltasMeth; labelsMeth = intLabelsMeth };

  let stringDeltasMeth = Hashtbl.create 1 in  Hashtbl.add stringDeltasMeth "print" 0; Hashtbl.add stringDeltasMeth "println" 1;
  let stringLabelsMeth = Hashtbl.create 1 in Hashtbl.add stringLabelsMeth "print" "String_print"; Hashtbl.add stringLabelsMeth "println" "String_println";
  Hashtbl.add classesInfo "String" { space = 1; deltasAttr = Hashtbl.create 0; deltasMeth = stringDeltasMeth; labelsMeth = stringLabelsMeth };

  (* Note they have been computed *)
  Hashtbl.replace computed "Integer" true; Hashtbl.replace computed "String" true;

  while !nbComputed <> Hashtbl.length env do
    Hashtbl.iter (
      fun k v -> (
        if not (Hashtbl.find computed k) then ( (* If not already computed *)
          match v.extends with
            Some s -> ( (* It has a super *)
              if Hashtbl.find computed s then ( (* If super's memory needing is already known *)
                Hashtbl.add classesInfo k (compute_class_infos v s (Hashtbl.find classesInfo s)); (* Storage for class attributes + its parents ones *)
                Hashtbl.replace computed k true;
                nbComputed := !nbComputed + 1
              )
            )
          | None -> (
            Hashtbl.add classesInfo k (compute_class_infos v "" { space = 0; deltasAttr = Hashtbl.create 0; deltasMeth = Hashtbl.create 0; labelsMeth = Hashtbl.create 0 }); (* Storage for class attributes *)
            Hashtbl.replace computed k true;
            nbComputed := !nbComputed + 1
          )
        )
      )
    ) env
  done; classesInfo


(****************************************************************************************************************)
(*************************************************** COMPILER ***************************************************)
(****************************************************************************************************************)


(* Output file *)
let chan = open_out "compiled"

(* These two hashtables store the delta with GP (FP, respectively) of virtual tables and global (local, resp.) variables *)
let globalDeltas = Hashtbl.create 16 (* We don't know how much space is required for now *)
let localDeltas = Hashtbl.create 16 (* We don't know how much space is required for now *)

let vtDeltaCounter = ref 0 (* Used to know how many virtual tables exist (there are at the pile's bottom and don't count in global variables) *)
let globalDeltaCounter = ref 0 (* Used to allocate to each global variable a position from GP pointer *)
let localDeltaCounter = ref 0 (* Used to allocate to each local variable a position from FP pointer *)

(*
  Generates the code for an expression
  For Ids, we search in local variables first, and then in global variables
  When the expression is a message or an instantiation, it is managed by the function call_method
*)
let rec compile_expr e env classesInfo =
  match e with
    Id x -> (
    try output_string chan ("PUSHL " ^ string_of_int (Hashtbl.find localDeltas x) ^ "\n")
    with Not_found -> output_string chan ("PUSHG " ^ string_of_int (Hashtbl.find globalDeltas x) ^ "\n")
  )
  | Cste c -> output_string chan ("PUSHI " ^ string_of_int c ^ "\n")
  | StringCste s -> output_string chan ("PUSHS \"" ^ s ^ "\"\n")
  | Plus (g, d) ->
    compile_expr g env classesInfo;
    compile_expr d env classesInfo;
    output_string chan "ADD\n"
  | Minus (g, d) ->
    compile_expr g env classesInfo;
    compile_expr d env classesInfo;
    output_string chan "SUB\n"
  | Times (g, d) ->
    compile_expr g env classesInfo;
    compile_expr d env classesInfo;
    output_string chan "MUL\n"
  | Div (g, d) ->
    compile_expr g env classesInfo;
    compile_expr d env classesInfo;
    output_string chan "DIV\n"
  | Concat (g, d) ->
    compile_expr g env classesInfo;
    compile_expr d env classesInfo;
    output_string chan "CONCAT\n"
  | Comp (g, opComp, d) ->
    compile_expr g env classesInfo;
    compile_expr d env classesInfo;
    (match opComp with
      Eq -> output_string chan "EQUAL\n"
    | Neq -> output_string chan "EQUAL\nNOT\n"
    | Lt -> output_string chan "INF\n"
    | Le -> output_string chan "INFEQ\n"
    | Gt -> output_string chan "SUP\n"
    | Ge -> output_string chan "SUPEQ\n")
  | Cast (castType, expr) ->
    compile_expr expr env classesInfo;
    output_string chan ("DUPN 1\n PUSHG " ^ string_of_int (Hashtbl.find globalDeltas ("VT_" ^ castType)) ^ "\nSTORE 0 -- Cast " ^ get_expr_type expr env ^ " -> " ^ castType ^ " so storing " ^ castType ^ "'s Virtual Table address' at delta = 0\n")
  | Select (expr, name) -> compile_expr expr env classesInfo; output_string chan ("LOAD " ^ string_of_int (Hashtbl.find (Hashtbl.find classesInfo (get_expr_type expr env)).deltasAttr name) ^ "\n")
  | Instance _
  | Message _ -> call_method e env classesInfo

(*
  Generates the code to : 
  - prepare the environment (this, result, arguments)
  - call a function (method or constructor)
  - and clean the environment afterwards
*)
and call_method e env classesInfo =
  match e with
    Message (receiver, methodName, expr_list) -> (
    let receiver_type = get_expr_type receiver env in
    let c = Hashtbl.find env receiver_type in
    let meth = get_method c methodName env in
    
    (** Prepare the pile for calling **)
    (* Store arguments in right order *)
    compile_method_env meth expr_list env classesInfo;

    (* Store result address *)
    output_string chan "PUSHI 0 -- Reserve space for result\n";

    (* Store receiver address *)
    compile_expr receiver env classesInfo;

    (** Call method: Push its address and jump to it **)
    (match receiver with
      Id "super" -> ( (* Super is static *)
        output_string chan ("PUSHG " ^ string_of_int (Hashtbl.find globalDeltas ("VT_" ^ receiver_type)) ^ "\n");
        output_string chan ("LOAD " ^ string_of_int (Hashtbl.find (Hashtbl.find classesInfo receiver_type).deltasMeth methodName) ^ " -- Pushing the right method label according to the virtual table\n")
      )
    | _ -> ( (* All other are dynamic *)
      match receiver_type with
        "Integer" | "String" ->
        output_string chan ("PUSHG " ^ string_of_int (Hashtbl.find globalDeltas ("VT_" ^ receiver_type)) ^ "\n");
        output_string chan ("LOAD " ^ string_of_int (Hashtbl.find (Hashtbl.find classesInfo receiver_type).deltasMeth methodName) ^ " -- Pushing the right method label according to the virtual table\n")
      | _ -> output_string chan ("DUPN 1\nLOAD 0\nLOAD " ^ string_of_int (Hashtbl.find (Hashtbl.find classesInfo receiver_type).deltasMeth methodName) ^ " -- Pushing the right method label according to the virtual table\n")
      )
    );
    output_string chan "CALL\n";

    (** Clean the pile after call is finished **)
    (* Delete receiver address *)
    output_string chan "-- Leaving result on top of the pile\nPOPN 1\n"; 
    (* Delete each parameter and keep the result on top of the pile by swapping them each time *)
    clean_method_env meth expr_list env classesInfo
  )
  | Instance (className, expr_list) -> (
    let c = Hashtbl.find env className in
    let meth = List.hd (get_constructor c.body) in

    match meth with
      Method meth -> (
      (** Prepare the pile for calling **)
      (* Store arguments in right order *)
      compile_method_env meth expr_list env classesInfo;

      (* Store result and "this" addresses (which are the same) *)
      output_string chan ("ALLOC " ^ string_of_int (Hashtbl.find classesInfo className).space ^ " -- Creating space for an instance of " ^ className ^ "\n");
      output_string chan "DUPN 1\n";

      (* Storing virtual table in the new instance *)
      output_string chan ("DUPN 1\nPUSHG " ^ string_of_int (Hashtbl.find globalDeltas ("VT_" ^ className)) ^ "\nSTORE 0 -- Storing Virtual Table address' at delta = 0\n");

      (** Call method: Push its address and jump to it **)
      output_string chan ("DUPN 1\nLOAD 0\nLOAD 0 -- Pushing the constructor\n");
      output_string chan "CALL\n";

      (** Clean the pile after call is finished **)
      (* Delete receiver address *)
      output_string chan "POPN 1\n";
      (* Delete each parameter and keep the result on top of the pile by swapping them each time *)
      clean_method_env meth expr_list env classesInfo
    )
    | _ -> assert false
  )
  | _ -> assert false

(*
  When entrying a method call or compilation, add parameters in local deltas and environment
  If expr_list is empty, it means this method has been called when compiling the method body
  Else, it means this method has been called when calling the method
  We add a prefix "param_" to avoid parameters overriding local or global variables
*)
and compile_method_env m expr_list env classesInfo =
  let paramDelta = ref (-2 - List.length m.params) in
  match expr_list with 
    [] -> (
      List.iter (fun (isVar, paramName, paramType) ->
        Hashtbl.add localDeltas paramName !paramDelta;
        Hashtbl.add env paramName (Hashtbl.find env paramType);
        paramDelta := !paramDelta + 1
      ) m.params
    )
  | _ -> (
    List.iter2 (fun e (isVar, paramName, paramType) ->
      (* Push each expression on the pile, and remember param name and delta *)
      compile_expr e env classesInfo;
      Hashtbl.add localDeltas ("param_" ^ paramName) !paramDelta;
      paramDelta := !paramDelta + 1
    ) expr_list m.params
  )

(*
  When leaving a method call or compilation, remove parameters from local deltas and environment
  If expr_list is empty, it means this method has been called when compiling the method body
  Else, it means this method has been called when calling the method, so we also POP attributes from the pile to only let the method result at the top of the pile
*)
and clean_method_env m expr_list env classesInfo =
  match expr_list with 
    [] -> (
      List.iter (fun (isVar, paramName, paramType) ->
        Hashtbl.remove localDeltas paramName;
        Hashtbl.remove env paramName;
      ) m.params
    )
  | _ -> (
    List.iter2 (fun e (isVar, paramName, paramType) ->
      output_string chan ("SWAP\n" ^ "POPN 1\n");
      Hashtbl.remove localDeltas ("param_" ^ paramName)
    ) expr_list m.params
  )

(*
  Compiles a local declaration, add it to environment and store it at the rigth fp delta
*)
let compile_local_decl (d : decl) env classesInfo =
  match d.value with
    Some e -> (
      output_string chan "PUSHI 0 -- Leaving space for decl\n";
      compile_expr e env classesInfo;
      Hashtbl.add localDeltas d.declName !localDeltaCounter;
      Hashtbl.add env d.declName (Hashtbl.find env d.declType);
      output_string chan ("STOREL " ^ string_of_int !localDeltaCounter ^ "\n");
      localDeltaCounter := !localDeltaCounter + 1
    )
  | None -> (
    output_string chan "PUSHI 0 -- Leaving space for decl\n";
    Hashtbl.add localDeltas d.declName !localDeltaCounter;
    Hashtbl.add env d.declName (Hashtbl.find env d.declType);
    localDeltaCounter := !localDeltaCounter + 1
  )

(*
  Compiles a global declaration, add it to environment and store it at the rigth gp delta
*)
and compile_global_decl (d : decl) env classesInfo =
  match d.value with
    Some e -> (
      compile_expr e env classesInfo;
      let adr = Hashtbl.find globalDeltas d.declName in
      output_string chan ("STOREG " ^ string_of_int adr ^ "\n")
    )
  | None -> ()

(*
  Compiles intructions
  nthIf is used to count imbricated "if conditions" (starts at one)
  envName is used to correctly label "if" in a specific method of a specific class 
*)
let rec compile_instr i env classesInfo nthIf envName =
  match i with
    Expr e -> compile_expr e env classesInfo
  | Return r -> (
    match r with
      Some s -> compile_expr s env classesInfo; output_string chan "STOREL -2 -- Store result in reserved space\n";
    | None -> ());
    output_string chan "RETURN\n"
  | Bloc b -> compile_local_bloc b env classesInfo (envName ^ "If" ^ string_of_int nthIf) (1)
  | Affectation (Id x, e2) -> (* Affectations can only be done to either an id or a selection *)
    compile_expr e2 env classesInfo;
    (try output_string chan ("STOREL " ^ string_of_int (Hashtbl.find localDeltas x) ^ "\n")
    with Not_found -> output_string chan ("STOREG " ^ string_of_int (Hashtbl.find globalDeltas x) ^ "\n"))
  | Affectation (Select (e1, name), e2) ->
    compile_expr e1 env classesInfo; compile_expr e2 env classesInfo;
    output_string chan ("STORE " ^ string_of_int (Hashtbl.find (Hashtbl.find classesInfo (get_expr_type e1 env)).deltasAttr name) ^ "\n")
  | Affectation (_, _) -> assert false
  | Ite (condition, i1, i2) ->
    compile_expr condition env classesInfo;
    output_string chan "JZ "; output_string chan (envName ^ "Else" ^ string_of_int nthIf ^ "\n");
    compile_instr i1 env classesInfo (nthIf + 1) envName;
    output_string chan "JUMP "; output_string chan (envName ^ "End" ^ string_of_int nthIf ^ "\n");
    output_string chan (envName ^ "Else" ^ string_of_int nthIf ^ ": NOP\n");
    compile_instr i2 env classesInfo (nthIf + 1) envName;
    output_string chan (envName ^ "End" ^ string_of_int nthIf ^ ": NOP\n")

(*
  Compiles a local bloc, compile local declarations, then compile instructions
  LocalDeltaCounter is reset to 0 at the end
*)
and compile_local_bloc (b : bloc) env classesInfo envName nthBloc =
  match b with
    (ldecl, li) ->
      List.iter (fun d -> compile_local_decl d env classesInfo) ldecl;
      List.iteri (fun i instr ->
        match instr with
        | Bloc b2 -> compile_local_bloc b2 env classesInfo envName (nthBloc + 1)
        | _ -> compile_instr instr env classesInfo 1 (envName ^ "Bloc" ^ string_of_int nthBloc ^ "Instr" ^ string_of_int i)
      ) li;
      List.iter (fun decl -> Hashtbl.remove env decl.declName) ldecl;
      localDeltaCounter := 0

(*
  Compiles main bloc, compile global declarations if needed, then compile instructions
*)
let rec compile_main_bloc (b : bloc) env classesInfo nthBloc =
  match b with (ldecl, li) ->
    List.iter (fun decl -> compile_global_decl decl env classesInfo; Hashtbl.add env decl.declName (Hashtbl.find env decl.declType)) ldecl;
    List.iteri (fun i instr ->
      match instr with
        Bloc b2 -> compile_main_bloc b2 env classesInfo (nthBloc + 1)
      | _ -> compile_instr instr env classesInfo 1 ("MainBloc" ^ string_of_int nthBloc ^ "Instr" ^ string_of_int i)
    ) li;
    List.iter (fun decl -> Hashtbl.remove env decl.declName) ldecl

(*
  Compile a method body
  Steps:
    - Put the right label
    - Compile method environment (i.e. associate parameters names with the right delta, last parameter is at fp-3)
    - If it's a constructor:
      - Go through all body and compile attributes' defaults values (if there are)
      - Store all var parameters in the right attribute
      - Call implicitly the super constructor and copy all its attributes
    - If it is an expression method, compile expression and store it in dedicated result place (fp-2)
    - If it is a bloc method, compile local bloc
*)
let compile_method (m : _method) cName classesInfo env =
  output_string chan ((Hashtbl.find (Hashtbl.find classesInfo cName).labelsMeth m.name) ^ ": NOP\n");
  compile_method_env m [] env classesInfo;
  if m.isConstructor then (
    List.iter (
      fun attrMeth ->
      match attrMeth with 
        Attribute a -> (
        match a.value with
          Some e -> (
          output_string chan "PUSHL -1\n";
          compile_expr e env classesInfo;
          output_string chan ("STORE " ^ string_of_int (Hashtbl.find (Hashtbl.find classesInfo cName).deltasAttr a.declName) ^ "\n")
        )
        | None -> ()
      )
      | _ -> ()
    ) (Hashtbl.find env cName).body;
    List.iteri (
      fun i (isVar, pName, pType) ->
        if (isVar) then output_string chan ("PUSHL -1\nPUSHL " ^ string_of_int (i - 2 - (List.length m.params)) ^ " -- Pushing the ith parameter on top of the pile\nSTORE " ^ string_of_int (Hashtbl.find (Hashtbl.find classesInfo cName).deltasAttr pName) ^ " -- Storing parameter to the right delta\n")
    ) m.params;
    (* Call super constructor *)
    match m.super with
      Some (superName, expr_list) -> (
      call_method (Instance (superName, expr_list)) env classesInfo;
      Hashtbl.iter (
        fun attrName attrDelta ->
          output_string chan ("-- Copying super attribute " ^ attrName ^ "\nDUPN 1\nLOAD " ^ string_of_int attrDelta ^ "\nPUSHL -1\nSWAP\nSTORE " ^ string_of_int attrDelta ^ "\n")
      ) ((Hashtbl.find classesInfo superName).deltasAttr);
      output_string chan "POPN 1 -- Deleting new instance created by super constructor\n"
    )
    | None -> ()
  );
  (match m.bodyExpr with
    Some e -> compile_expr e env classesInfo; output_string chan "STOREL -2\nRETURN\n";
  | None -> (
    match m.bodyBloc with
      Some b -> compile_local_bloc b env classesInfo (cName ^ m.name) 1; output_string chan "RETURN\n"
    | None -> () (* We don't assert false as this case is matched for Integer and String constructors *)
  ));
  clean_method_env m [] env classesInfo

(*
  Creates the virtual tables of each classes
  For each class:
    - Alloc as much space as number of methods
    - Duplicate VT adress as many times as number of methods (each store will delete one adress)
    - For each method label:
      - Push label and store it in allocated adress
*)
let create_virtual_tables classesInfo =
  Hashtbl.iter (
    fun cName cInf ->
      Hashtbl.add globalDeltas ("VT_" ^ cName) !vtDeltaCounter; vtDeltaCounter := !vtDeltaCounter + 1;
      output_string chan ("ALLOC " ^ (string_of_int (Hashtbl.length cInf.deltasMeth)) ^ " -- Allocating VT_" ^ cName ^ "\n");
      Hashtbl.iter (
        fun mName mLabel ->
          output_string chan ("DUPN 1\nPUSHA " ^ mLabel ^ "\nSTORE " ^ string_of_int (Hashtbl.find cInf.deltasMeth mName) ^ "\n")
      ) cInf.labelsMeth
  ) classesInfo

(*
  Objects are added as global variables
  Store deltas in globalDeltas Hashtbl and PUSHN enough space for all global variables
*)
let save_global_variables_space (b : bloc) env =
  globalDeltaCounter := !vtDeltaCounter;
  Hashtbl.iter (
    fun cName c ->
      if not c.isClass then (Hashtbl.add globalDeltas cName !globalDeltaCounter; globalDeltaCounter := !globalDeltaCounter + 1)
  ) env;
  let rec sgvs b =
    match b with
      (ldecl, li) ->
        List.iter (fun d -> Hashtbl.add globalDeltas d.declName !globalDeltaCounter; globalDeltaCounter := !globalDeltaCounter + 1) ldecl;
        List.iter (fun i -> match i with Bloc b' -> sgvs b' | _ -> ()) li;
  in sgvs b;
  output_string chan ("PUSHN " ^ string_of_int (!globalDeltaCounter - !vtDeltaCounter) ^ "\n")

(*
  Objects are static, there's an unique instance
  We allocate enough memory to store this unique instance and if some attributes have default value, compile them
*)
let compile_objects classesInfo env =
  Hashtbl.iter (
    fun cName c -> (
      if not c.isClass then (
        output_string chan ("ALLOC " ^ string_of_int (Hashtbl.find classesInfo cName).space ^ " -- Creating space for object " ^ cName ^ "\n");
        output_string chan ("DUPN 1\nPUSHG " ^ string_of_int (Hashtbl.find globalDeltas ("VT_" ^ cName)) ^ "\nSTORE 0 -- Storing Virtual Table address' at delta = 0\n");
        List.iter (
          fun am ->
            match am with
              Attribute a -> (
                match a.value with
                  Some e -> output_string chan ("DUPN 1\n"); compile_expr e env classesInfo; output_string chan ("STORE " ^ string_of_int (Hashtbl.find (Hashtbl.find classesInfo cName).deltasAttr a.declName) ^ " -- Storing attribute " ^ a.declName ^ "\n")
                | None -> ()
              )
            | _ -> ()
        ) c.body;
        let adr = Hashtbl.find globalDeltas cName in
        output_string chan ("STOREG " ^ string_of_int adr ^ "\n")
      )
    )
  ) env

(*
  Compiles unique Integer method toString
*)
let compile_integer c classesInfo env =
  output_string chan ((Hashtbl.find (Hashtbl.find classesInfo "Integer").labelsMeth "toString") ^ ": NOP\n");
  output_string chan "PUSHL -1\nSTR\nSTOREL -2\nRETURN\n"

(*
  Compiles both two String methods which are print and println
*)
let compile_string c classesInfo env =
  output_string chan ((Hashtbl.find (Hashtbl.find classesInfo "String").labelsMeth "println") ^ ": NOP\n");
  output_string chan "PUSHL -1\nWRITES\n";
  compile_expr (StringCste("\\n")) env classesInfo;
  output_string chan "WRITES\nRETURN\n";

  output_string chan ((Hashtbl.find (Hashtbl.find classesInfo "String").labelsMeth "print") ^ ": NOP\n");
  output_string chan "PUSHL -1\nWRITES\nRETURN\n"

(*
  Compiles code for all methods (for objects and classes)
  String and Integer are compiled separatly
*)
let compile_all_methods classesInfo env =
  Hashtbl.iter (
    fun cName c -> (
      match cName with
        "String" -> compile_string c classesInfo env
      | "Integer" -> compile_integer c classesInfo env
      | _ -> (
        Hashtbl.add env "this" c;
        add_super c env;
        List.iter (
          fun am ->
            match am with
              Method m -> compile_method m cName classesInfo env
            | _ -> ()
        ) c.body;
        Hashtbl.remove env "this"; Hashtbl.remove env "super"
      )
    )
  ) env

(*
  Steps:
    - Begin with START
    - Put all virtual tables at the bottom of the pile
    - Save enough space for global variables and objects without compiling affectations
    - Compile objects (they are static so they are implicity created)
    - Put a jump to main bloc
    - Put all methods codes (labeled)
    - Compile main bloc
    - Finish with STOP
*)
let compile_program (b : bloc) env classesInfo =
  (* Never removed or added because it's always true *)
  Hashtbl.add localDeltas "this" (-1);
  Hashtbl.add localDeltas "result" (-2);
  Hashtbl.add localDeltas "super" (-1);

  output_string chan ("START\n");

  output_string chan ("\n-- Virtual tables creation\n");
  create_virtual_tables classesInfo;

  output_string chan ("\n-- Adding global variables to pile\n");
  save_global_variables_space b env;
  output_string chan ("\n-- Allocating objects\n");
  compile_objects classesInfo env;

  output_string chan "JUMP Mainbloc -- Compiled methods must not be executed except with a CALL\n";
  output_string chan ("\n-- Compiling all methods\n");
  compile_all_methods classesInfo env;

  output_string chan ("\n-- Compiling main bloc\n");
  output_string chan "Mainbloc: NOP\n";
  compile_main_bloc b env classesInfo 1;

  output_string chan ("STOP\n");
  flush chan;
  close_out chan
