(* Comparison operators *)
type opComp = Eq | Neq | Lt | Le | Gt | Ge

(* Expressions (returns value) *)
type expr =
	  Id of string
  | Cste of int
  | StringCste of string
	| Plus of expr * expr
	| Minus of expr * expr
	| Times of expr * expr
  | Div of expr * expr
  | Concat of expr * expr
	| Comp of expr * opComp * expr
  | Cast of string * expr
  | Select of expr * string
  | Instance of string * (expr list)
  | Message of expr * string * (expr list)

(* Variable declaration with potential affectation *)
type decl = {
  declName: string;
  declType: string;
  value: expr option;
}

(* Instructions (no returned value) *)
type instr =
    Expr of expr
  | Return of expr option
  | Bloc of bloc
  | Affectation of expr * expr
  | Ite of expr * instr * instr

(* A bloc is either an instructions list (maybe empty) or a non-empty declarations list followed by a non-empty instructions list *)
and bloc = decl list * instr list

(****************************************************** ClassType ******************************************************)

(*
  A parameter list is a list of tuples of isParameterVar * parameterName * parameterType.
  When isParameterVar is true, it means this parameter can be assigned implicitly (i.e. a "var" stands before parameter).
  It works only if parameter's name is the same as class' attribute.
  When isParameterVar is false, the parameter needs to be explicitly assigned.
*)
type paramsList = (bool * string * string) list

(*
  A class contains a list of either attributes or methodes in a random order.
  An attribute is the same as a declaration.
*)
type attrMeth =
    Attribute of decl
  | Method of _method

(*
  A method can be written in three ways:
    - In either way, it can possibly override another one (bool), has a name (string), then a list of parameters (paramsList).
    Apart from these common things, it then diverges:
      - A return type (string) and an expression (expr). This type is named : SingleExprMethod.
      - An optional return type (string option) and a bloc (bloc). This type is named : BlocMethod.
  A constructor has an identical name as the class one (string), the same class parameter list as the class one.
  If the class extends another one, constructor can optionally call its parent class constructor (string * paramsList).
  Finally, a constructor contains a bloc.
*)
and _method = {
  override: bool;
  name: string;
  params: paramsList;
  returnType: string option;
  bodyExpr: expr option;
  bodyBloc: bloc option;
  super: (string * (expr list)) option;
  isConstructor: bool;
}

(*
  A class has a name (string), a list of class parameter (paramsList) (differing from methode's parameters, class parameters car have "var" in front of them),
  an optional extends class (string option), a constructor and a body.
  - A body is a list of attributes and methods in an undifined order.

  An object is simpler than a class. It only has a name and the same body definition as class one.
*)
type _class = {
  name: string;
  parameters: paramsList;
  extends: string option;
  mutable body: attrMeth list; (* mutable allow us later to add var parameters to body *)
  isClass: bool;
}

(*
  Our program is composed of a list of classes (and objects, which are considered like classes), followed by a main block.
*)
type program_type = _class list * bloc