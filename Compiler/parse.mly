%{
open Ast
open Print
open Verifs
%}
%token <string> ID (* token *)
%token <string> IDCLASS
%token <int> CSTE (* constant *)
%token <string> STRINGCSTE
%token <Ast.opComp> RELOP (* comparaison operators *)
%token PLUS MINUS TIMES DIV (* algebric operators *)
%token UNARY (* unary symbols *)
%token CONCAT (* & *)
%token LPAREN RPAREN (*( )*) LBRACE RBRACE (*{ }*)
%token IF THEN ELSE (* if then else *)
%token SEMICOLON (* ; *)
%token ASSIGN (* := *)
%token COLON (* : *)
%token COMMA (* , *)
%token DOT (* . *)
%token IS VAR EXTENDS DEF OVERRIDE NEW AS CLASS OBJECT RETURN (* reserved tokens *)
%token EOF (* end of file *)

%type <Ast.expr> expr
%type <bool * string * string> param, paramClass
%type <string * (expr list)> superClass
%type <Ast._method> methodOrConstructor
%type <string option * expr option * bloc option> endMethod

%nonassoc RELOP
%left PLUS MINUS CONCAT
%left TIMES DIV
%left UNARY
%left DOT               /* highest precedence */

%start<Ast.program_type> program
%%
program:
    ld = list(_class) b = bloc EOF					    { ld, b } 

bloc:
	  li = delimited(LBRACE, list(instr), RBRACE)	{ ([], li) }
	| decls_instrs = delimited(LBRACE, separated_pair(nonempty_list(decl), IS, nonempty_list(instr)), RBRACE)
                                                { let decls, instrs = decls_instrs in (decls, instrs) }

decl:
		id1 = ID COLON id2 = IDCLASS e = option(assign) SEMICOLON
                                                { { declName = id1; declType = id2; value = e; } }

assign:
    ASSIGN e = expr															{ e }

///////////////////////////////////////////////////////////////////////////////////////////

_class:
		CLASS className = IDCLASS params = delimited(LPAREN, separated_list(COMMA, paramClass), RPAREN)
		ext = option(extends) IS LBRACE lam = list(attrMeth) RBRACE
		                                            {
                                                  {
                                                    name = className;
                                                    parameters = params;
                                                    extends = ext;
                                                    body = lam;
                                                    isClass = true;
                                                  }
                                                }
  | OBJECT objectName = IDCLASS IS lam = delimited(LBRACE, list(attrMeth), RBRACE)
                                                {
                                                  {
                                                    name = objectName;
                                                    parameters = [];
                                                    extends = None;
                                                    body = lam;
                                                    isClass = false;
                                                  }
                                                }

paramClass:
		var = boption(VAR) paramName = ID COLON paramType = IDCLASS
                                                { var, paramName, paramType } // var is a boolean

param:
    paramName = ID COLON paramType = IDCLASS
                                                { false, paramName, paramType } // var is a boolean
extends:
    EXTENDS id = IDCLASS											  { id }

superClass:
	  COLON idSuperClass = IDCLASS params = delimited(LPAREN, separated_list(COMMA, expr), RPAREN)
                                                { idSuperClass, params }

attrMeth:
	  a = attr 																		{ Attribute a }
	| DEF m = methodOrConstructor									{ Method m }

attr:
    VAR v = decl                                { v }

methodOrConstructor:
    o = boption(OVERRIDE) idMeth = ID lparams = delimited(LPAREN, separated_list(COMMA, param), RPAREN)
    em = endMethod                              {
                                                  let (returnedType, e, b) = em in
                                                  {
                                                    override = o;
                                                    name = idMeth;
                                                    params = lparams;
                                                    returnType = returnedType;
                                                    bodyExpr = e;
                                                    bodyBloc = b;
                                                    super = None;
                                                    isConstructor = false;
                                                  }
                                                }
  | idClass = IDCLASS lparams = delimited(LPAREN, separated_list(COMMA, paramClass), RPAREN)
		superClass = option(superClass) IS b = bloc	{
                                                  {
                                                    override = false;
                                                    name = idClass;
                                                    params = lparams;
                                                    returnType = Some idClass;
                                                    bodyExpr = None;
                                                    bodyBloc = Some b;
                                                    super = superClass;
                                                    isConstructor = true;
                                                  }
                                                }

endMethod:
    COLON returnedType = IDCLASS ASSIGN e = expr{ (Some returnedType, Some e, None) }
  | t = option(_type) IS b = bloc               { (t, None, Some b) }

_type:
    COLON returnType = IDCLASS                  { returnType }

///////////////////////////////////////////////////////////////////////////////////////////

expr:
    id = ID                                     { Id id }
  | id = IDCLASS                                { Id id }
  | cste = CSTE                                 { Cste cste }
  | stringCste = STRINGCSTE                     { StringCste stringCste }
  | e = delimited(LPAREN, expr, RPAREN)         { e }
  | LPAREN AS id = IDCLASS COLON e = expr RPAREN{ Cast(id, e) }
	| NEW id = IDCLASS le = delimited(LPAREN, separated_list(COMMA, expr), RPAREN)
                                                { Instance(id, le) }
  | e = expr DOT id = ID                        { Select(e, id) }
  | e = expr DOT id = ID le = delimited(LPAREN, separated_list(COMMA, expr), RPAREN)
                                                { Message(e, id, le) }
  | g = expr op = RELOP d = expr                { Comp(g, op, d) }
  | e1 = expr CONCAT e2 = expr                  { Concat(e1, e2) }
  | e1 = expr PLUS e2 = expr                    { Plus(e1, e2) }
  | e1 = expr MINUS e2 = expr                   { Minus(e1, e2) }
  | e1 = expr TIMES e2 = expr                   { Times(e1, e2) }
  | e1 = expr DIV e2 = expr                     { Div(e1, e2) }
  | MINUS e = expr %prec UNARY                  { Minus(Cste 0, e) }
  | PLUS e = expr %prec UNARY                   { e }

instr:
    e = expr SEMICOLON                          { Expr e }
  | b = bloc                                    { Bloc b }
  | RETURN e = option(expr) SEMICOLON           { Return e }
  | a = assignable ASSIGN e = expr SEMICOLON    { Affectation(a, e) }
  | IF e = expr THEN i1 = instr ELSE i2 = instr { Ite(e, i1, i2) }

assignable:
    id = ID                                     { Id id }
  | e = expr DOT id = ID                        { Select(e, id) }
