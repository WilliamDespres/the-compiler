INTERFACES = parse.mli
SOURCES    = ast.ml print.ml misc.ml verifs.ml compil.ml parse.ml lex.ml main.ml
GENERATED  = lex.ml parse.ml parse.mli parse.automaton parse.conflicts

compiler: parse.mli $(SOURCES)
	ocamlc -c ast.ml
	ocamlc $(INTERFACES)
	ocamlc -o compiler $(SOURCES)

lex.ml: lex.mll parse.mli ast.ml
	ocamllex lex.mll

parse.mli : parse.mly ast.ml
	menhir --dump --explain --strict parse.mly

clean:
	rm -rf *.o *.cmi *.cmo *.cmx *~ $(GENERATED)
