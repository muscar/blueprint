all: frontend main.ml
	ocamlc ast.cmo lexer.cmo parser.cmo main.ml -o blueprint

frontend: parser lexer

parser: ast.ml parser.mly
	ocamlc -c ast.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli parser.ml

lexer: lexer.mll
	ocamllex lexer.mll
	ocamlc -c lexer.ml

clean:
	rm -f parser.ml parser.mli lexer.ml
	rm -f *.cm[iox]
	rm -f blueprint
	rm -f *~
