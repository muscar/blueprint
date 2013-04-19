all: frontend toplevel
	ocamlc ast.cmo lexer.cmo parser.cmo main.cmo -o blueprint

toplevel: frontend main.ml
	ocamlc -c main.ml

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
