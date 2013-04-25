all: utils common frontend ir analysis backend toplevel
	ocamlc utils.cmo symtab.cmo ast.cmo lexer.cmo parser.cmo ir.cmo analysis.cmo emit.cmo main.cmo -o blueprint

toplevel: frontend main.ml
	ocamlc -c main.ml

utils: utils.ml symtab.mli symtab.ml
	ocamlc -c utils.ml
	ocamlc -c symtab.mli symtab.ml

common: common.ml
	ocamlc -c common.ml

frontend: utils parser lexer

parser: ast.ml parser.mly
	ocamlc -c ast.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli parser.ml

lexer: lexer.mll
	ocamllex lexer.mll
	ocamlc -c lexer.ml

ir: utils ir.ml
	ocamlc -c ir.ml

analysis: utils analysis.ml
	ocamlc -c analysis.ml

backend: utils frontend emit.ml
	ocamlc -c emit.ml

linecount:
	cat `find . -name "*.ml*"` | wc -l

clean:
	rm -f parser.ml parser.mli lexer.ml
	rm -f *.cm[iox]
	rm -f blueprint
	rm -f *~
