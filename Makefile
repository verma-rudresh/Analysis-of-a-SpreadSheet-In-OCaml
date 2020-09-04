all:
	ocamllex lexer.mll       # generates lexer.ml
	ocamlyacc parser.mly     # generates parser.ml and parser.mli
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c function.ml
	ocamlc -c parser.ml
	ocamlc -c calc.ml
	ocamlc -o calc str.cma function.cmo lexer.cmo parser.cmo calc.cmo

clean:
	rm -rf calc
	rm -rf *.cmo
	rm -rf *.cmi
	rm -rf lexer.ml
	rm -rf parser.ml 
	rm -rf *.mli