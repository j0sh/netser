all: netser

netser:
	ocamlbuild -cflag "-g" -use-menhir -menhir "menhir --explain" netser.native netser.cma

toplevel: clean netser
	ocamlfind ocamlmktop -o netser.toplevel -I _build netser.cma -thread -linkpkg -package utop netser_toplevel.ml

clean:
	rm -rf _build netser.byte netser.native *.cmi *.cmo
