all: clean netser

netser:
	ocamlbuild -cflag "-g" -libs str -use-ocamlfind -use-menhir -menhir "menhir --explain" netser_main.native netser.cma

toplevel: clean netser
	ocamlfind ocamlmktop -o netser.toplevel -I _build netser.cma -thread -linkpkg -package utop netser_toplevel.ml

test_reader:
	ocamlfind ocamlc -package ocplib-endian,oUnit -linkpkg netser_io_reader.ml test_reader.ml

clean:
	rm -rf _build netser.byte netser.native *.cmi *.cmo
