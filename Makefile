all:
	ocamlopt unix.cmxa log.ml extensions.ml util.ml tiles.ml world.ml main.ml -o wld2txt
clean: 
	rm *.o *.cmi *.cmx
