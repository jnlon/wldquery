SRC=log.ml extensions.ml util.ml world.ml main.ml
LIB=unix.cmxa
OUT=wldquery

all:
	ocamlopt $(LIB) $(SRC) $(OCAMLPARAM) -o $(OUT)
clean: 
	rm *.o *.cmi *.cmx
