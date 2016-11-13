SRC=log.ml extensions.ml util.ml world.ml main.ml
LIB=unix.cmxa
OUT:=wldquery

ifeq ($(OS),Windows_NT)
  OUT := $(OUT).exe
endif

all:
	ocamlopt $(LIB) $(SRC) $(OCAMLPARAM) -o $(OUT)
clean: 
	rm *.o *.cmi *.cmx
