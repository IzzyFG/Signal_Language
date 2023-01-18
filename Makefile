# "make all" builds the executable

.PHONY : all

all : signal.native

# "make signal.native" compiles the compiler

signal.native :
	opam exec -- \
	ocamlbuild -pkgs llvm src/signal.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -f *.out *.output
	rm -f testcode/*.out testcode/*.output


