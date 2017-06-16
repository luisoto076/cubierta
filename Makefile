RESULT = cubierta
SOURCES   = src/grafica.ml src/conf.ml src/greedy.ml src/to_dot.ml src/cubierta.ml 
DOC_FILES = src/grafica.ml src/greedy.ml src/to_dot.ml src/cubierta.ml
LIBS = str
INCDIRS = +str
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
