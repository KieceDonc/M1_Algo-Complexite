OUTPUT = projet
FILE = $(OUTPUT).ml
MATRIX_FILE = matrix.ml
MATRICES_FILE = matrices.ml

compile: clean
	ocamlc -o $(OUTPUT) $(MATRIX_FILE) $(MATRICES_FILE) $(FILE)

run: compile
	./$(OUTPUT)

clean:
	rm -f *.cmi *.cmo $(OUTPUT)