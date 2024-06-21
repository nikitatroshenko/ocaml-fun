all: hw2

hw2:
	ocamlopt -o hw2 hw2.ml hw2_tests.ml

test: run

run: hw2
	./hw2

clean:
	rm -f hw2*.cmi hw2*.cmx hw2*.o hw2
