all: hw1

hw1:
	ocamlopt -o hw1 hw1.ml hw1_tests.ml

test: run

run: hw1
	./hw1

clean:
	rm hw1*.cmi hw1*.cmx hw1*.o hw1
