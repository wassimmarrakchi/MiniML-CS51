all: Read-eval Testing

Read-eval: miniml.ml
	ocamlbuild miniml.byte

Testing: expr_test.ml evaluation_test.ml
	ocamlbuild expr_test.byte evaluation_test.byte

clean:
	rm -rf _build *.byte
