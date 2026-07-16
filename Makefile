all:
	ocamlyacc lib/parser.mly
	rm -rf lib/parser.mli
	ocamllex lib/lexer.mll
	ocamlc -I lib -for-pack row_union lib/ast.ml lib/parser.ml lib/lexer.ml lib/typing.ml lib/eval.ml -pack -o row_union.cmo
	ocamlc -I . row_union.cmo bin/main.ml -o row_union
	rm -rf lib/parser.ml lib/lexer.ml bin/*.cm* lib/*.cm* *.cm*
run: all
	./row_union example/test.row

test:
	ocamlyacc lib/parser.mly
	rm -rf lib/parser.mli
	ocamllex lib/lexer.mll
	ocamlc -I lib -for-pack row_union lib/ast.ml lib/parser.ml lib/lexer.ml lib/typing.ml lib/eval.ml -pack -o row_union.cmo
	ocamlc -I . -I test row_union.cmo test/test_typing.ml test/test_parse.ml -o row_union_test
	rm -rf lib/parser.ml lib/lexer.ml lib/*.cm* *.cm* test/*.cm*
	./row_union_test

web:
	dune build web/app.js

.PHONY: test web
clean:
	rm -rf lib/parser.ml lib/lexer.ml bin/*.cm* lib/*.cm* test/*.cm* *.cm* row_union row_union_test _build
