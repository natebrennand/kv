
dictionary:
	@cd .. && ocamlc -c dictionary.ml

tester: dictionary
	@mv ../dictionary.{cmi,cmo} .
	@ocamlc dictionary.cmo dict_test.ml -o tester

test: tester
	./tester

