
dictionary:
	cd .. && ocamlc -c dictionary.ml

tester: dictionary
	mv ../dictionary.cmi ../dictionary.cmo .
	ocamlc dictionary.cmo dict_test.ml -o tester

test: tester
	./tester

clean:
	rm -f *.cmi *.cmo tester

