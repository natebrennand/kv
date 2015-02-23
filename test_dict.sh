#!/bin/sh

ocamlc -c dictionary.ml
ocamlc dictionary.cmo dict_test.ml -o dict_tester
./dict_tester

