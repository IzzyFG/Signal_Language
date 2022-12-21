#!/bin/bash    
echo "\n########## Running parser and semant tests ##########\n"
# Testing the parser
ocamlbuild testcode/test1.native && cat testcode/example.sgl | ./test1.native > testcode/example1.out

# Testing the semant checker
ocamlbuild testcode/test2.native && cat testcode/example.sgl | ./test2.native > testcode/example2.out

# Testing the list implementation
ocamlbuild testcode/test2.native && cat testcode/test_lists.sgl | ./test2.native > testcode/test_lists.out

# Testing advanced list implementation
ocamlbuild testcode/test2.native && cat testcode/test_dot_product.sgl | ./test2.native > testcode/test_dot_product.out

# Testing modulo implementation
ocamlbuild testcode/test2.native && cat testcode/example_extraOp.sgl | ./test2.native > testcode/example_extraOp.out

# Running full code
echo "\n########## Running full test example1 ##########\n"
sh signal.sh testcode/example
echo "\n########## Running lists test ##########\n"
sh signal.sh testcode/test_lists
echo "\n########## Running advanced lists test ##########\n"
sh signal.sh testcode/test_dot_product
echo "\n########## Running modulo operator test ##########\n"
sh signal.sh testcode/example_extraOp