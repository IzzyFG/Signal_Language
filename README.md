# Signal Language

## Requirements

- The test environment was Ubuntu 22.04.1 LTS on WSL2 and macOS Moneterey 12.5
- The following ubuntu packages were installed:
  1. opam 2.1.2
  2. llvm-runtime 1:14.0-55~exp2
- The follwing opam packages were installed:
  1. ocaml 4.13.1
  2. ocamlbuild 0.14.2
  3. llvm 14.0.6

## How to run the Signal Compiler

1. Clone the repo

    ```console
    git clone https://github.com/IzzyFG/PLT_Signal
    ```

2. Install the required packages

    ```console
    #Ubuntu
    $ add-apt-repository ppa:avsm/ppa
    $ apt update
    $ apt install opam llvm-runtime 1:14.0-55~exp2 
    ```

3. Initialize opam in your home directory
  
    ```console
    $ opam init
    $ eval $(opam env)
    $ opam switch create 4.14.0
    $ eval $(opam env)
    $ opam install llvm.14.0.6 ocamlbuild.0.14.2
    ```

4. Then go into the Compiler directory

    ```console
    cd PLT_Signal
    ```

## Running the compiler

1. Build the Signal compiler

    ```console
    make all
    ```

2. Run our entrypoint script

    ```console
    sh signal.sh testcode/example # sh signal.sh filename
    ```

3. Clean generated files

    ```console
    make clean
    ```

## Testing

```console
sh test_suite.sh
```

## File Structures

### Compiler files

- `ast.ml`: abstract syntax tree (AST) definition
- `scanner.mll`: scanner
- `signalparse.mly`: parser
- `sast.ml`: definition of the semantically-checked AST
- `semant.ml`: semantic checking
- `irgen.ml`: LLVM IR code generator

### Other files

- `README.md`: file containing important details regarding the Signal language
- `Makefile`: file to build the executable and to clean byproduct files
- `signal.sh`: entrypoint script that compiles signal code and runs it
- `testcode`: folder containing testcode
- `test_suite.sh1: file to run all tests at once
- `test1.ml`: the file to test the scanner and parser
- `test2.ml`: the file to test the semantic checker
- `signal.ml`: top-level file to test and run microc compiler
- `example.sgl`: a sample signal source code
- `example_fail.sgl`: a sample signal source code that should fail and throw an error
- `example.out`: a sample compiled code of example.sgl & example_fail.sgl

## Signal Features

- function declerations
- functions with or without parameters
- variable declerations
- variable assignments
- print statements for multiple variable types
- for loops
- while loops
- if-else statements
- list (arrays)
