# TheCompiler

Wonderful M1 project written with 2 classmates, that consists in a compiler for an abstract pile machine.


## Usage

Commands to execute in root folder...

### To be able to use the abtsract pile machine:

```
cd AbstractMachina
make
cd ..
```
### To be able to use the compiler:

```
cd Compiler
make
cd ..
```

### To compile files:

```
Compiler/compiler <file-to-compile-path>
```
e.g.
```
Compiler/compiler CodeExamples/big_example.txt
```

### To execute the compiled file:

```
AbstractMachina/interp ./compiled
```



## Implementation details

### Done in Contextual Verifications

- [x] Check duplicate names of classes and objects.
- [x] Define existant classes (they can not be redefined or have heirs):
    - [x] Integer: arithmetic operations, comparison expressions, toString method.
    - [x] String: same as in C language, can not be modified, print and println methods. Only binary operator is concatenation with & symbol.
- [x] Check if there is an unique constructor within a class body, verify its name is the same as its class and has same parameters list.
- [x] Check if all parameters with var specified are also existing attributes with same names.
- [x] Check if expressions given to super constructor are valid
- [x] Check if all parameters types exist and are differents from current class name.
- [x] If a class extends from another, check if parent class exists.
- [x] If a class extends from another, its constructor needs to call its parent construtor with right parameters.
- [x] Whenever something is defined, verify that its type exists.
- [x] Get expression type.
- [x] Check in a declaration if the expression returns the same type as declared.
- [x] Check if a method returns the right type according to its declaration. If no return, automatic return of pseudo-variable result so check its type.
- [x] If there is an override, verify that it is effectively overriding a parent's method.
- [x] Verify that class attributes are in "protected" visibility.
- [x] Manage pseudo variable "result".
- [x] Manage reserved variable "this".
- [x] Reserved variable "this" can access to parents attributes and methods.
- [x] Manage reserved variable "super".
- [x] When a cast is done, check that casted class is a parent class to original type.
- [x] When an instance is created, check that class name is valid, as well as given parameters (number and types).
- [x] Keywords "result" and "return" are forbidden in a constructor.
- [x] Within an heritage, only redefinitions are authorised (overloads are forbidden). Especially, forbid same return type and name but different parameters.
- [x] When a method has same signature as a parent's one, force overriding attribute.
- [x] Heritage loops are forbidden.
- [x] Check that selections are authorized (expression returns a class that contains the attribute).
- [x] Check that "envois de messages" are authorized (expression returns a class that contains the method).
- [x] VAR parameters in constructor initialize attributes automatically (+ remove verification that these attributes are declared).
- [x] Check multiple definitions of methods.
- [x] Check multiple definitions of attributes.
- [x] Integer and String are not instanciable.
- [x] Integer and String are not derivable.
- [x] CV for objects.

- [x] Optimize how return are verified, for now it needs two times to add and remove decls in env.
- [x] Change print.ml methods so they do not print directly but build a string (so we can use it in verifs to add expr in error message).

### Done in Compiler

- [x] Compute needed space to store all classes, attributes and variables.
- [x] Resolve class that has an attribute containing itself problem.
- [x] Write virtual tables at the begining of the pile.
- [x] Then, save enough space for all global variables.
- [x] Compile constructors.
- [x] Then, write all methods codes with the right labels.
- [x] Verify how to manage pseudo-variable result.
- [x] Create a protocol for calling methods with or without parameters.
- [x] Go through main bloc, compile all declarations and instructions.
- [x] Call super constructor.
