# TheCompiler

Wonderful M1 project that consists in writing a compilator for an abstract pile machina.


## Utilisation

Commandes à exécuter depuis la racine de ce dossier :

### Pour pouvoir se servir de la machine abstraite fournie :

cd AbstractMachina
make
cd ..

### Pour pouvoir se servir du compilateur :

cd Compiler
make
cd ..

### Pour compiler des fichiers :

Compiler/compiler <chemin-fichier-à-compiler>
Exemple : Compiler/compiler CodeExamples/big_example.txt

### Pour exécuter le fichier compilé :

AbstractMachina/interp Compiler/compiled




## Done in Lexing and Parsing

- [x] Revoir définition des chaines qui est trop laxiste. Elle autorise par exemple "abc\\\\"" qui devrait être refusée ainsi que "abc def" (pas de retour-charriot dans une chaine). Avoir la forme correcte peut demander de filtrer plus proprement ce qui est contenu dans une chaine, comme pour les commentaires. Cependant, à la différence des commentaires, on doit renvoyer la chaine reconnue en résultat, ce qui oblige à la construire caractère par caractère à l'aide d'un Buffer (module disponible Ocaml). Vous trouverez comment faire en regardant la partie Lexing  du document suivant https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html. Il y a un exemple de traitement des chaines qui ressemble.

- [x] Ne plus distinguer UPLUS et UMINUS qui ne servent qu'à donner la même priorité à des règles.

- [x] Faut-il mettre DOT au même niveau ?

- [x] Dans la grammaire, l'usage de separated_pair obscurcit certaines règles (dont celles sur les opérateurs) plus qu'il ne les simplifie. Un usage "raisonnable" (à mon goût) de separated_pair correspond plutôt à ID : IDCLASS puisque le ':' joue juste le rôle d'un symbole de ponctuation. Ce n'est pas le cas d'un opérateur.

- [x] Ne pas différencier expr et exprOperator.

- [x] Voir si on peut empêcher les VAR. Vous autorisez syntaxiquement des VAR devant les paramètres des méthodes alors que ce n'est justifié que pour les paramètres du constructeur. Ce n'est pas faux, mais ça fera une verif contextuelle à faire en plus.

- [x] Dans un return l'expression est optionnelle.

- [x] Dans l'ast vous distinguez inutilement des constructions similaires. Ce n'est pas faux mais ça va vous obliger à dupliquer du code pour rien dans la suite. Par exemple pour les blocs, il y a une forme de déclaration qui est un cas particulier de l'autre. Dans l'ast on ne devrait garder que la forme la plus générale, l'autre s'y ramenant.

- [x] De même pour les 3 formes de déclarations de méthodes. La structure dans l'ast devrait être adaptée à la plus générale et les cas particuliers devraient s'y ramener (assez facilement). Fait-on aussi entrer le constructeur dans la meme forme d'ast que les méthodes (même si la syntaxe différe) quitte à le stocker dans un champ différent de votre ast ? Ca se discute. Ca pourrait simplifier les VC et la génération de code.

- [x] Idem pour la différence entre classe et objet.

## Done in Contextual Verifications

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

## Done in Compiler

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