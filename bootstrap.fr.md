_caml2csl_ convertit les fichiers `.caml` en `.ml`.
`renamer` renomme les fichiers `.ml` afin que _caml2csl_ sache quoi en faire :
```sh
renamer src/*.ml src/*.mli
```

Le traducteur a besoin des dépendances du fichier sur lequel il travaille.
On a de la chance, _camldep_ a été utilisé pour compléter le `Makefile` :
```make
.SUFFIXES : .mli .ml .zi .zo .mlp


.mli.zi:
	camlc -c $(COMPFLAGS) $<

.ml.zo:
	camlc -c $(COMPFLAGS) $<

#...

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
lexer.zi: parser.zi 
parser.zi: location.zo syntax.zo 
changes.zo: lexer.zi globals.zo conv.zo syntax.zo location.zo modules.zo \
    print.zo emit.zo 
#...
```

On transforme donc ce `Makefile` pour traduire le code au lieu de le compiler :
```sh
sed -e '1,/^### DO NOT DELETE THIS LINE/d' -e 's/\.zi/.mli/g' -e 's/\.zo/.ml/g' src/Makefile
```

Voici le résultat; j'ai ajouté un prélude à la main :
```make
all: translator renamer

.SUFFIXES : .ml .mli .caml .camli

.caml.ml:
	caml2csl $<

.camli.mli:
	caml2csl $<

OBJS= location.ml globals.ml syntax.ml modules.ml print.ml \
      lexer.ml par_aux.ml parser.ml emit.ml conv.ml changes.ml enter.ml \
      parse_mlc.ml compiler.ml

translator: translator.ml main.ml
translator.ml: $(OBJS)

renamer: renamer.ml

lexer.mli: parser.mli 
parser.mli: location.ml syntax.ml 
changes.ml: lexer.mli globals.ml conv.ml syntax.ml location.ml modules.ml \
    print.ml emit.ml 
#...
```

On lance la traduction et ça échoue :
```sh
$ make
caml2csl location.caml
caml2csl globals.caml
Warning: uses module of conversion.
caml2csl syntax.caml
caml2csl modules.caml
Fatal error: Syntax error at (4509,4511).
make: *** [Makefile:5: modules.ml] Error 2
```

La position rapportée en nombre de caractères depuis le début du fichier
est incorrecte : il n'y a rien de mauvais sous le curseur
après avoir tapé `4509 ` dans _vim_ (il faudrait creuser et trouver où est le bug).
Cependant, un peu plus loin, je vois des points-virgules surnuméraires :
```ocaml
let opened_modules = ref
  { mod_name = "";
    mod_values = hashtbl__new 73;
    mod_constrs = hashtbl__new 53;
    mod_labels = hashtbl__new 41;
    mod_types = hashtbl__new 29;
    mod_arity = hashtbl__new 53 }; (* <- ici *)

;;
```

TODO vérifier si c'est un bug de _caml2csl_ en recompilant avec Caml Light

Voici un patch qui règle le problème :
```diff
146c146
<     mod_arity = hashtbl__new 53 };
---
>     mod_arity = hashtbl__new 53 }
156c156
<     mod_arity = hashtbl__new 53 };
---
>     mod_arity = hashtbl__new 53 }
```

Après les avoir éliminés, `modules.caml` est traduit sans problème
mais on bloque maintenant sur `parser.mly` :
```sh
$ make
caml2csl modules.caml
Warning: uses module of conversion.
caml2csl print.caml
caml2csl lexer.camli
Fatal error: Cannot find the compiled conversion file parser.zc.
make: *** [Makefile:7: lexer.mli] Error 2
```

On ne peut pas donner à _caml2csl_ le résultat d'_ocamlyacc_ :
il exige la syntaxe ancienne;
le résultat ne servira qu'à _caml2csl_, on traduira plus tard ses _actions_ à la main.
En effet, on ne veut pas se retrouver à maintenir une grammaire compilée !
On compile donc **temporairement** `parser.mly` avec _camlyacc_ (sans _o_) :
```sh
$ camlyacc parser.mly
camlyacc: 19 shift/reduce conflicts.
$ mv parser.ml parser.caml #on aura p-ê des warnings
$ mv parser.mli parser.camli
```

On finit la traduction avec une liste de choses à faire :
```make
$ make
caml2csl parser.camli
-- TO DO -- : copy type declaration of token.
caml2csl lexer.camli
-- TO DO -- : copy type declaration of lexical_error.
-- TO DO -- : copy exception declaration of Lexical_error.
caml2csl par_aux.caml
caml2csl parser.caml
-- TO DO -- : expression in (112181,112219) should be a 2-uple
                instead of a type cast.
-- TO DO -- : expression in (118927,118965) should be a 2-uple
                instead of a type cast.
-- TO DO -- : expression in (119024,119062) should be a 2-uple
                instead of a type cast.
-- TO DO -- : expression in (123621,123645) should be a 2-uple
                instead of a type cast.
-- TO DO -- : expression in (127623,127663) should be a 2-uple
                instead of a type cast.
caml2csl emit.caml
caml2csl conv.caml
Warning: uses module of conversion.
caml2csl changes.caml
caml2csl enter.caml
caml2csl parse_mlc.caml
Warning: uses module of conversion.
caml2csl compiler.caml
caml2csl main.caml
Warning: uses module of conversion.
caml2csl renamer.caml
Warning: uses module of conversion.
```

Avant d'y revenir, convertissons `parser.mly` et `lexer.mlp`.
Pour `parser.mly`, c'est facile (`\U` et `\L` changent la casse) :
```sh
$ sed -i -e 's/#open "\(.\)\([^"]*\)";;/open \U\1\L\2/' \
         -e 's/location__location/Location.location/' \
         -e 's/mem/List.mem/' parser.mly
$ rm parser.ml parser.mli
```

Dans le `Makefile`, on voit que
_cpp_ (C PreProcessor, pas C++) transforme `lexer.mlp` en `lexer.mll` :
```make
lexer.mll: lexer.mlp
	$(CPP) lexer.mlp > lexer.mll
```
La variable `$(CPP)` est définie dans `config`; je me suis permis
d'ajouter `-traditional-cpp` pour éviter des erreurs
avec les versions modernes de l'outil :
```make
CPP=cpp -P -Dunix -traditional-cpp
```
Le code est un peu long pour le traduire à la main,
et le mélange de règles, d'actions et de directives au préprocesseur
empêche son parsing par _caml2csl_.
Avant de _bootstrap_ ce traducteur, j'avais écrit
[un _proof of concept_](https://github.com/ghuysmans/ocaml_of_cl)
qui évitait seulement de massacrer les commentaires et les chaînes de caractères.
Utilisons-le ici et vérifions que tout s'est bien passé :
```sh
$ ocaml_of_ci -c lexer.mlp >l #-c pour gérer les faux commentaires
$ diff lexer.mlp l
4,5c4,5
< (**) #open "parser";;
<      #open "location";;
---
> (**) open Parser;;
>      open Location;;
13c13
< let keyword_table = (hashtbl__new 149 : (string, token) hashtbl__t);;
---
> let keyword_table = (Hashtbl.new 149 : (string, token) Hashtbl.t);;
20,21c20,21
<  hashtbl__clear keyword_table;
<  do_list (fun (str,tok) -> hashtbl__add keyword_table str tok) [
---
>  Hashtbl.clear keyword_table;
>  do_list (fun (str,tok) -> Hashtbl.add keyword_table str tok) [
...
```
Il a abattu pas mal de travail, on corrige ce qu'il a manqué.

Revenons aux _warnings_ vus plus tôt.
Le premier à propos de `type token` est malvenu.
Le second et le troisième s'appliquent au préambule de `lexer.mlp`,
on y reprend les déclarations de `lexer.mli`.

Les _warnings_ au sujet des tuples sont difficiles à retrouver.
Pour le moment, on va faire comme si on ne les avait pas vus !

Il est maintenant temps d'essayer de compiler le projet.
On pourrait adapter l'ancien `Makefile` mais autant passer directement à _dune_ :
```sexp
(rule
 (deps lexer.mlp)
 (action
  (with-stdout-to lexer.mll
   (run %{bin:cpp} -P -Dunix -traditional-cpp %{deps}))))
(ocamllex (modules lexer))
(ocamlyacc (modules parser))
(executables
 (names main renamer)
 (libraries csl)
 (flags (:standard -w -3-9-10-27-39 -unsafe-string -no-strict-sequence))
 (preprocess (action (run %{bin:camlp4o} %{input-file}))))
```
Les _flags_ sont importants, notamment `-no-strict-sequence`
sans quoi on se retrouve avec des erreurs étranges.

Il restait aussi des erreurs à cause de backticks dans `lexer.mlp` :
```diff
diff --git a/src/lexer.mlp b/src/lexer.mlp
index 1a18d80..162d708 100644
--- a/src/lexer.mlp
+++ b/src/lexer.mlp
@@ -310,11 +310,11 @@ and char = parse
                             Lexing.lexeme_end lexbuf)) }

 and string = parse
-    '"` (*"'"'*)
+    '"' (*"'"'*)
       { () }
-  | `\\` ("\010" | "\013" | "\013\010") [` ` `\009`] *
+  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
       { string lexbuf }
-  | `\\` [`\\` `"' (*"'"'*) 'n' 't' 'b' 'r']
+  | '\\' ['\\' '"' (*"'"'*) 'n' 't' 'b' 'r']
       { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
         string lexbuf }
   | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
@@ -326,4 +326,3 @@ and string = parse
   | _
       { store_string_char(Lexing.lexeme_char lexbuf 0);
         string lexbuf }
-;;
```

On se retrouve maintenant avec des dépendances cycliques :
```sh
$ dune exec ./main.exe
Error: dependency cycle between modules in _build/default/src:
   Main
-> Changes
-> Lexer
-> Parser
-> Lexer
-> required by _build/default/src/main.exe
```
On a envie d'éliminer `open Lexer` dans `parser.mly`
mais il faut le faire intelligemment : on a besoin d'`infix_list`.
Le plus facile est de la mettre `list` toute seule dans `infix.ml`.

On reformule une condition qui utilisait `Filename.is_absolute` dans `globals.ml` :
```diff
diff --git a/src/globals.ml b/src/globals.ml
index 23cb5cd..4a73134 100644
--- a/src/globals.ml
+++ b/src/globals.ml
@@ -51,7 +51,7 @@ exception Cannot_find_file of string;;
 let find_in_path filename =
   if file_exists filename then
     filename
-  else if Filename.is_absolute filename then
+  else if not (Filename.is_relative filename) then
     raise(Cannot_find_file filename)
   else
     let rec find = function
```

_caml2csl_ a renommé `option` en `option0` dans `globals.ml`.
On supprime la définition et renomme partout le type et ses cas.

On retrouve enfin nos problèmes de tuples

`git diff --stat` montre que `tools.ml` a été effacé.
Il n'est pas utilisé mais contient de quoi mieux comprendre
le fonctionnement de l'outil. Demandons manuellement sa traduction :
```sh
$ caml2csl tools.caml
Fatal error: Syntax error at (2077,2079).
```
C'est encore un point-virgule de trop.
L'erreur suivante était un `#open "lexer"` manquant.

Il ne reste plus qu'à compiler `lib` avec `%{bin:caml2csl}`.
Un utilisateur lambda n'a pas besoin des `.zc` : on les installe avec _caml2csl_.
