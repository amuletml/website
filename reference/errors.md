---
title: Amulet Error Index
description: An index of all AMulet errrors, with long-form explanations.
---

This is an index of all the Amulet errors with long-form explanations,
as can be found [here] in the compiler repository, generated at 04/11/19.

[here]: https://github.com/amuletml/amulet/tree/master/doc

Keep in mind that the compiler can also print these explanations, using
the `amc explain [number]`{} subcommand.


### E0004: "UnclosedString" {#UnclosedString}
A string was opened with a double quote ('"'), but never closed.

1. Amulet strings cannot span multiple lines. If you want to write a
   multi-line string, use `^`{.amulet} to concatenate multiple strings together.

2. You can use a backslash ('\') to escape "special" characters within a
   string, such as single or double quotes. Make sure you've not
   accidentally escaped this string's closing quote.

### E0005: "UnclosedComment" {#UnclosedComment}
A comment was opened (using `(*`​), but never closed. Add
`*)`​ in order to end the comment.

1. Note that comments may be nested, meaning that you may need to close
   multiple comments.

2. It is possible to accidentally introduce a comment, especially when
   using operator sections. In these cases, add a space within the
   section's parenthesis.

   The most common case of this is referring to the multiplication (or
   product type) operators, which would be spelled `(*)`​. However,
   this is an unclosed comment, so write `( * )`​ instead.

### E0007: "MalformedClass" {#MalformedClass}
This class's head could not be parsed. Every class should be composed of
a (lower case) class name, followed by any number of type variables.

    class eq 'a

Classes may also require several "super classes", by placing them before
the class name in a tuple.

    class ord 'a => eq 'a

### E0008: "MalformedInstance" {#MalformedInstance}
This instance's head could not be parsed. Every instance should be
composed of a (lower case) class name applied to at least one argument.

    instance eq int

Instances may also impose additional constraints on their arguments,
such as requiring the elements of a list to also have an instance of
this class.

    instance eq 'a => eq (list 'a)

### E0009: "MisplacedWith" {#MisplacedWith}
Amulet has syntactic sugar for monadic computations, through the use of
begin/end blocks. In order to bind a monadic value, one should use a
with statement:

    begin
      with x <- f ()
      pure (x + 1)
    end

These statements cannot be used outside of monadic blocks. For instance
the following is invalid:

    let x = with x <- f ()

When using begin/end, make sure that all terms are aligned to the same
column:

    begin
      let y = 2
        with z <- f () (* Incorrect! *)
      pure (x + 1)
    end

### E0010: "BindQualified" {#BindQualified}
Amulet distinguishes between qualified names (those belonging to a
module, such as `Foo.bar`) and unqualified names (those accessed
directly, such as `foo`). While qualified and unqualified names may be
accessed interchangeably, it makes no sense to declare a qualified
name:

    let Foo.x = 1

Instead, one should just write the following:

    let x = 1

The same restriction applies to any declaration, including patterns,
types and constructors.

### E0011: "InvalidEscapeCode" {#InvalidEscapeCode}
Amulet allows you to escape special characters within a string by
prefixing them with a `\`{.amulet}. If you need to write a backslash on its own,
this can be done by escaping the slash using `\\`{.amulet}.

The following escape codes are supported:

 * `\a`{.amulet} - Bell                  (0x07 in ASCII)
 * `\b`{.amulet} - Backspace.            (0x08 in ASCII)
 * `\f`{.amulet} - Form feed page break. (0x0C in ASCII)
 * `\n`{.amulet} - Newline/Line feed.    (0x0A in ASCII)
 * `\r`{.amulet} - Return carriage.      (0x0D in ASCII)
 * `\t`{.amulet} - Horizontal tab.       (0x09 in ASCII)
 * `\v`{.amulet} - Vertical tab.         (0x0B in ASCII)
 * `\"`{.amulet} - Double quote (`"`)    (0x22 in ASCII)
 * `\\`{.amulet} - Backslash (`\`)       (0x5C in ASCII)
 * For any other character, you can use the hexadecimal ASCII value with
   `\x`{.amulet}. For instance, `\x61`{.amulet} represents the character `a`{.amulet}.

### E0012: "UnalignedIn" {#UnalignedIn}
Amulet heavily relies on indentation in order to determine how a program
should be parsed. As a result, it's a good idea to ensure that your code
is laid out correctly.

Here, the warning is triggered when the `in`{.amulet} separator is not aligned
with the corresponding `let`:

    let x = 0
      in ()

The easiest way to fix this is just to remove the `in`{.amulet} token and adjust
the indentation:

    let x = 0
    ()

The following are also acceptable alternatives:

    let x = 0 in
    ()

    let x = 0
    in ()

### E1001: "NotInScope" {#NotInScope}
This name cannot be found in the current scope. The most likely cause of
this is that you have misspelled the name you are trying to use.
However, if you would expect this variable to be available, there are
several other things to try:

1. If the name is something you'd expect to be built in to Amulet, make
   sure you've imported the prelude:

        open import "prelude.ml" (* Needed for 'None' *)
        let x = None

2. If the name is defined in another file or module, make sure you have
   imported and opened the file.

        open import "./my_lib.ml"
        (* ^ Needed in order for 'my_function' to be in scope. *)

        let x = my_function ()

3. When writing recursive functions, make sure you have used the `rec`
   modifier:

         (* Without 'rec', you won't be able to use 'fib' within its
         definition. *)
         let rec fib = function
         | 0 -> 1
         | 1 -> 1
         | n -> fib (n - 1) * fib (n - 2)

### E1002: "Ambiguous" {#Ambiguous}
Amulet allows you to declare multiple variables with the same name at
the same time. However, if you try to use such a variable, the compiler
does not know which variable you wanted to use, and so will spit out an
error.

    let x = 0 and x = 1
    x (* Does this mean 0 or 1? *)

The solution is to rename the variables to all be something different.
If you don't need a variable at all, you can replace it with an
underscore (`_`):

    let x = 0 and _ = 1
    x (* x is definitely 0 *)

### E1003: "NonLinearPattern" {#NonLinearPattern}
This error occurs when attempting to bind the same variable multiple
times within a pattern. Such code is confusing, and it is not clear what
behaviour would be expected here.

    function
    | Just x, Just x -> true
    (* `x is declared twice. *)
    | _ -> false

In order to resolve this error, variables should be renamed to ensure
each one is unique.

One interpretation of the above code is that it would check that both
options contain the same value (as they are bound to the same
variable). This behaviour can be emulated with a pattern guard:

    function
    | Just x, Just y when x == y -> true
    | _ -> false

### E1004: "NonLinearRecord" {#NonLinearRecord}
In Amulet, records cannot have multiple fields with the same name.
Allowing this, would lead to confusing behaviour: which value did you
want to access here:

    let person = { name = "Clara",
                   name = "Clive" }
    person.name (* Clara or Clive? *)

This error most likely occurs due to a field name being misspelled, or
an accidental copy-and-paste resulting in a field being declared
multiple times.

### E1007: "IllegalMethod" {#IllegalMethod}
Let bindings within class instances have slightly different behaviour to
let bindings on the top-level of a program. Every binding must declare a
single method and so cannot be composed of a pattern.

Instead of writing the following:

    instance applicative list
      let (pure, (<*>)) = (List.pure, List.( <*> ))

You should write each binding separately:

    instance applicative list
      let pure = List.pure
      let (<*>) = List.( <*> )

### E1008: "LastStmtNotExpr" {#LastStmtNotExpr}
Because of how `begin`{.amulet} expressions are desugared, the last statement in
the block needs to be an expression.

The desugaring of `begin`{.amulet} blocks is described by the following rules:

1. A 'with' binding is turned into a use of the monadic (>>=) operator:

        begin
          with p <- e;
          ...
        end
        (* => *)
        e >>= fun p -> begin ... end

2. A 'let' statement is turned into a 'let' expression:

        begin
          let p = e;
          ...
        end
        (* => *)
        let p = e in begin ... end

3. An expression used in a non-tail, non binding context is transformed
   into a match against the unit `()`:

        begin
          e
          ...
        end
        (* => *)
        let () = e in begin ... end

4. `begin`{.amulet} of a single expression is turned into that expression.

        begin
          e
        end
        (* => *)
        e

### E1009: "LetOpenStruct" {#LetOpenStruct}
While `let open`{.amulet} can be used to bring existing modules into scope, it
cannot be used to declare new modules. Instead, lift the module to the
top-level. For instance

    let y =
      let open begin let x = 0 end
      x

Can be safely converted into:


    module M = let x = 0
    let y =
      let open M
      x

Note, there is no theoretical reason for this limitation. However, such
modules may reference locally bound type variables or other names, which
make compilation much harder. This limitation will be lifted in the
future.

### E1010: "UnresolvedImport" {#UnresolvedImport}
The file you are trying to import cannot be found.

Amulet supports two kinds of imports, make sure you are using the
correct one:

1. If an import starts with "../" or "./", it will be resolved relative
   to the current file. This should be used for referencing files within
   the same project.

   In this case, make sure the file name is spelled correctly, and that
   the relative path is correct.

2. Otherwise, Amulet will attempt to load from the library path. This
   style of import should be used when trying to access external
   libraries.

   The default library path will only include Amulet's standard
   library. If you need to add external libraries, you can add them to
   the path with --lib.

### E1011: "ImportLoop" {#ImportLoop}
Amulet does not allow two files to import each other.

While it may sound unsavoury, if you find yourself needing to have two
files reference each other, consider refactoring your code so that such
cycles can be eliminated.

Aside from class instances, Amulet does not require that a type's
behaviour is defined in the same file as its definition. Therefore, it's
often possible to reorganise modules to declare types in one file, and
certain behaviours in others.

### E1012: "TFClauseWrongHead" {#TFClauseWrongHead}
The clauses of type functions all need to have the type function being
declared itself as the head of the application. That is, the following
is legal:

    type function f 'a 'b 'c begin
      f 'a 'b 'c = ()
    end

But the following is not:

    type function f 'a 'b 'c begin
      g 'a 'b 'c = ()
    end

### E1013: "TFClauseWrongArity" {#TFClauseWrongArity}
All clauses of the type function must have as many *visible* arguments
as given in the type function declaration. That is, the following is
legal only when n = k:

    type function f 'a1 ... 'an begin
      f t1 ... tk = ()
    end

For more information, see also the explanation of E2024, which concerns
arity mismatches in associated type definitions.

### E2001: "NotEqual" {#NotEqual}
This error indicates that there was a mismatch between concrete types in
the inferred and expected types for an expression. For example:

    let x : unit = 1000 + 200 + 30 + 4
    (*  ~~~~~~~~   ~~~~~~~~~~~~~~~~~~~  *)
    (*   |            Has type int      *)
    (*  Claims to be type unit          *)

This can also happen when e.g. the elements of a list, or members of a
record, have incompatible types:

    let { x = x : int } = { x = "foo" }
    (*        ~~~~~~~       ~~~~~~~~~                    *)
    (*          |              | Member x is a string    *)
    (*          | Member x is expected to be of type int *)

Type annotations are not the only place where this can crop up:

    let add_1 x = x + 1
    (* val add_1 : int -> int *)
    let _ = add_1 "foo"
    (* Couldn't match actual type string with expected type int *)

### E2002: "Occurs" {#Occurs}
Amulet does not support equi-recursive, or infinite, types.

This error happens when Amulet is trying to solve an equality constraint
between a type variable and a compound type that mentions that variable.

For example:

    let x = [x] in ()

If we say that 'x : T', for any type 'T', then that must also be the
type of '[x]'. However, the latter has type 'list T', and thus the
equation `T ~ list T`{.amulet} arises. This equation does have a unique solution,
namely the infinite type

    list (list (list (list (list (list (list (list ...)))))))

However, this is generally not what the user wants, even if it would be
easy to support, since infinite types tend to hide genuine programming
errors. If we allowed infinite types, then the following functions would
all type-check, even though they are all buggy:

    let search p = function
      | [] -> error "not found"
      | Cons (x, xs) ->
          if p x then
            x
          else
            search p (* forgot to pass xs *)

    let search' p = function
      | [] -> error "not found"
      | Cons (x, xs) ->
          if p x then
            x
          else
            search (* forgot to pass p and xs *)

    let search'' p = function
      | [] -> error "not found"
      | Cons (x, xs) ->
          if p x then
            x
          else
            p (* returned the predicate instead of searching *)


### E2003: "CustomTypeError" {#CustomTypeError}
Compilation was aborted because of a custom type error (see [1]).

Please consult the documentation of whatever library you are using to
know more.

[1]: https://amulet.ahti.space/amulet/99-builtins#type-errors

### E2004: "NotInScope" {#NotInScope}
The type checker could not find a value in scope. This is a bug in the
compiler, since badly-scoped programs should be rejected by the resolver
(see E1001).

Please upload your program to a pastebin site and open an issue in the
Amulet repository:

  https://github.com/amuletml/amulet/issues/new?title=TC%20Not%20in%20scope

Try to reduce your program to the smallest possible that still exhibits
the bug.

### E2005: "FoundHole" {#FoundHole}
The type checker encountered a hole (`_`) expression in the program.
Furthermore, if it can find suitable expressions of the type this hole
was inferred to have, it will print them.

    let swap : forall 'a. ('a * 'b) -> ('b * 'a) = _

Here, the compiler suggests the following expression as an
implementation for swap, since it is unambiguous.

    fun (x, y) -> (y, x)

### E2006: "ImpredicativeApp" {#ImpredicativeApp}
A polymorphic type (`forall 'a. ...`{.amulet}, `forall 'a -> ...`{.amulet}, or `p => ...`)
was used as an argument to a type constructor in a context where
impredicativity is disallowed.

Currently, this only happens in instance heads.

### E2008: "EscapedSkolems" {#EscapedSkolems}
A rigid type variable has escaped its scope. This can happen for one of
two reasons:

1. The type variable was made rigid because it is an existential declared
   by a GADT constructor, and it has escaped to the return type of the
   pattern-match:

        type foo = Foo : forall 'a. show 'a => 'a -> foo

        let un_foo (Foo x) = x

   Here, `un_foo`{.amulet} can not be given a meaningful type, since there is no
   way to statically refer to the type contained in the `foo`{.amulet} outside of
   the scope of the pattern match.

   One might think that the function `un_foo`{.amulet} would have the following
   type:

        val un_foo : forall 'a. foo -> 'a

   However, this type is bogus: it says that `un_foo`{.amulet} can return _any_
   type when given a foo (consider `un_foo @int`{.amulet}, `un_foo @string`);
   However, `un_foo`{.amulet} can only return the type that was stored in the
   `Foo`{.amulet} constructor!

   Using constraints of the rigid type variables is permissible, as long
   as they are specified in the GADT constructor. So, with the `Foo`
   above, the following program is legal:

         instance show foo where
           show (Foo x) = "Foo " ^ show x
         end

   The use of `show x`{.amulet} gives rise to a `show α`{.amulet} constraint (where `α`{.amulet} is
   the rigid type variable introduced by matching on `Foo`), which can be
   satisfied with the `show α`{.amulet} dictionary packaged inside `Foo`{.amulet}.

2. The type variable was made rigid because of a rank-N type
   (with N ≥ 2), and an unification variable from an outer scope was
   unified with it.

   A function with higher-rank type such as the following, in which `'r`
   and `'s`{.amulet} are bound at distinct scopes, implies that `'r`{.amulet} can not
   depend on `'s`

        val foo : forall 'r. (forall 's. 's -> 'r) -> 'r

   As an example of why this is useful, consider a scoped state
   transformer monad, that can be used purely:

        type st 's 'a  (* here, and below, 's is the state thread *)
        type st_ref 's

        val run_st : forall 'a. (forall 's. unit -> st 's 'a) -> 'a

   Since the `'a`{.amulet} in the type of `run_st`{.amulet} can not depend on the `'s`{.amulet},
   it's impossible for a `st_ref`{.amulet} to escape its scope.

### E2009: "SkolBinding" {#SkolBinding}
When a type variable is explicitly made polymorphic (with a type
annotation, with an existential variable in a GADT constructor, with
an instance head, or with the LHS of a type function), it becomes
"rigid" and different from all other types (except for itself).

This error happens when the programmer has tried to use some concrete
type in place of a rigid type variable:

    let id : forall 'a. 'a -> 'a =
      fun x -> x + 1
    (*         ^ ^ expects type int *)
    (*         | has type a         *)

### E2011: "CanNotInstance" {#CanNotInstance}
The "hole" in a polymorphic record type can only be instanced with
another record type. For example, this is illegal, because the type
variable `'a`{.amulet} is used in a polymorphic record but is later instantiated
to be an integer.

    let foo : forall 'a. 'a -> { 'a | x : int } -> int =
      fun _ r -> r.x

    foo 123 { x = 1 }

### E2013: "PatternRecursive" {#PatternRecursive}
Pattern bindings can not be used in a 'let rec'.

### E2014: "DeadBranch" {#DeadBranch}
This branch of a (GADT) pattern match has unsatisfiable constraints, and
thus can never be executed. Generally, these are equality constraints
arising between indices of the generalised type:

    type vect 'n 'a =
      | Nil : vect Z 'a
      | Cons : 'a * vect 'n a -> vect (S 'n) 'a

    let foo (x : vect Z 'a) =
      match x with
      | Nil    -> () (* This branch is fine; Z ~ Z *)
      | Cons _ -> ()
        (* This branch will never be executed since Cons isn't an
        inhabitant of `vect Z _`{.amulet}, and the type checker can tell you
        since Z ~/~ S _ *)

### E2015: "AmbiguousType" {#AmbiguousType}
The type for this function is ambiguous, that is, it has variables that
appear in a constraint (i.e. will affect the runtime behaviour of the
function) that can't be determined by unification.

This can happen because:

1. The type variable never appears in the type after it appears in the
   constraint.

2. The type variable only appears as an argument to a type family
   (either an associated type or a type function), in which case it can
   not be determined because type families aren't injective.

### E2016: "NotValue" {#NotValue}
Amulet does not permit expressions which are not syntactically values to
take on polymorphic types, because that would compromise soundness.
Consider the `let`-binding

    let x = ref None

This would have type `forall 'a. ref (option 'a)`: Trouble.

    x := Some 1 (* use x @int *)
    !x 123      (* use x @(int -> unit) *)

Furthermore, top-level bindings can not have any generalisable variables
in their types, for the same reason (the compiler can't insert a
`forall`{.amulet} to generalise them).

### E2017: "AmbiguousMethodTy" {#AmbiguousMethodTy}
This method signature has an ambiguous type, i.e. it doesn't mention
some of the variables bound by the class head.

Class methods have to mention at least a subset of the variables bound
by the class head, with the condition that the subset is enough to
determine all of the other bound variables via functional dependencies.
For example:


    class foo 'a 'b begin
      val x : unit -> 'a
    end

The type of x is ambiguous since it only mentions `'a`{.amulet}, and that's not
enough to determine `'b`{.amulet}.

    class bar 'a 'b | 'a -> 'b begin
      val y : unit -> 'a
    end

Here, the type of y is the same as that of x, but it is not ambiguous
for the class `bar`{.amulet} since knowing the instantiation of `'a`{.amulet} is enough to
figure out the instantiation of `'b`{.amulet}.

### E2018: "UnsatClassCon" {#UnsatClassCon}
The body of this binding gave rise to constraints which Amulet could not
quantify over.

Amulet can not quantify over constraints in these situations:

 1. The binding is not a function: Adding a constraint would turn it
    into a function, thus changing the performance characteristics of
    the program

 2. The binding is a pattern binding: Any choice of instantiation for an
    unsatisfied constraint in a pattern binding body would be arbitrary and
    thus the behaviour of the program would depend on type-checking
    internals

 3. The unsatisfiable constraint is in an instance method or default
    method: add the solution to the class context, instance context, or
    context of the method's type

 4. The binding has a type signature and the constraint could not be
    deduced from that context: add it to the context

 5. The constraint is "too concrete", and an instance can not possibly
    be added because of the orphan check

### E2019: "ClassStackOverflow" {#ClassStackOverflow}
While solving for a particular type class instance, more than 25 nested
class constraints had to be solved, and the solver gave up.

### E2020: "WrongClass" {#WrongClass}
This method implementation is not of a method of the class being
declared. For example:

    class foo 'a begin
      val x : unit -> 'a
    end

    instance foo int begin
      let y _ = ...
    end

### E2021: "Overlap" {#Overlap}
These two instance declarations are both valid choices for a particular
instantiation, and any choice between them would be arbitrary and depend
on implementation details of the compiler. Thus, the second instance is
flagged as an error.

Example:

    class foo 'a begin
      val x : 'a -> string
    end

    instance foo int begin
      let x _ = "foo"
    end

    instance foo int begin
      let x _ = "bar"
    end


    put_line (x 123)

Here, either instance could be selected, and the runtime behaviour of
the program would depend on what instance the compiler decided to pick,
so the program is rejected.

### E2022: "UndefinedMethods" {#UndefinedMethods}
This instance declaration is missing some methods.

### E2023: "UndefinedTyFam" {#UndefinedTyFam}
This instance declaration is missing a definition for an associated
type.

### E2024: "TyFamLackingArgs" {#TyFamLackingArgs}
This associated type declaration appears with a different number of
arguments than it was declared to have.

For example:

    class foo 't begin
      type bar ('x : type)
      type quux : type -> type
    end

These associated types seem to both have two arguments (the class
argument `'t`{.amulet} and the other argument), since they both have the same
kind. However, this is not the case:

1. `bar`{.amulet} reduces to a type expression of kind `type`{.amulet} after receiving
   two arguments (`'t`{.amulet} and `'x`)

2. quux reduces to a type expression of kind `type -> type`{.amulet} after
   receiving a single argument, namely `'t`{.amulet}.

### E2025: "MagicInstance" {#MagicInstance}
An attempt was made to write an instance of one of the following magic
classes:

1. Amc.row_cons
2. Amc.known_int
3. Amc.known_string
4. typeable

The first three are solved "magically" by the compiler, and the latter
can have instances declared with `deriving instance typeable foo`{.amulet}.

### E2026: "TypeFamInInstHead" {#TypeFamInInstHead}
Type families (associated types, type functions) can not appear in
instance heads. This is because type families are not injective, i.e.
one can not run them "backwards" from the solution to get the arguments
to the function, and so if they were allowed in instance heads, instance
selection would be undecidable.

A workaround: Introduce a fresh type variable to your instance head,
then constraint it to be equal to the type function application in the
instance context. That is, turn

    instance foo int (fam 'a) begin end

into

    instance 'b ~ fam 'a => foo int 'b begin end

This causes instance selection to *always* pick that instance, then
afterwards constrain the second parameter to be equal to `fam 'a`{.amulet}.

### E2027: "InvalidContext" {#InvalidContext}
None of these are valid in a class/instance context, unless under an
application to a type constructor:

1. Record types
2. Type variables
3. Lifted tuple types
4. The 'type' type
5. Type-level literals
6. Function types

### E2028: "OrphanInstance" {#OrphanInstance}
When defining an instance of a class, care must be taken to ensure there
is only one possible instance for any type (see also error 2021).

In order to ensure this, we require that every instance must be defined
in the same module as the types it mentions, or the class your are
implementing.

Consider a definition of pairs of integers, with an associates `show`
instance:

    class foo 'a
      val x : 'a -> string

    instance foo int
      let x _ = "foo"

This is valid, as the instance is defined alongside the class. However,
if you tried to define the instances elsewhere, you could bypass the
overlap check:

    module A =
      instance foo int
        let x _ = "foo"

    module B =
      instance foo int
        let x _ = "bar"

### E2030: "CanNotVta" {#CanNotVta}
For a visible type application `f @t`{.amulet} to be legal, the type of `f`{.amulet} must
have a quantifier of visibility *at least* 'Spec' at its head, i.e. the
type of `f`{.amulet} must start with a user-written `forall`{.amulet}.

Note that type variables mentioned in indirect user supplied type
signatures are also bound by 'Spec' foralls:

    let foo (x : 'a) (y : 'b) : 'a = x
    let _ = foo @int
    (* legal *)

However, if the type hints are incomplete, no type variables are
available for VTA:

    let foo (x : 'a) y : 'a = x
    let _ = foo @int
    (* Can not apply expression of type 'b -> 'a -> 'b
         to visible type argument int *)

If the type signature for a function has a `forall 'a ->`{.amulet} quantifier
(i.e. a forall quantifier with visibility 'Req'), it _must_ be given a
visible type argument, even if the argument is a wildcard.

### E2031: "NotPromotable" {#NotPromotable}
When a data constructor is used as a type expression, we require that
the type of the constructor be *promotable* to a kind, and furthermore
that the promoted kind be useful.

This means that, currently, constructors that take records as arguments
can't be promoted. Here is what can be promoted:

1. Function types, qualified types and quantified types
   (`a -> b`{.amulet}, `a => b`{.amulet} and `forall a. b`{.amulet} respectively)
2. Tuple types (using the promoted tuple types)
3. All type constructors, and applications thereof
4. Promoted data constructors, and applications thereof
5. Type variables, wildcards
6. The `int`{.amulet}, `bool`{.amulet} and `string`{.amulet} types

### E2032: "WildcardNotAllowed" {#WildcardNotAllowed}
Type wildcards allow you to leave off parts of a type, making them
substantially shorter. However, there are some places where wildcards
cannot be used, and the full type must be given.

    type foo = Foo of _
    (* You must give a concrete type here, such as *)
    type foo = Foo of int

Type wildcards cannot be used in:
 - Constructor definitions
 - Class method signatures
 - Within type functions. When writing a type function which matches on
   any value, use a type variable instead of a wildcard.

       type function foo 'a
         foo _ = ()

   Should become

       type function foo 'a
         foo 'a = ()

### E2034: "UnsaturatedTS" {#UnsaturatedTS}
A type synonym (or type function) can't appear with less arguments than
it was declared to have. This is because such an under-saturated
application would correspond to a type-level lambda, and type-level
lambda expressions make type inference undecidable.

#### Possible fix: eta reduction

In the synonym:

    type foo 'a <- list 'a

If you try to use the type `foo`{.amulet} unsaturated (as a synonym for `list`),
the type checker will rightly complain. However, by removing the `'a`
argument in both sides of the equation, `foo`{.amulet} can be used unsaturated:

    type foo <- list

### E2035: "NotCovered" {#NotCovered}
This instance declaration violates at least one functional dependency of
the class declaration. For type checking with fundeps to be terminating
it is required that instances satisfy the following condition, called
the "coverage condition":

> For each functional dependency `'tvs_left -> 'tvs_right`{.amulet} of the class,
> every type variable that appears in the types corresponding to
> `tvs_right`{.amulet} must appear in the types corresponding to `tvs_left`{.amulet} in
> the instance head.

Concretely, given the following class:

    class r 'a 'b | 'a -> 'b begin end

The following instances are admissible:

    instance r string string       begin end
    (* no type variables *)

    instance r (list 'a) (list 'a) begin end
    (* 'a appears in both sides *)

    instance r ('a * 'b) 'a        begin end
    (* 'a in the right => 'a in the left *)

But the following are not:

    instance r string 'a    begin end
    (* 'a appears only on the right *)

    instance r 'a ('a * 'b) begin end
    (* 'b appears only on the right *)

### E2036: "MightNotTerminate" {#MightNotTerminate}
This type function equation might not terminate because of a recursive
call in which the arguments are not strictly decreasing in size. For
example:

    type function foo 'a begin
      foo int = foo int
    end

Reducing `foo int ~ 'a`{.amulet} leads the compiler to conclude `'a ~ foo int`{.amulet}...
and then it gets stuck in a loop, since now it needs to reduce the same
equation as the one it had in the first place.

For totality to hold, a recursive call can only be made using types that
were arguments to type applications in the LHS:

    type function foo 'a begin
      foo (option 'a) = foo 'a (* fine: 'a < option 'a *)
      foo (list int)  = foo int (* fine : int < list int *)
    end

### E2037: "TyFunInLhs" {#TyFunInLhs}
The left-hand-side of this closed type function equation mentions an
application of a type function or synonym, because type functions might
not be /injective/: i.e., they might map different arguments to the same
type.

Consider this program:

    type function foo 'a : type begin
      foo int    = int
      foo string = int
      foo bool   = bool
    end

    type function bar 'a begin
      bar (foo 'a) = 'a
    end

Now, consider we're trying to prove the equality

    bar int ~ 'a

Here, we have a problem. `int`{.amulet} is equal to both `foo int`{.amulet} and `foo
string`{.amulet}, at the same time, and so there's no hope for an unambiguous
reduction of the type family! Moreover, this compromises type safety:

    bar int ~ int    (* by int ~ foo int *)
    bar int ~ string (* by int ~ foo string *)
    int ~ string     (* by the first and second equations *)

### E2038: "DICan'tDerive" {#DICan'tDerive}
Amulet is unable to derive an instance for this class. There may be
several reasons for this:

 - The class is not derivable at all. Currently only the `typeable`
   class may be derived.

 - The instance definition is malformed. Deriving instances must take
   the form:

       type foo = Foo
       deriving instance typeable foo

### E2039: "NotAnIdiom" {#NotAnIdiom}
Idiom brackets provide a convenient syntax for writing functions
involving applicative computations.

    (| f x y |)

Is equivalent to writing

    f <$> x <*> y

While other application-like expressions can be written in idiom-bracket
notation (such as binary operators), most other expressions do not make
sense in that context, and so are rejected.

### E3001: "MalformedRecursiveRhs" {#MalformedRecursiveRhs}
The right hand side of this recursive definition is invalid. In order to
compute the value of this variable, we'd need to know it's value
already.

For instance, consider this definition:

    let rec x = x

Computing x would result in an infinite loop - in order to evaluate x,
we need to evaluate x. This can be avoided by making x lazy, or
converting it into a function:

    let rec x = lazy (force x)

    let rec x () = x ()

### E3002: "DefinedUnused" {#DefinedUnused}
This definition is not used anywhere within your program. This means the
variable definition can be removed, replaced with a pattern wildcard
(`_`), or entirely removed.

For instance, the binding

    let (x, y) = do_something ()
    y

Can be replaced with:

    let (_, y) = do_something ()
    y

In the cases where no variables are used at all, and the definition has
no side effects, then it may be removed entirely.

### E3003: "ParseErrorInForeign" {#ParseErrorInForeign}
While external values are normally bound to simple names, they may
actually contain any Lua expression. These definitions are emitted
verbatim into the compiled program, meaning if they are not valid Lua,
then your code will fail to run.

If you receive this warning, check your expression is a valid Lua
_expression_. If you need to evaluate a statement, you may wrap it in a
closure and call it immediately:

    external val f : () = "(function() for i = 1, 10 do print(i) end)()"

Please note that Amulet's parser may not be entirely correct. If you
receive this error, and you are sure that your code is syntactically
valid, please file a bug report[1].

[1]: https://github.com/amuletml/amulet/issues/new?title=Lua%20parser%20bug

### E3004: "LazyLet" {#LazyLet}
When an expression is expected to have the type of 'lazy', Amulet will
try to convert it to a thunk.

Consider:

    let x = f () || g ()

Here, `( || )`{.amulet} expects the second argument to be lazy. As a result, this
is translated into the following:

    let x = f () || lazy (g ())

However, due to how type inference works, this automatic thunking does
not quite work as expected when in the presence of bindings. Imagine
binding `g ()`{.amulet} to a variable:

    let x = f () || let y = g () in y

This is elaborated into the following:

    let x = f () || (let y = g () in lazy y)

This is clearly not what you would expect, as ideally the whole
expression would be made lazy. In these cases, one should either
refactor the expression into a smaller function, or use `lazy`
explicitly.

### E3005: "RedundantArm" {#RedundantArm}
The arm of this pattern is covered by some combination of the patterns
preceding it, and so will never be executed.

The easiest way to resolve this error is to remove any arms marked as
redundant. This will have no impact on your program's behaviour.

However, there may be times you want this pattern arm to be used, but
the compiler is insisting it isn't. There may be several reasons for
this:

 - The first pattern to match will be the one which executed. For
   instance, in the following code, the last case will never be executed
   as `n`{.amulet} will always match first.

        let rec fac = function
        | 0 -> 1
        | n -> n * fac (n - 1)
        | 1 -> 1

   In these cases, you may wish to reorder your patterns, so that the
   more restrictive patterns appear first.

 - If you do not want to reorder your patterns, you may wish to make
   them more restrictive, replacing wildcards with more explicit checks.

 - If this pattern is not covered by previous patterns, or this is the
   first pattern, your type is 'uninhabited'. This means that there is
   no way your code could ever create a value which has this type, and
   so you shouldn't ever need to match for it.

   These cases can be replaced with an empty match (`match x with ()`{.amulet} or
   `function ()`).

### E3006: "MissingPattern" {#MissingPattern}
This pattern matching expression does not handle every possible case
that may occur. If such a case does occur, your code will throw an error
and the program will most likely crash.

Amulet will inform you which pattern arms are missing. You should just
be able to copy each arm, and add some implementation for it.

Note, if this pattern occurs within a let binding or function, you will
need to replace it with a `match`{.amulet} expression. For instance these
definitions:

    let [x] = xs
    let 2 + 2 = 5

Should be written as:

    let x = match xs with
            | [x] -> x
            | _ -> error "TODO"
    let x + y =
      match x, y with
      | 2, 2 -> 5
      | x, y -> x + y

### E3007: "MatchToLet" {#MatchToLet}
If you have a `match`{.amulet} expression with a single clause, you can rewrite
it as a pattern-matching let expression instead. This reduces
indentation, and should make your code easier to read.

For instance this expression

    match f () with
    | (a, b) -> a + b

may be converted into the simpler

    let (a, b) = f ()
    a + b

### E3008: "MatchToFun" {#MatchToFun}
`function`{.amulet} expressions provide a convenient way of writing functions
which split on a single argument. However, if your function only has one
arm to split on, it may be written more easily using `fun`{.amulet}. This makes
your code shorter, and should be easier to understand:

For instance, this expression:

    function
    | (a, b) -> a + b

may be rewritten as the simpler

    fun (a, b) -> a + b

### E3009: "ToplevelRefBinding" {#ToplevelRefBinding}
Bindings with type `ref _`{.amulet} at the top-level are global, mutable state
and should generally be avoided. Prefer to give `ref`{.amulet} bindings the
smallest possible scope. For example, instead of writing

    let counter = ref 0
    let next_int () =
      let cur = !counter
      counter := cur + 1
      cur

Write instead:

    let next_int =
      let counter = ref 0
      fun () ->
        let cur = !counter
        counter := cur + 1
        cur

