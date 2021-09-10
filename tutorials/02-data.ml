---
title: Everything You Need To Know To Write Amulet
description: A quick walk through all the basic Amulet syntax.
---

(* `open import "foo.ml"`{.amulet} imports `foo` from the library path. *)
open import "prelude.ml"


(* Let bindings introduce variables. These variables can not be mutated. *)
let foo = 1
let bar = ""

(*
Type inference can figure out the type of a variable without the
programmer having to supply it.
*)

let foo x = x + x

(* `(+)`{.amulet} operates on ints, `(+.)`{.amulet} on floats. *)
let bar x = x +. x

(* The `type`{.amulet} construct can be used to define a new data type.
   Types are defined by listing their constructors *)
type foo =
  | Foo_str of string
  | Foo_int of int

(*
Values of algebraic data types can be taken apart with pattern
matching.
*)

let get_foo_str x =
  match x with
  | Foo_str x -> x
  | Foo_int _ -> "it wasn't a string!"

(* The `function`{.amulet} keyword abbreviates a pattern matching function. *)

let get_foo_str' = function
  | Foo_str x -> x
  | Foo_int _ -> "not a string again?"

(*
Pattern matching on `bool`{.amulet} can be done with
`if .. then .. else ..`{.amulet}.
*)

let is_gte x y =
  if x >= y then
    "it is"
  else
    "it's not"

(* User-defined data types can be recursive. *)

type chain =
  | Chain_end
  | Chain_add of string * chain

(* Recursive functions need a `rec`{.ocaml} keyword, as in OCaml. *)

let rec str_of_chain = function
  | Chain_end -> "end"
(* Concatenation is done using the `(^)`{.amulet} function. *)
  | Chain_add (x, rest) ->
      x ^ " + " ^ str_of_chain rest

(* Types can have parameters. These stand for another type, and can be
   inferred.  *)

type li 'a =
  | Nil_
  | Cons_ of 'a * li 'a

(*
```amulet
val map : ('a -> 'b) -> li 'a -> li 'b
```
*)
let rec map f = function
  | Nil_ -> Nil_
  | Cons_ (x, xs) -> Cons_ (f x, map f xs)

(* Amulet has a built-in type of lists, `list 'a`{.amulet}. *)

let foo : list int = [1, 2, 3, 4, 5]

(* Lists can be manipulated using list comprehensions. *)

let greater_than_3 =
  [ x
  | with x <- foo
  , x > 3
  ]

(* List literal syntax can also be used in patterns. *)

let [a, b, c] = [1, 2, 3]
let _ = a + b + c

(* Type classes are used for principled overloading. *)

class to_string 'a begin
  val to_str : 'a -> string
end

instance to_string int begin
  let to_str _ = "int"
end

instance to_string bool begin
  let to_str _ = "bool"
end

(*
Methods can be used with any type that has a corresponding
`instance`{.amulet}.
*)

let "int" = to_str 123
let "bool" = to_str true

(* Type class methods can be polymorphic in their return types. *)

type a_box 'a = Box of 'a

(* The class `into_box` is parametrised by a type constructor. *)
class into_box 'f begin
  val into : 'a -> 'f 'a
end

instance into_box list begin
  let into x = [x]
end

instance into_box a_box begin
  let into = Box
end

(*
Amulet selects the instance to use automatically, based on the context
`into` is used in.
*)

let Box 123 = into 123
let [123] = into 123

(*
`into_box` is almost the standard class `applicative`{.amulet}.
Amulet has special syntax for using `applicative`{.amulet} functors,
idiom brackets:
*)

let cartesian (xs : list _) ys =
  (| (,) xs ys |)

(* Idiom brackets desugar into uses of `pure` and `<*>`{.amulet}. These
definitions of `cartesian` are identical. *)

let cartesian (xs : list _) ys =
  pure (,) <*> xs <*> ys

(* Amulet has special syntax for use of the `monad`{.amulet} class. *)

let might_fail x =
  if x >= 10 then
    None
  else
    Some x

(* The `monad option`{.amulet} instance aborts the computation when it
encounters a `None`{.amulet}. *)
let None =
  let! x = might_fail 1
  let! y = might_fail 11
  pure (x + y)

(* Computations without `None`{.amulet} are simply chained. *)
let Some 2 =
  let! x = might_fail 1
  let! y = might_fail 1
  pure (x + y)
