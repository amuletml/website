---
title: Getting Started
description: A brief introduction into using Amulet.
next: 02-data.html
maths: true
---

Hello and welcome. This series is meant as an introduction to the [Amulet] programming language. It
assumes some previous programming experience, but no experience with
functional languages. If you're already familiar with ML-family
languages, feel free to skip this one.

### Introduction

Like all languages in the ML family, Amulet is a _strict_
_strongly-typed_ language with pervasive type inference; The compiler
can check your code for correctness even if you omit most (if not all)
type annotations. As a simple example, the compiler can easily figure
out the function `magnitude`{.amulet} below works on floating-point
values:

```amulet
let magnitude r = (r.x *. r.x +. r.y *. r.y) **. 0.5
```

Unlike most other major languages in the ML tradition (OCaml and
Standard ML, for instance), Amulet is able to infer a principal type for
the `magnitude` function even though the set of fields in the record `r`
isn't statically known.

> A note on presentation: Code blocks presented in block quotes (such as
> this one) are not executable, and may not even parse (here, for
> instance, Amulet has no `val` signatures.)

<blockquote>
```amulet
val magnitude : { 'a | x : float, y : float } -> float
```
</blockquote>

The principal type for the `magnitude` function indicates that it can
take a record with _any number of fields_, as long as the `x` and `y`
fields are both present and are `float`{.amulet}s.

While Amulet does not have a module system as complex as other MLs', it
has a more expressive type system than most, with support for
_higher-rank polymorphism_, _kind polymorphism_, _GADTs_, _type classes
with associated types_, _closed type functions_, _functional
dependencies_ and more[^1]. As of recently, Amulet can even be used to
prove simple properties of functions on the type level!

### Arithmetic

Amulet has two built-in number types, `int`{.amulet} and
`float`{.amulet}; These are the types of literals, such as `123` and
`0.5`. Note that since Amulet has no overloading for numeric literals or
subtyping, `123 : float`{.amulet} is a type error. Even further, Amulet
does not overload numeric _operators_, so we have two sets: The familiar
`+ - * /`{.amulet}, which work on `int`{.amulet}egers[^2], and `+. -. *.
/.`{.amulet}, which work on `float`{.amulet}ing point numbers. There are
also exponentiation operators `**`{.amulet} and `**.`{.amulet}.

We can do simple arithmetic in the
<span class=definition title="Read-eval-print-loop">REPL</span>:

> Lines starting with a `>` are user input. The rest is printed by the
> compiler.

```amulet
> 1 + 1
_ = 2
```

Definitions can be made using the `let ... = ...` form:

```amulet
> let x = 2
x = 2
> x * x
_ = 4
```

Amulet will prevent us from adding a `float`{.amulet}ing-point number to
an `int`{.amulet}eger:

```amulet
> x + 2.0
(*
=stdin[1:5 ..1:7]: error
  │
1 │ x + 2.0
  │     ^^^
  Couldn't match actual type float
    with the type expected by the context, int
*)
```

### Defining Functions

It was shown in the first section that functions can be defined with
`let function_name argument = body ...`{.amulet}. However, this is slightly
misleading, as it (almost) implies that functions can only be defined by
using `let`{.amulet}.

This is not the case: Functions, like almost everything in Amulet, are
expressions, and there is an associated literal notation for expressing
functional values: `fun argument -> body`{.amulet}. Notice that
`argument` here is singular.

This isn't a typo: Amulet functions can only have one parameter.

_"What?!"_, I hear you say. This is not as limiting as you can think (in
fact, it is not limiting at all): Since functions are first-class
values, we can return functions from functions. This lets us define
functions of multiple arguments by _currying_: For each parameter,
there's an intervening function.

Concretely, the function `(x, y, z) => x + y + z`{.javascript} (in
JavaScript notation) is defined in Amulet by the expression

```amulet
> :t fun x -> fun y -> fun z -> x + y + z
fun x -> fun y -> fun z -> x + y + z : int -> int -> int -> int
```

Function arrows `->`{.amulet} _associate to the right_: That is, the
type `A -> B -> C`{.amulet} is to be read as `A -> (B -> C)`{.amulet},
never `(A -> B) -> C`{.amulet}: The latter is a function of a single
parameter (that is also a function).  Since writing `fun x -> fun y ->
fun z -> ...`{.amulet} is boring, you can write multiple arguments in a
single `fun`{.amulet}: `fun x y z -> ...`{.amulet}. This is directly
equivalent to the longer formulation.

This might sound like it leads to a terrible performance cost, what with
allocating all those closures, but the Amulet compiler aggressively
optimises these away.

One might wonder if these curried functions are truly equivalent to the
familiar functions of multiple arguments in other programming languages.
We can verify this [using amc-prove], by asking
for an isomorphism between `(A -> B -> C)`{.amc-prove} and `A * B ->
C`{.amc-prove}.

```amc-prove
> (A -> B -> C) <-> (A * B -> C)
yes:
  Equiv (fun h (a, b) -> h a b, fun f y z -> f (y, z))
```

_"What's that `*`{.amulet}?"_, you ask. This leads us nicely to our next
topic.

### Tuples and Records

Amulet has two primitive, non-extensible[^3] heterogeneous collections:
_tuples_ (in reality, _pairs_) and _records_.

A pair is just that: Two values, associated. Pairs are written like `(x,
y)`{.amulet}, and their type reflects the types of both components. For
instance, `(1, 2)`{.amulet} has type `int * int`{.amulet}. Why are pair
types written with a `*`{.amulet}? This notation, traditional to the ML
family, is because pairs are like Cartesian products of types. Moreover,
if the type `A` has $a$ elements and the type `B` has $b$ elements, the
type `A * B`{.amulet} has $ab$ elements.

Concretely, let us take pairs of booleans as an example. How many
elements does `bool * bool`{.amulet} have? Well, `bool`{.amulet} has 2
elements, so it stands to reason that `bool * bool`{.amulet} has 2 * 2 =
4 elements. Indeed:

<blockquote>
```amulet
(true, true)
(true, false)
(false, false)
(false, true)
```
All of these expressions have type `bool * bool`{.amulet}.
</blockquote>

The syntax `(a, b, c)`{.amulet} is shorthand for `(a, (b, c))`{.amulet}.

Destructuring pairs can be done with _pattern matching_. Here, we use
the wildcard pattern `_` to ignore the second component of the pair, or
Amulet will complain that we bound a variable and did not use it.

```amulet
> let first (x, _) = x
_ = <function>
> :t first
first : ('a * 'b) -> 'a
```

Records, unlike pairs, can contain many values (as opposed to just two),
and the positions are labelled:

```amulet
> { x = 1, y = 2.0 }
_ = { x = 1, y = 2.0 }
```

Record accesses are written postfix: `r.x`{.amulet}. Unlike in OCaml and
Standard ML, this does not fix the type of the record, as mentioned in
the introduction; It merely constrains `r` to have the field `x`. This
lets us have almost dynamic-language levels of flexibility when
accessing records, since unused fields are simply ignored!

You can never have a type error from having _too much_ in a record, just
from having _not enough_.

### Using the Compiler

The Amulet compiler, `amc`, supports both batch compilation (where a
file is compiled into Lua) and interactive execution, where you load a
file and can interact with it from the REPL.

There is as of writing no mechanism for compiling multiple files other
than listing them all in the `amc` invocation, in dependency order. For
instance, if `main` depends on `mod1`, which depends on `mod2`, you'd
have to compile them as such (calling the output file `main.lua`):

```bash
$ amc compile main.ml -o main.lua
```

If you want to enter the REPL with files loaded, replace the `compile`
command with `repl` .

Note that, in the top-level of a file, only declarations can be present.
To evaluate an expression for its side effects, bind it to the wildcard
pattern `_`{.amulet}. When the expression is of type `unit`{.amulet},
you may also use `()`{.amulet} for greater clarity:

```amulet
let () = put_line "hello, world!"
```

Foreign functions can be defined using the `external val` construct. The
text in quotes should be a Lua expression (It will be syntax-checked and
pretty-printed by the Amulet compiler). To use multiple lines in the
definition, escape the newline character by placing a backslash (` \\ `)
right before the line break.

```amulet
external val put_line : string -> unit = "print"
```


[^1]: Don't worry, I'm just namedropping terms here; They'll be
explained in later tutorials, as they are very complicated indeed.

[^2]: The division operator on `int`{.amulet}s returns a
`float`{.amulet}.

[^3]: Records can be extended [using magic](https://abby.how/posts/2019-09-22.html).

[editor integration]: https://github.com/amuletml/amulet/tree/master/editor
[using amc-prove]: https://abby.how/posts/2019-09-29.html
[Amulet]: https://github.com/amuletml/amulet
