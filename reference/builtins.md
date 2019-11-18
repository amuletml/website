---
title: Complete List of Built-ins
description: All built-ins in Amulet.
---

This is a list of all the built-ins in Amulet.

## Implicitly in scope

### Integers

```amulet
type int
```

### Floats

```amulet
type float
```

### Strings

```amulet
type string
```

### Lists

```amulet
type list ('a : type) =
  | Nil
  | Cons of 'a * list 'a
```

### Functions

```amulet
type (->) ('a : type) ('b : type)
type ( * ) ('a : type) ('b : type)
val (@@) : ('a -> 'b) -> 'a -> 'b
```

### Lazy values

Lazy values are guaranteed to be forced only once.

```amulet
type lazy ('a : type)
val lazy : 'a -> lazy 'a
val force : lazy 'a -> 'a
```

### References

```amulet
type ref ('a : type)
val ref : 'a -> ref 'a
val (!) : ref 'a -> 'a
val (:=) : ref 'a -> 'a -> unit
```

### Built-in classes

```amulet
(* The return kind of classes *)
type constraint
class 'a ~ 'b
```

## `module Amc`

### Type-level literals

```amulet
class known_int ('a : int) begin
  val int_value : forall 'a -> int
end

class known_string ('a : string) begin
  val string_value : forall 'a -> string
end
```

### Row manipulation

```amulet
class
  row_cons ('record : type) ('key : string) ('type : type) ('new : type)
  | 'record 'key 'type -> 'new
  , 'new 'key -> 'record 'type
begin
  val extend_row : forall 'key -> 't -> 'r -> 'n
  val restrict_row : forall 'key -> 'n -> 't * 'r
end
```

### Custom type errors {#type-errors}

Reduction of the `type_error` function, or its use as a class
constraint, causes compilation to halt with the given `error_message`.

```amulet
type error_message =
  | String of string
  | ShowType of type
  | (:<>:)  : error_message -> error_message -> error_message
  | (:<#>:) : error_message -> error_message -> error_message

type function type_error ('message : error_message)
```
