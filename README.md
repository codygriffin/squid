# Squid

## Operator Precedence

Unlike other languages, there is no operator precedence in squid which it comes to 
binary expressions.  Expressions are evaluated from left to right, in order.

```
1 + 4 * 3 // 15, not 13
```

This means that parentheses are necessary to group expressions.


## Types

Squid's type system can be thought of as a functional interpreted language that runs 
at compile time.  Just as an interpreted language evaluates expressions into values,
Squid's type system evaluates type expressions into types.

Type expressions are made up of type variables and type operators.  For example, the 
expression `i32 + [i32]`  represents sum type which contains either an i32, or an 
[i32].  `i32 * [i32]` represents product type which is equivalent to the tuple type
`(i32, [i32]).

Function types are constructed with the `->` operator:

```
type BinaryOp = (i32, i32) -> i32;
```

Because tuples are product types, this could also be represented as:

```
type BinaryOp = i32 * i32 -> i32
```

Type constructors are like functions which take types and return new types. 

```
type BinaryOp<T> = (T, T) -> T;
```

Now we have a "free" type variable, T.

Types are equivalent if they have the same signature.  In the above example,
the types `BinaryOp<i32>` is exactly equivalent to `i32\*i32 -> i32` which is
exactly equivalent to `(i32, i32) -> i32`.


Type constructors also support variadic arguments.  As an example, let's create
a variant type, which takes a list of types which the variant can be.

```
type Variant<A> = T;
type Variant<A, B...> = A + Variant<B...>;
```

The first declaration says that a variant of a single type can only hold that type.
The second case says that a variant of a type A and some other types B... is a sum
type of A and the Variant of the other types.  

```
Variant<i8, f32, bool> = i8 + Variant<f32, bool>; // match second case, A = i8, B... = (f32, bool)
Variant<f32, bool> = f32 + Variant<bool>;         // match second case, A = f32, B... = (bool)
Variant<bool> = bool;                             // match first case, A = bool

// so after the whole type expression is evaluated we get

Variant<i8, f32, bool> == i8 + f32 + bool;
```

## Declarations

In Squid, declarations introduce new identifiers into a scope.  These can be 
local variables, functions, types or structures.  Declarations are statements,
and don't return values.

To introduce a variable x of type i32, we use the let declaration:

```
let x : i32 = 42;
```


Some declarations also create a new scope:

```
fn add(a: i32, b: i32) -> i32 {
  return a + b;
}
``` 

Here, the identifier 'add' is added to the current scope, and a new scope is created
within the function body containing identifiers a and b.

A struct works in a similar manner:
```
struct Foo {
  a: i32;
  b: string;
}
```

The type Foo is added to the current scope, and the identifiers a and b are added to
the scope of the struct.

Structs can be turned into type constructors by adding type parameters.

```
struct Foo<A> {
  a: A,
  b: string
};
```
Every struct type, regardless of the structure, will have a unique signature.

Functions are not technically types, but can also contain type parameters.

```
fn add<A>(a: A, b: A) -> A {
  return a + b;
}
```

### Destructuring

Let declarations allow destructuring of tuples:

```
let (a, b, c) : (i32, i32, i32) = (1, 2, 3); 
```

## Struct Embedding

One struct can be embedded in another by including the name in the struct.  Embeddings
must be the first listed members of the struct.

```
struct Foo {
  a: i32;
  b: bool;
};

struct Bar {
  Foo;
  c: string;
}

let x : Bar = Bar {
  a: 0;
  b: false;
  c: "woohoo";
}
```

When a struct is embedded into another struct, the outter struct becomes a subtype of 
the inner struct:  anywhere the inner struct is valid, the outer struct will also be
valid.

In the event that there is a conflict (an embedded struct contains a member with the 
same name as the outer, or multiple embedded structs contain the same member), the 
embedding can be qualified with the type name:

```
struct A {
  a: i32;
};

struct B {
  A
  a: i32;
}

let x : Bar = Bar {
  a: 0; // error: A is uninitialized
}

let x : Bar = Bar {
  A.a: 0;
  a: 1; 
}
```

## Dynamic Dispatch 

```
struct RigidBody {
  x: f32;
  y: f32;
};

struct Collision {
  radius: f32;
};

struct Ship {
  RigidBody;
  Collision;
};

struct Rock {
  RigidBody;
  Collision;
};

trait Physics {
  fn update(dt: f32);
};

fn update(rock: Rock, dt: f32) {
  // update rock
};

fn update(ship: Ship, dt: f32) {
  // update ship
};

let rocks = Vector[Rock {}, Rock {}, Rock {}];
let ships = Vector[Rock {}, Rock {}, Rock {}];
```

## Boxes

Like C, Squid arrays and pointers are very closely related.  In Squid, a pointer 
is called a 'box', and its value is the address in memory in which you can find
the contents of the box.

Unlike C, 'indexing' into an array returns the address, and not the value.  This
allows pointer arithmetic with a tiny amount of safety.  

```
let x : i32 = 42;   // let's say the stack ptr is 0xdeadbeef
let y : [i32] = &x; // here, the value of y is 0xdeadbeef 
let z : i32 = *y;   // z is the contents of the box, 42

let a : [i32;3] = [1, 2, 3];  // *a is a compile error (can't dereference an array)
let b : [i8] = a[0];          // b == a (as addresses); *b = 1
let c : [i8] = a[1];          // c == a + 4;            *c = 2
let d : [i8] = a[2];          // d == a + 8             *d = 3
let e : [i8] = a[3];          // comile error - 3 is out of bounds
```

Boxes can be indexed exactly like arrays, but without the compile-time error.

Boxes and arrays shouldn't be used directly - instead use the collection and smartptr types:


## Examples

```
type string = [i8]           // [] without size is a pointer - a 'box'
type FourLetterWord = [i8;4] // [] with size is an array

struct Foo {
  a: i32;
  b: string;
};

fn puts(msg: string) -> i32;

fn main(argc: i32, argv: [string]) {
  print("testing", 1, 2, 3);

  let foo = Foo {
    .a = 42;
    .b = "foo";
  };

  print(serialize(foo));
  return 0;
}

fn print<A, B...> (msg: string, a: A, b...: B) { 
  print<*B>(msg + "X", b...);
}

fn print(msg: string) { 
  puts(msg + "\n");
}

fn serialize(foo: Foo) -> string {
  return "later";
}
```
