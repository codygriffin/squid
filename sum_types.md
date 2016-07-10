# Sum Types

In squid, sum types can be created with the | operator:

```
type Variant = A | B | C; 
```

The expression 'A | A' is exactly the same as 'A'.

Functions which return multiple types will have an inferred 
return type that is the sum type of all return types.

```
fn validate(a) {
  if a.isValid() {
    return Error();
  };

  return a;
}
```
.
Here, the type of validate is inferred to be a -> (a | Error).  

```
switch validate("testing") {
  case e: Error {
    puts("error: " + String(e)) 
  };
  case a: String {
    puts(a)
  };
};
```

## Polymorphism
 
Usually we would like to avoid switch statements.  When a polymorphic
method function is called with a sum type, the most-specific version of the
function can be chosen at run-time.

```
type S = A | B | C;
// S can be an A, B or C - which is kinda like saying A, B and C are sub-types of S

fn dynamic(s: S) {};
fn dynamic(a: A) {};
fn dynamic(B: B) {};
fn dynamic(c: C) {};

let s : S = A()

if (cond) {
  s = B()
}

dynamic_func(s) // will call either A or B's version depending on cond
```

