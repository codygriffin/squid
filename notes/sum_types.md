# Sum Types

In squid, sum types can be created with the | operator:

```
type Variant = A | B | C; 
```

The expression 'A | A' is exactly the same as 'A' (each type is its own identity).  
The expression 'A | void' is exacty the same as 'void' (void is the zero type)

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

## Values of sum types

A value of a sum type can be any of its types at runtime.  These types are said to be
subtypes of the sum type.  If we have the type:

```
type A = ...
type B = ...
type C = ...
type Variant = A | B | C;
```

Then the following type relations are true:

```
Variant > A
Variant > B
Variant > C
```

Sum types can be safely switched at runtime:

```
let s : Variant = A();  // Create a variant which is initialized to a value of type A

match (s) {
  a: A {
    // do something when the value of s is of type A
  }
  b: B {
    // do something when the value of s is of type B
  }
  c: C {
    // do something when the value of s is of type C
  }
}
```

Match statements MUST cover all possible types in the sum type.

