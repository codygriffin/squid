# Squid

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
