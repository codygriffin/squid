# Boxes and Arrays


## Arrays

An array has a header which describes its geometry, allowing it to be safely accessed at runtime.

```
type Vector4 = [Float; 4];       // shape = 4,        stride = 1
type Matrix4 = [Float; 4, 4];    // shape = (4, 4),   stride = 4, 1
type Image   = [Float; 4, 4, 2]; // shape = (4, 4, 2) stride = 8, 2, 1
```

As you can see, the size of the header is determined by the dimension of the array.  

Arrays can be indexed with the [] operator.  Arrays support 'currying' in that a single index into a two 
dimensional array yields a one-dimensional array.

```
let img = Image(...);

let pixel = img[0,0];
//pixel : Array[Float; 2]

let c1 = pixel[0]; 
//c1 : [Float]
```

Indexing a one dimensional array does not return the value - it instead returns the Box corresponding to 
that location.  

# Boxes

Boxes are pretty much pointers, without supporting pointer arithmetic.  If you need pointer arithmetic, 
you should be using arrays.  
