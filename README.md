# Herring-lang

## TODO 
- More features 

## Example 
```
bar : (x : Int) -> (y : Int) -> Int 
    return x + y
foo : (x : Int) -> Int 
    return bar (x, 1)
main : Int
    let y = 69 in
    let x = 7 in 
    return x + (foo (y))
```
