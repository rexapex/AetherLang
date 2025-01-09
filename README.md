# Aether

Aether is a functional programming language written with a familiar syntax.

```
module hello

proc main() -> IO<> {
    let f = factorial(10)
    println(f)
}

fn factorial(x uint) -> uint {
    return match (x) {
        0 -> 1
        v -> x * factorial(v - 1)
    }
}
```

