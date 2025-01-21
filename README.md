# Aether

Aether is a functional programming language with a familiar syntax.
The following is a sample of what Aether code will look like.

```
module hello

proc main() -> IO<> {
    println("Hello World!")
    let f1 = factorial1(10)
    let f2 = factorial2(10)
    assert(f1 == f2)
}

fn factorial1(x uint) -> uint {
    return match (x) {
        0 -> 1
        x -> x * factorial(x - 1)
    }
}

// With tail-call optimisation
fn factorial2(x uint) -> uint {
    let f = fn (x uint, y uint) {
        return match (x, y) {
            (0, y) -> y
            (x, y) -> factorial2(x - 1, x * y)
        }
    }
    return f(x, 1)
}
```

