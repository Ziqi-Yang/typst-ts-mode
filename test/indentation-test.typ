// Note that some indentation bugs can only be found on real time inserting
// Use `typst-ts/util/setup-indent-debug-environment' function in `side/utils.el'
// you may also want to turn off `auto-visited-file-mode` or similar (for testing
// end of buffer condition)

// typst-ts-mode-indent-offset: 4


#show: cv.with(
    author: "",
    [
        emacs
        eacs
        emacs
        emacs
    ]
)

= El
== Psy
1. Kongaroo
== ERROR indentation

= Header 1
stuff
== Header 2
stuff
== Header 2
- list
+ list
    + list
    2. list
        - list \
            El Psy Kongaroo
            El Psy Kongaroo \
            El Psy Kongaroo

stuff
=== Header 3
stuffs
stuffs

= Raw Block
```rust
fn main() {
    println!("El Psy Kongaroo");
}
```

```rust
  fn main() {

  }
```

= Script
#if 1 < 2 [
    This is shown
] else [
    This is not
]

#let n = 2
#while n < 10 {
    n = (n * 2) - 1
    (n,)
}

#for letter in "abc nope" {
    if letter == " " {
        break
    }

    letter
}

#(
    "a, b, c"
        .split(", ")
        .join[ --- ]
)

= Math
$
    A = pi r^2
$
  
#[
    $
        cal(A) := {
            x in RR | x "is natural"
        }
    $
]

$
    cal(A) :=
    {
        x in RR | x "is natural"
    }
$

$ sum_(k=0)^n k
    &= 1 + ... + n \
    &= (n(n+1)) / 2 $

$
    sum_(k=0)^n k
        &= 1 + ... + n \
        &= (n(n+1)) / 2
$
