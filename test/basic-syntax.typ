// This file only serves for testing highlight, and it is not a syntax completion test.

#let a = 1
#let b = 2
#let c = 3
#let t = true
#let f = false
#let values = (1, 7, 4, -3, 2)
#set heading(numbering: "1")

// comment

-? $ -> $ // shorthand

// header face
= headline _emph_
https://www.google.com // url
_emph_ // emphasis
*strong* // strong
- item // item
/ term1: value
"El Psy Kongraoo" // quote
hello \ // line break
`El Psy Kongraoo` // raw span
// raw block
```bash
sudo rm -rf *
```
= hi <label> // label
@label // reference
   
Hello\nWorld // escape


#(4.2) // number
#"El Psy Kongaroo" // string
#[El Psy Kongraoo] // content
#true #false  // boolean
#sym.bar.h  // builtin
#set text(lang: "zh") // call & builtin
#none // none
#auto // auto
#(a + b) // ident

#("0" in "0" and "0" not in "1") // in
#(t and f or not t) // and, or, not
#(2 + - 1) #(2 - -1) // sign
#(1 + 1) // add
#(1 - 1) // sub
#(1 * 1) // mul
#(1 / 1) // div
#if 2 > 1 [] // cmp
#import "dumb_import.typ": * // wildcard

#let a = b  // let
#if f {} else {} // branch
#while a < 10 { // while
    a += 1
}
#for i in (a, b, c) {} // for
#import "dumb_import.typ": x, y as yyy // import
#include "dumb_import.typ" // include
#show: columns.with(2) // show
#set text(lang: "en") // set
#let a() = { // return
    return 2
}
#context text.lang // context
#for letter in "abc nope" { // flow
    if letter == " " {
        break
    } else if letter == "a" {
        continue
    }
    letter
}

#range(5).zip(range(10)).map( // method
    ((a,b)) => a + b // TODO lambda
)
#range(20, step: 10) // tagged

#values.0 // field

$ a $ // math
$ 1 + 1 = 2 $
$ E = m * c^2 $
$ eq.not(0) $
$ cal(A) := { x in RR | x "is natural" } $
