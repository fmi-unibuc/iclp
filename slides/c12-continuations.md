---
title: Implementarea Concurenței în limbaje de programare
subtitle: Async functions --- Continuation Passing Style
author: Traian Florin Șerbănuță
institute: FMI @ UNIBUC
abstract: |
---

# Asynchronous functions compilation

## Recall asynchronous functions

```js
async function select(p1, p2, p3) {
    if (await p1) return 1 + await p2
    else return 1 - await p3
}

async function showChoice() {
    console.log("start: " + new Date())
    var v = await select2(p1, p2, p3)
    console.log("Chosen value: " + v)
    console.log("  end: " + new Date())
}
```

- Evaluates `p1` and, depending on its value, either `p2`  or `p3`
- `await` can be used (as unary operator) in any expression
- Since select returns a promise, it can be `await`ed on


## Semantics of asynchonous functions

- The remainder of a function after an await is transformer into a callback
- The callback is scheduled as a handler for the event fulfilling the promise
- The callback is a function that
  - takes as argument the result computed by the await call
  - and __continues__ executing the remainder of the function
  - enter __continuations__

# Continuations

## Continuations (control as data)

- Ideas initiated in functional programming

### Reason 1
- Functional programs have a lot of function calls
- Traditional function calls require maintaining a call stack
  - to know where to return control
- However, what if we _don't need to return_?
  - then no call stack is needed

### Resson 2
- Functional programs usually have no control over the execution
- This makes it harder to capture control-intensive behavior
  - return, break/continue, try/catch
- However, what if the execution context can be passed around and altered?
  - then one can implement control-intensive behavior functionally

## Continuation Passing Style (CPS)

### [Following Mattox Beckman's presentation](https://pages.github-dev.cs.illinois.edu/cs421-sp20/web/slides/05.1.1-cps.pdf)

## Exception handling using continuations

```js
    function test(x, y) {
      if (y == 0) throw "division by 0"
      else return x / y;
    }

    function main() {
      try {
        console.log("Result: " + test(3,1))
      } catch(e) {
        console.log("Error: " + e)
      }
      console.log("Done")
    }

    main()
```

Can be rewritten into continuation passing style as

```js
    function ktest(x, y, k, e) {
      if (y == 0) return e("Division by 0")
      else return k (x / y)
    }

    function k(v) {
      console.log("Result: " + v)
      console.log("Done")
    }

    function e(v) {
      console.log("Error: " + v)
      console.log("Done")
    }

    function kmain() {
      ktest(3, 0, k, e)
    }

    kmain()
```


## Coroutines

```js
async function select(p1, p2, p3) {
    if (await p1) return 1 + await p2
    else return 1 - await p3
}
```
. . . 
```js
function select(p1, p2, p3, k) {
  p1.then(\v -> k1(v, p2, p3))
}
```
. . .
```js
function k1(v1, p2, p3) {
  if (v1) return p2.then(\v -> k2(v))
  else return p3.then(\v -> k3(v))
}
```
. . .
```js
function k2(v) {          function k3(v) {
  return 1 + v              return 1 - v
}                         }
```

## CPS transform

### [Following Mattox Beckman's presentation](https://pages.github-dev.cs.illinois.edu/cs421-sp20/web/slides/05.1.2-cps-transform.pdf)

## Compiling async functions using CPS transforms

```js
async function select(p1, p2, p3) {
    if (await p1) return 1 + await p2
    else return 1 - await p3
}
```
- statements before first await can stay
- `C[await p]` is converted to
  `return p.then(v -> { C[v] })`{.js}
- transformation continues inside `C[v]`
```js
function select2(p1, p2, p3) {
    return p1.then(b => {
        if (b) return p2.then(v2 => v2 + 1);
        else return p3.then(v3 => 1 - v3);
    })
}
```

# Web Workers

## WebWorkers

### [Following Tobias Pfeiffer's presentation](https://www.slideshare.net/PragTob/javascript-web-workers)


# Event Emitters

## Event Emitters in NodeJS

### [Following Eyal Vardi's presentation](http://slideshare.net/EyalV/event-emitter)