---
title: Implementarea Concurenței în limbaje de programare
subtitle: JavaScript --- Promises and ~~lies~~ callbacks
author: Traian Florin Șerbănuță
institute: FMI @ UNIBUC
abstract: |
---


# JavaScript concurrency overview

## JavaScript execution model

- Execution environment
  - created when a new page is opened
  - contains everything needed for code interaction
  - acts as a sandbox (code cannot reach outside) 
- JavaScript Interpreter (runs the code)
  - Single threaded interpreter
  - Run-to-completion
- Event-based interaction
  - Task/Event queues (things to be run, with expiration times)
  - Event loop (for servicing the tasks queues)
- Web workers

## Event-based interaction
- Single Event-loop, servicing task/event queues:
  + UI events (mouse, keyboars)
  + network events (server request completed)
  + Script events (timers, promises)
- Task queues have priorities
  - User events are more important than network events
- Callbacks
  + code to be executed when events are triggered
- Events cannot interrupt callbacks

## Web Workers

- Instances of the interpreter
- No UI capabilities
- Interact with parents (and other workers) through messages
  - method `postMessage`

### Worker 
- a running worker thread
- allowing passing messages to the running worker code.

### SharedWorker
- a special worker
- can be accessed from several browsing contexts
  (windows, iframes, other workers)
- communicates with others through `MesssagePort`s

# Synchronizing with Promises

## Promises

- An abstraction for tasks producing values
  + decoupling the value (future) ...
  + ... from how it was computed (promise)
  + Originally proposed in 1976 by Daniel Friedman and David Wise
- Proxies for results which are yet to be computed
- Offer an uniform way to refer to tasks
  - making concurrency/parallelism transparent to programmer


## Promises in JavaScript

- Wrap (asynchronous) actions
- Can associate handlers for the success /  failure of the action
- Blends well with the asynchronous nature of JavaScript
  - can wrap AJAX requests
  - can wrap asynchonous work done by a Web Worker
  - can wrap synchonous tasks, too

## The state of a promise

Pending

: initial state of a promise

Fulfiled

: The operation completed succesfully, producing a value


Rejected

: The operation failed, with a reason (error)


- Every promise starts in the initial state
- Transitions _either_ to the __fulfilled__ or __rejected__

## Creating a promise

```js
var promiseObj = new Promise(executor);
```

### Executor

A function to be run asynchronously
```js
function executor(resolve, reject) {
    setTimeout(() => {
        var r = Math.random()
        if (r > 0.5) resolve(r)
        else reject("Too small!")
    }, 2000)
}
var promiseObj = new Promise(executor)
```

`resolve(value)`

: signals the promise being __fulfilled__

`reject(value)`

: signals the promise being __rejected__

## Pure (synchronous) promises

`Promise.resolve(value)`

: a promise which immediately resolves to `value`

`Promise.reject(error)`

: a promise which immediately gets rejected with `value`

## Sequencing promises

A promise can be extended with code to obtain another promise

![](https://mdn.mozillademos.org/files/15911/promises.png)

## Promise sequencing operations (methods of Promise)

`then(result => ... )`

: new promise based on fullfilled state

`catch(error => ... )`

: new promise based on rejected state

`finally(() => ... )`

: Runs for both cases, without changing them

`then(result => ..., error => ... )`

: new promise handling both fulfilled and rejected states


## Sequencing example

```js
promiseObj
    .finally(() => console.log(new Date() + "Promise finished!"))
    .then(
        res => Math.floor(res * 10),
        err => Promise.reject(err + "\nwhen calling mul"))
    .then(
        res => res + 1,
        err => Promise.reject(err + "\nwhen calling inc"))
    .then(res => console.log("Final result: " + res))
    .catch(err => console.log("Err: " + err))
```

__Note:__ `Promise.reject`{.js} used to propagate rejection


## Parallel composition of futures

`Promise.all(futures ...)`

: future waiting for all futures to succeed (in parallel)

`Promise.allSettled(futures ...)`

: future waiting for all futures to finish (in parallel)

`Promise.race(futures ...)`

: future waiting for one future to succeed (in parallel)

`Promise.any(futures ...)`

: future finding the first future to succeed (sequentially)
: not yet supported (proposal stage)


## `Promise.all(futures ...)`

- future synchronizing all futures (in parallel)
- if succeeding, resolves to the list of results
- if either of them is rejected, rejects with the first reason


```js
var p1 = Promise.resolve(3);
var p2 = new Promise((resolve, reject) =>
  setTimeout(() => resolve("foo"), 3000)
); 
var p3 = new Promise((resolve, reject) =>
  setTimeout(() => resolve("bar"), 2000)
); 

Promise.all([p1, p2, p3]).then(values => { 
  console.log(new Date() + values);
}); // [3, "foo", "bar"] 
```

- Takes ~ 3 seconds (timers started at the same time)

## `Promise.all` example with failures

```js
var p1 = Promise.resolve(3);
var p2 = new Promise((resolve, reject) =>
    setTimeout(() => resolve("foo"), 3000)
);
var p3 = new Promise((resolve, reject) =>
    setTimeout(() => reject("bar"), 2000)
);
  
Promise.all([p1, p2, p3])
    .finally(() => console.log(new Date() + "Promise finished!"))
    .then(values => console.log("Result: " + values))
    .catch(err => console.log("Error: " + err));
console.log(new Date() + "Promise started!")
```

- takes ~ 2 seconds (until p3 fails)
- prints `Error: bar` (and no result)

## `Promise.allSettled`

- future synchronizing all futures (in parallel)
- resolves to list of all results (be them fulfilled or rejected)

```js
    var p1 = Promise.resolve(3);
    var p2 = new Promise((resolve, reject) =>
        setTimeout(() => resolve("foo"), 3000)
    );
    var p3 = new Promise((resolve, reject) =>
        setTimeout(() => reject("bar"), 2000)
    );
    Promise.allSettled([p1, p2, p3]).then(console.log)
```
- Prints 
```json
[ {status: "fulfilled", value: 3}
, {status: "fulfilled", value: "foo"}
, {status: "rejected", reason: "bar"}
]
```

## `Promise.race(futures ...)`

- future waiting for one future to finish (in parallel)

```js
var p2 = new Promise((resolve, reject) =>
    setTimeout(() => resolve("foo"), 3000)
);
var p3 = new Promise((resolve, reject) =>
    setTimeout(() => resolve("bar"), 2000)
);

Promise.race([p2, p3])
    .finally(() => console.log(new Date() + "Promise finished!"))
    .then(value => {
        console.log("Result: " + value); // Result: bar
    });
console.log(new Date() + "Promise started!")
```

- Takes ~ 2 seconds (for p3 to finish)

## `Promise.race(futures ...)`

- future waiting for one future to finish (in parallel)
- Propagates both resolved values and rejects

```js
var p2 = new Promise((resolve, reject) =>
    setTimeout(() => resolve("foo"), 3000)
);
var p3 = new Promise((resolve, reject) =>
    setTimeout(() => reject("bar"), 2000)
);

Promise.race([p2, p3])
    .finally(() => console.log(new Date() + "Promise finished!"))
    .then(value => console.log("Result: " + value))
    .catch(error => console.log("Error: " + error)) // Error: bar
    ;
console.log(new Date() + "Promise started!")
```

- Takes ~ 2 seconds (for p3 to finish)

# Asynchronous functions with  async / await 

## Asynchronous functions: Basic idea

- Let us write asynchronous code in a functional style
- Code flow looks more natural (no need for callbacks)
  - we _await_ for values from promises
- It is compiled into a sequential compositions of promises
- As such it is itself a promise and can be *await*ed from other functions.
- While an async function awaits, it yields control.

## Async / await example: Debugging a promise

```js
async function debug(p, name) {
    console.log(name + " started at " + new Date())
    try {
        var v = await p; //yields control
        console.log(name + " result: " + v);
    } catch (err) {
        console.log(name + " error:" + err);
    }
    console.log(name + " ended at " + new Date())
}
```

### `await`
  - can only be used inside `async function`s
  - waits for the promise to finish
  - if promise rejected, throws the reason as exception
  - yields execution back to calling context (returns a promise)

## Comparing async / await with chaining promises chaining

### Async function
```js
async function debug(p, name) {
    console.log(name + " started at " + new Date())
    try {
        var v = await p; //yields control
        console.log(name + " result: " + v);
    } catch (err) {
        console.log(name + " error:" + err);
    }
    console.log(name + " ended at " + new Date())
}
```

### Chaining promises
```js
function debug2(p, name) {
    console.log(name + " started at " + new Date())
    p   .then(v => console.log(name + " result: " + v))
        .catch(err => console.log(name + " error:" + err))
        .finally(() => console.log(name + " ended at " + new Date()))
}
```

## Example: An asynchronous choice

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

## Compiling async functions ([CPS transformation](https://wiki.c2.com/?CpsTransformation))

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