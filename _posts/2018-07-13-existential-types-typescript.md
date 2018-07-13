---
title: Encoding Existential Types In TypeScript
layout: post
categories: [Programming, TypeScript]
---

In this post we will take a look at a use case where the encoding for existential types helped me create a small utility wrapper for the [`immutable-assign`](https://github.com/engineforce/ImmutableAssign) library.

# The What

I have been using immutable-assign for updating immutable datastructures. This library provides the function `iassign` for deep immutable update on plain javascript objects. For example, if we have `x = { a: { b: 1 } }` then we can use `iassign(x, x => x.a.b, x => 5)` to obtain `{ a: { b: 5 } }` without mutating `x`.

However, one thing that had been bothering me is updating multiple properties in one go. For example, if we want to update all values in `{ a: { b: 1, c: 2}, d: 3 }` by 1. Then we have to do:

```ts
iassign(iassign(iassign(
  { a: { b: 1, c: 2 }, d: 3 },
  x => x.a.b, x => x + 1),
  x => x.a.c, x => x + 1),
  x => x.d, x => x + 1);
```

We need one `iassign` for each property that is updated. At the end of the post we will be able to slightly compress this and use the following syntax:

```ts
focus(
  { a: { b: 1, c: 2}, d: 3 },
  over(x => x.a.b, x => x + 1),
  over(x => x.a.c, x => x + 1),
  over(x => x.d, x => x + 1),
);
```

# The How Not

Why would we need something as scary sounding as 'existential types'? Let's have a first attempt without anything fancy.
Our use case boils down to writing a function which takes a value of type `S` and then a collection of getters `S => A` and modifiers `A => A`.
The type `GetModOps<S, A>` encodes a record with such a getter and modifier function.
Then the `focus` function accumulates the results of consecutive `iassign` calls with these getters and modifiers, achieved by a `reduce`.

```ts
type GetModOps<S, A> = { get: (s: S) => A, modify: (a: A) => A };

function focus<S, A>(
  s: S,
  ...ops: GetModOps<S, A>[]
) {
  return ops.reduce((acc, op) => iassign(acc, op.get, op.modify), s);
}
```

That seemed simple enough, let's try it out.

```ts
const x = { a: { b: 1 } };

focus(x,
  { get: x => x.a.b, modify: (x: number) => x + 1 }
);
```

Okay, let's try a more complex example.

```ts
const x = { a: { b: 1, c: "a" } };

focus(x,
  { get: x => x.a.b, modify: (x: number) => x + 1 },
  { get: x => x.a.c, modify: (x: string) => x + "a" }, // error :(
);
```

Whoops, A type error! We can see what is going wrong by looking at how the generic types for `focus` are instantiated.

First the parameter `x` instantiates type `S` to `{ a: { b: number, c: string } }`.

```ts
focus<{ a: { b: number, c: string } }, A>(x,
  ...ops: GetModOps<{ a: { b: number, c: string } }, A>[]
)
```

Now, the first passed `GetModOps` instantiates type `A` to `number`.

```ts
focus<{ a: { b: number, c: string } }, number>(x,
  { get: x => x.a.b, modify: (x: number) => x + 1 },
  ...ops: GetModOps<{ a: { b: number, c: string } }, number>[]
)
```

Then, the last `GetModOps` has type `string` where `A` was originally, but has now been instantiated to `number`.

```ts
focus<{ a: { b: number, c: string } }, ???>(x,
  { get: x => x.a.b, modify: (x: number) => x + 1 },
                                      ^
 A cannot be both string and number ! |
			   	      v
  { get: x => x.a.c, modify: (x: string) => x + "a" },
)
```

The type `string` and `number` do not unify and this results in a type error.

Currently, `focus` specifies that it works for any choice of `S` and `A`, but the choice of this `S` and `A` is _fixed_.
So when we try to have updates focused on a property of type `number` and `string` at the same time it does not work.

Instead, what we intended is that the parameter `A` is existentially quantified on the type `GetModOps`.
This means that for each `op` in `ops` there _exists_ a type `A` for which we have an `S => A` and `A => A`.


# The How

So, our intended `focus` function is more like this: `focus` works for all types `S` and each `op` in `ops` has a, potentially different, type `A` associated with it.

```ts
type GetModOps<S> = <exists A>{ get: (s: S) => A, modify: (a: A) => A }

function focus<S>(
  s: S,
  ...ops: GetModOps<S>[]
) {
  return ops.reduce((acc, op) => iassign(acc, op.get, op.modify), s);
}
```

Where `<exists A>` is invented syntax to specify the existential type `A`.
Obviously, typescript does not support this syntax, so what do we do?

Luckily, we remember the incantation `<exists A>(T<A>) = <R>(cont: (<A> (t: T<A>) => R)) => R` from our [spell book](http://www.cis.upenn.edu/~bcpierce/tapl/) (slightly adapted to fit the syntax used in typescript).

We create the type `GetMod<S>` which encodes `<exists A>(GetModOps<S, A>)`.

```ts
type GetModOps<S, A> = { get: (s: S) => A, modify: (a: A) => A };
type GetMod<S> = <R>(cont: <A>(t: GetModOps<S, A>) => R) => R;
```

... Let's take a step back here, what exactly is `GetMod` and how do we use it?

## Continuations

The type `GetMod` has the form `(_ => R) => R`, which is the typical form of continuation passing style.

Let's first take a look at the slightly simpler `type NumberCont<R> = (cont: (x: number) => R) => R`.
It is a function that takes a function `(x: number) => R` as parameter.
We can think of `(x: number) => R` as awaiting a number for completion, also known as a continuation.
So, the following function `yourNumber` is waiting for a `number` to return a `string`.

```ts
function yourNumber(x: number): string {
  return ("your number is " + x);
}
```

Then, `myNumber` of type `NumberCont<string>` passes the value `1` to a continuation waiting for a `number`.

```ts
const myNumber: NumberCont<string> = cont => cont(1);
```

We obtain our result value by passing the continuation `yourNumber` to `myNumber`, or `myNumber(yourNumber)` return `"your number is 1"`.

## Polymorphic Continuations

The use of `GetMod` is very similar to the `NumberCont` type.
To explain it, we will go back to the 'more complicated' example where we wanted to transform `{ a: { b: 1, c: "a" } }` with the following modifications:

```ts
{ get: x => x.a.b, modify: x => x + 1 }
{ get: x => x.a.c, modify: x => x + "a" }
```

Let's start with creating a `focus` function for this case specifically.
We create a continuation corresponding to each of the modifications we want to apply.
The first continuation waits for a `GetModOps` and applies it to `x`.
We obtain the result of the modification by passing the continuation to the modification, remember that a modification of type `GetMod` will call the passed continuation with a provided value.
The interesting part is that the type parameter `A` of each continuation is linked to the continuation, not to the `focus` function, meaning that `A` can be _different for each modification_. Which is of course exactly what we wanted!

```ts
function focus<S>(
  x: S,
  modification1: GetMod<S>,
  modification2: GetMod<S>, 
) {
  const continuation1 = <A>(op: GetModOps<S, A>) => iassign(x, op.get, op.modify);
  const acc1 = modification1(continuation1);
  const continuation2 = <A>(op: GetModOps<S, A>) => iassign(acc1, op.get, op.modify);
  return modification2(continuation2);
}
```

A `GetMod` is created by passing the `get` and `modify` functions as a record to the received continuation.
I called this `over` for a slightly more memorable syntax, the name is inspired by its namesake of the [Haskell lens library](http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Setter.html#v:over).

```ts
function over<S, A>(
  get: (s: S) => A,
  modify: (a: A) => A,
): GetMod<S> {
  return e => e({ get, modify });
}
```

Using `over`, we can tansform our object as advertised at the start of the post.

```ts
const transformedX = focus(
  { a: { b: 1, c: "a" } },
  over(x => x.a.b, x => x + 1),
  over(x => x.a.c, x => x + "a"),
);
// transformedX = { a: { b: 2, c: "aa" } }
```

We can easily generalize the `focus` function with `reduce` so it takes any number of `GetMod`s.

```ts
function focus<S>(
  s: S,
  ...mods: GetMod<S>[]
): S {
  return mods.reduce((acc, mod) => mod(op => iassign(acc, op.get, op.modify)), s);
}
```

# Conclusion

In this post we covered the use of existential types encoding to create a wrapper for the [`immutable-assign`](https://github.com/engineforce/ImmutableAssign) library.
We took a closer look at the use of this encoding with an analogy to continuation passing style.
