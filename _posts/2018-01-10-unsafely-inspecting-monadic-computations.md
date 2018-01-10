---
title: Unsafely Inspecting Monadic Computations
layout: post
categories: [Programming, Purescript]
---

After some discussion on reddit following [Luka's post on optimizing 
tagless final computations](http://lukajcb.github.io/blog/functional/2018/01/03/optimizing-tagless-final.html), I realized a trick for inspecting monadic 
programs, but in an unsafe manner. Luka's technique of optimizing computations is 
rooted in the idea of the `analyzeFreeAp` function of the [Free Applicative](https://github.com/ethul/purescript-freeap/blob/master/src/Control/Applicative/Free.purs) (or 
`runAp_` in Haskell's [free package](https://github.com/typelevel/cats/blob/master/free/src/main/scala/cats/free/FreeApplicative.scala)). This idea is applied in the context 
of tagless final programs, but the fact still remains that it is a safe 
way of inspecting programs. Safe meaning that whatever handler we write, 
it can be applied to applicative programs without any runtime error. 
Unsafe in this context means that we will be cheating the type system 
and as a result runtime errors might surface when the wrong handler is 
combined with the wrong program.

*For this reason I would disadvise using this trick in actual code*, 
make sure you are aware of the risks and are able to deal with them if you ever intend to use this.

The examples are given in Purescript. The effect of laziness (for example in Haskell) on this trick is discussed in section 'Laziness'.

# Monadic Computations

As opposed to computations with `Applicative` constraints, this post 
will only handle computations which have `Monad` constraints.
For example consider the following computation:

```haskell
prog1 :: forall f r a.
         Monad f =>
         { get :: String -> f a
         , put :: String -> a -> f Unit | r } ->
         a -> f (Array a)
prog1 k mouse = do
   f <- k.get "Cats"
   s <- k.get "Dogs"
   k.put "Mice" mouse
   t <- k.get "Cats"
   pure [f, s, t]
```

The type signature tells us that we need a handler `k` which interprets at 
least the `get` and `put` operations, it also takes an input for the `"Mice"` 
location named `mouse`. The computation returns all retrieved data as an `f (Array a)`, 
`f` being the type constructor the handler interprets to.

We want to interpret the computation into two `Set`s, one to gather all keys
from `get` operations and the other to gather all keys and values from `put` operations.
These `Set`s enable the creation of an optimized program: a unique
get and put for each location, but we won't go into detail on this here.

However, we cannot use applicative inspection using `Const` to realize this. The program 
is written in `do` notation and thus has a `Monad` constraint and 
`Const` doesn't have a `Monad` instance. This is where the trick comes 
in. We will write an interpretation to `Writer`. Normally we would 
consider this impossible because of the following problem:

We need to write an interpretation for the `get` and `put` operation. 
However, note the spots marked with `?`. We need to construct the writer 
by giving it both a value `a` and a value of `(Set, Set)`. Remember 
that `Writer w a` is isomorphic to `(w, a)`. The value `a` is missing 
since we are not actually running the computation, merely inspecting it.

```haskell
get1 :: forall a. String -> Writer (Tuple (S.Set String) (S.Set (Tuple String Int))) a
get1 key = writer (Tuple ? (Tuple (S.singleton key) (S.empty)))

put1 :: forall a. String -> Int -> Writer (Tuple (S.Set String) (S.Set (Tuple String Int))) a
put1 key value = writer (Tuple ? (Tuple (S.empty) (S.singleton (Tuple key value))))
```

The trick is actually very simple, we will use a function which tricks the type system:

```haskell
undef :: forall a. a
undef = unsafeCoerce "oops!"
```

We use this new `undef` as a replacement for `?`, 
essentially cheating the type system by saying the value `a` is available while it actually isn't.
However, we never intend to do anything with this value, so this fact doesn't matter.
We replace ? by `undef` to obtain the handler:

```haskell
get1 :: forall a. String -> Writer (Tuple (S.Set String) (S.Set (Tuple String Int))) a
get1 key = writer (Tuple undef (Tuple (S.singleton key) (S.empty)))

put1 :: forall a. String -> Int -> Writer (Tuple (S.Set String) (S.Set (Tuple String Int))) a
put1 key value = writer (Tuple undef (Tuple (S.empty) (S.singleton (Tuple key value))))
```

This is already sufficient to inspect `prog1` and give us the result 
we want, for the gets ["Cats", "Dogs"] and for the puts [("Mice", 1)]:

```haskell
 > snd $ runWriter $ prog1 {get:get1, put:put1} 1
(Tuple (fromFoldable ("Cats" : "Dogs" : Nil)) (fromFoldable ((Tuple "Mice" 1) : Nil)))
```

We can also see the type system lying to us (because we cheated!):

```haskell
 > :t fst $ runWriter $ prog1 {get:get1, put:put1} 1
Array Int
```

Which, when evaluated, gives us `["oops!","oops!","oops!"]`, definitely not an array of `Int`s!
But, again, the intention of the inspection is to ignore the actual result of the computation, the `a` part of the writer is intended to never be used.
So, while somewhat weird, this isn't really the problem with the trick.

# Laziness

Laziness has some interesting implication for this trick.
Consider a slightly altered version of our scenario. The `get` operation 
returns `Maybe a` instead of `a`:

```haskell
prog2_strict :: forall f r a.
                Monad f =>
                { get :: String -> f (Maybe a)
                , put :: String -> a -> f Unit | r } ->
                a -> f (Array a)
prog2_strict k mouse = do
   f <- k.get "Cats"
   s <- k.get "Dogs"
   k.put "Mice" mouse
   t <- k.get "Cats"
   pure (catMaybes [f, s, t])
```

The only change we have to make is returning the final array using `catMaybes` to compress `Array (Maybe a)` into `Array a`.

What happens if we inspect this program?

```haskell
 > snd $ runWriter $ prog2_wrong {get:get1,put:put1} "a"
Uncaught Error: Failed pattern match at Data.Maybe line ...
```

Whoops! Purescript is not lazy, so it tries to interpret the `"oops!"`s as `Maybe a`s because it wants to know if it is a `Just a` or `Nothing`, due to the `catMaybes` call, even though we never requested to calculate the result of the computation...

When using Haskell, this problem shouldn't surface because it is lazy by default. And if we never request the result array, the dark secret won't be revealed.

A solution in Purescript is to use [opt-in laziness](https://github.com/purescript/purescript-lazy) and create a thunk out of the result array:

```haskell
prog2_lazy :: forall f r a.
              Monad f =>
              { get :: String -> f (Maybe a)
              , put :: String -> a -> f Unit | r } ->
              a -> f (Lazy (Array a))
prog2_lazy k mouse = do
   f <- k.get "Cats"
   s <- k.get "Dogs"
   k.put "Mice" mouse
   t <- k.get "Cats"
   pure (defer (\_ -> catMaybes [f, s, t]))
```

Which makes the example work again:


```haskell
 > snd $ runWriter $ prog2_lazy {get:get1, put:put1} 1
(Tuple (fromFoldable ("Cats" : "Dogs" : Nil)) (fromFoldable ((Tuple "Mice" 1) : Nil)))
```

# Applicability

So, why go through the trouble to use this unsafe trick when a perfectly viable alternative exists?
The reason is that the alternative is not as perfect as it seems on first sight.

Categorizing computations as applicative or monadic is very coarse-grained.
This results in `Monad` constraints popping up in all sorts of situations, which disables the applicative inspection.
But it doesn't mean that these programs are not inspectable at all.
In some sense this coarse modeling of computations results in the type system preventing us of running some inspections we would want to do.

Take `prog3` for example:

```haskell
prog3 :: forall f r a.
         Monad f =>
         { get :: String -> f a
         , put :: String -> a -> f Unit | r } ->
         f (Array a)
prog3 k = do
   f <- k.get "Cats"
   s <- k.get "Dogs"
   k.put "Mice" f
   t <- k.get "Cats"
   pure [f, s, t]
```

The difference with `prog1` is that we pass the value `f` from `k.get "Cats"` to `k.put "Mice" f`.
Obviously, we cannot inspect this computation to return the values of all puts, they are not statically known.
But, we are able to inspect all locations which will be touched by only gathering the keys.
We can easily adapt the handler:

```haskell
get2 :: forall a. String -> Writer (Tuple (S.Set String) (S.Set String)) a
get2 key = writer (Tuple undef (Tuple (S.singleton key) (S.empty)))

put2 :: forall a. String -> Int -> Writer (Tuple (S.Set String) (S.Set String)) a
put2 key value = writer (Tuple undef (Tuple (S.empty) (S.singleton key)))
```

We see the sets ["Cats", "Dogs"] and ["Mice"] as result of our inspection:

```haskell
> snd $ runWriter $ prog3 {get:get2, put:put2}
(Tuple (fromFoldable ("Cats" : "Dogs" : Nil)) (fromFoldable ("Mice" : Nil)))
```

Another example of a `Monad` constraint popping up is when we add some logging to our computation:

```haskell
prog4 :: forall f r a.
         Monad f =>
         Show a =>
         { get :: String -> f a
         , put :: String -> a -> f Unit
         , log :: String -> f Unit | r } ->
         a -> f (Array a)
prog4 k mouse = do
   f <- k.get "Cats"
   k.log ("Cats: " <> show f)
   s <- k.get "Dogs"
   k.log ("Dogs: " <> show s)
   k.put "Mice" mouse
   t <- k.get "Cats"
   k.log ("Cats: " <> show t)
   pure [f, s, t]
```

I am aware that we could rewrite the program to fit the applicative inspection style, but it feels quite awkward that this is necessary.

By essentially turning off the logging to prevent any errors from surfacing (although I don't think it is a problem even if it actually logged things):

```haskell
log1 :: forall f a. Applicative f => String -> f a
log1 s = pure undef
```

We can analyze the computation as well, without having to modify its definition:

```haskell
> snd $ runWriter $ prog4 {get:get1, put:put1, log:log1} 1
(Tuple (fromFoldable ("Cats" : "Dogs" : Nil)) (fromFoldable ((Tuple "Mice" 1) : Nil)))
```

There are more situations this applies to, but hopefully the point is clear.

# Mismatching handler and computation

I want to highlight the fact that if we mismatch a handler and computation we will get into weird behaviour territory as well.
This is in fact the main issue I see with actually using this trick.
You are essentially throwing away the safety given by the type system to apply this trick.

For example inspecting `prog3` with `get1` and `put1`, which assume that all `put` values are statically available will result in an erroneous value since the computation will catch the unsafe `undef` value.

```haskell
> snd $ runWriter $ prog3 {get:get1, put:put1}
(Tuple (fromFoldable ("Cats" : "Dogs" : Nil)) (fromFoldable ((Tuple "Mice" oops!) : Nil)))
```

Notice the `"oops!"` value in the second set, if it was used as an `Int` it would most likely result in a runtime error somewhere.

# Conclusion

I think this trick highlights possibilities for alternative categorizations of computations, which are more fine-grained than applicative/monad, in fact also more fine-grained than applicative/arrow/monad.
The actual trick in itself, due to it actively cheating the type system, should probably not be used in practice however.

You can check out the full code in [this gist](https://gist.github.com/rubenpieters/42fd378331a7282ea5e7efd31c92d610), which can be pasted into [Try Purescript](http://try.purescript.org) to run it.