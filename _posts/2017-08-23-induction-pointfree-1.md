---
title: Induction for Point-Free Equational Reasoning
layout: post
categories: [Induction, EquationalReasoning]
use_math: true
---

In standard equational reasoning we can make use of inductive proofs. We declare that we want to do induction on one of the variables and assume an induction hypothesis which let's us prove a certain property.

For example with a definition of the natural numbers:

```haskell
data Nat = Zero | S Nat
```

And some functions:

```haskell
-- returns whether a natural number is even
even' :: Nat -> Bool
even' Zero = True
even' (S n) = not (even' n)

-- returns 2* the input natural number
times2 :: Nat -> Nat
times2 Zero = Zero
times2 (S n) = S (S (times2 n))
```

We can prove that `even' (times2 x)` evaluates to `True` for all `x` by induction on `x`. Let's try that:

We have two cases. First we need to prove that it holds when `x = Zero`

```haskell
even' (times2 Zero)
-- definition times2
= even' Zero
-- definition even'
= True
```

For `X = S n` we prove `even' (times2 n) = True` implies `even' (times2 (S n)) = True`.

```haskell
even' (times2 (S n))
-- definition times2
= even' (S (S (times2 n)))
-- definition even'
= not (even' (S (times2 n)))
-- definition even'
= not (not (even' (times2 n)))
-- property of not
= even' (times2 n)
-- using induction hypothesis
= True
```

Having proven the property for both cases we have given a proof that `even' (times2 x) = True` for all `x` by induction.

In point-free equational reasoning however there are no 'points', there are no variables on which we can declare induction. But we would still like to be able to prove certain things in a similar manner.

So our earlier property written point-free is `even' . times2 = const True`, but how can we argue that this property holds? There is no `x` on which we can declare induction...

# Initiality and $F$-Algebras

To correctly state our proof some mathematical concepts are to be introduced. I assume some basic familiarity with category theory, so I won't reintroduce the definitions of a category or functor here. (If you are not familiar though, my suggestion would be to take a look at Bartosz' [category theory for programmers](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/))

**Initial Object** An initial object is an object with a unique morphism to every other object. This uniqueness is an essential property we will use to prove that two morphisms are equal.

**$F$-Algebra** $F$-Algebras are composed of two things: a carrier $A$ and an action $a : FA \rightarrow A$, where $F$ is an endofunctor. I will use the notation $\langle A, a \rangle$ for $F$-Algebras.

Algebra homomorphisms are then functions on the underlying carriers which adhere to a special condition. A function $f : A \rightarrow B$ is an algebra homomorphism from $\langle A, a \rangle$ to $\langle B, b \rangle$ if $f \circ a = b \circ Ff$.

$$
\begin{CD}
FA @>Ff>> FB\\
@VaVV @VVbV\\
A @>f>> B
\end{CD}
$$

(For a more in-depth read on $F$-Algebras I suggest to read [this post](https://www.schoolofhaskell.com/user/bartosz/understanding-algebras) by Bartosz)

It turns out that $F$-Algebras and algebra homomorphisms form a category with the former as objects and the latter as morphisms. And in this category we have an initial object, the initial $F$-Algebra.

**Coproduct** One other thing I want to point out is the notation I will use for the universal property of coproducts. If we have morphisms $f : A \rightarrow X$ and $g : B \rightarrow X$ then I will use $[f, g] : A + B \rightarrow X$ to denote the universal property of the coproduct.

$$
\begin{CD}
A @>f>> X @<g<< B\\
@V\mathit{inl}VV @AA[f,g]A @VV\mathit{inr}V\\
@>>> A+B @<<<
\end{CD}
$$

An example of this is pattern matching on `Either`:

```haskell
coproduct :: (a -> x) -> (b -> x) -> (Either a b -> x)
coproduct f _ (Left a) = f a
coproduct _ g (Right b) = g b
```

To prove our example shown in the first section we actually only need to work with one functor, namely the one for which natural numbers are the initial algebra (with $[\mathit{zero}, \mathit{succ}]$ as action). This functor is $1 + X$, which instantiated as a Haskell `Functor` is `Either () a`.

Our initial algebra is then $\langle\mathbb{N}, [\mathit{zero}, \mathit{succ}] : 1 + \mathbb{N} \rightarrow \mathbb{N} \rangle$. Initiality for the $1 + X$ functor means the following diagram holds (where $f$ is the **unique** morphism which makes the diagram commute):

$$
\begin{CD}
1+\mathbb{N} @>id_1+f>> 1+B\\
@V[\mathit{zero}, \mathit{succ}]VV @VV[\phi_1, \phi_2]V\\
\mathbb{N} @>f>> B
\end{CD}
$$

Which is actually equivalent to the following two diagrams:

$$
\begin{CD}
1 @= 1\\
@V\mathit{zero}VV @VV\phi_1V\\
\mathbb{N} @>f>> B
\end{CD}
$$

$$
\begin{CD}
\mathbb{N} @>f>> B\\
@V\mathit{succ}VV @VV\phi_2V\\
\mathbb{N} @>f>> B
\end{CD}
$$

or in equations:

* $f \circ \mathit{zero} = \phi_1$
* $f \circ \mathit{succ} = \phi_2 \circ f$

So we will tackle our problem by proving that we have two different morphisms which satisfy these equations, but because we know $f$ is unique we know they must be equal.

# Point-Free Induction

The `Either () a` functor from earlier is also isomorphic to the following data type, defined for convenience:

```haskell
data NatF a = ZeroF | SF a

instance Functor NatF where
  fmap f ZeroF = ZeroF
  fmap f (SF a) = SF (f a)
```

`Maybe` you have seen this functor somewhere else.

Anyways, to create an initial algebra we also need an action:

```haskell
inNatF :: NatF Nat -> Nat
inNatF ZeroF = Zero
inNatF (SF n) = S n
```

Or in coproduct notation $[$`const Zero`, `S`$] :$ `NatF Nat -> Nat`. These two together create our initial algebra $\langle$`Nat`, $[$`const Zero`, `S`$] : $ `NatF Nat -> Nat` $\rangle$. ($\mathit{zero}$ = `const Zero`, $\mathit{succ}$ = `S`)

This initial algebra has a unique algebra homomorphism to any other algebra for our functor. Let's take the algebra $\langle$`Bool`, $[$`const True`, `id`$] : $ `NatF Bool -> Bool` $\rangle$, which we will need in our upcoming proof. ($\phi_1$ = `const True`, $\phi_2$ = `id`)

By initiality we have the unique function `f : Nat -> Bool` where the following two equations hold:

* `f . const Zero = const True`
* `f . S = id . f`

So intuitively speaking `f` is `True` when applied to `Zero` and then `f` applied on any successor `Nat` is equal to the result of its predecessor, so it results in always being `True`.

And indeed plugging in `const True` for `f` makes the two equations work out:

```haskell
(const True) . (const Zero)
= const True
```

```haskell
(const True) . S
= const True
= id . (const True)
```

So this seems like a step in the right direction. If we can show that the `even' . times2` function is also an algebra homomorphism to this algebra then by uniqueness we can conclude that they are the same function and our proof is complete!

And indeed substituting `f` for `even' . times2` makes the equations work out as well (we do have to adopt a point-free style for the `even'` and `times2` functions, but they are still the same):

```haskell
even' . times2 . (const Zero)
-- definition times2
= even' . (const Zero)
-- definition even'
= const True
```

```haskell
even' . times2 . S
-- definition times2
= even' . S . S . times2
-- definition even'
= not . even' . S . times2
-- definition even'
= not . not . even' . times2
-- property of not
= even' . times2
-- introduce id
= id . even' . times2
```

# Conclusion

So if you've ever wondered how initial algebras provide the framework for induction, in this post we explored an example illustrated with Haskell by applying the underlying theory for initiality of initial algebras.

When I was trying to figure out how this worked I couldn't really find any source explaining it in enough detail for me to understand it from the ground up, so I decided to write this up.