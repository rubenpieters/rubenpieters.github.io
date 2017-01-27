---
title: From Monad Transformers to the Eff Monad
layout: post
categories: [MonadTransformer, Cats, Eff]
---

Motivated by the post of [/u/saosebastiao](https://www.reddit.com/r/scala/comments/5pn36a/becoming_more_functional/dcsy6ud/) I will be covering an example of how to use Eff as a replacement for monad transformers, I will be using [Cats](https://github.com/typelevel/cats) for our monad transformers, and [Eff](https://github.com/atnos-org/eff) for the Eff monad.

I will use the following as our running example for the different approaches:

We will create a method that gets the state of an Int variable. If it is larger than 0 we will decrease it by 1, otherwise we will throw an error.

We can identify that we have two types of effects: a State-effect (accessing and mutating a mutable variable) and an Error-effect (exiting computation with an error).

# Monad Transformers

Let's first take a look at the monad transformer approach. In this approach we work with the actual monad transformers to define our method. Let's try that:

(Note that the `?` alternate syntax for type lambdas comes from [Kind Projector](https://github.com/non/kind-projector))

```scala
type MonadStackStateTEither[ErrorType, StateType, ReturnType] =
  StateT[Either[ErrorType, ?], StateType, ReturnType]

def decrMonadStackStateTEither: MonadStackStateTEither[String, Int, Unit] = ???
```

We can see that we compose the `Either` monad (which is what we'll be using as our Error-effect during this post) with the `State` monad by using the `StateT` monad transformer.

This gives us a monad which can do both Error-effects and State-effects, which I called `MonadStackStateTEither`. Let's complete our method definition:

```scala
def decrMonadStackStateTEither: MonadStackStateTEither[String, Int, Unit] = for {
  x <- StateT.get[Either[String, ?], Int]
  _ <- if (x > 0) { StateT.set[Either[String, ?], Int](x - 1) }
  else { StateT.lift[Either[String, ?], Int, Unit](Left("error")) }
} yield ()
```

We can call the get and set methods on the `StateT` object to get correctly typed State-effects. If we want to use an Error-effect though, we have to use `StateT.lift` to lift an effect from the Error-effect to our composed stack of State and Error-effects. The lifting makes the types match up so that the Error-effect correctly matches the `MonadStackStateTEither` type.

You might have noticed that we arbitrarily chose to use `StateT` on top of `Either` instead of the other way around. It might look like a small detail, but it is in fact something that can completely change the computed value. So the order in which the monad transformers are set up is important.

We can try the other way around as well. We will use the `EitherT` monad transformer on the `State` monad to receive a monad which can do Error and State-effects.

```scala
type MonadStackEitherTState[ErrorType, StateType, ReturnType] =
  EitherT[State[StateType, ?], ErrorType, ReturnType]

def decrMonadStackEitherTState: MonadStackEitherTState[String, Int, Unit] = for {
  x <- EitherT.liftT[State[Int, ?], String, Int](State.get[Int])
  _ <- if (x > 0) { EitherT.liftT[State[Int, ?], String, Unit](State.set(x - 1)) }
  else { EitherT.left[State[Int, ?], String, Unit](State.pure("error")) }
} yield ()
```

We see that now we have to lift State-effects into the correct type via the `EitherT.liftT` method.

If we run our method we can see that the results we get have different types, which is part of the reason why the order of the monad transformers matter.

(Note that in our second example we need the first `.value` to access the `.run` method from `State` and the second `.value` because `State[S, A]` is an alias for `StateT[Eval, S, A]`, which means that we have an `Eval` effect as well)

```scala
val resultStateTEither: Either[String, (Int, Unit)] =
  decrMonadStackStateTEither.run(0)
// returns: Left(error)
val resultEitherTState: (Int, Either[String, Unit]) =
  decrMonadStackEitherTState.value.run(0).value
// returns: (0,Left(error))
```

# [mtl](https://hackage.haskell.org/package/mtl)-Style Monad Transformers

In this style we have typeclasses for our effects. In our case we will be using `MonadState` and `MonadError`. These typeclasses define methods which make sense for their corresponding effects. So `MonadState` defines methods to interact with a State-effect (get, put, modify, ...) and `MonadError` defines methods to interact with an Error-effect (raise, catch, ...).

We declare the result type of our method to be a generic `F[_]`. Then we implicitly take a `MonadState[F]` and `MonadError[F]`. This means that this method is generic for any `F[_]` which can interact as both of those effects. In this way we do not have to lift any effects into our result type, this lifting will be done in the typeclass instances if necessary.

```scala
// beware: does NOT compile, see below
def decrMtlStateError[F[_]](implicit
  ms: MonadState[F, Int],
  me: MonadError[F, String]): F[Unit] = for {
    x <- ms.get
    _ <- if (x > 0) { ms.set(x - 1) }
    else { me.raiseError("error") }
  } yield ()
```

This method unfortunately does not compile (on Scala 2.12.1 and Cats 0.9.0). The reason for that is that `MonadState` and `MonadError` both are `Monad[F]` instances and so there is an ambiguous implicit. We can resolve this by explicitly using the `flatMap` method on one of both.

We can write it like this to get it compiled, but it looks a bit ugly:

```scala
def decrMtlStateError[F[_]](implicit
  ms: MonadState[F, Int],
  me: MonadError[F, String]): F[Unit] = {
  ms.flatMap(ms.get) { x =>
    if (x > 0) { ms.set(x - 1) }
    else { me.raiseError("error") }
  }
}
```

Then we can reuse this method for both monad types we used earlier in our example... If there would be a `MonadState` instance defined for `EitherT`, which as of writing this post is not the case (on Cats 0.9.0). But we can define our own for now (*).

```scala
val resultMtlStateTEither: Either[String, (Int, Unit)] =
  decrMtlStateError[StateT[Either[String, ?], Int, ?]].run(0)
// returns: Left(error)
val resultMtlEitherTState: (Int, Either[String, Unit]) =
  decrMtlStateError[EitherT[State[Int, ?], String, ?]].value.run(0).value
// returns: (0,Left(error))
```

We see we get the same results as in the previous paragraph, but we could reuse our generic method.

I'm not sure if this style is common in Scala, since playing with this simple example I already came across some issues. But I thought it would be interesting to include it, since it might provide some additional insight moving from monad transformers to Eff.

# Eff

The way we use effects in Eff is somewhat similar to the mtl-style. We also have to declare as implicit parameters which effects we want to use. Let's define some aliases for our effects (like in the Eff user guide):

```scala
type _eitherString[R] = Either[String, ?] |= R
type _stateInt[R] = State[Int, ?] |= R
```

In Eff the type variable `R` signifies the effect stack. This stack will contain all the effects which our methods can utilize. The `|=` helper functions means that the effect on the left is a member of the stack `R`. We will use this as an implicit parameter in the same way as we took `MonadState` and `MonadError` as implicit parameters.

Our method signature looks like this:

```scala
def decrEff[R : _eitherString : _stateInt]: Eff[R, Unit] = ???
```

Our effect stack `R` is a generic type and it must contain at least our `_eitherString` and `_stateInt` effect . Our result is `Eff[R, A]`, which means that we will get a result of type `A`, but we might also execute effects according to the effects in the stack `R`. For our method `A` is `Unit`.

By importing the Eff syntax (import org.atnos.eff.\_, org.atnos.eff.all.\_, org.atnos.eff.syntax.all.\_) we can get a method which looks very similar to our mtl-style method:

```scala
def decrEff[R : _eitherString : _stateInt]: Eff[R, Unit] = for {
  x <- get
  _ <- if (x > 0) { put(x - 1) }
  else { left("error") }
} yield ()
```

Now when calling our method we have to do two things: 1. Choose which effect types we want to use to handle the effects in our stack 2. Choose in which order we will run our effects.

Choosing the types can be done like this:

```scala
type FxStack = Fx.fx2[State[Int, ?], Either[String, ?]]
```

And we can choose the order of effects by composing the `runEither` and `runState` methods differently.

```scala
val resultRunStateRunEither: Either[String, (Unit, Int)] =
  decrEff[FxStack].runState(0).runEither.run
  // returns: Left(error)
val resultRunEitherRunState: (Either[String, Unit], Int) =
  decrEff[FxStack].runEither.runState(0).run
  // returns: (Left(error),0)
```

So hopefully by going through this example the basics of Eff are explained well enough to go through the [user guide](http://atnos-org.github.io/eff/) and learn more about it.

You can find all the code used in this post on this [gist](https://gist.github.com/rubenpieters/d154975fac5d0ca36c3082e7d4bf2878).

#### Remark 1 (*)

The `MonadState` instance for `EitherT` I used is:

```scala
  implicit def monadStateEitherT[F[_], E, S](implicit ms: MonadState[F, S])
    : MonadState[EitherT[F, E, ?], S] = new MonadState[EitherT[F, E, ?], S] {
    val F = Monad[F]
    override def get: EitherT[F, E, S] = EitherT.liftT(ms.get)
    override def set(s: S): EitherT[F, E, Unit] = EitherT.liftT(ms.set(s))
    // copied from cats EitherTMonad
    override def pure[A](a: A): EitherT[F, E, A] = EitherT(F.pure(Either.right(a)))
    override def flatMap[A, B](fa: EitherT[F, E, A])(f: A => EitherT[F, E, B]): EitherT[F, E, B] = fa flatMap f
    override def tailRecM[A, B](a: A)(f: A => EitherT[F, E, Either[A, B]]): EitherT[F, E, B] =
      EitherT(F.tailRecM(a)(a0 => F.map(f(a0).value) {
        case Left(l)         => Right(Left(l))
        case Right(Left(a1)) => Left(a1)
        case Right(Right(b)) => Right(Right(b))
      }))
  }
```