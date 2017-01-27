---
title: Wrapping Java-style methods with ApplicativeError and Cats
layout: post
categories: [ApplicativeError, Cats]
---

Let's take a look on some ways to convert Java-style methods (returning nulls and throwing exceptions) as pure Scala values.

At the end of the post I hope to define a function which you might find nicer to work with than some of the more obvious options. I will do this by using the ApplicativeError abstraction from Cats.

Imagine this contrived method def, the main point being certain inputs can return nulls and others might throw exceptions:

```scala
def unsafeVal(x: String) = x match {
  case "a" => "a"
  case "b" => null
  case "c" => throw new IllegalArgumentException
}
```

As our first idea to handle it as a pure value we might come up with the following:

(in the meantime lets quickly [import cats.\_, cats.data.\_, cats.implicits.\_](https://github.com/typelevel/cats) )

```scala
def safeWrap[A](f: => A): Either[Throwable, Option[A]] =
  Either.catchNonFatal(Option(f))
```

Great, we are using the `Either` and `Option` types, which means we are done right?

Technically yes, but let's not rush things. The function we have created will safely capture the nulls and exceptions, but do we always want to handle an `Either[Throwable, Option[A]]` type?

What happens if we do not want a `None` but we want a `Left(new NullPointerException)`, or we do not want to involve `Either`s and we only want `None` if something (no matter what) went wrong, or maybe we want to use something completely different than `Either`/`Option` as our datatype encoding Success/Failure?

No problem! You might say, we can easily pattern match on our `Either[Throwable, Option[A]]` type and convert it to whatever we want!

A helper method for `Option[A]`:

```scala
def safeWrapOption[A](f: => A, nullVal: Option[A] = None)
  : Option[A] = safeWrap(f) match {
  case Right(None) => nullVal
  case Right(Some(a)) => Some(a)
  case Left(e) => None
}
```

A helper method for `Either[Throwable, A]`:

```scala
def safeWrapEither[A](f: => A, nullVal: Either[Throwable, A] = Left(new NullPointerException))
  : Either[Throwable, A] = safeWrap(f) match {
  case Right(None) => nullVal
  case Right(Some(a)) => Right(a)
  case Left(e) => Left(e)
}
```

A helper method for `Future[A]`:

```scala
import scala.concurrent.Future

def safeWrapFuture[A](f: => A, nullVal: Future[A] = Future.failed(new NullPointerException))
  : Future[A] = safeWrap(f) match {
  case Right(None) => nullVal
  case Right(Some(a)) => Future.successful(a)
  case Left(e) => Future.failed(e)
}
```

etc...

These helpers are definitely useful, but it looks very [WET](https://en.wikipedia.org/wiki/Don't_repeat_yourself).

It seems we need some kind of typeclass which can give us what a Success and a Failure value is, potentially with the possibility to store the specific error as a `Throwable`.

Let's define our trait and some instances:

```scala
trait GenericSuccessFailure[F[_]] {
  def createSuccess[A](a: A): F[A]
  def createFailure(e: Throwable): F[A]
}

implicit val gsfOption = new GenericSuccessFailure[Option] {
  def createSuccess[A](a: A): Option[A] = Some(a)
  def createFailure[A](e: Throwable): Option[A] = None
}

type EitherThrow[A] = Either[Throwable, A]
implicit val gsfEither = new GenericSuccessFailure[EitherThrow] {
  def createSuccess[A](a: A): EitherThrow[A] = Right(a)
  def createFailure[A](e: Throwable): EitherThrow[A] = Left(e)
}

implicit val gsfFuture = new GenericSuccessFailure[Future] {
  def createSuccess[A](a: A): Future[A] = Future.successful(a)
  def createFailure[A](e: Throwable): Future[A] = Future.failed(e)
}
```

Now we can write our safeWrap method in terms of this typeclass: 

```scala
def safeWrapGeneric[F[_], A](f: => A, nullVal: F[A])(implicit
  gsf: GenericSuccessFailure[F]): F[A] = safeWrap(f) match {
  case Right(None) => nullVal
  case Right(Some(a)) => gsf.createSuccess(a)
  case Left(e) => gsf.createFailure(e)
}
```

That looks pretty good. But if we had searched around a bit we might have found that our typeclass already exists! D'oh.

It's called [ApplicativeError](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/ApplicativeError.scala), we can consider `pure` as our `createSuccess` and `raiseError` as our `createFailure`.

Let's rewrite our generic method:

```scala
def safeWrapApplicativeError[F[_], A](f: => A, nullVal: F[A])(implicit
  ae: ApplicativeError[F]): F[A] = safeWrap(f) match {
  case Right(None) => nullVal
  case Right(Some(a)) => ae.pure(a)
  case Left(e) => ae.raiseError(e)
}
```

That looks just as good, but with the added benefit that we don't have to define the instances for our typeclass ourselves when they are provided by Cats (*).

If we would so desire, we could easily refactor the generic method to handle `null` values and exceptions immediately (instead of using an intermadiate `Either` and `Option`). An immediate advantage for adhering to the DRY principle.

```scala
def safeWrapApplicativeErrorAlt[F[_], A](f: => A, nullVal: F[A])(implicit
  ae: ApplicativeError[F, Throwable]): F[A] =
  try {
    f match {
      case null => nullVal
      case notNull => ae.pure(notNull)
    }
  } catch {
    case NonFatal(e) => ae.raiseError(e)
  }
```

So, in the post we iterated through a design to convert Java-style methods to pure values. Ultimately we ended up with a generic method using the `ApplicativeError` from Cats. If you want you can use some of the other methods available on `ApplicativeError` to define other utilities as well.

You can take a look at the complete code from this post on this [scalafiddle](https://scalafiddle.io/sf/xADqbDO/0).

Also note that we can use [this](https://tpolecat.github.io/2015/07/30/infer.html) trick to have a neater type inference. You will only have to specify your error type, not your result type (I definitely recommend doing this).

#### Remark 1 (*):

There is no instance for `MonadError[Option, Throwable]`, only for `MonadError[Option, Unit]`

We can make use of the existing cats Option instances to define an instance like this:

```scala
  implicit val optionMonadErr = new MonadError[Option, Throwable] {
    override def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] =
      catsStdInstancesForOption.flatMap(fa)(f)
    override def tailRecM[A, B](a: A)(f: (A) => Option[Either[A, B]]): Option[B] =
      catsStdInstancesForOption.tailRecM(a)(f)
    override def handleErrorWith[A](fa: Option[A])(f: (Throwable) => Option[A]): Option[A] =
      catsStdInstancesForOption.handleErrorWith(fa)(_ => None)
    override def raiseError[A](e: Throwable): Option[A] =
      catsStdInstancesForOption.raiseError(())
    override def pure[A](x: A): Option[A] =
      catsStdInstancesForOption.pure(x)
  }
```

Do note that by using this instance you will be throwing away some information and behaviour. In `raiseError` you throw away the ability to store the occurred error and in `handleErrorWith` every error will be handled by a `None`. You could always define an instance more suited to your needs if you want, or keep the separation between `MonadError` with `Throwable`s and `Unit`s more distinct by not defining a new instance but creating different methods.

Also when using `Future` and Cats: don't forget to have an `ExecutionContext` in scope otherwise the implicit resolution won't find any instances!