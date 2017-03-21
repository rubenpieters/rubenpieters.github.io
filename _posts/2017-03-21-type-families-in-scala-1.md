---
title: Type Families in Scala - Typed sprintf
layout: post
categories: [TypeFamilies, AbstractTypes]
---

Lately I have been playing around a bit with porting some code samples from [Fun with Type Functions](https://wiki.haskell.org/Simonpj/Talk:FunWithTypeFuns) to Scala. I will be referring to the pdf `typefun.pdf` linked there as 'the pdf'.

I will explain how I did section 4.1 'Typed sprintf'.

# Setting

Let's explain what we want to achieve (paraphrasing from the pdf).

We want to make a typed printing function inspired by the not type-safe C function `sprintf`.

What this function does is print out a formatted string, but it can take a variable amount of parameters to be printed as well.

So for example `sprintf("Name %s")` requires an extra `String` argument, so we would want it to be a `String => String`. But we would want `sprintf("Name=%s, Age=%d")` to be a `String => Int => String`, since it takes an extra `Int` parameter. I assume you can extrapolate some other examples yourself.

The pdf then goes on to explain how it can be encoded in Haskell using type families. I will show how I ported this code to Scala.

# Representation

We will represent what we want to print (in the pdf called an `F`) as follows:

 - We can request to print a literal, we call this `Lit`. It must also take a `String` argument, this is the literal we want to print
 - We can request to print a value, we call this `Val`. This can have any type so we make it have a generic type. We also encode a way to print this value as an `A => String` function.
 - We need to be able to represent arbitrary combinations of the previous two, we call this `Cmp`. With `Cmp` we can combine two `F`s to create a bigger `F`.

We end up with this definition:

```scala
sealed trait F
case class Lit(str: String) extends F
case class Val[A](printer: A => String) extends F
case class Cmp[F1 <: F, F2 <: F](f1: F1, f2: F2) extends F
```

In this way we could encode the name/age example printing as follows:

```scala
val valInt = Val[Int](_.toString)
val valStr = Val[String](identity)
val nameAge = Cmp(Cmp(Lit("name="), valStr), Cmp(Lit(", age="), valInt))
```

A little cumbersome, but it does the job.

Now our type family `TPrinter` will be represented like this:

```scala
sealed trait TPrinter[A <: F, X] {
   type Out

   def print(f: A, k: String => X): Out
}
```

We can see this as: given a type `A` and `X` we will have a type `Out` corresponding to it. A type-level function in a way.

Our `TPrinter` also has a function `print` which takes an `F` (our representation to print) and a `k : String => X`. It will then return something of type `Out`, which in the name/age example should be `String => Int => String`.

The function `k` will be `String => String` (or `identity`) at the top level, but will become more important when we get to the implementation of the `Cmp` case for printing.

We're also gonna be using the [`Aux` pattern](http://stackoverflow.com/questions/38541271/what-does-the-aux-pattern-accomplish-in-scala) to be able to refer to the `Out` type in implicit parameters:

```scala
object TPrinter {
   type Aux[A <: F, X, Out0] = TPrinter[A, X] { type Out = Out0 }
}
```

# Implementing the Type Family

Remember when I referred to the the type family as a type-level function? This might come across clearer when you see what behaviour we expect:

```scala
// some example behaviour:
// given a Lit and a String, we want to receive type String
implicitly[TPrinter.Aux[Lit, String, String]]
// but also: given a Lit and an Int => String, we want to receive type Int => String
implicitly[TPrinter.Aux[Lit, Int => String, Int => String]]
// given a Val[Int] and a String, we want to receive type Int => String
implicitly[TPrinter.Aux[Val[Int], String, Int => String]]
// but also: given a Val[Int] and an Int => String, we want to receive type Int => Int => String
implicitly[TPrinter.Aux[Val[Int], Int => String, Int => Int => String]]
// given a Cmp[Lit, Val[Int]] and a String we want to receive type Int => String
implicitly[TPrinter.Aux[Cmp[Lit, Val[Int]], String, Int => String]]
// given a Cmp[Cmp[Lit, Val[String]], Cmp[Lit, Val[Int]]]
//   and a String we want to receive type String => Int => String
// notice this first type has the same type as `nameAge`
implicitly[TPrinter.Aux[Cmp[Cmp[Lit, Val[String]], Cmp[Lit, Val[Int]]],
           String, String => Int => String]]
```

More generally we want the following to hold:

When we have `A` = `Lit` then `Out` = `X`. A literal doesn't change the type of our `print` function.

Implementing this is quite straightforward. The `print` function will execute the `k` on the given literal string.
 
```scala
implicit def tPrinterLit[X]: TPrinter.Aux[Lit, X, X] = new TPrinter[Lit, X] {
   type Out = X

   override def print(f: Lit, k: (String) => X): X = k(f.str)
}
```
 
When we have `A` = `Val[V]` then `Out` = `V => X`. A value adds another typed parameter at the start of our function.

This is also fairly straightforward. We return a function which takes the typed parameter and when called will use the given `printer` to print this parameter as a `String`.

```scala
implicit def tPrinterVal[X, A]: TPrinter.Aux[Val[A], X, A => X] = new 
TPrinter[Val[A], X] {
   type Out = A => X

   override def print(f: Val[A], k: (String) => X): A => X =
   (x: A) => k(f.printer(x))
}
```

When we have `A` = `Cmp[F1, F2]` then `Out` = something which combines the output of the printer function for `F1` and `F2`.

Ouch this one seems a bit harder to express, let's make the implementation and leave some generics open and see if the type checker can help us.

We will need the `TPrinter` instances for `F1` and `F2`. These will be `printerF1 : TPrinter.Aux[F1, XF1, OF1]` and `printerF2 : TPrinter.Aux[F2, XF2, OF2]`.

Eventually we will need to print the two sides and return an `X` again. This is `k(s1 + s2)` and has signature `endStr : String => String => X`.

If we have the first part as a `String` (let's say `s1`) we can create the printer function for the second part by doing `printerF2.print(f.f2, s2 => endStr(s1, s2))`. This returns us the printer function determined by the second part `OF2`, so `print2 : String => OF2`.

Then we can create the definition for the overall printer function `printerF1.print(f.f1, s1 => print2(s1)`. This has type `OF1`, the output type of the first TPrinter.

Let's write out the first attempt. We can see the type checker isn't quite happy yet.

```scala
// WARNING: doesn't compile, for fixed version see below
implicit def tPrinterCmp[X, F1 <: F, F2 <: F, XF1, XF2, OF1, OF2, OUT]
(implicit printerF1: TPrinter.Aux[F1, XF1, OF1]
, printerF2: TPrinter.Aux[F2, XF2, OF2]
): TPrinter.Aux[Cmp[F1, F2], X, OUT] = new TPrinter[Cmp[F1, F2], X] {
  type Out = OUT

  override def print(f: Cmp[F1, F2], k: (String) => X): OUT = {
    def endStr(s1: String, s2: String): X = k(s1 + s2)
    def print2(s1: String) = printerF2.print(f.f2,
	// we have : String => XF2 , we need : String => X
	  s2 => endStr(s1, s2))
    def print1 = printerF1.print(f.f1,
	// we have : String => OF2 , we need : String => XF1
	  s1 => print2(s1))

	// we have : OF1 , we need : OUT
    print1
  }
}
```

Following the guidance of the type checker gets us at the correct result, but let's reason about it a little bit more.

`OF2` will give us the type of the second part of the print function. This will be `(A =>)* String`, where `()*` denotes zero or more occurrences.

We want the first printer part to add types at the start of the `OF2` type, as in `(B =>)* (A =>)* String`, this is `OF1`.

If `XF1` = `OF2` then we have the behaviour we want:

 - in case of `Lit`, `(B =>)*` is empty and we get back `(A =>)* String` for `OF1`.
 - in case of 1 `Val[V]` we get back `V => (A =>)* String` for `OF1`
 - etc...

Then we can also see that `OUT` must be equal to `OF1`, since `OF1` is the output type we want for the printer function for the `Cmp` case.

Since there is no recursive lookup for the `tprinterF2` case our `XF2` must be `X`. Ok, this explanation isn't that intuitive but the type checker insists on having it like this, so let's comply.

Doing all the substitutions gives us:

```scala
implicit def tPrinterCmp[X, F1 <: F, F2 <: F, OF1, OF2]
   (implicit printerF1: TPrinter.Aux[F1, OF2, OF1]
    , printerF2: TPrinter.Aux[F2, X, OF2]
   ): TPrinter.Aux[Cmp[F1, F2], X, OF1] = new TPrinter[Cmp[F1, F2], X] {
     type Out = OF1

     override def print(f: Cmp[F1, F2], k: (String) => X): OF1 = {
       def endStr(s1: String, s2: String): X = k(s1 + s2)
       def print2(s1: String) = printerF2.print(f.f2, s2 => endStr(s1, s2))
       def print1 = printerF1.print(f.f1, s1 => print2(s1))

       print1
     }
}
```

And lo and behold! We have implemented our very own typed printing function.

# Using the function

How can we use our function? Well that's easy, like this:

```scala
println(implicitly[TPrinter.Aux[Cmp[Cmp[Lit, Val[String]], Cmp[Lit, 
Val[Int]]], String, String => Int => String]].print(nameAge, identity)("ruben")(23))
```

Not convinced on the usability? Okay let's create an `implicit class` for some nicer ops

```scala
implicit class printerOps[A <: F](f: A) {
   def print[X, O](k: String => X)(implicit tp: TPrinter.Aux[A, X, O]): 
O = tp.print(f, k)
   def sprintf[O](implicit tp: TPrinter.Aux[A, String, O]): O = 
tp.print(f, identity)
}
```

Now we can do this (I have to use `.apply` since otherwise it thinks I'm passing the implicit `TPrinter` instance):

```scala
println(nameAge.sprintf[String => Int => String].apply("ruben")(23))
```

Okay that's a bit cooler, but if we leave out the type annotation then it doesn't compile anymore, which is a bit of a shame.

But! The guys at [dotty](https://github.com/lampepfl/dotty) have done their job well and using the dotty compiler we can write the example like this:

```scala
println(nameAge.sprintf.apply("ruben")(23))
```

Ok that's looking more like it. Now if only we could also parse `"name=%s, age=%d"` as it's corresponding `F` type (at compile-time), then we would have something pretty close to the original C function, but type-safe. This is probably possible by implementing type-level `String` parsing, but I'll leave that for another time...

Check out the full code for this post on this [scalafiddle](https://scalafiddle.io/sf/KEbRBWp/0).