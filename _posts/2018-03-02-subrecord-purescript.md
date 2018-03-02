---
title: SubRecords in Purescript
layout: post
categories: [Programming, Purescript]
---

Purescript has the `Record` type to nicely handle records.
A `Record x` *must* contain values at all labels in the row `x`.
For example, a value of type `Record ( x :: Int, y :: String )` must contain an `Int` value at label `x` and a `String` value at label `y`.

However, it lacks nice support for subrecords, a record which *may* contain values for the labels.
While we can create the `SubRow` class to denote in the context that we are dealing with a subrecord, it does not solve all our problems.

```haskell
-- | `Union a b c` means a ∪ b = c
-- | `SubRow a b` means a is a subrow of b
-- | or: there exists an x for which `Union a x b` holds
class SubRow (a :: # Type) (b :: # Type)
instance subRow :: Union a x b => SubRow a b
```

Let's say we wanted to create a subrecord as a value. The following will not compile:

```haskell
-- does not compile
testSubRecord1 :: forall a. SubRow a ( x :: Int, y :: String ) => Record a
testSubRecord1 = { x: 42 }
```

The problem is that we need an `exists` quantifier, instead of a `forall` quantifier.
Namely, there exists an `a`, in this case `( x :: Int )`, which is a subrow of `( x :: Int, y :: String )` and returned as value.
Function `testSubRecord1` does not work for all subrows of `( x :: Int, y :: String )`.

# purescript-subrecord

Introducing the Work-In-Progress [purescript-subrecord](https://github.com/rubenpieters/purescript-subrecord), a library which contains the `SubRecord` type.
A `SubRecord x` *may* contain values for the labels in row `x`.
For example, a value of type `SubRecord ( x :: Int, y :: String )` could be any of: `{}`, `{ x :: Int }`, `{ y :: String}` or `{ x :: String, y :: String }`.

So, how does it work? Let's take a look at the data declaration for `SubRecord`.

```haskell
foreign import data SubRecord :: # Type -> Type
```

It has the same kind as `Record`, but the data type does not have any constructor for its values. Instead a constructor function is provided,
giving us something similar to an `exists` quantifier. I got this idea from the [purescript-exists](https://github.com/purescript/purescript-exists) library.
To create a `SubRecord` we use `mkSubRecord`.

```haskell
mkSubRecord :: forall a x r.
               Union a x r =>
               Record a -> SubRecord r
mkSubRecord = unsafeCoerce
```

It uses `unsafeCoerce` under the hood, a `SubRecord` uses the same underlying representation as a `Record`.

We can use `mkSubRecord` on a `Record a`, from which it creates a `SubRecord r` if `a` is a subrow of `r`.
This constructor allows the returning of subrecords as values, for example:

```haskell
testSubRecord2 :: SubRecord ( x :: Int, y :: String )
testSubRecord2 = mkSubRecord { x: 42 }
```

And if we try to add a wrong label, the compiler will warn us.

```haskell
wrongLabel :: SubRecord ( x :: Int, y :: String )
wrongLabel = mkSubRecord { z: 42 }
             ^^^^^^^^^^^^^^^^^^^^^
   could not match ( z :: Int | r ) with ( x :: Int, y :: String )
```

We can go back to a `Record` by providing default values for all labels in the row.
(Note: For some reason the function `withDefaults` compiles only if the type is inferred, it doesn't compile if the annotation is given explicitly.)

```haskell
withDefaults :: forall a. Record a -> SubRecord a -> Record a
withDefaults defaults = unSubRecord (\r -> Record.build (Record.merge defaults) r)
```

Which makes use of the slightly more general `unSubRecord`.

```haskell
unSubRecord :: forall x r.
               (forall a.
                Union a x r =>
                Record a -> Record r
               ) ->
               SubRecord r -> Record r
unSubRecord = passNullContext
```

Which is basically `unsafeCoerce` but it passes an `undefined` into the dictionary argument of the `forall a. Union a x r => Record a -> Record r` function.
The `unSubRecord` is the deconstructor for `SubRecord`, it safely transforms a `SubRecord` into a `Record` by taking a function which works on *all* subrecords `a` of `r`.

An example use of `withDefaults` is:

```haskell
testWithDef :: { x :: Int, y :: String }
testWithDef = withDefaults { x: 0, y: "default" } testSubRecord2
```

Then `testWithDef.x` evaluates to `42`, set by `testSubRecord2`, and `testWithDef.y` evaluates to `"default"`, the given default for label `y`.

# Building SubRecords

With the `Data.SubRecord.Builder`, similar to the `Data.Record.Builder`, we can create a record by `insert`ing values one at a time.

```haskell
insert
  :: forall l a r1 r2
   . RowCons l a r1 r2
  => RowLacks l r1
  => IsSymbol l
  => SProxy l
  -> Maybe a
  -> Builder (SubRecord r1) (SubRecord r2)
insert l (Just a) = Builder \r1 -> unsafeInsert (reflectSymbol l) a r1
insert l Nothing = Builder \r1 -> unsafeCoerce r1
```

Notice that we can add `Maybe a` values to the `SubRecord`, meaning that we can extend the label of the `SubRecord` without actually adding a value.
For example:

```haskell
testInsert :: SubRecord ( x :: Int, y :: String )
testInsert =
  SubRecord.build
    (SubRecord.insert (SProxy :: SProxy "x") (Just 42) >>>
     SubRecord.insert (SProxy :: SProxy "y") Nothing
    ) (mkSubRecord {})
```

Is equivalent to `mkSubRecord { x: 42 } :: SubRecord ( x :: Int, y :: String )`.

# Feedback Welcome

Any feedback or suggestions are welcome, so they can be added before I make the first release.
I plan to port the `Data.Record.Builder` functions present in purescript-record to their `SubRecord` equivalent.
Feel free to add suggestions as a [github issue](https://github.com/rubenpieters/purescript-subrecord/issues).