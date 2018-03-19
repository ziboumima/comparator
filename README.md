# comparator

The project helps to create automatically a Comparator typeclass to be used in a regression test

The Comparator type class can say whether two values are equals and a detail of what are the differences between these two values. 

The project constructs instances for a large common use cases like `List`, `Map`, `Double`, `String`, `Int`. It can compose in many ways to match the required type

The comparator of `Double` is special because It should accept some errors due to numerical errors. In order to construct a doube comparator, we should define an implicit special object called `AcceptanceError`:

```scala
case class AcceptanceError(relative: Double, absolute: Double)
```
The 2 values `x` and `y` are accepted when (x - y) / x < `relative`. If x == 0 or y == 0 then the absolute value is used: abs(x - y) < absolute

# Examples

In tests, we should compare 2 objets by:

```scala
Comparator.assertEqual(first, second)
```

It will raise exception if first != second in print to the console the exact differences

## List

## Map

## case class

## sealed trait


# Why do not make a library ?

The project is quite simpe and it's an excellent way to learn `shapeless`, just copy/paste 3 files into your project and explore it

