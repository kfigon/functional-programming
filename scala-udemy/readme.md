# implicits

## implicit vals/params - to inject values

## implicit function - to convert values
```scala
def toFoo(i: Int): Foo = Foo(i, "dummy")
```

## implicit class - to enrich classes with new functionalities

```scala
implicit class Bike(i: Int) {
   def start: Unit = println("bike is starting")
   def stop: Unit = println("bike is stop")
 }
 val i: Int = 4
i.start
i.stop
```


# typeclasses

## 1. typeclass
```scala
trait MyTmpl[T] {
  def foo(v: T): String
}
```

## 2. typeclass instance for given type
```scala
implicit object MyTmplInstance extends MyTmpl[Int] {
  override def foo(v: Int): String = v.toString
}
```

## 3. invoke
### 3a. invoke by companion or method
```scala
object MyTmpl {
  def solve[T](v: T)(implicit instance: MyTmpl[T]): String = instance.foo(v)
}
```

### 3b. pimp by lib enrichment
this combines implicit conversion and typeclasses
```scala
implicit class Enriched[T](v: T) {
  def foo(implicit instance: MyTmpl[T]): String = instance.foo(v)
}

2.foo
```