import org.scalatest.funspec.AnyFunSpec

//way to ergonomically modify immutable data
//without it we need a lot of nested copies. Here too, but the code is simpler
class LensesSpec extends AnyFunSpec {

  case class Asd(b: Boolean)
  case class Bar(i: Int, asd: Asd)
  case class Foo(v: String, bar: Bar)

  it("without optics") {
    val f = Foo("foo", Bar(1, Asd(false)))

    // boilerplate
    val f2 = f.copy(
      bar = f.bar.copy(
        asd = Asd(true)
      )
    )

    assert(f2 == Foo("foo", Bar(1, Asd(true))))
  }

  case class Lens[A,B](
                        get: A => B,
                        set: (A,B) => A
                      )

  def composeLenses[Outer, Inner, Value](
                                    outer: Lens[Outer, Inner],
                                    inner: Lens[Inner, Value]
                                  ): Lens[Outer, Value] = Lens[Outer, Value](
    // get = outer.get andThen inner.get,
    get = v => inner.get(outer.get(v)),
    set = (obj, value) => outer.set(obj, inner.set(outer.get(obj), value))
  )

  it("optics - compose lenses to get/set the val. We define them once") {
    val fooLense = Lens[Foo, Bar](
      get = f => f.bar,
      set = (f, v) => f.copy(bar = v)
    )

    val barAsdLense = Lens[Bar, Asd](
      get = _.asd,
      set = (obj,v) => obj.copy(asd = v)
    )

    val asdBoolLense = Lens[Asd, Boolean](
      get = _.b,
      set = (a, b) => a.copy(b = b)
    )

    val fooAsdBooleanLense = composeLenses(composeLenses(fooLense, barAsdLense), asdBoolLense)

    val f = Foo("foo", Bar(1, Asd(false)))
    val f2 = fooAsdBooleanLense.set(f, true)

    assert(f2 == Foo("foo", Bar(1, Asd(true))))

  }
}
