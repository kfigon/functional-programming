import org.scalatest.funspec.AnyFunSpec

class FunctionalDependencyInjectionSpec extends AnyFunSpec {

  trait Dependency {
    def doIt: Int
  }

  case class Db() extends Dependency {
    override def doIt: Int = 42
  }

  def calculate(a: Int, b: Int, dep: Dependency): Int = a + b + dep.doIt
  def calculateBetter(a: Int, b: Int): Dependency => Int = dep => a + b + dep.doIt

  it("bad example") {
    val bad = calculate(1,2, Db())
    assert(bad == 45)
  }

  it("just use currying - pass the dependency after setting a pure pipeline") {
    val good = calculateBetter(1,2)(Db())
    assert(good == 45)
  }
//  there's also a reader monad and cake pattern with self type. TBD
}
