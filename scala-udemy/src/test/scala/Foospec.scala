import org.scalatest.funspec.AnyFunSpec

class Foospec extends AnyFunSpec {
  implicit class BetterFoo[T](left: T) {
    def ====(other: T) = if(left != other) throw new Exception(s"$left != $other")
  }

  it("foobar") {
    assertThrows[Exception](123 ==== 4567)
  }
}
