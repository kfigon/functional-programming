package course.part4implicits

import org.scalatest.funspec.AnyFunSpec

class OrganizingImplicitsSpec extends AnyFunSpec {
  val l = List(1,4,2,3,5)

//  implicits -> val/var, objects, defs without parentheses

  // order of implicits:
  // local scope,
  // imported,
  // companion objects of all types involved in the signature - this is preferred. Then import it in the scope. Or dedicated companion objects for each implicit and import them

  it("sorting") {
    assert(l.sorted == List(1,2,3,4,5)) // in scala.Predef there's an implicit sorting for ints
  }

  it("rev sorting") {
    implicit val revOrder: Ordering[Int] = Ordering.fromLessThan((a,b) => a > b)
    assert(l.sorted == List(5,4,3,2,1)) // locally scoped order, won over the global one
  }

  it("own sorting") {
    case class Person(name: String, age: Int)
    val people = List(
      Person("John", 3),
      Person("Jimmy", 13),
      Person("Abby", 13),
      Person("Jane", 50),
    )

    object PersonOrdering {
      implicit val peopleByAgeAndName: Ordering[Person] = Ordering.fromLessThan{
        case (a,b) if a.age < b.age => true
        case (a,b) if a.age > b.age => false
        case (a,b)  => a.name < b.name
      }

      implicit val peopleByAge: Ordering[Person] = Ordering.fromLessThan(_.age < _.age)
    }

    import PersonOrdering.peopleByAgeAndName

    assert(people.sorted == List(Person("John", 3), Person("Abby", 13), Person("Jimmy", 13), Person("Jane", 50)))
  }
}
