package course.part4implicits

import course.part4implicits.Converter.explicitCall
import org.scalatest.funspec.AnyFunSpec

object Json {
  sealed trait JsonValue {
    def jsonify: String
  }

  case class JsonString(v: String) extends JsonValue {
    override def jsonify: String = "\""+v+"\""
  }

  case class JsonNumber(v: Int) extends JsonValue {
    override def jsonify: String = v.toString
  }

  case class JsonBool(v: Boolean) extends JsonValue {
    override def jsonify: String = v.toString
  }

  case class JsonArray(v: List[JsonValue]) extends JsonValue {
    override def jsonify: String = v.map(_.jsonify).mkString("[", ",", "]")
  }

  case class JsonObject(vs: Map[String, JsonValue]) extends JsonValue {
    override def jsonify: String = vs.map {
      case (key, value) => "\"" + key + "\"" + ":" + value.jsonify
    }.mkString("{",",","}")
  }
}

object Domain {
  case class User(name: String, age: Int, active: Boolean)
  case class Post(content: String)
  case class Feed(user: User, posts: List[Post])
}

object Converter {
  // we need:
  // - typeclass
  // - typeclass instances (often implicit)
  // - method that uses the typeclass. It can be conversion (pimp my lib/enrichment) or method

  trait JsonConverter[T] {
    def convert(v: T): Json.JsonValue
  }

  implicit object UserConverter extends JsonConverter[Domain.User] {
    def convert(v: Domain.User): Json.JsonValue = Json.JsonObject(Map(
      "name" -> Json.JsonString(v.name),
      "age" -> Json.JsonNumber(v.age),
      "active" -> Json.JsonBool(v.active),
    ))
  }

  implicit object PostConverter extends JsonConverter[Domain.Post] {
    def convert(v: Domain.Post): Json.JsonValue = Json.JsonObject(Map(
      "content" -> Json.JsonString(v.content)
    ))
  }

  implicit object FeedConverter extends JsonConverter[Domain.Feed] {
    def convert(v: Domain.Feed): Json.JsonValue = Json.JsonObject(Map(
      "user" -> UserConverter.convert(v.user),
      "posts" -> Json.JsonArray(v.posts.map(_.content).map(Json.JsonString))
    ))
  }

  // pimp my lib
  implicit class JsonOps[T](v: T) {
    def toJson(implicit converter: JsonConverter[T]): Json.JsonValue = converter.convert(v)
  }

  // implicit conversion
  implicit def makeItAJson[T](v: T)(implicit converter: JsonConverter[T]): Json.JsonValue = converter.convert(v)

  // explicitly using method to use typeclass
  //
  def explicitCall[T](v: T)(implicit converter: JsonConverter[T]): Json.JsonValue = converter.convert(v)
}

class TypeclassesJsonSpec extends AnyFunSpec {
  describe("json") {
    it("json map") {
      val v = Json.JsonObject(Map(
        "foo" -> Json.JsonString("bar"),
        "asd" -> Json.JsonNumber(3),
        "collection" -> Json.JsonArray(List(Json.JsonBool(true), Json.JsonBool(false)))
      ))

      assert(v.jsonify == """{"foo":"bar","asd":3,"collection":[true,false]}""")
    }
  }

  describe("conversions") {
    val u = Domain.User(name = "John", age = 25, active = true)
    val p1 = Domain.Post("blah blah")
    val p2 = Domain.Post("this is awesome")
    val f = Domain.Feed(user = u, posts = List(p1, p2))

    it("enrichement") {
      import course.part4implicits.Converter._
      // pimp by lib to implicitly add toJson (convert to JsonValue using typeclass) to Feed and call jsonify
      assert(f.toJson.jsonify == """{"user":{"name":"John","age":25,"active":true},"posts":["blah blah","this is awesome"]}""")
    }

    it("implicit conversion, hard to debug") {
      // it uses makeItAJson implicitly

      import course.part4implicits.Converter._
      assert(f.jsonify == """{"user":{"name":"John","age":25,"active":true},"posts":["blah blah","this is awesome"]}""")
    }

    it("manually invoke typeclass") {
      assert(explicitCall(f).jsonify == """{"user":{"name":"John","age":25,"active":true},"posts":["blah blah","this is awesome"]}""")
    }
  }
}

