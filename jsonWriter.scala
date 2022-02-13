// Define a very simple JSON AST
sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
final case object JsNull extends Json
// The "serialize to JSON" behaviour is encoded in this trait
//an interface to a write method that produces a JSON Object
trait JsonWriter[A] {
    def write(value: A): Json
}

final case class Person(name: String, email: String)

//writer instance class
//can be used implicitly by the compiler by declaring them as implicit vals
object JsonWriterInstances {
    implicit val stringWriter: JsonWriter[String] =
        new JsonWriter[String] {
            def write(value: String): Json =
                JsString(value)
            }

    implicit val personWriter: JsonWriter[Person] =
        new JsonWriter[Person] {
            def write(value: Person): Json =
                JsObject(Map(
                    "name" -> JsString(value.name),
                    "email" -> JsString(value.email)
                    ))
            }
}

//Json object with the function toJson
object Json {
    def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = //parameter list in implict to be constructed by the compiler
        w.write(value)
}

// calls such as: 
Json.toJson(Person("name", "email")) //implicit JsonWriter[Person] inserted by compiler type A figured out by parameter supplied to toJson write

object JsonSyntax{

    implicit class JsonWriterOps[A](value : A){
        def toJson(value: A)(implicit w : JsonWriter[A]) : Json = 
        w.write(value)
    }

}

Person("name", "email").toJson // implicity converted to JsonWriterOps and implicit parameter supplied to the toJson method.
//Same as:
JsonWriterOps(Person("name", "email")).toJson(personWriter)

//for handling options:
//relies on w to write the case when Some(val), this creates a definition for writing Option values.
implicit def optionWriter[A](implicit w : JsonWriter[A]) : JsonWriter[Option[A]] = {
    new JsonWriter[Option[A]]{
        def write(option: Option[A]) : Json = {
            case Some(value) => w.write(value)
            case None => JsNull
        }

    }
} 
//abstracting the code allows not to keep defining values:
//  implicit val optionIntWriter: JsonWriter[Option[Int]] = ???
//  implicit val optionPersonWriter: JsonWriter[Option[Person]] = ???


// The call 
Json.toJson(Option("A String")) // looks for:
Json.toJson(Option("A String"))(optionWriter[String]) //which looks recursivley for the parameter:
Json.toJson(Option("A String"))(optionWriter(stringWriter))