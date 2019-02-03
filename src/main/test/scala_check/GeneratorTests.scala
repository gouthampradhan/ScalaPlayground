package scala_check

import org.scalacheck.{Gen, Prop, Properties}
import org.scalacheck.Gen.{choose, oneOf}
import org.scalacheck.Arbitrary

/**
  * Created by gouthamvidyapradhan on 29/01/2019
  */
object GeneratorTests extends Properties("Person") {

  case class Person(
      firstName: String,
      lastName: String,
      age: Int
  ) {
    def isTeenager = age >= 13 && age <= 19
  }

  val person = for {
    fName <- Gen.oneOf("Amit", "John", "Xeon")
    sName <- Gen.oneOf("Watson", "Pradhan", "Chow")
    age <- Gen.choose(1, 45)
  } yield Person(fName, sName, age)

  implicit val arbitrayPerson = Arbitrary(person)

  val personProperty = Prop.forAll { person: Person =>
    person.isTeenager == (person.age >= 13 && person.age <= 19)
  }

  personProperty.check
}
