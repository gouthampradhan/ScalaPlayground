package scala_check

import org.scalacheck.{Gen, Prop, Properties}
import org.scalacheck.Gen.{alphaStr, listOf, numChar}

/**
  * Created by gouthamvidyapradhan on 28/01/2019
  */
object StringUtilsTest extends Properties("StringUtils") {

  property("truncate") = Prop.forAll { (s: String, n: Int) =>
    val t = StringUtils.truncate(s, n)
    (s.length <= n && t == s) ||
    (n < 0 && s == t) ||
    (s.length > n && t == s.take(n) + "...")
  }

  property("tokenize") = Prop.forAll(listOf(alphaStr), numChar) {
    (ts, c) => {
      val mList = ts.filter(!_.isEmpty)
      val str = mList.mkString(c.toString)
      lazy val result = StringUtils.tokenize(str, c).toList
      (mList.nonEmpty && result == mList) || mList.isEmpty
    }
  }

  val eventInt = for {
    v <- Gen.choose(-100, 100)
  } yield {v * 2}

  property("is even") = Prop.forAll(eventInt) {
    e => e % 2 == 0
  }

  import org.scalacheck.Gen.{choose, oneOf}

}
