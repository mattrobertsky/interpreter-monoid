import cats.{Id, Monad}
import cats.implicits._
import ltbs.uniform._
import ltbs.uniform.interpreters.monoidinterpreter.MonoidInterpreter
import ltbs.uniform.validation.Rule
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import shapeless.HList

class MonoidInterpreterSpec extends AnyFlatSpec with Matchers {

  "A MonoidInterpreter" should "interpret a program for know Monoids" in {

    type SupTell = NilTypes
    type SupAsk = (Int,String) :: Int :: String :: List[Int] :: List[String] :: NilTypes

    def testProgram[UF[_] : Monad](int: Language[UF, SupTell, SupAsk]): UF[String] = {
      // the yield returns a String hence : [UF[String]] is the return type
      import int._
      for {
        a <- ask[String]("name")
        b <- ask[Int]("favourite-number")
        c <- ask[(Int, String)]("foo") // ask is equivalent to interact (below)
        d <- interact[Unit,Int](id = "bar", (), None, Rule.alwaysPass[Int], Map.empty[String,(String,List[Any])])
      } yield s"$a $b $c $d" // we return a String inside the given Monad
    }


    testProgram(MonoidInterpreter()) shouldBe " 0 (0,) 0"
//    testProgram[Id](MonoidInterpreter()) shouldBe " 0 (0,) 0" the UF of Id is inferred

  }

}
