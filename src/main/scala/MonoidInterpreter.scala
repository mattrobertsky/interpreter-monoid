package ltbs.uniform
package interpreters.monoidinterpreter

import shapeless._
import cats.implicits._
import cats.Monoid
import cats.Id
import ltbs.uniform.validation.Rule

/**
  * By definition a Monoid provides `empty` on top of Semigroups `combine`.
  * cats provides Monoids for Int, String, List etc.
  *
  * MonoidInterpreter is only going to `ask` so we don't worry about any `tell`.
  * SupportedTell is only here to fit the constructor shape of `Language`
  *
  * We are using the `cats.Id` (identity) Monad here, usually we'd be using a `WebMonad`.
  *
  * For comparison it is a good idea to see Uniforms `GenericWebInterpreter`
  *
  * @param askSummoner
  * @tparam SupportedAsk
  * @tparam SupportedTell
  */
case class MonoidInterpreter[
     SupportedAsk <: HList, // n.b. <: means SupportedAsk must be a subtype of HList
     SupportedTell <: HList // don't care (only needed here so we can extend Language)
]()(implicit
     askSummoner: TypeclassList[SupportedAsk, Monoid]
) extends Language[Id, SupportedTell, SupportedAsk] {

     override def interact[Tell, Ask](
       id: String, // don't care
       tell: Tell, // don't care
       default: Option[Ask], // don't care
       validation: Rule[Ask], // don't care
       customContent: Map[String, (String, List[Any])] // don't care
     )(implicit
       selectorTell: IndexOf[SupportedTell, Tell], // don't care
       selectorAsk: IndexOf[SupportedAsk, Ask]
     ): Id[Ask] = { // <- note Id
          val askMonoid: Monoid[Ask] = askSummoner.forType[Ask]
          askMonoid.empty
     }
}
	
