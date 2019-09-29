package scalisp.tokens

sealed trait ScalispToken

case class SYMBOL(tok : String) extends ScalispToken
case class INT(tok : String) extends ScalispToken
case class FLOAT(tok : String) extends ScalispToken
case object PLUS extends ScalispToken
case object MINUS extends ScalispToken
case object DIV extends ScalispToken
case object MULT extends ScalispToken
case object GT extends ScalispToken
case object LT extends ScalispToken
case object GTE extends ScalispToken
case object LTE extends ScalispToken
case object QUOTE extends ScalispToken
case object CAR extends ScalispToken
case object CDR extends ScalispToken
case object CONS extends ScalispToken
case object COND extends ScalispToken
case object DEFINE extends ScalispToken
case object SET extends ScalispToken
case object LAMBDA extends ScalispToken
case object NULL extends ScalispToken
case object ATOM extends ScalispToken
case object EQ extends ScalispToken
case object BEGIN extends ScalispToken
case object LOOP extends ScalispToken
case object IF extends ScalispToken
case class SEXP(exp : List[ScalispToken]) extends ScalispToken