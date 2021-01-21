package lab7

sealed trait Reason
case object Win extends Reason
case object Loose extends Reason
case object Draw extends Reason
