package pl.pawelklecha.kalaha
package events

import server.Gameboard

object ServerActionEvent {

  case class MakeMove(gameboard: Gameboard)

  case object NextMove

  case object EndGame

  case object AbortGame

}
