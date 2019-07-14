package com.example.data


sealed trait Action

object Action extends Enum[Action] {

  case object UP extends Action

  case object RIGHT extends Action

  case object DOWN extends Action

  case object LEFT extends Action

  case object JUMP0 extends Action

  case object JUMP1 extends Action

  case object JUMP2 extends Action

}