package com.example.data


sealed trait Bonus

object Bonus {

  case object HAND extends Bonus

  case object WHEELS extends Bonus

  case object DRILL extends Bonus

  case object TELEPORT extends Bonus

  case object CLONE extends Bonus

}