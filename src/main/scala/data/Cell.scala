package data


sealed trait Cell

object Cell extends Enum[Cell] {

  case object EMPTY extends Cell

  case object BLOCKED extends Cell

  case object WRAPPED extends Cell

}