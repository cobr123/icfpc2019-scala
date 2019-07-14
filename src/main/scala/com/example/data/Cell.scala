package com.example.data


sealed trait Cell

object Cell {

  case object EMPTY extends Cell

  case object BLOCKED extends Cell

  case object WRAPPED extends Cell

}