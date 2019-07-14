package com.example.data

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Plan(plan: mutable.ListBuffer[Action] = new ListBuffer[Action](),
                pos: Point,
                wheels: Int,
                drill: Int,
                drilled: mutable.HashSet[Point] = new mutable.HashSet[Point]()
               )