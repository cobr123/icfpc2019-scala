package com.example.data

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

final case class Plan(plan: mutable.ArrayBuffer[Action] = new ArrayBuffer[Action](),
                pos: Point,
                wheels: Int,
                drill: Int,
                drilled: mutable.HashSet[Point] = new mutable.HashSet[Point]()
               )