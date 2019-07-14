package com.example.data

import scala.collection.mutable

case class Plan(plan: Vector[Action],
                pos: Point,
                wheels: Int,
                drill: Int,
                drilled: mutable.HashSet[Point])