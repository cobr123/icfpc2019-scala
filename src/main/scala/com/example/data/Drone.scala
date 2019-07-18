package com.example.data

import com.example.Main

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

final case class Drone(var pos: Point,
                 hands: mutable.ArrayBuffer[Point] = mutable.ArrayBuffer(Point(0, 0), Point(1, -1), Point(1, 0), Point(1, 1)),
                 var wheels: Int = 0,
                 var drill: Int = 0,
                 path: StringBuilder = new StringBuilder(),
                 var plan: mutable.ArrayBuffer[Action] = new mutable.ArrayBuffer[Action](),
                 var zone: Zone = Zone.UNDECIDED_ZONE) {

  def wrap_bot(level: Level): Unit = {
    val to_wrap: mutable.HashSet[Point] = new mutable.HashSet()
    Main.would_wrap(level, this, pos, to_wrap)
    for (p <- to_wrap) {
      level.wrap_cell(p.x, p.y)
    }
  }

  def choose_zone(taken: List[Zone], level: Level): Boolean = {
    if (zone == Zone.UNDECIDED_ZONE || level.zones_empty(zone.idx) == 0) {
      val not_empty = level.zones_empty.zipWithIndex.filter { case (c, z) => c > 0 }.map { case (c, z) => z }.toList
      val not_taken = not_empty.filter(z => !taken.contains(z))
      val looking_in = if (not_taken.nonEmpty) {
        not_taken
      } else {
        not_empty
      }

      def rate(level: Level, drone: Drone, pos: Point): Double = {
        if (level.get_cell(pos.x, pos.y) == Cell.EMPTY && looking_in.contains(level.get_zone(pos.x, pos.y).idx)) {
          1.0
        } else {
          0.0
        }
      }

      Main.explore_impl(level, this, rate) match {
        case Some((newplan, newpos, _)) =>
          zone = level.get_zone(newpos.x, newpos.y)
          plan = new ArrayBuffer[Action]().addAll(newplan)
        case _ => throw new Exception("No zone left to choose")
      }
      true
    } else {
      false
    }
  }

  def collect(level: Level): Unit = {
    level.bonuses.get(pos) match {
      case Some(bonus) =>
        if (level.collected.contains(bonus)) {
          level.collected(bonus) += 1
        } else {
          level.collected.addOne(bonus, 1)
        }
        level.bonuses.remove(pos)
      case _ =>
    }
  }

  def wear_off(): Unit = {
    if (wheels > 0) {
      wheels -= 1
    }
    if (drill > 0) {
      drill -= 1
    }
  }

  def has_space(level: Level): Boolean = {
    ((1 until 5).forall(i => level.valid(pos.x, pos.y + i) && level.get_cell(pos.x, pos.y + i) != Cell.BLOCKED)
      || (1 until 5).forall(i => level.valid(pos.x, pos.y - i) && level.get_cell(pos.x, pos.y - i) != Cell.BLOCKED)
      || (1 until 5).forall(i => level.valid(pos.x + i, pos.y) && level.get_cell(pos.x + i, pos.y) != Cell.BLOCKED)
      || (1 until 5).forall(i => level.valid(pos.x - i, pos.y) && level.get_cell(pos.x - i, pos.y) != Cell.BLOCKED))
  }

  def activate_wheels(level: Level): Boolean = {
    if (level.collected.getOrElse(Bonus.WHEELS, 0) > 0
      && wheels == 0
      && has_space(level)) {
      Main.update(level.collected, Bonus.WHEELS, -1)
      wheels = 51
      path.append("F")
      true
    } else {
      false
    }
  }

  def activate_drill(level: Level): Boolean = {
    if (level.collected.getOrElse(Bonus.DRILL, 0) > 0 && drill == 0) {
      Main.update(level.collected, Bonus.DRILL, -1)
      drill = 31
      path.append("L")
      true
    } else {
      false
    }
  }

  def activate_hand(level: Level): Boolean = {
    if (level.collected.getOrElse(Bonus.HAND, 0) > 0) {
      Main.update(level.collected, Bonus.HAND, -1)
      val new_hand = Point(1, hands.last.y + 1)
      path.append(s"B(${new_hand.x},${new_hand.y})")
      hands.addOne(new_hand)
      true
    } else {
      false
    }
  }

  def set_beakon(level: Level): Boolean = {
    if (level.collected.getOrElse(Bonus.TELEPORT, 0) > 0 && level.beakons.forall(b => (b.x - pos.x).abs + (b.y - pos.y).abs >= 50)) {
      Main.update(level.collected, Bonus.TELEPORT, -1)
      path.append("R")
      level.beakons.addOne(pos)
      true
    } else {
      false
    }
  }

  def reduplicate(level: Level): Option[Drone] = {
    if (level.collected.getOrElse(Bonus.CLONE, 0) > 0 && level.spawns.contains(pos)) {
      Main.update(level.collected, Bonus.CLONE, -1)
      path.append("C")
      Some(Drone(pos))
    } else {
      None
    }
  }

  def act(action: Action, level: Level): Unit = {
    val withWheels = wheels > 0
    val withDrill = drill > 0
    Main.step(level, this, pos, action, withWheels, withDrill, new mutable.HashSet()) match {
      case Some((newpos, new_wrapped, new_drilled)) =>
        pos = newpos
        action match {
          case Action.UP => path.append("W")
          case Action.DOWN => path.append("S")
          case Action.LEFT => path.append("A")
          case Action.RIGHT => path.append("D")
          case Action.JUMP0 => path.append(s"T(${level.beakons(0).x},${level.beakons(0).y})")
          case Action.JUMP1 => path.append(s"T(${level.beakons(1).x},${level.beakons(1).y})")
          case Action.JUMP2 => path.append(s"T(${level.beakons(2).x},${level.beakons(2).y})")
        }
        for (p <- new_wrapped) {
          level.wrap_cell(p.x, p.y)
        }
        for (p <- new_drilled) {
          level.drill_cell(p.x, p.y)
        }
      case _ => throw new Exception(s"Unwalkable from (${pos.x},${pos.y}) $action")
    }
  }
}
