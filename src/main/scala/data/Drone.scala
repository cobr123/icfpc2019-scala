package data

import data.Bonus.CLONE

import scala.collection.mutable

case class Drone(pos: Point,
                 hands: Vector[Point] = Vector(Point(0,0), Point(1,-1), Point(1,0), Point(1,1)),
                 wheels: Int = 0,
                 drill: Int = 0,
                 path: String = "",
                 plan: Vector[Action] =  Vector(),
                 zone: Zone = Zone.UNDECIDED_ZONE) {

  def wrap_bot( level:  Level):Unit = {
    val  to_wrap: mutable.HashSet[Point] = new mutable.HashSet()
    Main.would_wrap(level, this, pos, to_wrap)
    for (p <- to_wrap) {
      level.wrap_cell(p.x, p.y)
    }
  }

  def choose_zone( taken: List[Zone], level: Level) : Boolean = {
    if (zone == Zone.UNDECIDED_ZONE || level.zones_empty(zone) == 0) {
      val not_empty:  Vector<u8> = (0..level.zones_empty.len() as u8).filter(|&z| level.zones_empty[z ] > 0).collect();
      val not_taken:  Vector<u8> = not_empty.iter().cloned().filter(|&z| taken.iter().all(|&t| t != z)).collect();
      val looking_in: Vector<u8> = if not_taken.len() > 0 { not_taken } else { not_empty };
      val rate = |level: &Level, drone: &Drone, pos: &Point| {
        if level.get_cell(pos.x, pos.y) == Cell.EMPTY && looking_in.contains(&level.get_zone(pos.x, pos.y)) { 1. }
        else { 0. }
      };

      Main.explore_impl(level, this, rate) match {
        case Some((newplan, newpos, _)) => {
          zone = level.get_zone(newpos.x, newpos.y)
          plan = newplan
        }
        case _ => throw new Exception("No zone left to choose")
      }
      true
    } else {
      false
    }
  }

  def collect( level:  Level):Unit = {
    level.bonuses.get(pos) match {
      case Some(bonus) =>
      level.collected.get(bonus) match {
        case Some(collected) => collected += 1
        case _ => level.collected.addOne(bonus, 1)
      }
      level.bonuses.remove(pos)
    }
  }

  def wear_off():Unit = {
    if (wheels > 0) { wheels -= 1; }
    if (drill > 0) { drill -= 1; }
  }

  def has_space( level: &Level) : Boolean = {
    (1..5).all(   |i| level.valid(self.pos.x, self.pos.y+i) && level.get_cell(self.pos.x, self.pos.y+i) != Cell.BLOCKED)
    || (1..5).all(|i| level.valid(self.pos.x, self.pos.y-i) && level.get_cell(self.pos.x, self.pos.y-i) != Cell.BLOCKED)
    || (1..5).all(|i| level.valid(self.pos.x+i, self.pos.y) && level.get_cell(self.pos.x+i, self.pos.y) != Cell.BLOCKED)
    || (1..5).all(|i| level.valid(self.pos.x-i, self.pos.y) && level.get_cell(self.pos.x-i, self.pos.y) != Cell.BLOCKED)
  }

  def activate_wheels( level: &mut Level) : Boolean = {
    if (level.collected.getOrElse(Bonus.WHEELS, 0) > 0
    && self.wheels == 0
    && self.has_space(level)) {
      update(&mut level.collected, Bonus.WHEELS, -1);
      self.wheels = 51;
      self.path += "F";
      true
    } else { false }
  }

  def activate_drill( level: &mut Level) : Boolean = {
    if (level.collected.getOrElse(Bonus.DRILL, 0) > 0
    && self.drill == 0) {
      update(&mut level.collected, Bonus.DRILL, -1);
      self.drill = 31;
      self.path += "L";
      true
    } else { false }
  }

  def activate_hand( level: &mut Level) : Boolean = {
    if (level.collected.getOrElse(Bonus.HAND, 0) > 0) {
      update(&mut level.collected, Bonus.HAND, -1);
      val new_hand = Point(1, self.hands.last().unwrap().y + 1);
      self.path += &format!("B({},{})", new_hand.x, new_hand.y);
      self.hands.push(new_hand);
      true
    } else { false }
  }

  def set_beakon( level: &mut Level) : Boolean = {
    if (level.collected.getOrElse(Bonus.TELEPORT, 0) > 0
    && level.beakons.iter().all(|b| (b.x - self.pos.x).abs() + (b.y - self.pos.y).abs() >= 50)) {
      update(&mut level.collected, Bonus.TELEPORT, -1);
      self.path += "R";
      level.beakons.push(self.pos);
      true
    } else { false }
  }

  def reduplicate( level: &mut Level) : Option<Drone> = {
    if (level.collected.getOrElse(Bonus.CLONE, 0) > 0 && level.spawns.contains(&self.pos)) {
      update(&mut level.collected, Bonus.CLONE, -1);
      self.path += "C";
      Some(Drone(self.pos))
    } else { None }
  }

  def act( action: Action, level:  Level):Unit = {
    val withWheels = wheels > 0
    val withDrill = drill > 0
    step(level, this, pos, action, withWheels, withDrill, new mutable.HashSet()) match {
      case Some((newpos, new_wrapped, new_drilled)) =>
      pos = newpos
        action match  {
          case Action.UP    => path += "W"
          case Action.DOWN  => path += "S"
          case Action.LEFT  => path += "A"
          case Action.RIGHT => path += "D"
          case Action.JUMP0 => path += &format!("T({},{})", level.beakons[0].x, level.beakons[0].y)
          case Action.JUMP1 => path += &format!("T({},{})", level.beakons[1].x, level.beakons[1].y)
          case Action.JUMP2 => path += &format!("T({},{})", level.beakons[2].x, level.beakons[2].y)
      }
      for (p <- new_wrapped) {
        level.wrap_cell(p.x, p.y)
      }
      for (p <- new_drilled) {
        level.drill_cell(p.x, p.y)
      }
    case _ =>throw new Exception("Unwalkable from ({},{}) {:?}", self.pos.x, self.pos.y, action);
    }
  }
}
