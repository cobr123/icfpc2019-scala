package com.example

import com.example.data._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.util.matching.Regex

object Parser {

  val POINT_RE: Regex = """\((\d+),(\d+)\)""".r
  val BONUS_RE: Regex = """([BFLRC])\((\d+),(\d+)\)""".r
  val SPAWN_RE: Regex = """\((\d+),(\d+)\)""".r


  def grid_idx(x: Int, y: Int, width: Int): Int = x + y * width

  def parse_point(s: String): Point = {
    val captures = POINT_RE.findFirstMatchIn(s).get
    Point(captures.group(1).toInt, captures.group(2).toInt)
  }

  def parse_bonus(captures: Regex.Match): (Point, Bonus) = {
    (Point(captures.group(2).toInt, captures.group(3).toInt),
      captures.group(1) match {
        case "B" => Bonus.HAND
        case "F" => Bonus.WHEELS
        case "L" => Bonus.DRILL
        case "R" => Bonus.TELEPORT
        case "C" => Bonus.CLONE
        case _ => throw new Exception("Unknown bonus")
      })
  }

  def parse_contour(s: String): mutable.HashSet[Point] = {
    val points = POINT_RE.findAllMatchIn(s).map(m => parse_point(m.toString())).toList
    val walls = new mutable.HashSet[Point]()
    for ((p1, i) <- points.zipWithIndex) {
      val p2 = points((i + 1) % points.length)
      if (p1.x == p2.x) { // vercical only
        for (y <- if (p1.y < p2.y) {
          p1.y until p2.y
        } else {
          p2.y until p1.y
        }) {
          walls.add(Point(p1.x, y))
        }
      }
    }
    walls
  }

  def wall_on_left(x: Int, y: Int, walls: Vector[Line]): Boolean = {
    walls.exists(l => l.from.x == x
      && l.from.y <= y
      && l.to.y >= (y + 1))
  }

  def weights(grid: List[Cell], width: Int, height: Int): Vector[Int] = {
    val weights = new ListBuffer[Int]()
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        var sum = 0
        for ((dx, dy) <- List((0, 1), (0, -1), (-1, 0), (1, 0), (1, 1), (-1, -1), (-1, 1), (1, -1))) {
          val x2 = x + dx
          val y2 = y + dy
          if (x2 >= 0 && x2 < width && y2 >= 0 && y2 < height && grid(grid_idx(x2, y2, width)) == Cell.BLOCKED) {
            sum += 1
          }
        }
        weights += sum
      }
    }
    assert(grid.length == weights.length)
    weights.toVector
  }

  def zones(zones_count: Int, grid: List[Cell], width: Int, height: Int): (Vector[Zone], ListBuffer[Int]) = {
    val len = width * height

    val zones = new ListBuffer[Zone]()
    for (i <- 0 until len) {
      zones += Zone.UNDECIDED_ZONE
    }

    val zones_empty = new ListBuffer[Int]()
    for (i <- 0 until zones_count) {
      zones_empty += 0
    }

    val queue = new ListBuffer[(Point, Int)]()
    val rng = new Random(42)
    while (queue.length < zones_count) {
      val x = rng.nextInt(width)
      val y = rng.nextInt(height)
      val idx = grid_idx(x, y, width)
      val point = Point(x, y)
      if (grid(idx) == Cell.EMPTY && !queue.exists(p => p._1 == point)) {
        queue.addOne((point, queue.length))
      }
    }

    while (queue.nonEmpty) {
      queue.remove(0) match {
        case (Point(x, y), zone) =>
          val idx = grid_idx(x, y, width)
          if (zones(idx) == Zone.UNDECIDED_ZONE && grid(idx) == Cell.EMPTY) {
            zones_empty(zone) += 1
            zones(idx) = Zone(zone)
            if (y + 1 < height) {
              queue.addOne((Point(x, y + 1), zone))
            }
            if (y > 0) {
              queue.addOne((Point(x, y - 1), zone))
            }
            if (x + 1 < width) {
              queue.addOne((Point(x + 1, y), zone))
            }
            if (x > 0) {
              queue.addOne((Point(x - 1, y), zone))
            }
          }
      }
    }

    (zones.toVector, zones_empty)
  }

  def build_level(walls: mutable.HashSet[Point], zones_count: Int): Level = {
    val height = walls.maxByOption(_.y).getOrElse(Point(0, 0)).y + 1
    val width = walls.maxByOption(_.x).getOrElse(Point(0, 0)).x
    val grid = new ListBuffer[Cell]()
    var empty = 0
    for (y <- 0 until height) {
      var last_cell: Cell = Cell.BLOCKED
      for (x <- 0 until width) {
        if (walls.contains(Point(x, y))) {
          last_cell = if (last_cell == Cell.EMPTY) Cell.BLOCKED else Cell.EMPTY
        }
        grid.addOne(last_cell)
        if (last_cell == Cell.EMPTY) {
          empty += 1
        }
      }
      assert(walls.contains(Point(width, y)) == (Cell.EMPTY == last_cell))
    }
    val newweights = weights(grid.toList, width, height)
    val (zones_all, zones_empty) = zones(zones_count, grid.toList, width, height)
    Level(grid, newweights, zones_all, width, height, empty, zones_empty)
  }

  val CLONE_RE: Regex = """C(d+,d+)""".r

  val FRAGMENTS_RE: Regex = "(.*?)#(.*?)#(.*?)#(.*?)".r

  def parse_level(text: String): (Level, List[Drone]) = {
    val fragments = FRAGMENTS_RE.findFirstMatchIn(text)
    FRAGMENTS_RE.findFirstMatchIn(text) match {
      case Some(matcher) => {
        val walls_str = matcher.group(1)
        val start_str = matcher.group(2)
        val obstacles_str = matcher.group(3)
        val bonuses_str = matcher.group(4)

        val walls = parse_contour(walls_str)
        for (obstacle_str <- obstacles_str.split(";").filter(s => !s.isEmpty)) {
          walls.addAll(parse_contour(obstacle_str))
        }
        val clones = CLONE_RE.findAllMatchIn(bonuses_str).size
        val level = build_level(walls, clones + 1)

        for (captures <- BONUS_RE.findAllMatchIn(bonuses_str)) {
          val (pos, bonus) = parse_bonus(captures)
          level.bonuses.addOne(pos, bonus)
        }
        for (captures <- SPAWN_RE.findAllMatchIn(bonuses_str)) {
          val pos = Point(captures.group(1).toInt, captures.group(2).toInt)
          level.spawns.addOne(pos)
        }
        (level, List(Drone(parse_point(start_str))))
      }
      case _ => throw new Exception(s"incomplete file: ${text}")
    }
  }
}
