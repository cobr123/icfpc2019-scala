package data

import scala.collection.mutable

case class Level(grid: Vector[Cell],
                 weights: Vector[Int],
                 zones: Vector[Zone],
                 width: Int,
                 height: Int,
                 empty: Int,
                 zones_empty: Vector[Int],
                 spawns: mutable.HashSet[Point],
                 beakons: Vector[Point],
                 bonuses: mutable.HashMap[Point, Bonus],
                 collected: mutable.HashMap[Bonus, Int]) {

  def grid_idx(x: Int, y: Int): Int = x + y * width

  def get_cell(x: Int, y: Int): Cell = {
    assert(x >= 0 && x < width && y >= 0 && y < height)
    grid(grid_idx(x, y))
  }

  def get_zone(x: Int, y: Int): Zone = {
    assert(x >= 0 && x < width && y >= 0 && y < height)
    zones(grid_idx(x, y))
  }

  def wrap_cell(x: Int, y: Int): Unit = {
    assert(x >= 0 && x < width && y >= 0 && y < height)
    assert(get_cell(x, y) == Cell.EMPTY)
    val idx = grid_idx(x, y)
    empty -= 1
    val zone = zones(idx)
    if (zone.idx < 255) {
      zones_empty(zone) -= 1
    }
    grid(idx) = Cell.WRAPPED
  }

  def drill_cell(x: Int, y: Int): Unit = {
    assert(x >= 0 && x < width && y >= 0 && y < height)
    assert(get_cell(x, y) == Cell.BLOCKED)
    val idx = grid_idx(x, y)
    grid(idx) = Cell.WRAPPED
  }

  def valid(x: Int, y: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height

  def walkable(x: Int, y: Int): Boolean = valid(x, y) && get_cell(x, y) != Cell.BLOCKED

}
