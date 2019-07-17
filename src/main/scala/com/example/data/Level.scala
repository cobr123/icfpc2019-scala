package com.example.data

import scala.collection.mutable

case class Level(grid: Array[Cell],
                 weights: Vector[Int],
                 zones: Vector[Zone],
                 width: Int,
                 height: Int,
                 var empty: Int,
                 zones_empty: mutable.ListBuffer[Int],
                 spawns: mutable.HashSet[Point] = new mutable.HashSet(),
                 beakons: mutable.ListBuffer[Point] = new mutable.ListBuffer[Point](),
                 bonuses: mutable.HashMap[Point, Bonus] = new mutable.HashMap(),
                 collected: mutable.HashMap[Bonus, Int] = new mutable.HashMap()) {

  def grid_idx(x: Int, y: Int): Int = Level.grid_idx(x, y, width)

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
      zones_empty(zone.idx) -= 1
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

object Level {
  def grid_idx(x: Int, y: Int, width: Int): Int = x + y * width
}
