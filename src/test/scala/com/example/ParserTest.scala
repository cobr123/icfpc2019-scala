package com.example

import com.example.data.{Bonus, Point}
import org.scalatest.FunSuite

class ParserTest extends FunSuite {

  test("parseLevelEmpty1") {
    val thrown = intercept[Exception] {
      Parser.parse_level("")
    }
    assert(thrown.getMessage === "incomplete file: ")
  }

  test("parseLevelCorners") {
    val parsed = Parser.parse_level("(0,0),(6,0),(6,1),(8,1),(8,2),(6,2),(6,3),(0,3)#(1,3)##")
    assertResult(24)(parsed._1.grid.length)
    assertResult(20)(parsed._1.empty)
    assertResult(24)(parsed._1.zones.length)
    assertResult(1)(parsed._1.zones_empty.length)
    assertResult(0)(parsed._1.bonuses.size)
    assertResult(0)(parsed._1.spawns.size)
    assertResult(8)(parsed._1.width)
    assertResult(3)(parsed._1.height)
    assertResult(1)(parsed._2.headOption.map(_.pos.x).getOrElse(0))
    assertResult(3)(parsed._2.headOption.map(_.pos.y).getOrElse(0))
  }

  test("parseLevelStart") {
    val parsed = Parser.parse_level("#(1,18)##")
    assertResult(0)(parsed._1.grid.length)
    assertResult(0)(parsed._1.empty)
    assertResult(0)(parsed._1.zones.length)
    assertResult(0)(parsed._1.zones_empty.length)
    assertResult(0)(parsed._1.bonuses.size)
    assertResult(0)(parsed._1.spawns.size)
    assertResult(0)(parsed._1.width)
    assertResult(0)(parsed._1.height)
    assertResult(1)(parsed._2.headOption.map(_.pos.x).getOrElse(0))
    assertResult(18)(parsed._2.headOption.map(_.pos.y).getOrElse(0))
  }

  test("parseLevelBooster") {
    val parsed = Parser.parse_level("#(1,18)##X(9,13);(3,14);L(7,14);F(8,18);F(19,26)")
    assertResult(0)(parsed._1.grid.length)
    assertResult(0)(parsed._1.empty)
    assertResult(0)(parsed._1.zones.length)
    assertResult(0)(parsed._1.zones_empty.length)
    assertResult(3)(parsed._1.bonuses.size)
    assertResult(1)(parsed._1.spawns.size)
    assertResult(0)(parsed._1.width)
    assertResult(0)(parsed._1.height)

    assertResult(Bonus.DRILL)(parsed._1.bonuses(Point(7, 14)))
    assertResult(Bonus.WHEELS)(parsed._1.bonuses(Point(8, 18)))
  }
}
