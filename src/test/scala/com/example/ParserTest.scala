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

  test("parseLevelEmpty2") {
    val parsed = Parser.parse_level("###")
    assertResult(0)(parsed._1.bonuses.size)
    assertResult(0)(parsed._1.width)
    assertResult(0)(parsed._1.height)
    assertResult(0)(parsed._2.headOption.map(_.pos.x).getOrElse(0))
    assertResult(0)(parsed._2.headOption.map(_.pos.y).getOrElse(0))
  }

  test("parseLevelCorners") {
    val parsed = Parser.parse_level("(20,5),(21,6),(21,4),(17,4)###")
    assertResult(0)(parsed._1.bonuses.size)
    assertResult(21)(parsed._1.width)
    assertResult(6)(parsed._1.height)
    assertResult(0)(parsed._2.headOption.map(_.pos.x).getOrElse(0))
    assertResult(0)(parsed._2.headOption.map(_.pos.y).getOrElse(0))
  }

  test("parseLevelStart") {
    val parsed = Parser.parse_level("#(1,18)##")
    assertResult(0)(parsed._1.bonuses.size)
    assertResult(0)(parsed._1.width)
    assertResult(0)(parsed._1.height)
    assertResult(1)(parsed._2.headOption.map(_.pos.x).getOrElse(0))
    assertResult(18)(parsed._2.headOption.map(_.pos.y).getOrElse(0))
  }

  test("parseLevelBooster") {
    val parsed = Parser.parse_level("###X(9,13);(3,14);L(7,14);F(8,18);F(19,26)")
    assertResult(5)(parsed._1.bonuses.size)
    assertResult(0)(parsed._1.width)
    assertResult(0)(parsed._1.height)

    assertResult(Bonus.DRILL)(parsed._1.bonuses(Point(7, 14)))
    assertResult(Bonus.WHEELS)(parsed._1.bonuses(Point(8, 18)))
  }
}
