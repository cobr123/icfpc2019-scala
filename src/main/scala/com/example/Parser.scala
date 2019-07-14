package com.example

import com.example.data._

import scala.collection.mutable

object Parser {

    val POINT_RE = "\((?P<X>-?\d+),(?P<Y>-?\d+)\)"r
    val BONUS_RE = "(?P<P>[BFLRC])\((?P<X>-?\d+),(?P<Y>-?\d+)\)"r
    val SPAWN_RE = "X\((?P<X>-?\d+),(?P<Y>-?\d+)\)"r


  def grid_idx(x:Int, y:Int, width:Int):Int = x + y * width

  def parse_point(s: String) : Point ={
    val captures = POINT_RE.captures(s).unwrap();
    Point(captures["X"].parse.<Int>().unwrap(), captures["Y"].parse.<Int>().unwrap())
  }

  def parse_bonus(captures: Captures) : (Point, Bonus) ={
    (Point(captures["X"].parse.<Int>().unwrap(), captures["Y"].parse.<Int>().unwrap()),
    match &captures["P"] {
      "B" => { Bonus.HAND }
      "F" => { Bonus.WHEELS }
      "L" => { Bonus.DRILL }
      "R" => { Bonus.TELEPORT }
      "C" => { Bonus.CLONE }
      _   => throw new Exception("Unknown bonus")
    })
  }

  def parse_contour(s: String) : mutable.HashSet[Point] ={
    val points: Vector<Point> = POINT_RE.find_iter(s).map(|m| parse_point(m.as_str())).collect();
    val mut walls: mutable.HashSet<Point> = mutable.HashSet.with_capacity_and_hasher(points.len(), Default.default());
    for (i, &p1) in points.iter().enumerate() {
      val p2 = points[(i+1) % points.len()];
      if p1.x == p2.x { // vercical only
        for y in if p1.y < p2.y { p1.y .. p2.y } else { p2.y .. p1.y } {
          walls.insert(Point(p1.x, y));
        }
      }
    }
    walls
  }

  def wall_on_left(x: Int, y: Int, walls: Vector[Line]) : Boolean ={
    walls.exists(l => l.from.x == x
    && l.from.y <= y
    && l.to.y >= (y + 1) )
  }

  def weights(grid: &[Cell], width: Int, height: Int) : Vector<u8> ={
    val mut weights: Vector<u8> = Vector.with_capacity(grid.len());
    for y in 0..height {
      for x in 0..width {
        val mut sum: u8 = 0;
        for (dx, dy) in &[(0,1),(0,-1),(-1,0),(1,0),(1,1),(-1,-1),(-1,1),(1,-1)] {
          val x2 = x + dx;
          val y2 = y + dy;
          if x2 >= 0 && x2 < width && y2 >= 0 && y2 < height && grid[grid_idx(x2, y2, width)] == Cell.BLOCKED {
            sum += 1;
          }
        }
        weights.push(sum);
      }
    }
    weights
  }

  def zones(zones_count: Int, grid: &[Cell], width: Int, height: Int) : (Vector<u8>, Vector<Int>)= {
    val len = (width * height) ;

    val mut zones: Vector<u8> = Vector.with_capacity(len);
    for i in 0..len { zones.push(UNDECIDED_ZONE); }

    val mut zones_empty: Vector<Int> = Vector.with_capacity(zones_count);
    for i in 0..zones_count { zones_empty.push(0); }

    val mut queue: VecDeque<(Point, u8)> = VecDeque.with_capacity(len);
    val mut rng = rand_pcg.Pcg32.seed_from_u64(42);
    while queue.len() < zones_count {
      val x = rng.gen_range(0, width);
      val y = rng.gen_range(0, height);
      val idx = grid_idx(x, y, width);
      val point = Point(x, y);
      if grid[idx] == Cell.EMPTY && queue.iter().find(|(p, _)| *p == point).is_none() {
        queue.push_back((point, queue.len() as u8));
      }
    }

    while val Some((Point{x, y}, zone)) = queue.pop_front() {
      val idx = grid_idx(x, y, width);
      if zones[idx] == UNDECIDED_ZONE && grid[idx] == Cell.EMPTY {
        zones_empty[zone ] += 1;
        zones[idx] = zone;
        if y + 1 < height { queue.push_back((Point(x, y + 1), zone)); }
        if y > 0          { queue.push_back((Point(x, y - 1), zone)); }
        if x + 1 < width  { queue.push_back((Point(x + 1, y), zone)); }
        if x > 0          { queue.push_back((Point(x - 1, y), zone)); }
      }
    }

    (zones, zones_empty)
  }

  def build_level(walls: &mutable.HashSet<Point>, zones_count: Int) : Level ={
    val height = walls.iter().max_by_key(|p| p.y).unwrap().y + 1;
    val width = walls.iter().max_by_key(|p| p.x).unwrap().x;
    val mut grid = Vector.with_capacity((width * height) );
    val mut empty = 0;
    for y in 0..height {
      val mut last_cell = Cell.BLOCKED;
      for x in 0..width {
        if walls.contains(&Point(x, y)) {
          last_cell = if last_cell == Cell.EMPTY { Cell.BLOCKED } else { Cell.EMPTY };
        }
        grid.push(last_cell);
        if last_cell == Cell.EMPTY { empty += 1; }
      }
      assert_eq!(walls.contains(&Point(width, y)), Cell.EMPTY == last_cell);
    }
    val weights = weights(&grid, width, height);
    val (zones, zones_empty) = zones(zones_count, &grid, width, height);
    Level {
      grid, weights, zones, width, height, empty, zones_empty,
      spawns:    mutable.HashSet.default(),
      beakons:   Vector(),
      bonuses:   mutable.HashMap.default(),
      collected: mutable.HashMap.default()
    }
  }

  def parse_level(file: String) : (Level, Vector<Drone>) ={
    val fragments: Vector<String> = file.split("#").collect();
    match *fragments {
      [walls_str, start_str, obstacles_str, bonuses_str] => {
        val mut walls = parse_contour(walls_str);
        for obstacle_str in obstacles_str.split(";").filter(|s| !s.is_empty()) {
          walls.extend(parse_contour(obstacle_str));
        }
        val clones = Regex(r"C\(\d+,\d+\)").unwrap().find_iter(bonuses_str).count();
        val mut level = build_level(&walls, clones + 1);

        for captures in BONUS_RE.captures_iter(bonuses_str) {
          val (pos, bonus) = parse_bonus(captures);
          level.bonuses.insert(pos, bonus);
        }
        for captures in SPAWN_RE.captures_iter(bonuses_str) {
          val pos = Point(captures["X"].parse.<Int>().unwrap(), captures["Y"].parse.<Int>().unwrap());
          level.spawns.insert(pos);
        }
        (level, vec![Drone(parse_point(start_str))])
      }
      _ => throw new Exception("incomplete file")
    }
  }
}
