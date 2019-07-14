package com.example

import com.example.data._

import scala.collection.mutable

object Main {

  val DELAY_IN_MILLIS = 50

  def update[K](m: mutable.HashMap[K, Int], k: K, delta: Int):Unit = {
      val old_v: Int = m.getOrElse(k, 0)
      val new_v = old_v  + delta
      if (new_v > 0) {
        m.put(k, new_v )
      } else {
        m.remove(k)
      }
    }

  def hand_blockers() : Vector[Vector[Point]] = {
    val  res = new Vector[Vector[Point]](20)
    res.appended(Vector(Point(1, -1)))
    res.appended(Vector(Point(1,  0)))
    res.appended(Vector(Point(1,  1)))
    for (maxy <- 2 until 19) {
      val vec = new Vector[Point](maxy)
      for (y <- 1 until (maxy/2+1)) { vec.appended(Point(0, y )) }
      for (y <- (maxy+1)/2 until (maxy+1)) { vec.appended(Point(1, y )) }
      res.appended(vec)
    }
    res
  }

    // for hands with x == 1 and y == -1..18
    // indexed by y+1: y == -1 -> [0]; y == 0 -> [1], ...
    val HAND_BLOCKERS: Vector[Vector[Point]] = hand_blockers()

  def zone_char(zone: Zone) : Char = {
    if (zone.idx < Zone.UNDECIDED_ZONE.idx) {
      Char(65 + zone.idx)
    } else {
      '-'
    }
  }

  def print_level(level: Level, drones: Vector[Drone]):Unit = {
    val ymin = Math.max(0, Math.min(drones(0).pos.y - 25, level.height - 50))
    val ymax = Math.min(Math.max(drones(0).pos.y + 25, 50), level.height)
    val xmin = Math.max(0, Math.min(drones(0).pos.x - 50, level.width - 100))
    val xmax = Math.min(Math.max(drones(0).pos.x + 50, 100), level.width)

    for (y <- (ymin until ymax).reverse ) {
      for (x <- xmin until xmax) {
        val point = Point(x, y)

        val bg = if (drones.exists(d => d.hands.exists(h => {
          d.pos.x + h.x == x  && d.pos.y + h.y == y  && is_reaching(level, d.pos, h)
        }))) { """\x1B[48;5;202m""" }
        else if (level.bonuses.contains(point)) { """\x1B[48;5;33m\x1B[38;5;15m""" }
        else if (level.spawns.contains(point) )      { """\x1B[48;5;33m\x1B[38;5;15m""" }
        else if (level.beakons.contains(point))      { """\x1B[48;5;33m\x1B[38;5;15m""" }
        else {
          level.get_cell(x, y) match  {
           case Cell.EMPTY   =>  """\x1B[48;5;252m"""
           case Cell.BLOCKED =>  """\x1B[48;5;240m"""
           case Cell.WRAPPED =>  """\x1B[48;5;227m"""
          }
        }

        val ch = if val Some((idx, _)) = drones.zipWithIndex.find((d, idx) => d.hands.exists(h => {
          d.pos.x + h.x == x  && d.pos.y + h.y == y  && is_reaching(level, d.pos, h)
        })) { idx }
        else if val Some(bonus) = level.bonuses.get(point) {
          bonus match  {
            case Bonus.HAND     =>  "B"
            case Bonus.WHEELS   =>  "F"
            case Bonus.DRILL    =>  "L"
            case Bonus.TELEPORT =>  "R"
            case Bonus.CLONE    =>  "C"
          }
        } else if (level.spawns.contains(point)) {
          "X"
        } else if val Some(beakon_idx) = level.beakons.position(|&x| x == point) {
          beakon_idx
        } else {
          zone_char(level.get_zone(x, y))
        }
        print("""{}{}\x1B[0m""", bg, ch)
      }
      println()
    }
    println()
  }

  def max_wrapping(level: Level, drone: Drone, pos: Point) : Double = {
    if (level.get_zone(pos.x, pos.y) != drone.zone) { 0.0 }
    else if (level.bonuses.contains(pos)) { 100.0 }
    else {
      val wrapped = new mutable.HashSet[Point]()
      would_wrap(level, drone, pos,  wrapped)
      wrapped.map(p => Math.max(1.0, level.weights(level.grid_idx(p.x, p.y)))).sum
    }
  }

  def is_reaching(level: Level, from: Point, hand: Point) : Boolean = {
    hand.x == 0 || HAND_BLOCKERS(hand.y+1).forall(p=> level.walkable(from.x+p.x, from.y+p.y))
  }

  def would_wrap(level: Level, drone: Drone, pos: Point, wrapped:  mutable.HashSet[Point]):Unit = {
    for (hand <- drone.hands) {
      if (is_reaching(level, pos, hand)) {
        val hand_pos = Point(pos.x + hand.x, pos.y + hand.y)
        if (level.get_cell(hand_pos.x, hand_pos.y) == Cell.EMPTY) {
          wrapped.add(hand_pos)
        }
      }
    }
  }

  def step_move(level: Level, drone: Drone, from: Point, dx: Int, dy: Int, withWheels: Boolean, withDrill: Boolean, drilled: mutable.HashSet[Point]) : Option[(Point, mutable.HashSet[Point], mutable.HashSet[Point])] = {
      var to = Point(from.x + dx, from.y + dy)
      if (drilled.contains(to) || (withDrill && level.valid(to.x, to.y)) || level.walkable(to.x, to.y)) {
        val new_wrapped = new mutable.HashSet[Point]()
        val new_drilled = new mutable.HashSet[Point]()
        would_wrap(level, drone, to, new_wrapped)
        if (withDrill && !drilled.contains(to) && !level.walkable(to.x, to.y)) {
          new_drilled.add(to)
        }
        if (withWheels) {
          val to2 = Point(to.x + dx, to.y + dy)
          if (drilled.contains(to2) || (withDrill && level.valid(to2.x, to2.y)) || level.walkable(to2.x, to2.y)) {
            would_wrap(level, drone, to2, new_wrapped)
            if (withDrill && !drilled.contains(to2) && level.valid(to2.x, to2.y) && !level.walkable(to2.x, to2.y)) {
              new_drilled.add(to2)
            }
            to = to2
          }
        }
        Some((to, new_wrapped, new_drilled))
      } else {
        None
      }
    }

  def step_jump(level: Level, drone: Drone, beakon_idx: Int) : Option[(Point, mutable.HashSet[Point], mutable.HashSet[Point])] = {
      if (beakon_idx < level.beakons.length) {
        val to = level.beakons(beakon_idx)
        val new_wrapped = new mutable.HashSet[Point]()
        would_wrap(level, drone, to, new_wrapped)
        Some((to, new_wrapped, new mutable.HashSet[Point]()))
      } else {
        None
      }
    }

  def step(level: Level, drone: Drone, from: Point, action: Action, wheels: Boolean, drill: Boolean, drilled: mutable.HashSet[Point]) : Option[(Point, mutable.HashSet[Point], mutable.HashSet[Point])] = {
    action match  {
     case Action.LEFT  => step_move(level, drone, from, -1,  0, wheels, drill, drilled)
     case Action.RIGHT => step_move(level, drone, from,  1,  0, wheels, drill, drilled)
     case Action.UP    => step_move(level, drone, from,  0,  1, wheels, drill, drilled)
     case Action.DOWN  => step_move(level, drone, from,  0, -1, wheels, drill, drilled)
     case Action.JUMP0 => step_jump(level, drone, 0)
     case Action.JUMP1 => step_jump(level, drone, 1)
     case Action.JUMP2 => step_jump(level, drone, 2)
    }
  }

  def explore[F](level: Level, drone: Drone, rate: F) : Option[Vector[Action]] = {
      explore_impl(level, drone, rate).and_then(|(path, _, _)| Some(path))
    }

  def explore_impl[F](level: Level, drone: Drone, rate: F) : Option[(Vector[Action], Point, Double)] = {
      val seen = new mutable.HashSet[Point]()
      val queue = new Vector[Plan](100)
      val best: Option[(Vector[Action], Point, Double)] = None
      val max_len = 5
      queue.push_back(Plan{plan:    VecDeque(),
        pos:     drone.pos,
        wheels:  drone.wheels,
        drill:   drone.drill,
        drilled: mutable.HashSet.default() })

      loop {
        if val Some(Plan{plan, pos, wheels, drill, drilled}) = queue.pop_front() {
          if plan.len() >= max_len {
            if best.is_some() {
              break best
            } else {
              max_len += 5;
            }
          }

          val score = if (plan.is_empty()) { 0.0 } else { rate(level, drone, pos) / plan.len() }

          if best.is_some() {
            if score > best.as_ref().unwrap().2 { best = Some((plan.clone(), pos, score)); }
          } else {
            if score > 0. { best = Some((plan.clone(), pos, score)); }
          }

          for action in &[Action.LEFT, Action.RIGHT, Action.UP, Action.DOWN, Action.JUMP0, Action.JUMP1, Action.JUMP2] {
            if val Some((pos2, new_wrapped, new_drilled)) = step(level, drone, &pos, action, wheels > 0, drill > 0, &drilled) {
              if seen.contains(&pos2) { continue; }
              seen.insert(pos2);
              val mut plan2 = plan.clone();
              plan2.push_back(*action);
              val mut drilled2 = drilled.clone();
              for p in new_drilled { drilled2.insert(p); }
              queue.push_back(Plan{
                plan:    plan2,
                pos:     pos2,
                wheels:  if wheels > 1 { wheels - 1 } else { 0 },
                drill:   if drill > 1  { drill - 1 }  else { 0 },
                drilled: drilled2
              });
            }
          }
        } else { break best }
      }
    }

  def find_clone_score(level: Level, drone: Drone, pos: Point) : Double = {
    if (level.bonuses.get(pos) == Some(Bonus.CLONE)) { 1.0 } else { 0.0 }
  }

  def explore_clone(level: Level, drone: Drone, drone_idx: Int) : Option[Vector[Action]] = {
    if (drone_idx == 0
    && level.bonuses.values.exists(b => b == Bonus.CLONE)
    && level.collected.getOrElse(Bonus.CLONE, 0) == 0) {
      explore(level, drone, find_clone_score)
    } else {
      None
    }
  }

  def find_spawn_score(level: Level, drone: Drone, pos: Point) : Double = {
    if (level.spawns.contains(pos)) { 1.0} else { 0.0 }
  }

  def explore_spawn(level: Level, drone: Drone, drone_idx: Int) : Option[Vector[Action]] = {
    if (drone_idx == 0 && level.collected.getOrElse(Bonus.CLONE, 0) > 0) {
      explore(level, drone, find_spawn_score)
    } else {
      None
    }
  }

  def print_state(level: Level, drones: Vector[Drone]):Unit = {
    println("""\x1B[2J""")
    print_level(level, drones)
    println("Empty {:?} Collected {:?}", level.zones_empty, level.collected)
    for ((drone, i) <- drones.zipWithIndex) {
      val plan = drone.plan.map {
        case Action.UP => "↑"
        case Action.DOWN => "↓"
        case Action.LEFT => "←"
        case Action.RIGHT => "→"
        case Action.JUMP0 => "T0"
        case Action.JUMP1 => "T1"
        case Action.JUMP2 => "T2"
      }.toList
      println("{}: zone {} wheels {} drill {} at ({},{}) plan {}", i, zone_char(drone.zone), drone.wheels, drone.drill, drone.pos.x, drone.pos.y, plan.mkString(""))
    }
    Thread.sleep(DELAY_IN_MILLIS)
  }

  def solve_impl(level: Level, drones: Vector[Drone], interactive: Boolean) : String = {
    if (interactive) {
      println("""\x1B[?1049h""")
    }
    drones(0).wrap_bot(level)
    while (level.empty > 0) {
      if (interactive) {
        print_state(level, drones)
      }
      for (drone_idx <- 0 until drones.length) {
        if level.empty <= 0 { break; }

        val taken: Vector<_> = drones.map(|d| d.zone).collect();
        val mut drone = &mut drones[drone_idx];
        drone.collect(level);
        drone.wear_off();
        drone.choose_zone(&taken, level);

        if drone.plan.is_empty() {
          if val Some(clone) = drone.reduplicate(level) {
            drones.push(clone);
            continue;
          }

          if drone.activate_wheels(level)
          || drone.activate_drill(level)
          || drone.activate_hand(level)
          || drone.set_beakon(level)
          { continue; }

          if val Some(plan) = explore_clone(level, drone, drone_idx)
            .or_else(|| explore_spawn(level, drone, drone_idx))
            .or_else(|| explore(level, drone, max_wrapping)) {
              drone.plan = plan;
            }
        }

        if val Some(action) = drone.plan.pop_front() {
          drone.act(&action, level);
        } else if drone.wheels > 0 {
          drone.path += "Z";
        } else {
          throw new Exception("Nothing to do");
        }
      }
    }

    if (interactive) {
      print_state(level, drones)
      println("""\x1B[?1049l""")
    }

    val paths: Vector[String] = drones.map(_.path).toList
    paths.mkString("#")
  }

  def solve(filename: String, interactive: Boolean):Unit = {
    if val Ok(contents) = fs.read_to_string(filename) {
      val t_start = Instant.now()
      val (mut level, mut drones) = parser.parse_level(&contents)
      val solution = solve_impl(&mut level, &mut drones, interactive)
      val score = solution.split("#").map(|s| Regex(r"[A-Z]").unwrap().find_iter(s).count()).max().unwrap()
      println("{} \tscore {} \ttime {} ms", filename, score, t_start.elapsed().as_millis())

      val filename_sol = Regex(r"\.desc$").unwrap().replace(filename, ".sol")
      val mut file = File.create(filename_sol.into_owned()).unwrap()
      file.write_all(solution.as_bytes()).unwrap()
    } else {
      println("Failed to read {}", filename)
    }
  }

  def doall<T, F>(tasks: VecDeque<T>, threads: Int, f: F):Unit = {
      val m_queue = Arc(Mutex(tasks));
      val mut handles = vec![];

      for i in 0..threads {
      val m_queue = Arc.clone(&m_queue);
      val handle = thread.spawn(move || loop {
      val o_task = {
      val mut queue = m_queue.lock().unwrap();
      queue.pop_front()
    };
      if val Some(task) = o_task {
      f(task);
    } else {
      break;
    }
    });
      handles.push(handle);
    }

      for handle in handles {
      handle.join().unwrap();
    }
    }

  def main(args: Array[String]): Unit  = {
    val t_start = Instant.now();
    val args: Vector<String> = env.args().collect();
    val threads_re = Regex(r"--threads=([1-9][0-9]*)").unwrap();
    val mut interactive = false;
    val mut threads = 1;
    val mut filenames: VecDeque<String> = VecDeque();

    for arg in args[1..] {
      if arg == "--interactive" {
        interactive = true;
      } else if val Some(caps) = threads_re.captures(arg) {
        threads = caps.get(1).unwrap().as_str().parse.<Int>().unwrap() ;
      } else if arg.ends_with(".desc") {
        filenames.push_back(arg.clone());
      } else {
        throw new Exception("cargo run --release [--interactive] [--threads=N] <path/to/problem.desc>");
      }
    }

    val tasks = filenames.len();
    doall(filenames, threads, move |f| solve(&f, interactive));
    if tasks > 1 {
      println!("Finished {} tasks in {} ms", tasks, t_start.elapsed().as_millis());
    }
  }
}
