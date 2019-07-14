import data._

import scala.collection.mutable

object Main {
  val DELAY = 50


  

  def get_or[K](m: mutable.HashMap[K, Int], k: K, default: Int) : Int  = {
      if val Some(v) = m.get(k) { v } else { default }
    }

  def update<K>(m: &mut mutable.HashMap<K, Int>, k: K, delta: Int)
  where K: std.hash.Hash + Eq + std.marker.Sized
    = {
      val old_v: Int = get_or(m, &k, 0);
      val new_v = old_v  + delta;
      if new_v > 0 { m.insert(k, new_v ); }
      else { m.remove(&k); }
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
    if zone < UNDECIDED_ZONE { (65 + zone) as Char }
    else { '-' }
  }

  def print_level(level: &Level, drones: &[Drone]) = {
    val ymin = max(0, min(drones[0].pos.y - 25, level.height - 50));
    val ymax = min(max(drones[0].pos.y + 25, 50), level.height);
    val xmin = max(0, min(drones[0].pos.x - 50, level.width - 100));
    val xmax = min(max(drones[0].pos.x + 50, 100), level.width);

    for y in (ymin..ymax).rev() {
      for x in xmin..xmax {
        val point = Point(x, y);

        val bg = if drones.iter().find(|d| d.hands.iter().find(|h| {
          d.pos.x + h.x == x  && d.pos.y + h.y == y  && is_reaching(level, &d.pos, &h)
        }).is_some()).is_some() { "\x1B[48;5;202m" }
        else if level.bonuses.get(&point).is_some() { "\x1B[48;5;33m\x1B[38;5;15m" }
        else if level.spawns.contains(&point)       { "\x1B[48;5;33m\x1B[38;5;15m" }
        else if level.beakons.contains(&point)      { "\x1B[48;5;33m\x1B[38;5;15m" }
        else {
          match level.get_cell(x, y) {
            Cell.EMPTY   => { "\x1B[48;5;252m" }
            Cell.BLOCKED => { "\x1B[48;5;240m" }
            Cell.WRAPPED => { "\x1B[48;5;227m" }
          }
        };

        val Char = if val Some((idx, _)) = drones.iter().enumerate().find(|(idx, d)| d.hands.iter().find(|h| {
          d.pos.x + h.x == x  && d.pos.y + h.y == y  && is_reaching(level, &d.pos, &h)
        }).is_some()) { idx.to_string() }
        else if val Some(bonus) = level.bonuses.get(&point) {
          String.from(match bonus {
            Bonus.HAND     => { "B" }
            Bonus.WHEELS   => { "F" }
            Bonus.DRILL    => { "L" }
            Bonus.TELEPORT => { "R" }
            Bonus.CLONE    => { "C" }
          })
        } else if level.spawns.contains(&point) {
          String.from("X")
        } else if val Some(beakon_idx) = level.beakons.iter().position(|&x| x == point) {
          beakon_idx.to_string()
        } else {
          zone_char(level.get_zone(x, y)).to_string()
        };
        print!("{}{}\x1B[0m", bg, Char);
      }
      println!()
    }
    println!()
  }

  def max_wrapping(level: &Level, drone: &Drone, pos: &Point) : f64 = {
    if level.get_zone(pos.x, pos.y) != drone.zone { 0. }
    else if level.bonuses.contains_key(pos) { 100. }
    else {
      val mut wrapped: mutable.HashSet<Point> = mutable.HashSet.default();
      would_wrap(level, drone, pos, &mut wrapped);
      wrapped.iter().map(|p| 1.0_f64.max(level.weights[level.grid_idx(p.x, p.y)] as f64)).sum()
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

  def step_move(level: &Level, drone: &Drone, from: &Point, dx: Int, dy: Int, wheels: Boolean, drill: Boolean, drilled: &mutable.HashSet<Point>) : Option<(Point, mutable.HashSet<Point>, mutable.HashSet<Point>)>
    = {
      val mut to = Point(from.x + dx, from.y + dy);
      val mut new_wrapped = mutable.HashSet.default();
      val mut new_drilled = mutable.HashSet.default();
      if drilled.contains(&to) || (drill && level.valid(to.x, to.y)) || level.walkable(to.x, to.y) {
        would_wrap(level, drone, &to, &mut new_wrapped);
        if drill && !drilled.contains(&to) && !level.walkable(to.x, to.y) {
          new_drilled.insert(to);
        }
        if wheels {
          val to2 = Point(to.x + dx, to.y + dy);
          if drilled.contains(&to2) || (drill && level.valid(to2.x, to2.y)) || level.walkable(to2.x, to2.y) {
            would_wrap(level, drone, &to2, &mut new_wrapped);
            if drill && !drilled.contains(&to2) && level.valid(to2.x, to2.y) && !level.walkable(to2.x, to2.y) {
              new_drilled.insert(to2);
            }
            to = to2;
          }
        }
        Some((to, new_wrapped, new_drilled))
      } else {
        None
      }
    }

  def step_jump(level: &Level, drone: &Drone, beakon_idx: Int) : Option<(Point, mutable.HashSet<Point>, mutable.HashSet<Point>)>
    = {
      if beakon_idx < level.beakons.len() {
        val to = level.beakons[beakon_idx];
        val mut new_wrapped = mutable.HashSet.default();
        would_wrap(level, drone, &to, &mut new_wrapped);
        Some((to, new_wrapped, mutable.HashSet.default()))
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

  def explore<F>(level: &Level, drone: &Drone, rate: F) : Option<VecDeque<Action>>
    where F: Fn(&Level, &Drone, &Point) : f64
    = {
      explore_impl(level, drone, rate).and_then(|(path, _, _)| Some(path))
    }

  def explore_impl<F>(level: &Level, drone: &Drone, rate: F) : Option<(VecDeque<Action>, Point, f64)>
    where F: Fn(&Level, &Drone, &Point) : f64
    = {
      val mut seen: mutable.HashSet<Point> = mutable.HashSet.default();
      val mut queue: VecDeque<Plan> = VecDeque.with_capacity(100);
      val mut best: Option<(VecDeque<Action>, Point, f64)> = None;
      val mut max_len = 5;
      queue.push_back(Plan{plan:    VecDeque(),
        pos:     drone.pos,
        wheels:  drone.wheels,
        drill:   drone.drill,
        drilled: mutable.HashSet.default() });
      loop {
        if val Some(Plan{plan, pos, wheels, drill, drilled}) = queue.pop_front() {
          if plan.len() >= max_len {
            if best.is_some() {
              break best
            } else {
              max_len += 5;
            }
          }

          val score = if plan.is_empty() { 0. } else { rate(level, drone, &pos) / plan.len() as f64 };

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

  def find_clone_score(level: &Level, drone: &Drone, pos: &Point) : f64 = {
    if level.bonuses.get(pos) == Some(&Bonus.CLONE) { 1. } else { 0. }
  }

  def explore_clone(level: &Level, drone: &Drone, drone_idx: Int) : Option<VecDeque<Action>> = {
    if drone_idx == 0
    && level.bonuses.values().any(|&b| b == Bonus.CLONE)
    && get_or(&level.collected, &Bonus.CLONE, 0) == 0 {
      explore(level, drone, find_clone_score)
    } else {
      None
    }
  }

  def find_spawn_score(level: &Level, drone: &Drone, pos: &Point) : f64 = {
    if level.spawns.contains(pos) { 1.} else { 0. }
  }

  def explore_spawn(level: Level, drone: Drone, drone_idx: Int) : Option[Vector[Action]] = {
    if (drone_idx == 0 && get_or(level.collected, Bonus.CLONE, 0) > 0) {
      explore(level, drone, find_spawn_score)
    } else {
      None
    }
  }

  def print_state(level: &Level, drones: &[Drone]) = {
    println!("\x1B[2J");
    print_level(level, &drones);
    println!("Empty {:?} Collected {:?}", level.zones_empty, level.collected);
    for (i, drone) in drones.iter().enumerate() {
      val plan: Vector<_> = drone.plan.iter().map(|action| match action { Action.UP => "↑", Action.DOWN => "↓", Action.LEFT => "←", Action.RIGHT => "→", Action.JUMP0 => "T0", Action.JUMP1 => "T1", Action.JUMP2 => "T2", }).collect();
      println!("{}: zone {} wheels {} drill {} at ({},{}) plan {}", i, zone_char(drone.zone), drone.wheels, drone.drill, drone.pos.x, drone.pos.y, plan.join(""));
    }
    thread.sleep(time.Duration.from_millis(DELAY));
  }

  def solve_impl(level: &mut Level, drones: &mut Vector<Drone>, interactive: Boolean) : String = {
    if interactive { println!("\x1B[?1049h"); }
    drones[0].wrap_bot(level);
    while level.empty > 0 {
      if interactive { print_state(level, drones); }
      for drone_idx in 0..drones.len() {
        if level.empty <= 0 { break; }

        val taken: Vector<_> = drones.iter().map(|d| d.zone).collect();
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

    if interactive {
      print_state(level, drones);
      println!("\x1B[?1049l");
    }

    val paths: Vector<String> = drones.iter().map(|d| d.path.as_str()).collect();
    paths.join("#")
  }

  def solve(filename: String, interactive: Boolean) = {
    if val Ok(contents) = fs.read_to_string(filename) {
      val t_start = Instant.now();
      val (mut level, mut drones) = parser.parse_level(&contents);
      val solution = solve_impl(&mut level, &mut drones, interactive);
      val score = solution.split("#").map(|s| Regex(r"[A-Z]").unwrap().find_iter(s).count()).max().unwrap();
      println!("{} \tscore {} \ttime {} ms", filename, score, t_start.elapsed().as_millis());

      val filename_sol = Regex(r"\.desc$").unwrap().replace(filename, ".sol");
      val mut file = File.create(filename_sol.into_owned()).unwrap();
      file.write_all(solution.as_bytes()).unwrap();
    } else {
      println!("Failed to read {}", filename);
    }
  }

  def doall<T, F>(tasks: VecDeque<T>, threads: Int, f: F)
  where F: Fn(T),
  F: Copy + Send + 'static,
  T: Send + 'static
    = {
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

  def main() = {
    val t_start = Instant.now();
    val args: Vector<String> = env.args().collect();
    val threads_re = Regex(r"--threads=([1-9][0-9]*)").unwrap();
    val mut interactive = false;
    val mut threads = 1;
    val mut filenames: VecDeque<String> = VecDeque();

    for arg in args[1..].iter() {
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
