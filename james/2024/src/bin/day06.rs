use std::collections::{HashMap, HashSet};

use adventofcode::vector::Vector2D;

fn parse_obsticals(input: &str) -> HashSet<Vector2D> {
    input
        .split('\n')
        .zip(0i64..)
        .map(|(line, y)| {
            line.chars().zip(0i64..).filter_map(move |(c, x)| {
                if c == '#' {
                    Some(Vector2D { x, y })
                } else {
                    None
                }
            })
        })
        .flatten()
        .collect()
}

fn find_guard(input: &str) -> Vector2D {
    input
        .split('\n')
        .zip(0i64..)
        .find_map(|(line, y)| {
            line.chars().zip(0i64..).find_map(|(c, x)| {
                if c == '^' {
                    Some(Vector2D { x, y })
                } else {
                    None
                }
            })
        })
        .expect("no guard (^) found")
}

fn get_world_extend(input: &str) -> Vector2D {
    Vector2D {
        x: input
            .split('\n')
            .find_map(|l| Some(l.len()))
            .expect("failed to get width")
            .try_into()
            .expect("map too wide"),
        y: input
            .split('\n')
            .filter(|l| !l.is_empty())
            .count()
            .try_into()
            .expect("map too tall"),
    }
}

fn in_bounds(world_extent: Vector2D, pos: Vector2D) -> bool {
    pos.x >= 0 && pos.y >= 0 && pos.x < world_extent.x && pos.y < world_extent.y
}

fn turn_right_90(vel: Vector2D) -> Vector2D {
    match vel {
        Vector2D { x: 1, y: 0 } => Vector2D { x: 0, y: 1 },
        Vector2D { x: 0, y: 1 } => Vector2D { x: -1, y: 0 },
        Vector2D { x: -1, y: 0 } => Vector2D { x: 0, y: -1 },
        Vector2D { x: 0, y: -1 } => Vector2D { x: 1, y: 0 },
        _ => panic!("Not an expected velocity vector: {:?}", vel),
    }
}

fn find_guard_path(
    world_extent: Vector2D,
    obsticals: &HashSet<Vector2D>,
    guard: Vector2D,
) -> HashSet<(Vector2D, Vector2D)> {
    let mut pos = guard.clone();
    let mut velocity = Vector2D { x: 0, y: -1 };

    let mut visited = HashSet::<(Vector2D, Vector2D)>::new();

    loop {
        // Visited current position
        visited.insert((pos, velocity));

        let next_pos = pos + velocity;

        if obsticals.contains(&next_pos) {
            velocity = turn_right_90(velocity);
        } else if !in_bounds(world_extent, next_pos) {
            // Done, this would walk out
            break;
        } else {
            pos = next_pos;
        }
    }

    return visited;
}

fn find_guard_visited(
    world_extent: Vector2D,
    obsticals: &HashSet<Vector2D>,
    guard: Vector2D,
) -> HashSet<Vector2D> {
    HashSet::from_iter(
        find_guard_path(world_extent, &obsticals, guard)
            .iter()
            .map(|(p, _)| p)
            .copied(),
    )
}

fn blocks_that_create_loops(
    world_extent: Vector2D,
    obsticals: &HashSet<Vector2D>,
    guard: Vector2D,
) -> HashSet<Vector2D> {
    let mut visited = HashSet::<(Vector2D, Vector2D)>::new();
    let mut pos = guard.clone();
    let mut velocity = Vector2D { x: 0, y: -1 };

    let mut stepped_on = HashSet::<Vector2D>::new();
    let mut looping_obstical = HashSet::<Vector2D>::new();

    loop {
        // Visited current position
        visited.insert((pos, velocity));
        stepped_on.insert(pos);

        let next_pos = pos + velocity;

        if !in_bounds(world_extent, next_pos) {
            // Done, this would walk out
            break;
        } else if obsticals.contains(&next_pos) {
            velocity = turn_right_90(velocity);
        } else {
            // Pretend there is an obstical here, turn and see if we get to a
            // loop
            //let mut l_visited = visited.clone();
            let mut l_visited = HashSet::<(Vector2D, Vector2D)>::new();
            let mut l_pos = pos;
            let mut l_velocity = velocity;

            // Only consider spaces that haven't been stepped on (because this
            // would cut the loop short and we would never make it to our spot!)
            if !stepped_on.contains(&next_pos) {
                loop {
                    if l_visited.contains(&(l_pos, l_velocity)) {
                        looping_obstical.insert(next_pos);
                        break;
                    }

                    l_visited.insert((l_pos, l_velocity));
                    let l_next_pos = l_pos + l_velocity;

                    if !in_bounds(world_extent, l_next_pos) {
                        // No loop, walked out of bounds
                        break;
                    } else if obsticals.contains(&l_next_pos) || l_next_pos == next_pos {
                        // bumped into an obstical (including the new one)
                        l_velocity = turn_right_90(l_velocity);
                    } else {
                        // Now mark this one as visited (so the test above doesn't always pass!)
                        l_pos = l_next_pos;
                    }
                }
            }

            pos = next_pos;
        }
    }

    // Not allowed to use the guards space so remove it
    looping_obstical.remove(&guard);

    return looping_obstical;
}

fn debug_draw(
    world_extent: Vector2D,
    obsticals: &HashSet<Vector2D>,
    guard: Vector2D,
    obstical: Vector2D,
    l_visited: &HashSet<(Vector2D, Vector2D)>,
) {
    let mut updated = obsticals.clone();
    updated.insert(obstical);

    let broken: HashSet<Vector2D> = HashSet::from_iter(l_visited.iter().map(|(a, _)| a).copied());

    let mut visited = HashMap::<Vector2D, HashSet<Vector2D>>::new();
    let mut pos = guard.clone();
    let mut velocity = Vector2D { x: 0, y: -1 };

    let mut no_loop = false;

    loop {
        if let Some(true) = visited.get(&pos).map(|s| s.contains(&velocity)) {
            // Found loop
            break;
        }

        // Visited current position
        visited
            .entry(pos)
            .or_insert_with(|| HashSet::new())
            .insert(velocity);

        let next_pos = pos + velocity;

        if !in_bounds(world_extent, next_pos) {
            // Done, this would walk out
            no_loop = true;
            break;
        } else if updated.contains(&next_pos) {
            velocity = turn_right_90(velocity);
        } else {
            pos = next_pos;
        }
    }

    for y in 0..world_extent.y {
        for x in 0..world_extent.x {
            let p = Vector2D { x, y };
            if p == obstical {
                print!("@");
            } else if p == pos {
                // Where the loop happened
                print!("$");
            } else if p == guard {
                print!("^");
            } else if obsticals.contains(&p) {
                print!("#");
            } else if let Some(vel) = visited.get(&p) {
                let h = vel.contains(&Vector2D { x: 1, y: 0 })
                    || vel.contains(&Vector2D { x: -1, y: 0 });
                let v = vel.contains(&Vector2D { x: 0, y: 1 })
                    || vel.contains(&Vector2D { x: 0, y: -1 });
                if h && v {
                    print!("+");
                } else if h {
                    print!("-");
                } else {
                    print!("|");
                }
            } else if broken.contains(&p) {
                print!("?");
            } else {
                print!(".");
            }
        }
        println!("")
    }

    if no_loop {
        panic!("didn't loop??");
    }
}

fn main() {
    let input = std::fs::read_to_string("inputs/day06.txt").expect("failed to read input");

    let obsticals = parse_obsticals(&input);
    let guard_start = find_guard(&input);
    let world_extent = get_world_extend(&input);

    let visited = find_guard_visited(world_extent, &obsticals, guard_start);
    println!("Guard visits {} positions", visited.len());

    let loop_obsticals = blocks_that_create_loops(world_extent, &obsticals, guard_start);
    println!(
        "Places where an obstruction would cause a loop: {}",
        loop_obsticals.len()
    );
}

#[cfg(test)]
mod day06_tests {
    use super::*;
    use rstest::rstest;

    const EXAMPLE_INPUT: &'static str = "\
    ....#.....\n\
    .........#\n\
    ..........\n\
    ..#.......\n\
    .......#..\n\
    ..........\n\
    .#..^.....\n\
    ........#.\n\
    #.........\n\
    ......#...\n\
    ";

    #[rstest]
    fn test_parse_obsticals() {
        let locations = parse_obsticals(
            "\
            #..\n\
            .#.\n\
            ..#\n\
        ",
        );

        let expected = HashSet::from_iter(vec![
            Vector2D { x: 0, y: 0 },
            Vector2D { x: 1, y: 1 },
            Vector2D { x: 2, y: 2 },
        ]);

        assert_eq!(expected, locations);
    }

    #[rstest]
    fn test_find_guard() {
        let guard_at = find_guard(
            "\
            ..#\n\
            ..^\n\
            ..#\n\
        ",
        );

        assert_eq!(Vector2D { x: 2, y: 1 }, guard_at);
    }

    #[rstest]
    fn test_world_extent() {
        let world_extent = get_world_extend(
            "\
            ...\n\
            ...\n\
            ...\n\
            ...\n\
        ",
        );

        assert_eq!(Vector2D { x: 3, y: 4 }, world_extent);
    }

    #[rstest]
    fn test_example_part_1() {
        let obsticals = parse_obsticals(&EXAMPLE_INPUT);
        let guard_start = find_guard(&EXAMPLE_INPUT);
        let world_extent = get_world_extend(&EXAMPLE_INPUT);

        let visited = find_guard_visited(world_extent, &obsticals, guard_start);
        assert_eq!(41, visited.len());
    }

    #[rstest]
    fn test_example_part_2() {
        let obsticals = parse_obsticals(&EXAMPLE_INPUT);
        let guard_start = find_guard(&EXAMPLE_INPUT);
        let world_extent = get_world_extend(&EXAMPLE_INPUT);

        let blocks = blocks_that_create_loops(world_extent, &obsticals, guard_start);
        assert_eq!(6, blocks.len());
    }

    #[rstest]
    fn test_loops_hit_tail() {
        let input = "\
          #...\n\
          ....\n\
          ^#..\n\
          ...#\n\
          ....\n\
          #...\n\
          ..#.\n\
        ";
        let obsticals = parse_obsticals(&input);
        let guard_start = find_guard(&input);
        let world_extent = get_world_extend(&input);

        let blocks = blocks_that_create_loops(world_extent, &obsticals, guard_start);
        assert_eq!(1, blocks.len());
    }
}
