use adventofcode::vector::Vector2D;
use std::collections::{HashMap, HashSet};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct Robot {
    pos: Vector2D,
    vel: Vector2D,
}

#[derive(Debug, PartialEq, Eq)]
enum ParseRobotError {
    UnexpectedToken{found: String, expected: String},
    FailedIntParse,
    UnexpectedEnd,
    TrailingTokens,
}

impl std::str::FromStr for Robot {
    type Err = ParseRobotError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut tokens = s.split(&['=', ',', ' ']);

        // position first, expect a p
        let expect_p = tokens.next().ok_or(ParseRobotError::UnexpectedEnd)?;
        if expect_p != "p" {
            return Err(ParseRobotError::UnexpectedToken{found: expect_p.to_owned(), expected: "p".to_owned()});
        }

        // Grab the two coordinates
        let pos_x: i64 = tokens.next().ok_or(ParseRobotError::UnexpectedEnd)?.parse().map_err(|_| ParseRobotError::FailedIntParse)?;
        let pos_y: i64 = tokens.next().ok_or(ParseRobotError::UnexpectedEnd)?.parse().map_err(|_| ParseRobotError::FailedIntParse)?;

        // position first, expect a v
        let expect_v = tokens.next().ok_or(ParseRobotError::UnexpectedEnd)?;
        if expect_v != "v" {
            return Err(ParseRobotError::UnexpectedToken{found: expect_v.to_owned(), expected: "v".to_owned()});
        }

        // Grab the two coordinates
        let vel_x: i64 = tokens.next().ok_or(ParseRobotError::UnexpectedEnd)?.parse().map_err(|_| ParseRobotError::FailedIntParse)?;
        let vel_y: i64 = tokens.next().ok_or(ParseRobotError::UnexpectedEnd)?.parse().map_err(|_| ParseRobotError::FailedIntParse)?;

        if tokens.next().is_some() {
            return Err(ParseRobotError::TrailingTokens);
        }

        Ok(Robot{
            pos: Vector2D{x: pos_x, y: pos_y},
            vel: Vector2D{x: vel_x, y: vel_y},
        })
    }
}

impl Robot {
    fn simulate(self, seconds: i64, world_size: Vector2D) -> Robot {
        Robot {
            pos: (self.pos + self.vel * seconds).rem_euclid(world_size),
            vel: self.vel.clone(),
        }
    }
}

struct World {
    world_size: Vector2D,
    population: Vec<Robot>,
}

impl World {
    fn with_robots(world_size: Vector2D, robots: impl Iterator<Item=Robot>) -> World {
        World {
            world_size,
            population: robots.collect(),
        }
    }

    fn simulate(&mut self, time_seconds: i64) {
        let new_pop = self.iter_population().map(|r| r.simulate(time_seconds, self.world_size)).collect();
        self.population = new_pop;
    }

    fn iter_population(&self) -> impl Iterator<Item=&Robot> {
        return self.population.iter();
    }

    fn count_by_quadrant(&self) -> [usize; 4] {
        // World size is always odd...
        let mid_point = (self.world_size - Vector2D{x: 1, y: 1}) / 2i64; 
    
        let mut counts: [usize; 4] = [0, 0, 0, 0];
    
        for r in self.iter_population() {
            if r.pos.x == mid_point.x || r.pos.y == mid_point.y {
                // Don't include things on the midpoints
                continue;
            }
    
            let i = if r.pos.x < mid_point.x { 0 } else { 1 };
            let j = if r.pos.y < mid_point.y { 0 } else { 2 };
    
            counts[i+j] += 1;
        }
    
    
        return counts;
    }
}

impl std::fmt::Display for World {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let locations: HashSet<Vector2D> = self.population.iter().map(|r|r.pos).collect();

        for y in 0..self.world_size.y {
            for x in 0..self.world_size.x {
                let p = Vector2D{x, y};
                if locations.contains(&p) {
                    write!(f, "#")?; 
                } else {
                    write!(f, ".")?;
                }
            }
            write!(f, "\n")?;
        }

        Ok(())
    }
}


fn show_robots<'a>(robots: impl Iterator<Item=&'a Robot>, world_size: Vector2D) {
    let mut locations: HashMap<Vector2D, i64> = HashMap::new();

    for r in robots {
        *locations.entry(r.pos).or_default() += 1;
    }

    for y in 0..world_size.y {
        for x in 0..world_size.x {
            let pos = Vector2D{x, y};

            match locations.get(&pos) {
                Some(&count) => {
                    if count > 9 {
                        print!("*");
                    } else {
                        print!("{}", count);
                    }
                },
                None => {
                    print!(".");
                }
            }
        }
        println!();
    }

}

fn part1() {
    let input = std::fs::read_to_string("inputs/day14.txt").expect("failed to load input file");

    let mut world = World::with_robots(
        Vector2D{x: 101, y: 103},
        input.split('\n').filter(|s| !s.is_empty()).map(|s| s.parse().unwrap()),
    );

    world.simulate(100);

    let safety_factor: usize = world.count_by_quadrant().iter().product();

    println!("Safety factor after 100 seconds: {}", safety_factor);
}

fn part2() {
    let input = std::fs::read_to_string("inputs/day14.txt").expect("failed to load input file");

    let mut world = World::with_robots(
        Vector2D{x: 101, y: 103},
        input.split('\n').filter(|s| !s.is_empty()).map(|s| s.parse().unwrap()),
    );

    // .. try to find the tree
    for s in 0..10000 {
        println!("Seconds: {}", s);
        println!("{}", world);
        world.simulate(1);
    }
}

fn main() {
    part1();
    part2();
}

#[cfg(test)]
mod day14_tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    fn test_parse_robot() {
        let expected_robot = Robot {
            pos: Vector2D { x: 0, y: 4 },
            vel: Vector2D { x: 3, y: -3 },
        };

        let input = "p=0,4 v=3,-3";
        let parsed_robot: Robot = input.parse().expect("robot should be valid");

        assert_eq!(expected_robot, parsed_robot);
    }

    #[rstest]
    fn test_simulate() {
        let expected_robot = Robot {
            pos: Vector2D { x: 0, y: 1 },
            vel: Vector2D { x: 1, y: 2 },
        };

        let robot = Robot {
            pos: Vector2D { x: 1, y: 0},
            vel: Vector2D { x: 1, y: 2},
        }.simulate(2, Vector2D{x: 3, y: 3});

        assert_eq!(expected_robot, robot);
    }

    #[rstest]
    fn test_part_1_example() {
        let input = "\
        p=0,4 v=3,-3\n\
        p=6,3 v=-1,-3\n\
        p=10,3 v=-1,2\n\
        p=2,0 v=2,-1\n\
        p=0,0 v=1,3\n\
        p=3,0 v=-2,-2\n\
        p=7,6 v=-1,-3\n\
        p=3,0 v=-1,-2\n\
        p=9,3 v=2,3\n\
        p=7,3 v=-1,2\n\
        p=2,4 v=2,-3\n\
        p=9,5 v=-3,-3\n\
        ";

        let mut world = World::with_robots(Vector2D{x: 11, y: 7}, input.split('\n').filter(|s| !s.is_empty()).map(|s| s.parse().unwrap()));
        world.simulate(100);

        show_robots(world.iter_population(), world.world_size);

        let safety_factor: usize = world.count_by_quadrant().iter().product();

        assert_eq!(12, safety_factor);
    }
}
