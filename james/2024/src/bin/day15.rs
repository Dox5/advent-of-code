use adventofcode::pointmap::{from_letter_grid, PointMap};
use adventofcode::vector::Vector2D;
use std::fs;

fn parse_puzzle(input: &str) -> (PointMap<char>, String) {
    let parts: Vec<_> = input.split("\n\n").collect();

    if parts.len() != 2 {
        panic!("Didn't get exactly 2 parts");
    }

    return (
        from_letter_grid(parts[0].as_bytes()),
        parts[1].chars().filter(|c| *c != '\n').collect(),
    );
}

fn dir_to_vector(c: char) -> Vector2D {
    match c {
        '^' => Vector2D { x: 0, y: -1 },
        '>' => Vector2D { x: 1, y: 0 },
        'v' => Vector2D { x: 0, y: 1 },
        '<' => Vector2D { x: -1, y: 0 },
        _ => {
            panic!("what is this character");
        }
    }
}

// return the number of boxes the robot is trying to push, (ie the chain of boxes
// before a space. Returns None if the robot is stuck
fn n_boxes_to_move(map: &PointMap<char>, start: Vector2D, dir: Vector2D) -> Option<i64> {
    for steps in 1i64.. {
        let consider = start + dir * steps;

        // Walked off the end of the map
        if !map.contains(consider) {
            panic!("Should've stopped before escaping the map!")
        }

        match map[&consider] {
            'O' => {/*continue searching*/},
            '.' => {
                //  Found a gap at 'steps' steps, yay
                return Some(steps - 1);
            },
            '#' => {
                // Hit a wall before find a gap, so we are blocked
                return None;
            },
            c => {
                panic!("Unknown map character: {}", c);
            },
        };
    }
    panic!("should not be able to exit this look like this!");
}

fn find_robot(map: &PointMap<char>) -> Option<Vector2D> {
    map.items()
        .find_map(|(loc, &v)| match v {
            '@' => Some(loc),
            _ => None,
        })
}

fn run_moves(mut map: PointMap<char>, moves: &str) -> PointMap<char> {
    // First off find that dang robot
    let mut robot = find_robot(&map).expect("map should contain a robot");

    //println!("INITIAL:\n{}\n", &map);

    for m in moves.chars() {
        let dir = dir_to_vector(m);

        // Look for the first blank spot in this direction to see if we can move
        // if we can't find one, then we couldn't move
        if let Some(n_boxes) = n_boxes_to_move(&map, robot, dir) {
            // Shuffle the boxes to make a space for the robotif needed, if the
            // next space is empty (pushing no boxes) then can just let the robot
            // move
            if n_boxes > 0 {
                let empty = robot + dir * (n_boxes + 1);

                // Move a single box to perfom the shuffle, don' need to clear
                // up because the very next thing we do is move the robot to
                // cover the box we 'moved'
                map[&empty] = 'O';
            }

            // Move the robot
            map[&(robot + dir)] = '@';
            map[&robot] = '.';
            robot = robot + dir;
        }
        //println!("MOVE: {}\n{}\n", m, &map);
    }

    return map;
}

// Take a narrow map and make it wide ( O -> [])
fn wide_map(map: &PointMap<char>) -> PointMap<char> {
    let mut wide_map = PointMap::<char>::with_bound(&'.', Vector2D{x: map.width() * 2, y: map.height()});

    // Find and non empty spaces and add them to the wide map
    for (pos, c) in map.items().filter(|(_, &c)| c != '.') {
        // Create the related positions in the wide map
        let original_pos = pos * Vector2D{x:2, y:1};
        let created_pos = original_pos + Vector2D{x: 1, y: 0};

        match c {
            '#' => {
                wide_map[&original_pos] = '#';
                wide_map[&created_pos]  = '#';
            },

            '@' => {
                // Robot is same width
                wide_map[&original_pos] = '@';
            },

            'O' => {
                wide_map[&original_pos] = '[';
                wide_map[&created_pos]  = ']';
            },
            unknown => {
                panic!("Cannot handle character {}", unknown);
            }
        }
    }

    return wide_map;
}

// Performance improvements:
// - RLE the instructions so we can consider multiple moves at once
// - Remember if we were blocked which way we just tried to move to skip easy blocked moves

// Walk the binary tree of boxes we're potentially pushing and see if they will
// actually move
fn can_push(map: &PointMap<char>, robot: Vector2D, dir: Vector2D) -> bool {
    false
}

// Boxes must be able to move when this is called, performs the actual moving
fn push_boxes(map: &mut PointMap<char>, robot: Vector2D, dir: Vector2D) {
}

fn run_wide_moves(mut map: PointMap<char>, moves: &str) -> PointMap<char> {
    // First off find that dang robot
    let mut robot = find_robot(&map).expect("map should contain a robot");

    for m in moves.chars() {
        let dir = dir_to_vector(m);

        // Can we push all the connected boxes?
        if can_push(&map, robot, dir) {
            // yup, so do it all again but actually move them
            push_boxes(&mut map, robot, dir);

            // now move the robot where it wanted to go
            map[&robot] = '.';
            map[&(robot+dir)] = '@'
        }
    }

    return map;
}

fn checksum(map: &PointMap<char>) -> i64 {
    map.items()
        .filter_map(|(l, c)| match c {
            'O' => Some(l.y * 100 + l.x),
            _ => None,
        })
        .sum()
}

fn main() {
    let input = fs::read_to_string("inputs/day15.txt").expect("failed to read input file");

    let (map, moves) = parse_puzzle(&input);

    let completed = run_moves(map, &moves);
    println!("Sum of GPS coordinates: {}", checksum(&completed));
}

#[cfg(test)]
mod day15_test {
    use super::*;
    use rstest::rstest;

    #[rstest]
    fn test_part_1_small() {
        let input = "\
            ########\n\
            #..O.O.#\n\
            ##@.O..#\n\
            #...O..#\n\
            #.#.O..#\n\
            #...O..#\n\
            #......#\n\
            ########\n\
            \n\
            <^^>>>vv\n\
            <v>>v<<\n\
        ";

        let (map, moves) = parse_puzzle(&input);
        assert_eq!(moves, "<^^>>>vv<v>>v<<");

        let completed = run_moves(map, &moves);

        assert_eq!(2028, checksum(&completed));
    }

    #[rstest]
    fn test_part_1_big() {
        let input = "\
            ##########\n\
            #..O..O.O#\n\
            #......O.#\n\
            #.OO..O.O#\n\
            #..O@..O.#\n\
            #O#..O...#\n\
            #O..O..O.#\n\
            #.OO.O.OO#\n\
            #....O...#\n\
            ##########\n\
            \n\
            <vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\n\
            vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n\
            ><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n\
            <<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n\
            ^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n\
            ^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n\
            >^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n\
            <><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n\
            ^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\n\
            v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^\n\
        ";

        let (map, moves) = parse_puzzle(&input);
        let completed = run_moves(map, &moves);

        assert_eq!(10092, checksum(&completed));
    }

    #[rstest]
    fn test_part_2_widen_map() {
        let input = "\
            #######\n\
            #...#.#\n\
            #.....#\n\
            #..OO@#\n\
            #..O..#\n\
            #.....#\n\
            #######\n\
            \n\
            <vv<<^^<<^^\n\
        ";

        let expected = from_letter_grid("\
            ##############\n\
            ##......##..##\n\
            ##..........##\n\
            ##....[][]@.##\n\
            ##....[]....##\n\
            ##..........##\n\
            ##############\n\
        ".as_bytes());

        let (map, _) = parse_puzzle(&input);

        let widened = wide_map(&map);

        println!("expected\n{}", &expected);
        println!("actual\n{}", &widened);

        assert_eq!(expected, widened);
    }
}
