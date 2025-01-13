pub mod pointmap;
pub mod vector;

pub mod util {
    use std::fmt;
    use std::io;
    use std::ops;

    #[derive(PartialEq, Eq, Hash, Debug, Copy, Clone)]
    pub struct Point {
        pub x: i64,
        pub y: i64,
    }

    impl ops::Add for Point {
        type Output = Self;
        fn add(self, rhs: Self) -> Self::Output {
            Point {
                x: self.x + rhs.x,
                y: self.y + rhs.y,
            }
        }
    }

    impl ops::Sub for Point {
        type Output = Self;
        fn sub(self, rhs: Self) -> Self::Output {
            Point {
                x: self.x - rhs.x,
                y: self.y - rhs.y,
            }
        }
    }

    impl fmt::Display for Point {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "({}, {})", self.x, self.y)
        }
    }

    // TODO can support unpacking syntax instead?
    impl TryFrom<Point> for (f64, f64) {
        type Error = &'static str;

        fn try_from(value: Point) -> Result<Self, Self::Error> {
            Ok((value.x as f64, value.y as f64))
        }
    }

    impl Point {
        pub fn reverse(&self) -> Self {
            Point {
                x: -self.x,
                y: -self.y,
            }
        }

        pub fn within(&self, top_left: Point, bottom_right: Point) -> bool {
            self.x >= top_left.x
                && self.x <= bottom_right.x
                && self.y >= top_left.y
                && self.y <= bottom_right.y
        }

        pub fn cardinal_neighbours(&self) -> [Point; 4] {
            [
                self.clone() + Point { x: 0, y: -1 },
                self.clone() + Point { x: 1, y: 0 },
                self.clone() + Point { x: 0, y: 1 },
                self.clone() + Point { x: -1, y: 0 },
            ]
        }

        // Return neighbours in clockwise order, starting from north. Inclusive
        // of diagnols
        pub fn neighbours(&self) -> [Point; 8] {
            [
                self.clone() + Point { x: 0, y: -1 },  // N
                self.clone() + Point { x: 1, y: -1 },  // NE
                self.clone() + Point { x: 1, y: 0 },   // E
                self.clone() + Point { x: 1, y: 1 },   // SE
                self.clone() + Point { x: 0, y: 1 },   // S
                self.clone() + Point { x: -1, y: 1 },  // SW
                self.clone() + Point { x: -1, y: 0 },  // W
                self.clone() + Point { x: -1, y: -1 }, // NW
            ]
        }
    }

    pub fn get_world_extent<R>(input: R) -> Point
    where
        R: io::BufRead,
    {
        let mut width = 0i64;
        let mut height = 0i64;

        for l in input.lines().map(io::Result::unwrap) {
            width = l.len() as i64;
            height += 1;
        }

        Point {
            x: width - 1,
            y: height - 1,
        }
    }
}
