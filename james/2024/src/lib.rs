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
