use std::ops;

// A vector in 2D space
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Vector2D {
    pub x: i64,
    pub y: i64,
}

impl ops::Add for Vector2D {
    type Output = Vector2D;

    fn add(self, rhs: Vector2D) -> Vector2D {
        Vector2D{
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl ops::Sub for Vector2D {
    type Output = Vector2D;

    fn sub(self, rhs: Vector2D) -> Vector2D {
        Vector2D{
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

impl ops::Mul for Vector2D {
    type Output = Vector2D;

    fn mul(self, rhs: Vector2D) -> Vector2D {
        Vector2D{
            x: self.x * rhs.x,
            y: self.y * rhs.y,
        }
    }
}

// Multiply by scalar factor
impl ops::Mul<i64> for Vector2D {
    type Output = Vector2D;

    fn mul(self, rhs: i64) -> Vector2D {
        self * Vector2D{x: rhs, y: rhs}
    }
}

impl ops::Div for Vector2D {
    type Output = Vector2D;

    fn div(self, rhs: Vector2D) -> Vector2D {
        Vector2D{
            x: self.x / rhs.x,
            y: self.y / rhs.y,
        }
    }
}

// Divide by scalar factor
impl ops::Div<i64> for Vector2D {
    type Output = Vector2D;

    fn div(self, rhs: i64) -> Vector2D {
        self / Vector2D{x: rhs, y: rhs}
    }
}

impl ops::Rem for Vector2D {
    type Output = Vector2D;

    fn rem(self, rhs: Vector2D) -> Vector2D {
        Vector2D{
            x: self.x % rhs.x,
            y: self.y % rhs.y,
        }
    }
}


// Remainder by scalar factor
impl ops::Rem<i64> for Vector2D {
    type Output = Vector2D;

    fn rem(self, rhs: i64) -> Vector2D {
        self % Vector2D{x: rhs, y: rhs}
    }
}

impl std::fmt::Display for Vector2D {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
       write!(f, "({}, {})", self.x, self.y)
   }
}

impl Vector2D {
    pub fn rem_euclid_scalar(self, m: i64) -> Vector2D {
        Vector2D {
            x: self.x.rem_euclid(m),
            y: self.y.rem_euclid(m),
        }
    }

    pub fn rem_euclid(self, m: Vector2D) -> Vector2D {
        Vector2D {
            x: self.x.rem_euclid(m.x),
            y: self.y.rem_euclid(m.y),
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    fn test_add() {
        let v = Vector2D{x: 10, y: 15} + Vector2D{x: 3, y: 4};
        assert_eq!(Vector2D{x: 13, y: 19}, v);
    }

    #[rstest]
    fn test_sub() {
        let v = Vector2D{x: 10, y: 15} - Vector2D{x: 3, y: 4};
        assert_eq!(Vector2D{x: 7, y: 11}, v);
    }

    #[rstest]
    fn test_mul() {
        let v = Vector2D{x: 5, y: 11} * Vector2D{x: 6, y: 2};
        assert_eq!(Vector2D{x: 30, y: 22}, v);
    }

    #[rstest]
    fn test_mul_scalar() {
        let v = Vector2D{x: 5, y: 11} * 2i64;
        assert_eq!(Vector2D{x: 10, y: 22}, v);
    }

    #[rstest]
    fn test_div() {
        let v = Vector2D{x: 50, y: 64} / Vector2D{x: 10, y: 8};
        assert_eq!(Vector2D{x: 5, y: 8}, v);
    }

    #[rstest]
    fn test_div_scalar() {
        let v = Vector2D{x: 15, y: 21} / 3i64;
        assert_eq!(Vector2D{x: 5, y: 7}, v);
    }

    #[rstest]
    fn test_rem() {
        let v = Vector2D{x: 13, y: 27} % Vector2D{x: 3, y: 5};
        assert_eq!(Vector2D{x: 1, y: 2}, v);
    }

    #[rstest]
    fn test_rem_scalar() {
        let v = Vector2D{x: 21, y: 31} % 3i64;
        assert_eq!(Vector2D{x: 0, y: 1}, v);
    }

    #[rstest]
    fn test_display() {
        let v = Vector2D{x: 99, y: 0 };
        assert_eq!("(99, 0)", format!("{}", v));
    }
}
