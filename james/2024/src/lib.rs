pub mod pointmap;
pub mod vector;

pub mod util {
    use crate::vector::Vector2D;
    use std::io;

    pub fn get_world_extent<R>(input: R) -> Vector2D
    where
        R: io::BufRead,
    {
        let mut width = 0i64;
        let mut height = 0i64;

        for l in input.lines().map(io::Result::unwrap) {
            width = l.len() as i64;
            height += 1;
        }

        Vector2D {
            x: width - 1,
            y: height - 1,
        }
    }
}
