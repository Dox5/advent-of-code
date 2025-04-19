use crate::vector::Vector2D;
use std::io::{BufRead, BufReader, Read};
use std::fmt;

#[derive(Clone, PartialEq, Debug)]
pub struct PointMap<D> {
    map: Vec<D>,
    pub bound: Vector2D,
}

pub fn from_letter_grid(input: impl Read) -> PointMap<char> {
    let buf = BufReader::new(input);

    let mut map: Vec<char> = Vec::new();
    let mut width = 0i64;
    let mut height = 0i64;

    for line in buf.lines().map(std::io::Result::unwrap) {
        map.extend(line.chars());
        width = line.len() as i64;
        height += 1;
    }

    return PointMap {
        map,
        bound: Vector2D {
            x: width - 1,
            y: height - 1,
        },
    };
}

impl<D> PointMap<D> {
    pub fn with_bound(fill: &D, size: Vector2D) -> PointMap::<D>
        where
        D: Copy {
        // bounds are inclusive, so need 1 more for each row
        let cells = (size.x) * (size.y);
    
        PointMap::<D> {
            map: vec![*fill; cells.try_into().unwrap()],
            bound: size - Vector2D{x: 1, y: 1},
        }
    }

    pub fn width(&self) -> i64 {
        self.bound.x + 1
    }

    pub fn height(&self) -> i64 {
        self.bound.y + 1
    }

    pub fn contains(&self, p: Vector2D) -> bool {
        p.within(Vector2D { x: 0, y: 0 }, self.bound)
    }

    // Iterate all of the coordinates in the map
    // Order is determined by 'optmimum' memory access (based on storage order,
    // rows vs columns)
    pub fn coords(&self) -> impl Iterator<Item = Vector2D> {
        let width = self.width() as i64;
        let height = self.height() as i64;

        (0i64..height).flat_map(move |y| (0i64..width).map(move |x| Vector2D { x, y }))
    }

    pub fn items(&self) -> impl Iterator<Item = (Vector2D, &D)> {
        let width = self.width() as i64;
        let height = self.height() as i64;

        (0i64..height).flat_map(move |y| {
            (0i64..width).map(move |x| {
                let p = Vector2D { x, y };
                (p, &self[&p])
            })
        })
    }
}

impl<D> std::ops::Index<&Vector2D> for PointMap<D> {
    type Output = D;

    fn index(&self, index: &Vector2D) -> &Self::Output {
        if !self.contains(*index) {
            panic!("point access out of bounds")
        }

        let linear = index.x + index.y * self.width();
        return &self.map[linear as usize];
    }
}

impl<D> std::ops::IndexMut<&Vector2D> for PointMap<D> {
    fn index_mut(&mut self, index: &Vector2D) -> &mut Self::Output {
        if !self.contains(*index) {
            panic!("point access out of bounds")
        }

        let linear = index.x + index.y * self.width();

        return &mut self.map[linear as usize]
    }
}

impl fmt::Display for PointMap<char> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let height: usize = self.height().try_into().unwrap();
        let width: usize = self.width().try_into().unwrap();
        for y in 0usize..height {
            for x in 0usize..width {
                let linear = y * width + x;
                write!(f, "{}", self.map[linear])?;
            }

            write!(f, "\n")?;
        }

        Ok(())
    }

}
