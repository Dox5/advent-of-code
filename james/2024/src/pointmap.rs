use crate::util::Point;
use std::io::{BufRead, BufReader, Read};

pub struct PointMap<D> {
    map: Vec<D>,
    pub bound: Point,
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
        bound: Point {
            x: width - 1,
            y: height - 1,
        },
    };
}

impl<D> PointMap<D> {
    pub fn width(&self) -> i64 {
        self.bound.x + 1
    }

    pub fn height(&self) -> i64 {
        self.bound.y + 1
    }

    pub fn contains(&self, p: Point) -> bool {
        p.within(Point { x: 0, y: 0 }, self.bound)
    }

    // Iterate all of the coordinates in the map
    // Order is determined by 'optmimum' memory access (based on storage order,
    // rows vs columns)
    pub fn coords(&self) -> impl Iterator<Item = Point> {
        let width = self.width() as i64;
        let height = self.height() as i64;

        (0i64..height).flat_map(move |y| (0i64..width).map(move |x| Point { x, y }))
    }
}

impl<D> std::ops::Index<&Point> for PointMap<D> {
    type Output = D;

    fn index(&self, index: &Point) -> &Self::Output {
        if !self.contains(*index) {
            panic!("point access out of bounds")
        }

        let linear = index.x + index.y * self.width();
        return &self.map[linear as usize];
    }
}
