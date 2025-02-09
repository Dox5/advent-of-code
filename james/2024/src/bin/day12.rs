use std::collections::{HashSet, VecDeque};
use std::fs::File;
use std::ops::Add;

use adventofcode::pointmap::{from_letter_grid, PointMap};
use adventofcode::vector::Vector2D;

type PlotMap = PointMap<char>;

#[derive(Eq, PartialEq, Debug)]
struct Region {
    legend: char,
    plots: HashSet<Vector2D>,
}

impl Region {
    fn area(&self) -> i64 {
        self.plots.len() as i64
    }

    fn perimeter(&self) -> i64 {
        let mut perimeter = 0i64;
        for &plot in self.plots.iter() {
            // Each permieter adds up to 4 new edges
            perimeter += 4;

            // take away two for each of up and left if they are part of this plot
            // doing so will account for both tiles adding perimeter for this tile
            let up = plot.add(Vector2D { x: 0, y: -1 });
            if self.plots.contains(&up) {
                perimeter -= 2;
            }

            let left = plot.add(Vector2D { x: -1, y: 0 });
            if self.plots.contains(&left) {
                perimeter -= 2;
            }
        }

        return perimeter;
    }

    fn edges(&self) -> i64 {
        let mut corner_count = 0i64;

        for p in self.plots.iter() {
            let neigh = p.neighbours().map(|n| self.plots.contains(&n));

            // Needs to match pattern true, false. true from above to add an
            // edge
            let internal_corner_indices = &[[0, 1, 2], [2, 3, 4], [4, 5, 6], [6, 7, 0]];

            for corner in internal_corner_indices {
                let present = corner.map(|i| neigh[i]);

                match present {
                    [true, false, true] => {
                        corner_count += 1;
                    }
                    _ => (),
                }
            }

            let external_corner_indices = &[[0, 2], [2, 4], [4, 6], [6, 0]];

            for corner in external_corner_indices {
                // Only a corner if two adjcent cardinal tiles are empty
                let is_corner = corner.into_iter().all(|&i| !neigh[i]);
                if is_corner {
                    corner_count += 1;
                }
            }
        }

        return corner_count;
    }
}

// Given loc within a region, expand out the region to contain all plots
fn explore_region(map: &PlotMap, loc: Vector2D) -> Region {
    let mut plots: HashSet<Vector2D> = HashSet::new();

    let mut to_explore: VecDeque<Vector2D> = VecDeque::from([loc]);
    let plot_kind = map[&loc];

    while let Some(p) = to_explore.pop_front() {
        if plots.contains(&p) {
            // Seen this plot before
            continue;
        }

        if !map.contains(p) {
            // Walked outside the bounds of the map
            continue;
        }

        if map[&p] != plot_kind {
            // Not part of this region
            continue;
        }

        // Belongs to this region
        plots.insert(p.clone());

        // Queue neighbours to explore
        to_explore.extend(p.cardinal_neighbours());
    }

    // Now we have the whole region, calcuate the perimeter

    return Region {
        legend: plot_kind,
        plots,
    };
}

fn map_regions(map: &PlotMap) -> Vec<Region> {
    // walk from top left to bottom right and expand any region that is hit (and
    // not has not been seen before). Calculate the perimeter as we go

    let mut regions: Vec<Region> = Vec::with_capacity(32);

    let mut seen: HashSet<Vector2D> = HashSet::new();

    for p in map.coords() {
        // Already processed this point
        if seen.contains(&p) {
            continue;
        }

        // Discovered a new region, map it
        regions.push(explore_region(&map, p));

        // Mark all these plots as seen
        seen.extend(
            regions
                .last()
                .expect("added new region cannot be empty")
                .plots
                .iter(),
        );
    }

    return regions;
}

fn calculate_fence_cost(map: &PlotMap) -> i64 {
    map_regions(map)
        .iter()
        .map(|r| r.perimeter() * r.area())
        .sum()
}

fn calculate_fence_cost_with_discount(map: &PlotMap) -> i64 {
    map_regions(map).iter().map(|r| r.area() * r.edges()).sum()
}

fn read_map(path: &str) -> PlotMap {
    let fh = File::open(path).expect("failed to open input file");
    return from_letter_grid(fh);
}

fn main() {
    let map = read_map("inputs/day12.txt");

    println!(
        "Cost of fencing for all plots: {}",
        calculate_fence_cost(&map)
    );
    println!(
        "Cost of fencing for all plots (with discount): {}",
        calculate_fence_cost_with_discount(&map)
    );
}

#[cfg(test)]
mod day12_tests {
    use super::*;
    use rstest::rstest;

    fn assert_region_eq(expected: &Region, actual: &Region) {
        assert_eq!(expected.legend, actual.legend);
        assert_eq!(expected.area(), actual.area());

        let expected_plots: HashSet<Vector2D> = HashSet::from_iter(expected.plots.iter().cloned());
        let actual_plots: HashSet<Vector2D> = HashSet::from_iter(actual.plots.iter().cloned());
        assert_eq!(expected_plots, actual_plots);
    }

    #[rstest]
    fn test_explore_region_1() {
        let map = from_letter_grid(
            "\
            AAAC\n\
            BBAA\n\
            AADD\n\
        "
            .as_bytes(),
        );

        let region = explore_region(&map, Vector2D { x: 0, y: 1 });
        let expected_region = Region {
            legend: 'B',
            plots: HashSet::from_iter([Vector2D { x: 0, y: 1 }, Vector2D { x: 1, y: 1 }]),
        };

        assert_eq!(6, region.perimeter());

        assert_region_eq(&expected_region, &region);
    }

    #[rstest]
    fn test_explore_region_2() {
        let map = from_letter_grid(
            "\
            AAAC\n\
            BBAA\n\
            AADD\n\
        "
            .as_bytes(),
        );

        let region = explore_region(&map, Vector2D { x: 1, y: 0 });
        let expected_region = Region {
            legend: 'A',
            plots: HashSet::from_iter([
                Vector2D { x: 0, y: 0 },
                Vector2D { x: 1, y: 0 },
                Vector2D { x: 2, y: 0 },
                Vector2D { x: 2, y: 1 },
                Vector2D { x: 3, y: 1 },
            ]),
        };

        assert_region_eq(&expected_region, &region);
        assert_eq!(12, region.perimeter());
    }

    #[rstest]
    fn test_example_1_part_1() {
        let map = from_letter_grid(
            "\
            RRRRIICCFF\n\
            RRRRIICCCF\n\
            VVRRRCCFFF\n\
            VVRCCCJFFF\n\
            VVVVCJJCFE\n\
            VVIVCCJJEE\n\
            VVIIICJJEE\n\
            MIIIIIJJEE\n\
            MIIISIJEEE\n\
            MMMISSJEEE\n\
        "
            .as_bytes(),
        );

        let fence_cost = calculate_fence_cost(&map);
        assert_eq!(1930, fence_cost);
    }

    #[rstest]
    fn test_edge_count() {
        let map = from_letter_grid(
            "\
            .S.S.\n\
            SSSSS\n\
        "
            .as_bytes(),
        );

        let region = explore_region(&map, Vector2D { x: 1, y: 0 });
        assert_eq!(12, region.edges());
    }

    #[rstest]
    fn test_example_4_part_2() {
        let map = from_letter_grid(
            "\
            RRRRIICCFF\n\
            RRRRIICCCF\n\
            VVRRRCCFFF\n\
            VVRCCCJFFF\n\
            VVVVCJJCFE\n\
            VVIVCCJJEE\n\
            VVIIICJJEE\n\
            MIIIIIJJEE\n\
            MIIISIJEEE\n\
            MMMISSJEEE\n\
        "
            .as_bytes(),
        );

        let fence_cost = calculate_fence_cost_with_discount(&map);
        assert_eq!(1206, fence_cost);
    }

    #[rstest]
    fn test_broken_edges() {
        let map = from_letter_grid(
            "\
            RRRR.\n\
            RRRR.\n\
            ..RRR\n\
            ..R..\n\
        "
            .as_bytes(),
        );

        let region = explore_region(&map, Vector2D { x: 0, y: 0 });

        assert_eq!(10, region.edges());
    }
}
