use petgraph::algo::dijkstra;
use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::EdgeRef;
use petgraph::Direction;

use adventofcode::util::Point;

use std::collections::{HashMap, HashSet, VecDeque};
use std::io;
use std::io::BufRead;

type TopogPoint = (Point, i32);

type TopographicMap = DiGraph<TopogPoint, i32>;

fn load_topography(input: impl io::Read) -> TopographicMap {
    let mut topography: TopographicMap = DiGraph::default();

    let mut nodes = HashMap::<Point, NodeIndex>::new();

    for (maybe_line, y) in io::BufReader::new(input).lines().zip(0i64..) {
        let line = maybe_line.expect("Failed to read line");
        for (c, x) in line.chars().zip(0i64..) {
            let loc = Point { x, y };
            if c == '.' {
                // Unpassable, just skip entirely
                continue;
            }

            let height: i32 = c.to_digit(10).expect("Failed to parse height") as i32;

            let node_index = topography.add_node((loc, height));

            nodes.insert(loc, node_index);

            // Add edges for up and left
            for neighbour in &[Point { x, y: y - 1 }, Point { x: x - 1, y }] {
                if let Some(&neigh_index) = nodes.get(neighbour) {
                    let &(_, neighbour_height) = topography
                        .node_weight(neigh_index)
                        .expect("bug in graph building, node weight should exist");

                    // Add both directions
                    topography.add_edge(neigh_index, node_index, height - neighbour_height);
                    topography.add_edge(node_index, neigh_index, neighbour_height - height);
                }
            }
        }
    }

    return topography;
}

fn hiking_trail_map(topography: &TopographicMap) -> TopographicMap {
    topography.filter_map(
        |_, &node| Some(node),
        |_, &edge| if edge == 1 { Some(edge) } else { None },
    )
}

fn count_trails(topography: &TopographicMap) -> usize {
    let trail_map = hiking_trail_map(topography);

    let trail_ends: Vec<NodeIndex> = trail_map
        .node_indices()
        .filter(|&i| match topography[i] {
            (_, 9) => true,
            _ => false,
        })
        .collect();

    // Find each trail head and any paths to trail ends
    trail_map
        .node_indices()
        .filter(|&i| match trail_map[i] {
            (_, 0) => true,
            _ => false,
        })
        .map(|trail_head| {
            let shortest_paths = dijkstra(&trail_map, trail_head, None, |_| 1);

            trail_ends
                .iter()
                .filter(|trail_end| shortest_paths.contains_key(trail_end))
                .count()
        })
        .sum()
}

fn debug_print_graph<E>(graph: &DiGraph<TopogPoint, E>) {
    // assume 0,0 origin, workout the extent of the map
    let width = graph
        .node_weights()
        .map(|w| w.0.x)
        .max()
        .expect("graph should not be empty")
        + 1;
    let height = graph
        .node_weights()
        .map(|w| w.0.y)
        .max()
        .expect("graph should not be empty")
        + 1;

    let elevation: HashMap<_, _> = HashMap::from_iter(graph.node_weights().copied());

    for y in 0..height {
        for x in 0..width {
            match elevation.get(&Point { x, y }) {
                Some(e) => {
                    print!("{}", e);
                }
                None => {
                    print!(".")
                }
            }
        }

        println!("");
    }
}

fn prune_unreachable<N, E>(graph: &mut DiGraph<N, E>, start_nodes: impl Iterator<Item = NodeIndex>)
where
    N: std::fmt::Debug,
{
    // first build the set of reachable nodes from each of start_nodes
    let mut reachable: HashSet<NodeIndex> = HashSet::new();

    for from_node in start_nodes {
        let shortest_paths = dijkstra(&*graph, from_node, None, |_| 1);

        reachable.extend(shortest_paths.keys());
    }

    // Check each node and check that it is reachable. Start with the last one
    // such that the reindexing that happens on remove_node doesn't invalidate
    // the node indexes we've collected
    for node_index in graph.node_indices().rev() {
        if !reachable.contains(&node_index) {
            // Cannot reach this one, prune it
            graph.remove_node(node_index);
        }
    }
}

fn rate_trails(topography: &TopographicMap) -> usize {
    // First make a map of only the backwards steps

    let backwards = topography.filter_map(
        |_, &node| Some(node),
        |_, &edge| if edge == -1 { Some(edge) } else { None },
    );

    // Make a new map with edges that will contain the number of paths to trail
    // ends. This only has connections that head in the right direction.
    let mut ratings_map: DiGraph<TopogPoint, Option<usize>> = backwards.map(|_, &n| n, |_, _| None);

    // Must happen ahead of creating the visit queue as that stores node indices!
    // Prune anything that can't be reached from a 9 so that the rating algorithm
    // doesn't get stuck/fail to follow all paths
    let prepruned_trail_ends: Vec<_> = ratings_map
        .node_indices()
        .filter(|&i| match ratings_map[i] {
            (_, 9) => true,
            _ => false,
        })
        .collect();

    prune_unreachable(&mut ratings_map, prepruned_trail_ends.into_iter());

    //println!("Pruned map:");
    //debug_print_graph(&ratings_map);

    let mut visit_queue: VecDeque<_> = ratings_map
        .node_indices()
        .filter(|&i| match ratings_map[i] {
            (_, 9) => true,
            _ => false,
        })
        .collect();

    //println!("initial visit queue: {:?}", visit_queue);

    while let Some(node_index) = visit_queue.pop_front() {
        // Check compute outgoing count, if all incoming edges are known
        let maybe_rating: Option<usize> = ratings_map
            .edges_directed(node_index, Direction::Incoming)
            .map(|edge_ref| edge_ref.weight())
            .copied()
            .sum();

        // Not all known, don't update now (this node should be queued again via
        // another path)
        if let Some(rating) = maybe_rating {
            // Can compute outgoing connections now and then queue those nodes
            // for processing

            let outgoing_edges: Vec<(_, _)> = ratings_map
                .edges_directed(node_index, Direction::Outgoing)
                .map(|e| (e.id(), e.target()))
                .collect();

            // if the rating is 0 this is a trail_end and there is 1 way to get
            // to a trail end from here! (This makes the summing work :) )
            for (edge_id, _) in outgoing_edges.iter() {
                let maybe_existing_rating = &mut ratings_map[*edge_id];

                // If the rating is already correct then that is fine, otherwise
                // there is a bug (or the algorithm doesn't work!)
                if let Some(existing_rating) = maybe_existing_rating {
                    if *existing_rating != rating {
                        panic!("edge rating already set to a different value: {}, when trying to set to {}", existing_rating, rating);
                    }
                } else {
                    // Set the edge weight
                    let _ = maybe_existing_rating.insert(if rating > 0 { rating } else { 1 });
                }
            }

            // Add any outgoing edges to the visit queue
            visit_queue.extend(outgoing_edges.iter().map(|(_, target)| target));
        } /*else {
              // HACK: break if this is happening over and over
              visit_queue.push_back(node_index);
          }*/
    }

    // We think we are done.. now look for any node with remaining incoming paths
    // Yucky debug code.
    //ratings_map
    //    .node_indices()
    //    .filter(|&node_index| {
    //        ratings_map.edges_directed(node_index, Direction::Incoming).any(|edge_ref| edge_ref.weight().is_none())
    //    })
    //    .for_each(|node_index| {
    //        println!("Node {:?}: {:?} has missing connections", node_index, ratings_map.node_weight(node_index).unwrap());

    //        ratings_map
    //            .edges_directed(node_index, Direction::Incoming)
    //            .filter(|edge_ref| edge_ref.weight().is_none())
    //            .for_each(|edge_ref| {
    //                println!("\t from {:?}: {:?}", edge_ref.source(), ratings_map.node_weight(edge_ref.source()).unwrap());
    //            })

    //    });

    ratings_map
        .node_indices()
        .filter(|&i| match ratings_map[i] {
            (_, 0) => true,
            _ => false,
        })
        .map(|node_index| {
            ratings_map
                .edges_directed(node_index, Direction::Incoming)
                .map(|edge_ref| edge_ref.weight())
                .fold(0, |acc, maybe_rating| {
                    if let Some(r) = maybe_rating {
                        acc + r
                    } else {
                        acc
                    }
                })
        })
        .map(|total| total)
        .sum()
}

fn main() {
    let topog_file = std::fs::File::open("inputs/day10.txt").expect("failed to open input file");
    let topography = load_topography(topog_file);

    println!(
        "Sum of trailhead scores for map: {}",
        count_trails(&topography)
    );

    println!(
        "Sum of trailhead ratings for map: {}",
        rate_trails(&topography)
    );
}

#[cfg(test)]
mod day10_tests {
    use super::*;

    use rstest::rstest;

    #[rstest]
    fn test_simple_route() {
        let input = "\
        012\n\
        543\n\
        678\n\
        ..9\n\
        ";

        let topography = load_topography(input.as_bytes());
        assert_eq!(1, count_trails(&topography));
    }

    #[rstest]
    fn test_part_1_example_1() {
        let input = "\
        0123\n\
        1234\n\
        8765\n\
        9876\n\
        ";

        let topography = load_topography(input.as_bytes());
        assert_eq!(1, count_trails(&topography));
    }

    #[rstest]
    fn test_part_1_example_2() {
        let input = "\
        ...0...\n\
        ...1...\n\
        ...2...\n\
        6543456\n\
        7.....7\n\
        8.....8\n\
        9.....9\n\
        ";

        let topography = load_topography(input.as_bytes());
        assert_eq!(2, count_trails(&topography));
    }

    #[rstest]
    fn test_part_1_example_3() {
        let input = "\
        ..90..9\n\
        ...1.98\n\
        ...2..7\n\
        6543456\n\
        765.987\n\
        876....\n\
        987....\n\
        ";

        let topography = load_topography(input.as_bytes());
        assert_eq!(4, count_trails(&topography));
    }

    #[rstest]
    fn test_part_1_example_4() {
        let input = "\
        10..9..\n\
        2...8..\n\
        3...7..\n\
        4567654\n\
        ...8..3\n\
        ...9..2\n\
        .....01\n\
        ";

        let topography = load_topography(input.as_bytes());
        assert_eq!(3, count_trails(&topography));
    }

    #[rstest]
    fn test_part_1_example_5() {
        let input = "\
        89010123\n\
        78121874\n\
        87430965\n\
        96549874\n\
        45678903\n\
        32019012\n\
        01329801\n\
        10456732\n\
        ";

        let topography = load_topography(input.as_bytes());
        assert_eq!(36, count_trails(&topography));
    }

    #[rstest]
    fn test_part_2_example_1() {
        let input = "\
        .....0.\n\
        ..4321.\n\
        ..5..2.\n\
        ..6543.\n\
        ..7..4.\n\
        ..8765.\n\
        ..9....\n\
        ";

        let topography = load_topography(input.as_bytes());
        assert_eq!(3, rate_trails(&topography));
    }

    #[rstest]
    fn test_part_2_example_2() {
        let input = "\
        ..90..9\n\
        ...1.98\n\
        ...2..7\n\
        6543456\n\
        765.987\n\
        876....\n\
        987....\n\
        ";

        let topography = load_topography(input.as_bytes());
        assert_eq!(13, rate_trails(&topography));
    }

    #[rstest]
    fn test_part_2_example_3() {
        let input = "\
        012345\n\
        123456\n\
        234567\n\
        345678\n\
        4.6789\n\
        56789.\n\
        ";
        let topography = load_topography(input.as_bytes());
        assert_eq!(227, rate_trails(&topography));
    }

    #[rstest]
    fn test_part_2_example_4() {
        let input = "\
        89010123\n\
        78121874\n\
        87430965\n\
        96549874\n\
        45678903\n\
        32019012\n\
        01329801\n\
        10456732\n\
        ";

        let topography = load_topography(input.as_bytes());
        assert_eq!(81, rate_trails(&topography));
    }

    #[rstest]
    fn test_part_2_broken_example_1() {
        let input = "\
        ........\n\
        ........\n\
        ......65\n\
        ....9874\n\
        .......3\n\
        ......12\n\
        ......01\n\
        .......2\n\
        ";

        let topography = load_topography(input.as_bytes());
        assert_eq!(2, rate_trails(&topography));
    }

    #[rstest]
    fn test_part_2_broken_example_2() {
        let input = "\
        238\n\
        145\n\
        076\n\
        .89\n\
        ";

        let topography = load_topography(input.as_bytes());
        assert_eq!(1, rate_trails(&topography));
    }
}
