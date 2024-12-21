use std::io;

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
enum Entry {
    File { id: usize, len: usize },
    FreeSpace { len: usize },
}

fn ascii_to_digit(c: u8) -> Option<u8> {
    if 0x30 <= c && c <= 0x39 {
        Some(c - 0x30)
    } else {
        None
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
struct FileSystem {
    state: Vec<Entry>,
}

impl FileSystem {
    fn with_state(mut state: Vec<Entry>) -> FileSystem {
        // Ensure there is always free space at the end
        match state.last() {
            // Already got freespace at end
            Some(Entry::FreeSpace { len: _ }) => (),
            // Need to add it otherwise
            _ => state.push(Entry::FreeSpace { len: 0 }),
        }

        FileSystem { state }
    }

    fn from_dense(input: &mut impl io::Read) -> FileSystem {
        let mut buf: [u8; 2] = [0, 0];

        let mut id = 0usize;
        let mut entries = Vec::<Entry>::new();

        loop {
            let n_read = input.read(&mut buf).unwrap();

            if n_read == 0 {
                break;
            }

            let file_len = ascii_to_digit(buf[0]).expect("Failed to parse digit (file)");
            entries.push(Entry::File {
                id,
                len: file_len as usize,
            });

            id += 1;

            if n_read > 1 && buf[1] != 0x0A {
                let free_space = ascii_to_digit(buf[1]).expect("Failed to parse digit (freespace)");
                if free_space != 0 {
                    entries.push(Entry::FreeSpace {
                        len: free_space as usize,
                    });
                }
            }
        }

        FileSystem::with_state(entries)
    }

    // Find the right most fileblock and return it's index. Start searching from
    // 'start'. Returns None if no fileblock was found
    fn find_filler(&self, start: usize) -> Option<usize> {
        for i in (start..self.state.len()).rev() {
            if let Entry::File { id: _, len: _ } = self.state[i] {
                return Some(i);
            }
        }

        None
    }

    // Update freespace after `after`, either add to an existing freespace (if one exists)
    // or insert a new one if not
    fn add_freespace_at(&mut self, i: usize, len: usize) {
        if i > self.state.len() {
            panic!("can't add freespace past the end");
        } else if i == self.state.len() {
            self.state.push(Entry::FreeSpace { len: len });
        } else {
            match self.state[i] {
                Entry::File { id: _, len: _ } => {
                    // insert new freespace element
                    self.state.insert(i, Entry::FreeSpace { len });
                }
                Entry::FreeSpace { len: existing } => {
                    // Add to this freespace
                    self.state[i] = Entry::FreeSpace {
                        len: existing + len,
                    };
                }
            }
        }
    }

    fn combine(&mut self) {
        let mut write_i = 0;

        use Entry::{File, FreeSpace};

        for read_i in 1..self.state.len() {
            match (self.state[write_i], self.state[read_i]) {
                (
                    File {
                        id: id_a,
                        len: len_a,
                    },
                    file_b @ File {
                        id: id_b,
                        len: len_b,
                    },
                ) => {
                    if id_a == id_b {
                        // Combine same ID files
                        self.state[write_i] = File {
                            id: id_a,
                            len: len_a + len_b,
                        };
                    } else {
                        // Different IDs so write the new one
                        write_i += 1;
                        self.state[write_i] = file_b;
                    }
                }

                (FreeSpace { len: len_a }, FreeSpace { len: len_b }) => {
                    self.state[write_i] = FreeSpace { len: len_a + len_b };
                }

                (_, elm) => {
                    // Different types so write and move on
                    write_i += 1;
                    self.state[write_i] = elm;
                }
            }
        }

        self.state.truncate(write_i + 1);
    }

    fn compact(&mut self) {
        use Entry::{File, FreeSpace};

        let mut i = 0usize;

        while i < self.state.len() {
            match self.state[i] {
                FreeSpace { len: free_space } => {
                    // Locate file to fill this space with, which must appear
                    // after the block we are looking at
                    let maybe_filler = self.find_filler(i + 1);

                    match maybe_filler {
                        Some(f_index) => {
                            if let File { id, len } = self.state[f_index] {
                                if len > free_space {
                                    // Take part of this file for this space
                                    self.state[i] = File {
                                        id,
                                        len: free_space,
                                    };
                                    // Update the length of the file we took from
                                    self.state[f_index] = File {
                                        id,
                                        len: len - free_space,
                                    };
                                    // Add freespace for the bits we just moved
                                    self.add_freespace_at(f_index + 1, free_space);
                                } else if len == free_space {
                                    // Easy, wholesale move
                                    self.state.swap(i, f_index);
                                } else if len < free_space {
                                    // take whole file but insert remaining freespace
                                    self.state[i] = self.state[f_index];

                                    // Replace filler with space
                                    self.state[f_index] = Entry::FreeSpace { len };

                                    // Insert remaining space after this point
                                    self.add_freespace_at(i + 1, free_space - len);
                                }
                            } else {
                                panic!("bug: should have got a File from find_filler but got FreeSpace at index {}", f_index);
                            }
                        }
                        // No more filler blocks so must be done
                        None => {
                            break;
                        }
                    }
                }
                File { id: _, len: _ } => {
                    // Just a file, move on
                    i += 1;
                }
            }
        }

        self.combine();
    }

    fn compact_contiguous(&mut self) {
        let mut i = self.state.len() - 1;

        let mut seen = std::collections::HashSet::<usize>::new();

        // Don't need to check i == 0 because there can't be anything to the
        // left of it
        while i > 0 {
            match self.state[i] {
                Entry::File { id, len } => {
                    // Find first freespace large enough in the map
                    if !seen.contains(&id) {
                        seen.insert(id);
                        let maybe_freespace = self
                            .state
                            .iter()
                            .take(i)
                            .zip(0usize..)
                            .filter_map(|(&elm, idx)| match elm {
                                Entry::FreeSpace { len } => Some((len, idx)),
                                _ => None,
                            })
                            .find(|(free_len, idx)| *free_len >= len);

                        if let Some((free_len, free_i)) = maybe_freespace {
                            // Should be large enough for this file

                            // Plonk this file in to the space
                            self.state[free_i] = self.state[i];

                            // Mark the file we just moved as freespace
                            self.state[i] = Entry::FreeSpace { len };

                            // Might have some more freespace to put here too
                            if free_len > len {
                                self.add_freespace_at(free_i + 1, free_len - len);

                                // This changes what element i refers to because
                                // there are more elements now.
                                // Counteract that by incrementing (and thus `i`
                                // still refers to the same element as it did
                                // before)
                                i += 1;
                            }
                        }
                    }
                }

                _ => (),
            }

            i -= 1;
        }

        self.combine();
    }

    fn checksum(&self) -> usize {
        self.state
            .iter()
            .fold((0usize, 0usize), |(chk, pos), elm| match elm {
                Entry::File { id, len } => {
                    let s: usize = (pos..pos + len).map(|p| p * id).sum();
                    (chk + s, pos + len)
                }

                Entry::FreeSpace { len } => (chk, pos + len),
            })
            .0
    }
}

fn main() {
    let input = std::fs::read_to_string("inputs/day09.txt").expect("failed to read input file");
    let mut fs = FileSystem::from_dense(&mut input.as_bytes());
    let mut fs_compact = fs.clone();
    fs_compact.compact();

    println!("Checksum after compaction: {}", fs_compact.checksum());

    fs.compact_contiguous();
    println!("Checksum after contiguous compaction: {}", fs.checksum());
}

#[cfg(test)]
mod day09_tests {
    use super::*;
    use rstest::rstest;

    fn file(id: usize, len: usize) -> Entry {
        Entry::File { id, len }
    }

    fn free(len: usize) -> Entry {
        Entry::FreeSpace { len }
    }

    #[rstest]
    #[case(vec![file(0, 3)], vec![file(0, 3)])]
    #[case(vec![file(0, 3), free(1), file(1, 1)], vec![file(0, 3), file(1, 1), free(1)])]
    #[case(vec![file(0, 3), free(1), file(1, 1), free(1), file(2, 2)], vec![file(0, 3), file(2, 1), file(1, 1), file(2, 1), free(2)])]
    #[case(vec![file(0, 3), free(1), file(1, 2)], vec![file(0, 3), file(1, 2), free(1)])]
    #[case(vec![file(0, 3), free(1), file(1, 1), file(2, 2)], vec![file(0, 3), file(2, 1), file(1, 1), file(2, 1), free(1)])]
    fn test_compact_fs(#[case] input: Vec<Entry>, #[case] expected: Vec<Entry>) {
        let mut fs = FileSystem::with_state(input);
        fs.compact();
        assert_eq!(FileSystem::with_state(expected), fs);
    }

    #[rstest]
    #[case(vec![file(0, 3)], vec![file(0, 3)])]
    #[case(vec![file(0, 3), free(1), file(1, 1)], vec![file(0, 3), file(1, 1), free(1)])]
    #[case(vec![file(0, 3), free(2), file(1, 1), file(2, 2)], vec![file(0, 3), file(2, 2), file(1, 1), free(2)])]
    #[case(vec![file(0, 3), free(3), file(1, 1), file(2, 2)], vec![file(0, 3), file(2, 2), file(1, 1), free(3)])]
    fn test_compact_contiguous_fs(#[case] input: Vec<Entry>, #[case] expected: Vec<Entry>) {
        let mut fs = FileSystem::with_state(input);
        fs.compact();
        assert_eq!(FileSystem::with_state(expected), fs);
    }

    #[rstest]
    #[case("", vec![])]
    #[case("20", vec![file(0, 2)])]
    #[case("12", vec![file(0, 1), free(2)])]
    #[case("2043", vec![file(0, 2), file(1, 4), free(3)])]
    #[case("2", vec![file(0, 2)])]
    fn test_parse_filesystem_state(#[case] input: &str, #[case] expected: Vec<Entry>) {
        assert_eq!(
            FileSystem::with_state(expected),
            FileSystem::from_dense(&mut input.as_bytes())
        );
    }

    #[rstest]
    fn test_example_part1() {
        let input = "2333133121414131402";
        let mut fs = FileSystem::from_dense(&mut input.as_bytes());
        fs.compact();

        assert_eq!(1928, fs.checksum());
    }

    #[rstest]
    fn test_example_part2() {
        let input = "2333133121414131402";
        let mut fs = FileSystem::from_dense(&mut input.as_bytes());
        fs.compact_contiguous();
        println!("{:?}", fs);

        assert_eq!(2858, fs.checksum());
    }
}
