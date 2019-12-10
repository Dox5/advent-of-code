import day10
import pytest

def test_ast_count():
    lines = ['.#..#','.....','#####','....#','...##']
    (field,x,y) = day10.genField(lines)
    v = day10.genVectors(max(x,y))
    (ba, num) = day10.part1(field,v)
    assert ba == 3+4j
    assert num == 8

def test_destoyer():
    lines = ['.#....#####...#..','##...##.#####..##','##...#...#.#####.','..#.....#...###..','..#.#.....#....##']
    (field,x,y) = day10.genField(lines)
    v = day10.genVectors(max(x,y))
    (ba, num) = day10.part1(field,v)
    print(ba)
    la = day10.part2(field,v,ba,36)
    assert la == (14+3j)
