import networkx
import io
from matplotlib import pyplot as plt


def build_graph(fh):
    g = networkx.Graph()

    for l in fh:
        a, bs = l.split(":")
        a = a.strip()

        for b in bs.split():
            g.add_edge(a, b)

    return g


def dump_graph(g, name):
    plt.figure(figsize=(16, 16), dpi=160)
    networkx.draw(g, with_labels=True)
    plt.savefig(name)

def find_islands(g, n=3):
    islands = list(networkx.k_edge_components(g, n+1))
    return islands


def part_a(g):
    a, b = find_islands(g, n=3)
    return len(a) * len(b)




example_graph = build_graph(io.StringIO("""jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr
"""))


if __name__ == "__main__":
    print("(example) Island size product", part_a(example_graph))

    with open("input.txt") as fh:
        g = build_graph(fh)

    print("Island size product", part_a(g))
