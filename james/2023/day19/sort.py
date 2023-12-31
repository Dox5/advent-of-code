import networkx
import collections
import operator
import typing
import io
import doctest
import math


class Condition(typing.NamedTuple):
    attr: str
    comparison: collections.abc.Callable[[int, int], bool]
    val: int

    def test(self, part: dict[str, int]) -> bool:
        return self.comparison(part[self.attr], self.val)

    @classmethod
    def make(cls, condition: str) -> "Condition":
        attr = condition[0]
        comp = operator.lt if condition[1] == "<" else operator.gt
        val = int(condition[2:])
        return Condition(attr=attr, comparison=comp, val=val)


Workflows = dict[str, list[tuple[Condition, str]]]
def read_workflows(fh) -> Workflows:
    workflows = dict()

    for l in fh:
        l = l.strip()

        if l == "":
            # Blank line so we're done
            break

        name, flow_steps = l.split("{", 1)

        flows = []

        flow_steps = flow_steps.strip("}")
        for step in flow_steps.split(","):
            try:
                condition, target = step.split(":", 1)
                #print("Adding conditional edge between", name, target)
                flows.append((Condition.make(condition), target))
            except ValueError:
                target = step
                #print("Adding edge between", name, target)
                flows.append((None, target))

        workflows[name] = flows

    return workflows

def read_parts(fh) -> typing.Iterable[dict[str, int]]:
    for l in fh:
        l = l.strip().strip("{}")
        yield {
            attr: int(val) for attr, val in [q.split("=") for q in l.split(",")]
        }


def process_part(wf: Workflows, part) -> bool:
    n = "in"

    while n != "A" and n != "R":
        # Edges are reported in the order added (which is the order to try them
        # in)
        #print("-"*16)
        #print(n)
        for cond, nxt in wf[n]:
            #print("checking", nxt, cond)
            if cond is None:
                # Unconditional, take
                #print("unconditional")
                n = nxt
                break
            elif cond.test(part):
                #print("passed")
                # Conditional, and passed
                n = nxt
                break
            # Not taking this branch so try the next

    return n == "A"

example_1 = """px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
"""

def process_manifest(fh):
    """
    >>> process_manifest(io.StringIO(example_1))
    19114
    """
    wf = read_workflows(fh)
    total = 0

    for part in read_parts(fh):
        if process_part(wf, part):
            total += sum(part.values())

    return total


def find_ranges(fh):
    """
    >>> find_ranges(io.StringIO(example_1))
    167409079868000
    """
    wf = read_workflows(fh)

    ranges_that_work = set()

    full_range = range(1, 4001)

    to_explore = [("in", {"x": full_range, "m": full_range, "a": full_range, "s": full_range })]

    while to_explore != []:
        n, attrs = to_explore.pop()
        # Don't mess with this in case of duplicates
        attrs = attrs.copy()

        if n == "A":
            # Got to an accept state, this range of values are acceptable
            ranges_that_work.add(tuple(attrs.values()))
            # Nothing else to do
            continue
        elif n == "R":
            # Rejected so cull this flow
            continue

        for cond, nxt in wf[n]:
            # For each step, reduce the range as required and then queue the
            # follow on steps
            if cond is None:
                # We can take this without needing any adjustment
                to_explore.append((nxt, attrs))
                # We'll never take any more steps here though
                break
            else:
                r = attrs[cond.attr]
                if cond.comparison is operator.lt:
                    # The stop value can be at most cond.val (although it might
                    # already be smaller
                    taken_r = range(r.start, min(r.stop, cond.val))
                    # Not taken means that start >= cond.val (although it might
                    # already be)
                    not_taken_r = range(max(r.start, cond.val), r.stop)
                    
                else:
                    # The start value must be at least cond.val + 1 (or it might
                    # already be larger though)
                    taken_r = range(max(r.start, cond.val+1), r.stop)
                    # Not taken means that stop <= cond.val (although...)
                    not_taken_r = range(r.start, min(cond.val+1, r.stop))

                # Need to explore the 'taken' branch
                taken = attrs.copy()
                taken[cond.attr] = taken_r
                to_explore.append((nxt, taken))

                # Update attrs with the not-taken branch (because we'd have
                # matched, this eliminates some)
                attrs[cond.attr] = not_taken_r

    total = 0
    for w in ranges_that_work:
        total += math.prod(len(r) for r in w)

    return total









if __name__ == "__main__":
    doctest.testmod()

    find_ranges(io.StringIO(example_1))

    with open("input.txt") as fh:
        print("Manifest processing result", process_manifest(fh))

    with open("input.txt") as fh:
        print("Possible inputs", find_ranges(fh))
