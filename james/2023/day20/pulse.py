import typing
import enum
from dataclasses import dataclass
import collections
from pprint import pprint
import io

@dataclass
class Mem:
    size: int
    base: int

class Pulse(enum.Enum):
    High = True
    Low = False

class S:

    def __init__(self):
        self._alloc = {}
        self._state = 0
        self._next_id = 0


    def alloc_conj(self, n_inputs: int) -> Mem:
        # Create N contiguous bits of storage
        base = self._next_id
        self._next_id += n_inputs
        return Mem(size=n_inputs, base=base)

    def alloc_flipflop(self) -> Mem:
        base = self._next_id
        self._next_id += 1
        return Mem(size=1, base=base)


    def perform_flipflop(self, ff: Mem, pulse: Pulse) -> typing.Optional[Pulse]:
        if pulse == Pulse.High:
            return None
        else:
            bit = self[ff]
            if bit == 0:
                # Off, will now switch on
                self[ff] = 1
                return Pulse.High
            else:
                # On will now switch off
                self[ff] = 0
                return Pulse.Low

    def perform_conj(self, c: Mem, port: int, pulse: Pulse) -> Pulse:
        state = self[c]
        assert port in range(0, c.size)

        # Update the bit
        if pulse == Pulse.High:
            # set a 1
            state |= 1 << port
        else:
            # set a 0
            # Given number of ports is low, don't need to worry about >64bits
            # used (like the state allocator)
            state &= ~(1 << port)

        # Remember the updated state
        self[c] = state

        # Determine what pulse to send
        if int.bit_count(state) == c.size:
            return Pulse.Low
        else:
            return Pulse.High

            

    def __getitem__(self, m: Mem) -> int:
        # Make a mask of the right size
        mask = (2 ** m.size) - 1
        return (self._state >> m.base) & mask


    def __setitem__(self, m: Mem, v: int):
        mask = (2 ** m.size) - 1

        inv_mask = (2 ** self._next_id) - 1
        inv_mask ^= mask << m.base


        v & mask

        # Force the bits back to being 0
        self._state &= inv_mask

        # Now set them based on v
        self._state |= v << m.base

    def memorialise(self):
        # State is an int so can be easily compared
        return self._state


def test_flipflop():
    state = S()
    # Put something else in the state
    o = state.alloc_conj(3)

    f = state.alloc_flipflop()

    assert state[f] == 0

    res = state.perform_flipflop(f, Pulse.High)
    assert res is None
    assert state[f] == 0

    res = state.perform_flipflop(f, Pulse.Low)
    assert res == Pulse.High
    assert state[f] == 1

    assert state[o] == 0

def test_conj():
    state = S()
    # Put something else in the state
    o = state.alloc_conj(3)

    c = state.alloc_conj(5)
    assert state[c] == 0

    # Low pulse on port 2
    res = state.perform_conj(c, 2, Pulse.Low)
    assert res == Pulse.High
    assert state[c] == 0

    # High pulse doesn't change result..
    for port in range(0, 5-1):
        res = state.perform_conj(c, port, Pulse.High)
        assert res == Pulse.High
        assert state[c] != 0

    # Until the last one
    res = state.perform_conj(c, 4, Pulse.High)
    assert res == Pulse.Low
    assert state[c] == 0x1F

    # If one is low again then we send a High pulse again
    res = state.perform_conj(c, 3, Pulse.Low)
    assert res == Pulse.High
    assert state[c] != 0

    assert state[o] == 0

class Module(enum.Enum):
    Broadcaster = enum.auto()
    FlipFlop = enum.auto()
    Conj = enum.auto()

class F(typing.NamedTuple):
    kind: Module
    mem: Mem
    outputs: list[str]
    inputs: list[str]

    def with_inputs(self, state: S, inputs: list[str]) -> "F":
        mem = state.alloc_conj(len(inputs)) if self.kind == Module.Conj else self.mem
        return self._replace(mem=mem, inputs=inputs)

def read(lines: list[str], state: S) -> dict[str, F]:
    mod_inputs = dict()
    modules = dict()

    for l in lines:
        f, arrow, to = l.split(" ", 2)
        assert arrow == "->"

        targets = [t.strip() for t in to.split(",")]
        mem = None

        if f == "broadcaster":
            kind = Module.Broadcaster
            name = f
        elif f[0] == "%":
            kind = Module.FlipFlop
            name = f[1:]
            mem = state.alloc_flipflop()
        elif f[0] == "&":
            kind = Module.Conj
            name = f[1:]
        else:
            raise RuntimeError("Unknown" + f)

        # Remember all of the inputs to all the modules
        for t in targets:
            mod_inputs.setdefault(t, []).append(name)

        assert name not in modules
        modules[name] = F(kind=kind, mem=mem, outputs=targets, inputs=None)

    return {
        name: mod.with_inputs(state, mod_inputs.get(name, [])) for name, mod in modules.items()
    }


def send_signal(state: S,
                wiring: dict[str, F],
                to: str,
                pulse: Pulse,
                verbose=False,
                source="button"):

    q = collections.deque()
    q.append((to, pulse, source))

    sent_low = 0
    sent_high = 0

    def send_to_outputs(mod: F, pulse: Pulse, source: str):
        for t in mod.outputs:
            q.append((t, pulse, source))


    while True:
        try:
            target, pulse, source = q.popleft()
        except IndexError:
            break

        if verbose:
            print(f"{source} -{pulse.name}-> {target}")

        if pulse == Pulse.High:
            sent_high += 1
        else:
            sent_low += 1

        try:
            mod = wiring[target]
        except KeyError:
            # Untyped module
            continue


        if mod.kind == Module.FlipFlop:
            res = state.perform_flipflop(mod.mem, pulse)
            if res is not None:
                send_to_outputs(mod, res, target)

        elif mod.kind == Module.Conj:
            port = mod.inputs.index(source)
            res = state.perform_conj(mod.mem, port, pulse)
            send_to_outputs(mod, res, target)

        elif mod.kind == Module.Broadcaster:
            send_to_outputs(mod, pulse, target)
        else:
            raise RuntimeError("Can't handle kind")

    return sent_high, sent_low


def part_a(fh, button_presses=1000) -> int:
    state = S()
    wiring = read([l.strip() for l in fh], state)

    seen = set()
    path = []

    for _ in range(0, button_presses):
        h, l = send_signal(state, wiring, "broadcaster", Pulse.Low)
        if state.memorialise() in seen:
            # Found a loop
            break

        seen.add(state.memorialise())
        path.append((state.memorialise(), h, l))

    def sum_h_l(handl) -> tuple[int, int]:
        th = 0
        tl = 0
        for state, h, l in handl:
            th += h
            tl += l
        return th, tl



    # Split path into lead-in and loop sections
    cycle_start = [p[0] for p in path].index(state.memorialise())
    leadin = path[:cycle_start]
    loop = path[cycle_start:]

    remaining_loops = len(path) - len(leadin)
    whole_repeats = int(remaining_loops / len(loop))
    extras = remaining_loops / len(loop)

    # Leadin
    lh, ll = sum_h_l(leadin)

    # Whole loops
    wh, wl = sum_h_l(loop)

    # Any remaining partial loop
    ph, pl = sum_h_l(loop[:extras])


    return (wh*whole_repeats + lh + ph) * (wl*whole_repeats + ll + pl)


example_1 = """broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a
"""


example_2 = """broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output
"""


if __name__ == "__main__":
    print("Example 1 part a", part_a(io.StringIO(example_1)))
    print("Example 2 part a", part_a(io.StringIO(example_2)))
    with open("input.txt") as fh:
        print("Part a", part_a(fh))

