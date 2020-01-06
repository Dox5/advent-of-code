#!/usr/bin/python
import fileinput
import collections
import sys

def newdeck(cards):
    cards.reverse()
    return cards

def deckinc(cards, incr):
    ncards = len(cards)
    newlist = [None] * ncards
    pos = 0
    for i in range(ncards):
        newlist[pos] = cards[0]
        cards.rotate(-1)
        pos = (pos+incr) % ncards
    cards = collections.deque(newlist)
    return cards

def dealdeck(cards,mode):
    if mode == "stack":
        cards = newdeck(cards)
    else:
        cards = deckinc(cards,int(mode))

    return cards

def cut(cards, amount):
    a = -int(amount)
    cards.rotate(a)
    return cards

def buildMoves(moves):
    shuffle = []
    for m in moves:
        x = m.split(" ")
        if x[0] == "deal":
            shuffle.append((dealdeck, x[-1]))
        else:
            shuffle.append((cut, x[-1]))
    return shuffle

def shuffle(cards, moves):
    for m in moves:
        cards = m[0](cards, m[1])
    return cards

def main():
    txtmoves = [x.strip() for x in fileinput.input(sys.argv[1])]
    if len(sys.argv) == 3:
        ncards = int(sys.argv[2])
    else:
        ncards = 10007
    cards = collections.deque(range(ncards))
    moves = buildMoves(txtmoves)

    cards = shuffle(cards, moves)
    print("Part 1 Ans:", cards.index(2019))

    # we have 100 operations in script....
    #    ncards = 119315717514047 # known prime
    #    repeat = 101741582076661 # known prime
    # Primality of numbers must be an important thing.

    # The cards seem to be coming out in a pattern
    # Take first card, call it start, then there is a diff between it and the second card, called diff
    # Sequence seems to be start + n*diff mod ncards
    # So getting any card in a specific deck is start + n*diff mod ncards if we knew start and diff.
    # The factory fresh cards come with a start of 0 and a diff of 1.

    # For my input of part 1, after one shuffle the output is 6405 5671 4937.....
    # Difference isn't negative though, it's just mod ncards (10007 in that case)
    # So we've got something like 6405 + x = 5671 mod 10007
    # Which is 734 + x = 0 mod 10007. So that means diff is something like 9273 mod 10007?
    # Yeah, by hand it's just 1 lot of it, but it could be many lots of it?
    # Looks like the cards are in a loop - the last card + another diff is first card mod ncards...

    # Are those shuffle steps something that could tell us what start and diff are of an arbitrary 'round' of cards?

    # Deal into new stack - this reverses things. So since we're in a ring of cards, post this the card start would be (card[0]-diff) mod ncards
    #   Then we'd be going down rather than up, so diff is just inverted?

    # Cut N cards - we want to move around our ring of cards some amount. So diff isn't going to change, but start does by n * diff?

    # Increment N
    #  We always put the first card back in the first spot, so start isn't changing. Obviously something funky happening with diff though
    #  Card[0] => pos 0  (mod ncards)
    #  Card[1] => pos N  (mod ncards)
    #  Card[2] => pos 2N (mod ncards)
    #  ...
    #  Card[10006] => pos 10006N (mod ncards)
    #  so oldpos * N mod ncards = newpos mod ncards
    #  To figure out our new diff I want to know what the second card is (pos 1). So at somepoint
    #    oldpos * N mod ncards = 1 mod ncards
    #    and we knew what the value of old pos was because we had start and diff for that set already
    #      Dividing by N we have oldpos mod ncards = N**-1 mod ncards
    #        That's modular inversion! There's an algorithm for that!
    #  So if we've got our card[0] and card[1], can we figure out diff mod N right?
    #    (oldstart + olddiff * N**-1) - (oldstart) = difference
    #    olddiff * N**-1 = difference
    #
    # Can we combine these steps in some useful way? Can we do this backwards so we can track the 2020th place?
    #   Wait we don't need to. If we have the magic numbers, we just do start + diff * 2020
    #
    # So how do we do something useful here....
    # 
    # What happens if we just try this and compare numbers with part1
    print("Part 1 Check")
    (start, diff) = (0,1)
    for m in txtmoves:
        if m.startswith("deal into new stack"):
            start = (start - diff) % ncards
            diff  = (diff * -1) % ncards
        elif m.startswith("cut"):
            amount = int(m.split(" ")[-1])
            start = (start + (amount * diff)) % ncards
            # Diff stays the same
        elif m.startswith("deal with increment"):
            amount = int(m.split(" ")[-1])
            diff = (diff * pow(amount, -1, ncards)) % ncards
            # Start stays the same
        else:
            raise("Unexpected instruction")

    for i in range(ncards):
        if (start + (i*diff)) % ncards == 2019:
            print("Alternative approach results in answer:", i, "(Start,Diff)",start,diff)
            
    # Numbers match!
    #
    # So we can do one whole shuffle with a couple of numbers. How do we do loads of these shuffles without having to keep figuring out the start value by
    #  going through the steps. Must be a way of figuring out start + difference.
    #
    #  If we did lots of shuffle steps back to back, we'd change over and over again:
    #    difference is just getting multiplied by the same constant values every single time. So if we should be able to just repeatedly multiply
    #     by this same value for 'rounds' times and get the differnce for any given round. diff * (magicConstant**round) % ncards would do it
    #     Since we start with diff 1, the constant is whatever comes out of the code above :)
    #     It's a big exponent, but python seems happy with this in a mod operation with pow.
    magicDiffConstant = diff
    
    #    start - changes by -1 * diff or n * diff. So for any set of instructions, we will change start by adding some multiple of the diff at that point.
    #     This difference starts each shuffle instruction differently. Though, the multiple amount of it doesn't, e.g. maybe we always add on X*the original
    #      diff for round. Each shuffle instructions diff we can calculate.
    #
    #    start[0] = 0, diff[0] = 1
    #    start[1] = start[0] + multiple*diff[0]
    #    start[2] = start[1] + multiple*diff[1]
    #    start[3] = start[2] + multiple*diff[2]
    #
    #    Looks pretty repetitive as expected. Does anything simplify if we expand things?
    #
    #    start[0] = 0                                                 diff[0] = 1
    #    start[1] = 0 + multiple*1                                    diff[1] = diff[0] * magicDiff
    #    start[2] = 0 + multiple*1 + multiple*diff[0]*magicDiff
    #    start[3] = 0 + multiple*1 + multiple*diff[0]*magicDiff + multiple*diff[0]*magicDiff*magicDiff
    #
    #  Not simplier, but there is a pattern - some sort of infinite series?
    #
    #  Looks a bit like
    #    start[N] = 0 + multiple*(diff[0]*pow(magicDiff,0)) + multiple(diff[0]*pow(magicDiff, 1)) + multiple(diff[0]*pow(magicDiff,2)) + .... multiple(diff[0]*pow(magicDiff,N-1))
    #
    #  Bring out constant factor
    #    start[N] = 0 + multiple * (diff[0]*pow(magicDiff,0) + diff[0]*pow(magicDiff,1) + diff[0]*pow(magicDiff,2) + ... + diff[0]*pow(magicDiff,N-1))
    #    start[N] = 0 + (multiple * diff[0]) * (pow(magicDiff,0), + pow(magicDiff,1) + pow(magicDiff,2) + ... + pow(magicDiff,N-1))
    #
    #  Does that do anything for making calucalation simpler? Yes! It's a geometric series - we can directly compute
    #    with formula (thanks enigineering textbooks!)
    #      SUM(from k=0, to n-1) of (a(r**k) = a((1-r**n)/(1-r))
    #      In our case, a == 1, r == magicDiff, so
    #      SUM(form k=0, to n-1) of magicDiff**k = (1-magicDiff**rounds) / (1 - magicDiff)
    #       = (1-magicDiff**rounds) * pow(1-magicDiff, -1, ncards)
    #
    #  So since diff[0] = 1 start becomes
    #    start[N] = multiple * (1 - magicDiff ** rounds) * pow(1 - magicDiff, -1, ncards) % ncards
    #
    #  Since we started at 0 for start and we already said start[1] = 0 + multiple*1, then multiple must equal the value from one iteration!
    magicStartConstant = start
    
    # So using formulas above lets try to get the diff and start for after 1 round of shuffling doing the above
    rounds = 1
    def calcStart(magicStart, magicDiff, rounds, cards):
        return (magicStart * (1 - pow(magicDiff, rounds, cards)) * pow(1 - magicDiff, -1, cards)) % cards

    def calcDiff(magicDiff, rounds, cards):
        return pow(magicDiff, rounds, cards) % cards

    (start, diff) = (calcStart(magicStartConstant, magicDiffConstant, rounds, ncards), calcDiff(magicDiffConstant, rounds, ncards))
    print("Direct check. Should match above", start, diff)
    
    # That's right.  So what about doing two rounds of shuffling?
    cards = collections.deque(range(ncards))
    cards = shuffle(cards, moves)
    cards = shuffle(cards, moves)

    # First 10 cards please
    print("Multiple iterations check. Original approach yields")
    for i in range(10):
        print(cards[i], " ", end='')
    print()

    print("Multiple iterations check. Direct approach yields")
    rounds = 2
    (start, diff) = (calcStart(magicStartConstant, magicDiffConstant, rounds, ncards), calcDiff(magicDiffConstant, rounds, ncards))    
    for i in range(10):
        print((start + i*diff) % ncards, " ", end='')
    print()  

    # Part 2! At last!
    print("Part 2....")
    ncards = 119315717514047
    rounds = 101741582076661
    # Need new magic values since the number of cards are different
    (start, diff) = (0,1)
    for m in txtmoves:
        if m.startswith("deal into new stack"):
            start = (start - diff) % ncards
            diff  = (diff * -1) % ncards
        elif m.startswith("cut"):
            amount = int(m.split(" ")[-1])
            start = (start + (amount * diff)) % ncards
            # Diff stays the same
        elif m.startswith("deal with increment"):
            amount = int(m.split(" ")[-1])
            diff = (diff * pow(amount, -1, ncards)) % ncards
            # Start stays the same
        else:
            raise("Unexpected instruction")

    magicDiffConstant = diff
    magicStartConstant = start    
    (start, diff) = (calcStart(magicStartConstant, magicDiffConstant, rounds, ncards), calcDiff(magicDiffConstant, rounds, ncards))        
    print((start + 2020*diff) % ncards)
    
if __name__ == "__main__":
    main()
