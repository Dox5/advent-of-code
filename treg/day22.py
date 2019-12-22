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
    print(cards.index(2019))
    
if __name__ == "__main__":
    main()
