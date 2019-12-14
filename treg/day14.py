#!/usr/bin/python

import fileinput
import math
import collections

created = collections.defaultdict(int)
totalOre = 0

def normalProduction(reactions, target, amount, f):
    # Do we already have the amount required?
    if created[target] >= amount:
        return

    # How much extra stuff do we need
    recipe = reactions[target]
    required = amount - created[target]
    multiplier = math.ceil(required / recipe[0])      
        
    for part in recipe[1]:
        newAmount = part[0]*multiplier
        f(reactions, part[1], newAmount)
        # Remember that we've consumed whatever we've needed before next req
        created[part[1]] -= (part[0]*multiplier)
            
    # And we produce our stuff
    created[target] += (recipe[0]*multiplier)
        
def getOreRequirements(reactions, target, amount):
    global totalOre
    if target == "ORE":
        created["ORE"] += amount
        totalOre += amount
    else:
        normalProduction(reactions, target, amount, getOreRequirements)

def exhaustOre(reactions, target, amount):
    if target == "ORE":
        if amount > created["ORE"]:
            raise ArithmeticError("Too little ORE")
    else:
        normalProduction(reactions, target, amount, exhaustOre)
        
def main():
    reactions = {}
    rstr = list(fileinput.input())
    for r in rstr:
        r = r.strip().split(' => ')

        ingredients = []
        for ingredient in r[0].split(', '):
            q, w = ingredient.split(' ')
            ingredients.append((int(q), w))
        
        # Resultant
        q, w = r[1].split(' ')
        reactions[w] = (int(q), ingredients)
        
    getOreRequirements(reactions, "FUEL", 1)
    print(totalOre)

    global created
    reqFuel = 1

    # Start from the bottom and double requests until we fail
    while True:
        created = collections.defaultdict(int)
        created["ORE"] = 1000000000000            
        try:
            exhaustOre(reactions, "FUEL", reqFuel)
            # Success if here
            reqFuel *= 2
        except:
            # Failed, so min bound is
            reqFuel /= 2
            break

    # We know a lower and upper bound in powers of two now split them
    lower = reqFuel
    upper = reqFuel * 2

    while True:
        created = collections.defaultdict(int)
        created["ORE"] = 1000000000000                    
        middle = int((lower+upper) // 2)
        
        try:
            exhaustOre(reactions, "FUEL", middle)
            # Success. Let middle be our new lower, but check if done first
            if lower == middle:
                # Sovled
                print(lower)
                break

            lower = middle
            
        except:
            # O dear. Let middle be ouer new upper
            upper = middle
    
if __name__ == "__main__":
    main()
