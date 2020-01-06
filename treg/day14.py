#!/usr/bin/python

import fileinput
import math
import collections


def getOreCount(reactions, target, amount):
    created = collections.defaultdict(int)    
    return getOreRequirement(reactions, target, amount, created)
    
def getOreRequirement(reactions, target, amount, created):
    generatedOre = 0
    if target == "ORE":
        created["ORE"] += amount
        return amount
    else:
        # Do we already have the amount required?
        if created[target] >= amount:
            return 0

        # How much extra stuff do we need
        recipe = reactions[target]
        required = amount - created[target]
        multiplier = math.ceil(required / recipe[0])      
        
        for part in recipe[1]:
            newAmount = part[0]*multiplier
            generatedOre += getOreRequirement(reactions, part[1], newAmount, created)
            # Remember that we've consumed whatever we've needed before next req
            created[part[1]] -= (part[0]*multiplier)
            
        # And we produce our stuff
        created[target] += (recipe[0]*multiplier)
        return generatedOre
    
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

    # Part 1
    print(getOreCount(reactions, "FUEL", 1))

    # Part 2
    # Take some extreme bounds and binary search, checking if too much ore is used
    lower = 1
    upper = 1000000000000

    while True:
        middle = int((lower+upper) // 2)
        
        if getOreCount(reactions, "FUEL", middle) <= 1000000000000:            
            # Success. Let middle be our new lower, but check if done first
            if lower == middle:
                # Sovled
                print(lower)
                break

            lower = middle
        else:
            # O dear. Let middle be ouer new upper
            upper = middle
    
if __name__ == "__main__":
    main()
