package main

import (
	"fmt"
	"sort"
)

type MolCounts map[byte]uint64

type cacheKey struct {
	pair  PolymerPair
	steps int // How many more steps happened after this cached value
}

func (c cacheKey) LessThanEqualTo(other cacheKey) bool {
	if c.steps < other.steps {
		return true
	} else if c.steps == other.steps {
		return c.pair.LessThanEqualTo(other.pair)
	} else {
		// must be the case that c.steps > other.steps
		return false
	}
}

type cacheEntry struct {
	key   cacheKey
	value MolCounts
}

type PolymerCountCache struct {
	entries []cacheEntry
}

func (c *PolymerCountCache) Lookup(pair PolymerPair, steps int) (MolCounts, bool) {
	key := cacheKey{pair: pair, steps: steps}

	// Find it in entries
	at := sort.Search(len(c.entries), func(i int) bool {
		return key.LessThanEqualTo(c.entries[i].key)
	})

	if at < len(c.entries) && c.entries[at].key == key {
		return c.entries[at].value, true
	} else {
		return nil, false
	}
}

// Remember a count
func (c *PolymerCountCache) Put(root PolymerPair, steps int, counts MolCounts) {
	key := cacheKey{pair: root, steps: steps}

	at := sort.Search(len(c.entries), func(i int) bool {
		return key.LessThanEqualTo(c.entries[i].key)
	})

	if at < len(c.entries) && c.entries[at].key == key {
		panic(fmt.Sprintf("Already have an entry cached for %+v @ %v", root, steps))
	}

	c.entries = append(c.entries, cacheEntry{})
	copy(c.entries[at+1:], c.entries[at:])
	c.entries[at].key = key
	c.entries[at].value = counts
}
