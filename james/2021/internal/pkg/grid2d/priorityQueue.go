package grid2d

import (
	"math"
	"sort"
)

type cheapestKnown struct {
	cost  int
	point Point2D
}

type priorityQueue struct {
	queue []*cheapestKnown
}

// Create a queue with all points
func newPriorityQueue(width, height int) *priorityQueue {
	queue := make([]*cheapestKnown, width*height)

	i := 0
	for x := 0; x < width; x++ {
		for y := 0; y < height; y++ {
			queue[i] = &cheapestKnown{
				cost:  math.MaxInt,
				point: Point2D{X: x, Y: y},
			}

			i++
		}
	}

	return &priorityQueue{queue: queue}
}

func (q *priorityQueue) updateLower(what Point2D, cost int) {
	var i int
	for i = 0; i < len(q.queue); i++ {
		if q.queue[i].point == what {
			if cost < q.queue[i].cost {
				q.queue[i].cost = cost
				break
			} else {
				// Skip bubble code
				return
			}
		}
	}

	// Find expected location, can only be 'right' of current
	searchSlice := q.queue[i:]
	at := sort.Search(len(searchSlice), func(needle int) bool {
		return searchSlice[needle].cost < cost
	})

	if at == 0 {
		// Already in the right place
		return
	}

	toMove := q.queue[i]
	copy(searchSlice[0:], searchSlice[1:at])
	searchSlice[at-1] = toMove
}

func (q *priorityQueue) pop() (Point2D, int, bool) {
	if len(q.queue) == 0 {
		return Point2D{}, 0, false
	}

	node := q.queue[len(q.queue)-1]
	q.queue = q.queue[:len(q.queue)-1]

	return node.point, node.cost, true
}
