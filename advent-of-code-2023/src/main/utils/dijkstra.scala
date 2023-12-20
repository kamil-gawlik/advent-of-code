package utils

import scala.collection.mutable.{HashMap, HashSet}

trait dijkstra {
  type V = (Int, Int)

  class WeightedDiGraph[V]() {
    val arcs = new HashMap[V, HashMap[V, Float]]
    val vertices = new HashSet[V]()

    def adjacents(u: V) = arcs(u).keys

    def cost(u: V, v: V) = arcs(u)(v)

    def addArc(from: V, to: V, kost: Float) = {
      if (arcs.contains(from)) {
        val adjMap = arcs(from)
        adjMap += to -> kost
      }
      else {
        val adjMap = new HashMap[V, Float]
        adjMap += to -> kost
        arcs += from -> adjMap
      }
      vertices += from
      vertices += to
    }

    def shortestPath(start: V, end: V): List[(V, Float)] = {
      // get shortest-distances, predecessors, end result
      val (dist, pred, endReached) = dijkstra(start, end)
      // build path from end based on predecessors
      var path: List[(V, Float)] = Nil
      if (endReached) {
        var v = end
        while (v
          != start
        ) {
          path = (v, dist(v)) :: path
          // iterate on predecessor      
          v = pred(v)
        }
      }
      // return path
      path
    }

    def dijkstra(source: V, end: V) = {
      assume(arcs.contains(source), "source not in arcs origins")
      if (end != null) assume(vertices.contains(end), "end not in graph")
      // initialize
      val dist = new HashMap[V, Float] // distances
      val Q = new HashSet[V] // priority queue
      val Settled = new HashSet[V] // settled vertices
      val pred = new HashMap[V, V] // predecessors

      def minimumDistVertex(Q: HashSet[V]): V = {
        assume(Q.nonEmpty && Q.forall(dist.isDefinedAt));
        val iterator = Q.iterator;
        val w = iterator.next; // first element, because Q is not empty
        // calculate and return
        iterator.foldLeft(w) { (u, v) => if (dist(u) <= dist(v)) u else v }
      }
      // start with source vertex
      dist += source -> 0F
      Q += source
      var endReached = false
      while (Q.nonEmpty && !endReached) {
        // extract minimumDistVertex from Q, add to Settled ones
        val u = minimumDistVertex(Q)
        Q -= u
        Settled += u
        if (end != null) endReached = (u == end)
        // update neighbours distances
        // and add updated ones to Q
        if (!endReached)
          for (v <- adjacents(u) if !Settled.contains(v)) {
            val vNewDist = dist(u) + cost(u, v)
            if (!dist.isDefinedAt(v) || vNewDist < dist(v)) {
              dist += (v -> vNewDist)
              pred += v -> u
              Q += v
            }
          }
      }
      // return distances, predecessors, endReached
      (dist, pred, endReached)
    }
  }
}