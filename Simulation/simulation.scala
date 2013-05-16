import scala.io.Source
import scala.util.Random
import scala.collection.mutable.Map
import scala.collection.mutable.{ArrayBuffer => AB}

class Graph[T](
  V: Seq[T],
  E: Map[T, _ <: Seq[T]]) {

  def this() = this(List(), Map())
  def this(V: Seq[T]) = this(V, Map()) 

  val vertices: AB[T] = AB(V: _*)
  val edges   : Map[T, AB[T]] = Map()
  V foreach { v => 
    edges += v -> {if (E.contains(v)) AB(E(v): _*) else AB() }
  }
  
  def degree(v: T) = {
    edges(v).length
  }

  def addVertex(v: T, neighbors: Seq[T] = AB()) = {
    edges += (v->AB())
    vertices += v
    neighbors foreach {u => 
      edges(v) += u
      edges(u) += v
    }
  }
  
  def removeVertex(v: T) = {
    val neighbors = edges(v)
    neighbors foreach { edges(_) -= v }
    edges -= v
  }

  def addEdge(v: T, u: T) = {
    edges(v) += u
    edges(u) += v
  }
  
  def removeEdge(v: T, u: T) = {
    edges(v) -= u
    edges(u) -= v
  }
  
  override def clone() = {
    new Graph(vertices, edges)
  }
}

def samplingAlgorithm[T <: Int](g: Graph[T], c: Int, a: Int): Int = {
  val hitCount: Map[T, Int] = Map().withDefaultValue(0)
  val leftVertices = g.vertices filter {_ > 0}
  leftVertices foreach { u =>
    val neighbors = g.edges(u)
    sample(neighbors, c) foreach {hitCount(_) += 1}
  }
  var total = 0
  hitCount.keySet.foreach {v => if (hitCount(v) >= a) total += 1}
  return total
}

def greedyAlgorithm[T <: Int](g: Graph[T], c: Int, a: Int): Int = {
  val usedDegree: Map[T, Int] = Map().withDefaultValue(0)
  val rightVertices = g.vertices filter { _ < 0}
  val solution: AB[Int] = AB()

  rightVertices foreach { v => 
    val eligibleNeighbors = g.edges(v).filter(usedDegree(_) < c)
    if (eligibleNeighbors.length >= a) {
      (0 until a) foreach {t => usedDegree(eligibleNeighbors(t)) += 1}
      solution += v
    }
  }
  return solution.length
}

def iteratingAlgorithm[T <: Int](gg: Graph[T], c: Int, a: Int): Int = {
  val g = gg.clone
  val usedDegree: Map[T, Int] = Map().withDefaultValue(0)
  val leftVertices  = g.vertices filter { _ > 0}
  val rightVertices = g.vertices filter { _ < 0}
  val buckets       = AB(AB(rightVertices:_*))

  
  (1 to a) foreach { i =>
    buckets += AB()
    buckets(i-1) foreach { v =>
      val eligibleNeighbors = g.edges(v) filter {usedDegree(_) < c}
      if (!eligibleNeighbors.isEmpty) {
        val u = eligibleNeighbors minBy (usedDegree(_))
        g.removeEdge(v,u)
        buckets(i) += v
        usedDegree(u) += 1
      }
    }
  }
  return buckets(a).length
}

val r = new Random

def genRandomRegularGraphInt(l: Int, r: Int, d: Int): Graph[Int] = {
  val leftVertices  = 1 to l
  val rightVertices = 1 to r map {-_}
  val G = new Graph(rightVertices)

  leftVertices foreach { u => G.addVertex(u, sample(rightVertices, d)) }
  return G
}

def sample[T](l: IndexedSeq[T], count: Int = 1): Seq[T] = {
  val choices: AB[Int] = AB()
  while (choices.length < count) {
    val nextChoice = r.nextInt(l.length)
    if (!choices.contains(nextChoice) )
      choices += nextChoice}
  choices map {l(_)}
}


def benchmark(
  c: Int, a: Int,
  algo: (Graph[Int], Int, Int)=>Int,
  trials: Int = 10): Int = {
  
  val results = (1 to trials) map { t =>
    val G = genRandomRegularGraphInt(1000,10000,50)
    algo(G, c, a)
  }
  return results.sum / results.length
}

for (c <- 1 until 10) {
  for (a <- 1 until 10) {
    println("c=%d, a=%d".format(c, a))
    println("Greedy   : %d".format(benchmark(c, a, greedyAlgorithm)))
    println("Sampling : %d".format(benchmark(c, a, samplingAlgorithm)))
    println("CTO      : %d".format(benchmark(c, a, iteratingAlgorithm)))
    println
  }
}