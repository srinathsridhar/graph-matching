import networkx as nx
from random import *
from collections import *

def rand_fixed_degree_graph(l, r, d):
  # create nodes
  left_nodes  = ["l"+str(i).zfill(4) for i in range(l)]
  right_nodes = ["r"+str(i).zfill(5) for i in range(r)]
  g = nx.Graph()
  g.add_nodes_from(left_nodes , bipartite="left")
  g.add_nodes_from(right_nodes, bipartite="right")

  # create edges
  for u in left_nodes:
    for sample_index in sample(xrange(r), d):
      g.add_edge(u, right_nodes[sample_index])

  return g

def sampling_algorithm(g, c, a):
  hit_count = defaultdict(int)
  left_vertices = (u for u in g if g.node[u]["bipartite"]=="left")

  for u in left_vertices:
    neighbors = g.neighbors(u)
    if len(neighbors) < c:
      samples = neighbors
    else:
      samples = [neighbors[i] for i in sample(xrange(len(neighbors)), c)]

    for sample_ in samples:
      hit_count[sample_] += 1

  return [v for v in hit_count if hit_count[v] >= a]

def greedy_algorithm(g, c, a):
  degree_count = defaultdict(int)
  right_vertices = (u for u in g if g.node[u]["bipartite"]=="right")
  solution = []

  for v in right_vertices:
    eligible_neighbors = [u for u in g.neighbors(v) if degree_count[u] < c]
    if len(eligible_neighbors) >= a:
      for u in eligible_neighbors[:a]:
        degree_count[u] += 1
      solution.append(v)

  return solution

# parameters
trials = 10
l      = 10000
r      = 100000
d      = 10

for a in range(1,10):
  for c in range(1,10):
    optimal = min(r, c*l/a)
    for _ in xrange(trials):
      g = rand_fixed_degree_graph(l, r, d)
      s1 = sampling_algorithm(g, c, a)
      s2 = greedy_algorithm(g, c, a)
      print "c = %d, a = %d" % (c, a)
      print "Sampling: %d" % (len(s1))
      print "Greedy  : %d" % (len(s2))
      print "Optimal : %d" % (optimal)
      print 