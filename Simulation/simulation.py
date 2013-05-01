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

def karp_sipser(g, c, a):
  gg = g.copy()
  left_vertices  = (u for u in gg if gg.node[u]["bipartite"]=="left")
  right_vertices = (v for v in gg if gg.node[v]["bipartite"]=="right")
  
  left_used_degree = defaultdict(int)
  right_rem_degree = defaultdict(set)
  for v in right_vertices:
    if gg.degree(v) >= a:
      right_rem_degree[gg.degree(v)].add(v)

  solution = []
  try:
    min_degree = min(d for d in right_rem_degree if len(right_rem_degree[d])>0)
  except:
    min_degree = 0

  while min_degree >= a:
    # pick vertex on the right with lowest degree and remove it
    v = sample(right_rem_degree[min_degree],1)[0]
    right_rem_degree[min_degree].remove(v)
    eligible_neighbors = gg.neighbors(v)
    gg.remove_node(v)

    # pick a neighbors. if they become saturated, remove them
    for u in eligible_neighbors[:a]:
      left_used_degree[u] += 1
      if left_used_degree[u] == c:
        second_neighbors = gg.neighbors(u)
        for vv in second_neighbors:
          prev_degree = gg.degree(vv)
          if prev_degree < a: continue

          right_rem_degree[prev_degree].remove(vv)
          if prev_degree > a:
            right_rem_degree[prev_degree-1].add(vv)
        gg.remove_node(u)

    solution.append(v)
    degrees = [d for d in right_rem_degree if len(right_rem_degree[d])>0]
    if len(degrees)==0: break
    min_degree = min(degrees)

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
      s3 = karp_sipser(g, c, a)
      print "c = %d, a = %d" % (c, a)
      print "Sampling    : %d" % (len(s1))
      print "Greedy      : %d" % (len(s2))
      print "Karp-Sipser : %d" % (len(s3))
      print "Optimal     : %d" % (optimal)
      print 