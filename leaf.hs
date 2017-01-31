data TREE a = LEAF a | NODE a (TREE a) (TREE a)

k1 (LEAF x) g = LEAF x
k1 (NODE x y z) g = NODE (g x) (k1 y g) (k1 z g)

k2 (LEAF x) g = LEAF (g x)
k2 (NODE x y z) g = NODE (g x) (k2 y g) (k2 z g)

--k3 (LEAF x) g = g x
--k3 (NODE x y z) g = NODE (g x) (k3 y g) (k3 z g)

k4 (LEAF x) g = x
k4 (NODE x y z) g = NODE (g x) (k4 y g) (k4 z g)