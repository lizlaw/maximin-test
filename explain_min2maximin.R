# takes in a compiled model (cm) from prioritizr (which comes in as a minimization objective)
# and a vector of the feature targets wanting to be maximized (ft)
# as well as a start solution (ss) and a model name (mn)
# converts to a maximize the minimum achievement of feature targets objective

# From the basics: say we want to 
# maximize the min of the set (x1,x2,x3)
# s.t.     x1 + x2 + x3 = 17
# to do this we need to create a new variable, Z, which we will maximize
# max Z
# s.t. x1 + x2 + x3 = 17
# Z <= x1   
# Z <= x2
# Z <= x3

# in terms of our problem, say we have a 3 species problem, and we want to maximize the minimum achievement of them, as well as achieve targets:
# the x1 above is sum achievement of spp1 (ie benefits from each cell x zone for spp1)
# and Z is the minimum achievement for all species of interest
# let us symbolize sum achievement for species 1 as V1, etc for simplicity.
# we need to reformulate so RHS is a single, a priori number
# so now the objective function is 
# (A below is the old A matrix width, ie cells*zones, with the target rows being identical to the old A)
#     (Z, Zsp1, Zsp2, Zsp3,     A)  RHS
# obj (Z,    0,    0,    0,     0)  .     # maximize   
# con (Z,   -V1,   0,    0,     0)  <= 0     # spp1 minimum
# con (Z,    0,  -V2,    0,     0)  <= 0     # spp2 minimum
# con (Z,    0,    0,  -V3,     0)  <= 0     # spp3 minimum
# con (0,   -V1,   0,    0,     sb1)  = 0     # spp1 sum benefits = V1
# con (0,    0,  -V2,    0,     sb2)  = 0     # spp2 sum benefits = V2
# con (0,    0,    0,  -V3,     sb3)  = 0     # spp3 sum benefits = V3
# con (0,    0,    0,    0,     sb1)  <= t1   # spp1 targets
# con (0,    0,    0,    0,     sb2)  <= t2   # spp2 targets
# con (0,    0,    0,    0,     sb3)  <= t3   # spp3 targets
# then all the other A constraints with 0 for the Z/V area of the matrix
# so this formulation adds 2 x focal species rows, and 1 + focal species columns to the old A matrix.
# remember, the new columns must be continuous, positive variables (and the others remain binary)
# Z and Vs are to = 1 in the matrix. Unless if you want to weight the focal species in which case you could make them not = 1. 