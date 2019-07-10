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

# in terms of our problem, say we have a 3 species problem, and we want to maximize the minimum achievement of them:
# the x1 above is the old A for row spp1 (ie benefits from each cell x zone for spp1)
# and Z is the c( A for row spp1, A for row spp2, ...) for all species of interest
# let us symbolize this as V1, etc for simplicity.
# but we need to convert the constraints so that the RHS is a single number 
# we can do this by reformulating as Z - x1 <= 0
# so now the objective function is 
#     (Z, Zsp1, Zsp2, Zsp3,     A)  RHS
# obj (Z,    0,    0,    0,     0)  .     # maximize   
# con (Z,   -V1,   0,    0,     0)  <= 0     # spp1 minimum
# con (Z,    0,  -V2,    0,     0)  <= 0     # spp2 minimum
# con (Z,    0,    0,  -V3,     0)  <= 0     # spp3 minimum
# con (0,    0,    0,    0,     V1)  <= t1   # spp1 targets
# con (0,    0,    0,    0,     V2)  <= t2   # spp2 targets
# con (0,    0,    0,    0,     V3)  <= t3   # spp3 targets
# then all the other A constraints with 0 for the Z area of the matrix, 
# then these same constrains need to be transferred over to the Z side as well?? 

# ie to convert our problem into a maximin problem will vastly expand it. If the original constriants matrix (A) was zones*cells wide, this will mean that the new matrix is (2*nspecies)*zones*cells+1 wide. 
# luckily it only adds the number of focal species rows, and it can be sparse. 
# the alternative is to repetitively iterate, which we know is slow.

## example ======================================= 
# lets try the simpelest example possible, 2 species, 2 zones, 4 cells
    nz <- 2
    ns <- 2
    nc <- 4

# to run the max aggregation would be 
    bs1 <- runif(nz*nc)  # benefits for species 1 in zone 1,2
    bs2 <- runif(nz*nc)  # benefits for species 2 in zone 1,2
    t1 <- 0.1*sum(bs1) # a target for spp1, 2
    t2 <- 0.1*sum(bs2)  
# then, the original one zone only and mandatory allocation to a zone would be 
    ozo <- cbind(diag(4), diag(4))

# the initial model would be:
    ms <- list()
    ms$A          <- rbind(bs1, bs2, ozo)
    ms$obj        <- colSums(rbind(bs1, bs2))
    ms$modelsense <- 'max'
    ms$rhs        <- c(t1, t2,  rep(1, nrow(ozo)))
    ms$sense      <- c(rep('>=', ns),  rep('=', nrow(ozo)))
    ms$lb         <- rep(0, ncol(ms$A)) 
    ms$ub         <- rep(1, ncol(ms$A)) 
    ms$vtype      <- rep('B', ncol(ms$A))  # binary decisions
    
    gurobi(ms)

# now, the larger problem:
    Z <-  1  
    za <- rep(0, nz*nc)        # create a string of zeros to pad them out
    
    o1 <-  c(Z,   za,   za,    za) # the objective function
    ma1 <- c(Z,  -bs1,  za,    za) # the constraints matrix for spp1 min
    ma2 <- c(Z,   za,  -bs2,   za) # the constraints matrix for spp2 min
    sp1 <- c(0,   za,   za,   bs1) # the constraints matrix for spp1 target
    sp2 <- c(0,   za,   za,   bs2) # the constraints matrix for spp1 target

# the one zone only idea needs to be extended to we only select one of the two zones, and that that Zscores select the same zones as the targets, as well as each other

    ze1 <- cbind(diag(4), diag(4)*0)  # selection in zone 1 only
    ze2 <- cbind(diag(4)*0, diag(4))  # selection in zone 2 only
    
    ozo2 <- rbind(  cbind(0, ozo*0, ozo*0, ozo*1),   # ozo constraints rhs = 1
                    cbind(0, ze1*1, ozo*0, ze1*-1),  # same selected constraints rhs = 0
                    cbind(0, ozo*0, ze2*1, ze2*-1),
                    cbind(0, ze1*1, ze1*-1, ozo*0),
                    cbind(0, ze2*1, ze2*-1, ozo*0))
                
    model <- list()
    model$A          <- rbind(ma1, ma2, sp1, sp2, ozo2)
    model$obj        <- o1
    model$modelsense <- 'max'
    # now the rhs       z groups          targets         one zone only constraints, selected cells same
    model$rhs        <- c(rep(0, ns),     t1, t2,         rep(1, nrow(ozo)), rep(0, nrow(ozo)*4))
    model$sense      <- c(rep('<=', ns),  rep('>=', ns),  rep('=', nrow(ozo2)))
    model$vtype      <- c('C', rep('B', ncol(model$A)-1))  # binary decisions for all but the Z
    
    gp <- list(Presolve = -1, Timelimit = 1000, MIPGap = 0.01, IntFeasTol = 0.001, DualReductions = 0)

    s1 <- gurobi(model, gp)

    sy <- c(1,3,4,6)   
    min(sum(bs1[sy]), sum(bs2[sy]))
    sx <- sample(1:8, 4)
    min(sum(bs1[sx]), sum(bs2[sx]))

