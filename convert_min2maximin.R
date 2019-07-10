# convert min to maximin
# this formulation takes the prioritizr built input (the cm), and converts to a maximin problem
# ie maximize the minimum representation of all the (focal) species (ft)
# ft must come in as the entry number of the focal targets (e.g. 4,5,8,10)
# and a start solution (ss) and a model name (mn)

# for explanation of this function see explain_min2maximin

convert_min2maximin <- function(cm, ft, ss, mn){
  nz <- number_of_zones(cm)
  nc <- number_of_planning_units(cm)
  nf <- number_of_features(cm)
  nft <- length(ft) # number of focal targets
  dimA <- dim(cm$A()) # this is the nf + cells, by cells x zones
  
  Z <-  1  
  za <- rep(0, nz*nc)        # create a string of zeros for padding
  
  # the objective function is to maximize Z only
  obj <-  c(Z,  rep(0, nft), za) 
  
  # the constraints matrix 
  # we can make this from the components of the prior matrix
  # it will need to go in a matrix that is 
  # nrow = nft + original A matrix rows (which contain feature targets, one zone only and and transitions) + making sure the z targets are selected the same cells as the ozo. 
  # ncol = z(1) + (nft + 1) * nz*nc (as need to equate Z selections with one zone only)
  narows <- nft + dimA[1] + (nft * nz * nc)
  nacols <- 1 + (nft + 1) * (nz * nc)
  newA <- Matrix(0, nrow = narows, ncol = nacols)
  ai <- 1
  
  # for each focal target, need a line that is Z - bs
  for (i in 1:nft){
    fti <- ft[i]
    bsi <- cm$A()[fti,]
    newA[ai, ] <- c(Z,  rep(za, i-1), -bsi,  rep(za, nft-i), za)
    ai <- ai + 1
  }
  
  # for each feature, want need to reach the target 
  # (as this is how we did it before, it keeps option open for targets, and it helps us calculate the features later)
  for (i in 1:nf){
    bsi <- cm$A()[i,]
    newA[ai, ] <- c(0,  rep(za, nft), bsi)
    ai <- ai + 1
  }

# then we need a one zone only + transition constraints matrix, which is the rest of the original A matrix
# padded out with zeros
  ozo <- cm$A()[-c(1:nf), ]
  # ozo1 is the basic, one zone only matrix, applied to the targets only
  endai <- ai + dim(ozo)[1] -1
  newA[ai:endai, ] <- cBind(Matrix(0, nrow = dim(ozo)[1], ncol = 1 + (nft*nc*nz), sparse = TRUE), ozo) 
  ai <- endai + 1
  
  # then we specify that the same cells need to be selected in the same zone, for both the targets and the feature zconstraints (between the feature constraints is then redundant)
  for(zi in 1:nz){
    # for each zone, need a diag matrix in the zone position, and at the end
    bzzi <- cBind( Matrix(0, nrow = nc),  # zrow
                   Matrix(0, nrow = nc, ncol = nc*(zi-1)),
                   diag(nc), 
                   Matrix(0, nrow = nc, ncol = nc*(nz-zi)),
                   diag(x = -1, nc))
    # then each feature z score needs to equate to the others. 
    ozzi <- cBind( Matrix(0, nrow = nc),  # zrow
                   do.call(cbind, replicate(nft, bzd)),         # repeat the nz*nc matrix nft times
                   diag(nc) * -nft )      # negate the targets one by nft times to give rhs of 0
    # and join to the list
    endai <- ai + dim(ozzi)[1] -1
    newA[ai:endai, ] <- ozzi 
    ai <- endai + 1
  }
  
  nA <- dplyr::bind_rows(newA)
  nA <- Matrix(nA, sparse = TRUE)

  
  
# now we can rebuild the model  
    
  model <- list()
  model$A          <- rbind(ma1, ma2, sp1, sp2, ozo2)
  model$obj        <- o1
  model$modelsense <- 'max'
  # now the rhs       z groups          targets         one zone only constraints, selected cells same
  model$rhs        <- c(rep(0, ns),     t1, t2,         rep(1, nrow(ozo)), rep(0, nrow(ozo)*2))
  model$sense      <- c(rep('<=', ns),  rep('>=', ns),  rep('=', nrow(ozo2)))
  model$vtype      <- c('C', rep('B', ncol(model$A)-1))  # binary decisions for all but the Z
  
  gp <- list(Presolve = -1, Timelimit = 1000, MIPGap = 0.01, IntFeasTol = 0.001, DualReductions = 0)
  
  
  list(
    modelname = mn,
    modelsense = "max",  # max Z
    obj = n_obj,     
    rhs = n_rhs,      # the first nf of these are the absolute targets.
    lb = n_lb,
    ub = n_ub,
    vtype = n_vtype,
    sense = n_sense,
    A = n_A,
    Start = n_ss)  # add start solution
}