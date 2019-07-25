# convert min to maximin
# this formulation takes the prioritizr built input (the cm), and converts to a maximin problem
# ie maximize the minimum representation of all the (focal) species (ft)
# ft must come in as the entry number of the focal targets (e.g. 4,5,8,10)
# and a start solution (ss) and a model name (mn)

# for explanation of this function see explain_min2maximin

convert_min2maximin <- function(cm, ft, ss, mn){
  
  # cm <- o
  # ft <- c(2:4)
  # ss <- start_solution
  # mn <- 'test_maximin'
  
  
  nz <- number_of_zones(cm)
  nc <- number_of_planning_units(cm)
  nf <- number_of_features(cm)
  nft <- length(ft) # number of focal targets
  dimA <- dim(cm$A()) # this is the nf + cells, by cells x zones
  
  # the objective will be to minimize the continuous value Z.
  # the constraints matrix (A) will now be comprized of 
  # columns: Z (1) + benefits for each feature in each cell*zone (targetfeatures*nc*nz) + the original feature matrix (nc*nz) 
  # rows: 
  #    Z-targetfeatures (ntargetfeatures), 
  #    cells selected in zone X in targetfeatures = cells selected in other targetfeatures= cells selected in features (nzones) = ntargetfeat+1
  #    one zone only & additional zone constraints (original A matrix) 
  
  Z <-  1  
  za <- rep(0, nz*nc)        # create a string of zeros, in the original A width, for padding
  
  # the objective function is to maximize Z only
  o1 <-  c(Z,  rep(za, nft), za) 
  
  # the constraints matrix 
  # we can make this from the components of the prior matrix
  # it will need to go in a matrix that is 
  # nrow = nft + original A matrix rows (which contain feature targets, one zone only and and transitions) + making sure the z targets are selected the same cells as the ozo (for each cell*zone). 
  # ncol = z(1) + (nft + 1) * nz*nc (as need to equate Z selections with one zone only)
  narows <- nft + dimA[1] + (nft * nz * nc)
  nacols <- 1 + (nft + 1) * (nz * nc)
  newA <- Matrix(0, nrow = narows, ncol = nacols)
  ai <- 1
  
  # for each focal target, need a line that is Z - benefits for that target in each cell in each zone (ie the original A)
  for (i in 1:nft){
    fti <- ft[i]
    bsi <- cm$A()[fti,]
    newA[ai, ] <- c(Z,  rep(za, i-1), -bsi,  rep(za, nft-i), za)
    ai <- ai + 1
  }
  
  # for each feature, need to reach the target - this is simply the original A, padding the front with zeros
  # (as this is how we did it before, it keeps option open for targets, and it helps us calculate the features later)
  for (i in 1:nf){
    bsi <- cm$A()[i,]
    newA[ai, ] <- c(0,  rep(za, nft), bsi)
    ai <- ai + 1
  }

  # then we need a one zone only + transition constraints matrix, which is the rest of the original A matrix, padding the front with zeros
  ozo <- cm$A()[-c(1:nf), ]
  # ozo is the basic, one zone only matrix, applied to the targets only
  endai <- ai + dim(ozo)[1] -1
  newA[ai:endai, ] <- cbind(Matrix(0, nrow = dim(ozo)[1], ncol = 1 + (nft*nc*nz), sparse = TRUE), ozo) 
  ai <- endai + 1
  
  # then we specify that the same cells need to be selected in the same zone, for all  the feature zconstraints and the targets
  # we need to do this for each nft, equating it with the fts (ie 1 -1 = 0)

  for(zi in 1:nz){
    
    # for each zone, need a diag matrix in the zone position, and at the end
    zzi <- cbind( Matrix(0, nrow = nc, ncol = nc*(zi-1)),
                  diag(nc), 
                  Matrix(0, nrow = nc, ncol = nc*(nz-zi)))
    
    # then each feature z score needs to equate to the original A matrix, with one column of zeros to pad the Z column 
    for (fti in 1:nft) {
      ozzi <- cbind( Matrix(0, nrow = nc),  # zrow
                    do.call(cbind, replicate(fti-1, zzi*0)),
                    zzi,
                    do.call(cbind, replicate(nft - fti, zzi*0)),
                    zzi * -1)
      # and join to the list
      endai <- ai + dim(ozzi)[1] -1
      newA[ai:endai, ] <- ozzi 
      ai <- endai + 1
    }
  }

# now we can rebuild the model  
    
  model <- list()
  model$modelname <- mn
  model$modelsense <- 'max'
  model$obj        <- o1
  model$A          <- newA
  
  # other transition constraints are kept in the lower bound (lock in = 1) and upper bound (lock out = 0)
  # these need to repeat the original lb and ub nft+1 times, and pad the front with -Inf, Inf respectively.
  model$lb <- c(-Inf, rep(cm$lb(), nft+1))
  model$ub <- c(Inf, rep(cm$ub(), nft+1))

  # now the rhs            z groups              targets              one zone only constraints,    selected cells same
  model$rhs        <- c(   rep(0,    nft),       cm$rhs()[1:nf],      rep(1,   nrow(ozo)),          rep(0,   nft*nc*nz))
  model$sense      <- c(   rep('<=', nft),       rep('>=',  nf),      rep('=', nrow(ozo)),          rep('=', nft*nc*nz))
  model$vtype      <- c('C', rep('B', ncol(model$A)-1))  # binary decisions for all but the Z
  
  # model start solution is more complicated than the old start solution, repeated nft+1 times, and prefaced with 1.
  model$Start <- c(0, rep(binary_stack(ss) %>% values() %>% c(), nft + 1))
  gp <- list(Presolve = -1, Timelimit = 1000, MIPGap = 0.01, IntFeasTol = 0.001, DualReductions = 0)

  return(model)  
}

