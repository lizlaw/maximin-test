# convert min to maximin
# this formulation takes the prioritizr built input (the cm), and converts to a maximin problem
# ie maximize the minimum representation of all the (focal) species (ft)
# ft must come in as the entry number of the focal targets (e.g. 4,5,8,10)
# and a start solution (ss) and a model name (mn) should also be provided

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
  # columns: Z (1) + V (for nft) + the original feature matrix (nc*nz) 
  # rows: 
  #    Z-targetfeatures (for nft), 
  #    V = targetfeature (for nft)
  #    targets (for nf)
  #    one zone only & additional zone constraints (original A matrix, padded) 
  
  # the objective function is to maximize Z only
  o1 <-  c(1, rep(0, nft), rep(0, nz*nc))  
  
  # the constraints matrix 
  # we can make this from the components of the prior matrix
  rA <- cm$A()
  #rA <- round(rA) %>% as(., "sparseMatrix")
  
  # for each focal target, need a line that is Z - V = 0
  newA_zv <- cbind(1,  as(-diag(nft), "sparseMatrix"), Matrix(0, nft, nc*nz))
  # and a line that equates the V to the sumbenefits for that feature
  newA_vt <- cbind(0, as(-diag(nft), "sparseMatrix"), rA[ft, ])
  # then the original feature targets and one zone only constraints (ie all the old A, padded)
  newA_ft <- cbind(0, Matrix(0,dimA[1],nft), rA)
  
  # now we can rebuild the model  
  model <- list()
  model$modelname <- mn
  model$modelsense <- 'max'
  model$obj        <- o1
  model$A          <- rbind(newA_zv, newA_vt, newA_ft)
  
  # other transition constraints are kept in the lower bound (lock in = 1) and upper bound (lock out = 0)
  # these need to take the original lb and ub, and pad the front with -Inf, Inf and 0, Infs respectively.
  model$lb <- c(0, rep(0, nft), cm$lb())
  model$ub <- c(Inf, rep(Inf, nft), cm$ub())
  
  # now the rhs            z groups              v groups          targets              one zone only constraints
  model$rhs        <- c(   rep(0,    nft),       rep(0,   nft),   cm$rhs()[1:nf],      rep(1,   dimA[1]-nf))
  model$sense      <- c(   rep('<=', nft),       rep('=', nft),   rep('>=',  nf),       rep('=', dimA[1]-nf))
  # and column types
  model$vtype      <- c(   'C', rep('C', nft),   rep('B', ncol(model$A)-(1+nft)))  # Z and V need to be C, others binary.
  
  # model start solution is more complicated than the old start solution, repeated nft+1 times, and prefaced with 1.
  model$Start <- c(0, rep(0, nft), binary_stack(ss) %>% values() %>% c())
  gp <- list(Presolve = -1, Timelimit = 1000, MIPGap = 0.01, IntFeasTol = 0.001, DualReductions = 0)
  
  return(model)  
}