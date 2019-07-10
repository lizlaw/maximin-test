# simulate_start

sim_start <- function(trns) {
  nc <- ncell(trns[[1]])
  nz <- nlayers(trns)
  sts <- trns[[1]]*0
  
  for (i in 1:nc){
    zin <- trns[[1:nz]][i] * 1:nz
    zin <- zin[zin != 0]
    sts[i] <- sample(zin, 1)
  } 
  return(sts)
}
