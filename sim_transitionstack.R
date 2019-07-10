# sim_transitionstack & template

sim_transitionstack <- function(fz, prb){
  template <- fz[[1]][[1]] * 0
  transitions <- stack()
  nz <- length(fz)
  nc <- ncell(template)
  
  set0 <- 1
  
  for (i in 1:nz){
   seti <- rbinom(nc, 1, prb + (1-prb)*(1-set0))
   transitions <- addLayer(transitions, template + seti)
   set0 <- set0*seti
  }
  
  names(transitions) <- zone_names(sim_features_zones)
  return(transitions)
}

sim_puid <- function(fz){
  template <- fz[[1]][[1]] * 0
  template + 1:ncell(template)
}