# create manual target tibble

create_mtargets <- function(fz, mf, prp){
  nz <- length(fz)
  nf <- length(feature_names(fz))
  tm <- tibble::tibble( 
                  feature = feature_names(fz),
                  zone = list(attr(fz, "zone_names"))[rep(1,nf)], # all zones can contribute
                  target = mf*prp,  
                  type = rep("absolute", nf)  # specify as absolute targets 
  )
  return(tm)
}