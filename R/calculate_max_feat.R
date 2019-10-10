# funtion to calculate max feat:
# the sum of each feature, when land use is maximal for that feature, across the study region
# note this assumes feats have same extent (inc NAs) as pu layer

calculate_max_feat <- function(zones_object) { 
  nzones <- length(zones_object)
  nfeats <- nlayers(zones_object[[1]])
  featmax <- numeric()
  for (i in 1:nfeats){
    tempstack <- stack()
    for (j in 1:nzones){
      tempstack <- addLayer(tempstack, zones_object[[j]][[i]])
    }
    featmax[i] <- cellStats(max(tempstack), sum)
  }
  return(featmax)
}


