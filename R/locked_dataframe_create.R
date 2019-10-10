# create a lock dataframe from a stack of transition rasters (1 = allowed, 0 = not, for each zone)
# transition raster stack names must be = zone names

locked_dataframe_create <- function(transition_rasters, puid_raster){
  # should first check if NA and extent the same. this not done here.
  locked_dataframe <- data.frame( pu = numeric(),
                                  zone = character(),
                                  status = numeric()
  )
  
  for (i in 1:nlayers(transition_rasters)){
    # want only to lock OUT, where not allowed to transition
    zone_raster <- transition_rasters[[i]] 
    zname <- names(transition_rasters)[i]
    pui <- puid_raster[which(values(zone_raster)==0)]
    
    locked_dataframe <- rbind( locked_dataframe,
                               data.frame( pu = pui,
                                           zone = zname,
                                           status = 0
                               )
    )
  }
  return(locked_dataframe)
}