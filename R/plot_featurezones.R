# plot_featurezones

plot_featurezones <- function(fz){
  par_old <- par()
  par(mar=c(2,2,2,2))
  nz<- length(fz)
  nf <- raster::nlayers(fz[[1]])
  par(mfrow = c(nz, nf))
  for (i in 1:nz){
    for (j in 1:nf){
      plot(fz[[i]][[j]],
           main = paste0("Zone ", i, " (species ", j, ")" ))
    }}
  suppressWarnings(par(par_old))
}