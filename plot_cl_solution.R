# plot raster category layer solution

plot_cl_solution <- function(sr){
  spdf <- as(rs, "SpatialPixelsDataFrame")
  df <- as.data.frame(spdf)
  colnames(df) <- c("Zone", "x", "y")
  ggplot() +  
    geom_raster(data=df, aes(x=x, y=y, fill=factor(Zone))) + 
    scale_fill_viridis_d() + guides(fill = guide_legend(title = "Zone", title.position = "top", title.hjust = 0.5)) +
    coord_equal() +
    theme_map() +
    theme(legend.position="bottom", legend.justification = 'centre') 
}
