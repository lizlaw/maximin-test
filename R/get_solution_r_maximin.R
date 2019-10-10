# get (category layer) solution

get_solution_r_maximin <- function(sol, nft, p0, ss){
  nc <- number_of_planning_units(p0)
  nz <- number_of_zones(p0)
  rs <- stack(replicate(nz, ss))
  rs[] <- sol$x[-c(1:(nft+1))] 
  return(category_layer(rs))
  }
