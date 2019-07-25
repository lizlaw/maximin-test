# get (category layer) solution

get_solution_r_maximin <- function(sol, p0, ss){
  nc <- number_of_planning_units(p0)
  nz <- number_of_zones(p0)
  rs <- stack(replicate(nz, ss))
  rs[] <- sol$x[-1][1:(nc*nz)] 
  return(category_layer(rs))
  }
