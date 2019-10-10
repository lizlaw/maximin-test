# Simple version of using prioritizr to compile problems for gurobi, and modifying 
# the problem to include maximum, maximin, & multiple objectives, over a range of 
# agriculture and carbon targets

# problem has 3 main trade/off dimensions: agricultural production, carbon mitigation, and biodiversity
# biodiversity is comprised of multiple species, however. In previous versions we could maximise the 
# aggregate score for biodiversity, and/or achieve targets for features.

# maximise allows us to maximise aggregate biodiversity without needing to convert to a minimum problem.
# maximin allows us to maximise the minimum achievement for a set of features. 

# libraries
library(prioritizr)
library(gurobi)
library(tidyverse)
#library(rlist)
library(Matrix)
library(ggthemes)

# Make test data  =====================================

# for the features, we can use the default already created zone feature layer from prioritizr:
## 10 x 10 cells
## 5 features, feature_1 : 5 
## 3 zones each cell can be in
sim_features_zones
# plot these
source('R/plot_featurezones.R')
plot_featurezones(sim_features_zones) 

# create an 'aggregate biodiversity' layer to maximize
# for each of the 3 zone stacks, sum over the last 3 features
source('R/aggsum_features.R')
sum_spp <- aggsum_features(sim_features_zones, 3:5) 
plot(sum_spp)

# we have allowed and not allowed transitions
# transitions are a raster stack, nzones layers long, binary (0) not allowed, (1) allowed. 
# Cells must be allowed in at least one of the zones
source('R/sim_transitionstack.R')
transitions <- sim_transitionstack(sim_features_zones, 0.9)
puid <- sim_puid(sim_features_zones)

# now we convert it into the right format:
source('R/locked_dataframe_create.R')
locked_dataframe <- locked_dataframe_create(transitions, puid)

# need max feat, as targets are based off these (ie all scaled to % of relative maximum)
source('R/calculate_max_feat.R')
max_feat <- calculate_max_feat(sim_features_zones)

# create manual target tibble, with any starting prop (or vector of props over length max_feat)
source('R/create_mtargets.R')
mtgts0 <- create_mtargets(sim_features_zones, max_feat, 0.1)

# create a start solution
# seems to always run better if coming from a start solution, particularly for larger problems
# I typically use current landuse as start solution
source('R/sim_start.R')
start_solution <- sim_start(transitions)
plot(start_solution)

# create the base problem in prioritizr
# only developed for problems with mandatory allocations and binary decisions!
p0 <- problem(sum_spp, sim_features_zones) %>% 
  add_manual_targets(mtgts0) %>%
  add_manual_locked_constraints(locked_dataframe) %>%
  add_mandatory_allocation_constraints() %>%
  add_min_set_objective() %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0.01, presolve = 2, time_limit = 1000)

# compile the problem for gurobi via prioritizr  
o <- compile(p0)

# recompile for gurobi ===================================

# to solve as a maximize problem only (maximize aggregate biodiversity, st all other targets)
# reconfigure the problem
source('R/convert_min2max.R')
gm_max <- convert_min2max(o, start_solution, 'test_max') 

# create gurobi parameters list
gp <- list(Presolve = -1, Timelimit = 1000, MIPGap = 0.01, IntFeasTol = 0.001)

# test solve problem using gurobi directly
solution <- gurobi(gm_max, gp)

# to solve as a maxi-min problem only (maximize the minimum feature target achievement)  
# reconfigure the problem (compiled min problem, focal feats, start solution, name)
source('R/convert_min2maximin.R')
gm_maximin <- convert_min2maximin(o, c(2:4), start_solution, 'test_maximin')
solution <- gurobi(gm_maximin, gp)

# which constraints are binding/more flexible?
# the first nft bars are the zconstraints, and then there are nf others. Here it is the first feature target that is constraining the solution
source('R/slack_plot_maximin.R')
slack_plot_maximin(solution, p0, 3) 

# what is the solution? create a template from the start solution, paste in the solution values (just one nz*nc set), and convert. 
source('R/get_solution_r_maximin.R')
source('R/plot_cl_solution.R')
rs <- get_solution_r_maximin(solution, 3, p0, start_solution)
plot_cl_solution(rs)
