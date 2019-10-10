# solution slack

# the first nft bars are the zconstraints, and then there are nf others. Here it is the first feature target that is constraining the solution

slack_plot_maximin <- function(sol, p0, nft, c1 = 'red', c2 = 'blue'){
  nf <- number_of_features(p0)
  slack <- solution$slack[1:(nf+nft)] %>% abs()
  fname <- c(paste('max_tf', 1:nft), paste('target', 1:nf))
  clrs <- c(rep(c1, nft), rep(c2, nf))
  svs <- tibble(fname, slack) 
  ggplot(svs, aes(fname, slack)) + 
    geom_col(fill = clrs, colour = clrs) + 
    theme_light() +
    labs(x = 'Constraint', y = 'Slack / surplus')
}