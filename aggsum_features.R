# aggsum_features
# aggregate and some select features

aggsum_features <- function(fz, fi){
  map(fz, function(x){ x[[fi]] %>% sum() }) %>% 
  unlist() %>% 
  stack()
}