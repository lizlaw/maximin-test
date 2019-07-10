
# takes in a compiled model (cm) and a start solution (ss) and a model name (mn)
# converts to a maximize objective

convert_min2max <- function(cm, ss, mn){
  list(
    modelname = mn,
    modelsense = "max", # convert this to max
    obj = cm$obj(),
    rhs = cm$rhs(),      # the first nf of these are the absolute targets.
    lb = cm$lb(),
    ub = cm$ub(),
    vtype = cm$vtype(),
    sense = cm$sense(),
    A = cm$A(),
    Start = ss)  # add start solution
}