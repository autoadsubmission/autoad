# Computation time ----

library(autoad)
library(reticulate)
use_condaenv('autoad')

microbenchmark::microbenchmark(suppressWarnings(get_metafeatures_metaod(iris[,1:4], show_message = F)))
microbenchmark::microbenchmark(suppressMessages(get_metafeatures_catch22(iris[,1:4], time_var_name = NULL)))
