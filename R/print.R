#' @export
print.recommendation <- function(rec){
  for(i in 1:length(rec)){
    algorithm <- strsplit(rec[i], '_', fixed = T)[[1]][1]
    print(paste0('Dataset ', i, ': ', algorithm_parser[[algorithm]](rec[i])))
  }
}

algorithm_parser <- list()
algorithm_parser[['rhf']] <- function(x){
  splitted <- strsplit(x, '-', fixed = T)[[1]]
  paste0('RHF (',
                splitted[2], ' trees, ',
                splitted[3], ' max height, ',
                ifelse(as.logical(splitted[4]),
                       yes = 'check duplicates)',
                       no = 'do not check duplicates)'))
}
algorithm_parser[['hst']] <- function(x){
  splitted <- strsplit(x, '-', fixed = T)[[1]]
  paste0('HST (',
         splitted[2], ' trees, ',
         splitted[3], ' max tree depth, ',
         as.numeric(splitted[4])*100, '% of data as initial sample)')
}
algorithm_parser[['isolation_forest']] <- function(x){
  splitted <- strsplit(x, '-', fixed = T)[[1]]
  paste0('Isolation Forest (',
         as.numeric(splitted[3]), '% of data per tree, ',
         splitted[4], ' max tree depth, ',
         splitted[5], ' trees, ',
         splitted[6], ' columns per split)')
}
algorithm_parser[['loda_batch']] <- function(x){
  splitted <- strsplit(x, '-', fixed = T)[[1]]
  paste0('LODA (batch, ',
         splitted[2], ' bins per histogram, ',
         splitted[3], ' random cuts)')
}
algorithm_parser[['lof']] <- function(x){
  splitted <- strsplit(x, '-', fixed = T)[[1]]
  paste0('LOF (',
         'Minkowski exponent = ', splitted[2], ', ',
         'leaf size = ', splitted[3],
         ', number of neighbors = ', splitted[4], ')')
}
algorithm_parser[['xstream']] <- function(x){
  splitted <- strsplit(x, '-', fixed = T)[[1]]
  paste0('xStream (',
         'Random projections of size ', splitted[2], ', ',
         splitted[3], ' chains of depth ', splitted[4], ', ',
         as.numeric(splitted[6])*100, '% of data as initial sample)')
}

#' @export
print.recommender <- function(object){
  print(paste0('Fit recommender of type ', object$recommender_type))
}

