# Functions

estimate_variability_across_seeds <- function(exps_list, strat, recommender, metaod = T){
  internal_df <- map_dfr(exps_list, function(x){
    vars <- c('dataset', recommender)
    x <- x %>%
      filter(strategy == strat,
             mfs_metaod == metaod)
    if(!(recommender %in% names(x$result[[1]]))) return(NULL)
    x$result[[1]] <- x$result[[1]] %>% select(all_of(vars))
    x$result[[1]]
    })
  names(internal_df) <- c('dataset', 'percentile')
  internal_df %>%
    group_by(dataset) %>%
    summarise(sds = sd(percentile, na.rm = T)) %>%
    pull(sds) %>%
    mean(., na.rm = T) %>%
    round
}

get_variability_df <- function(){
  variability_df <- expand.grid(strat = c('stratified', 'grouped'),
                                recommender = c('hydra', 'regression', 'metaod', 'nearest_neighbor'),
                                metaod = c(TRUE, FALSE),
                                stringsAsFactors = F) %>%
    mutate(variability = pmap_dbl(list(strat,
                                       recommender,
                                       metaod),
                                  estimate_variability_across_seeds,
                                  exps_list = experiments))
  names(variability_df)[1:3] <- c('scenario', 'recommender', 'M_MetaOD')
  variability_df
}

variability_df <- get_variability_df()
lookup_max_difference <- function(strat, rec){
 variability_df %>% filter(scenario == strat, recommender == rec) %>% pull(variability) %>% max()
}

extract_proportions_both <- function(exps_list, strat, recommender){
  max_diff <- lookup_max_difference(strat, recommender)
  exps_list <- map(exps_list, function(x) x %>% filter(strategy == strat))
  map_dfr(exps_list, function(x){
    res <- abs(x$result[[1]][[recommender]] - x$result[[2]][[recommender]])
    data.frame(event = length(which(res < max_diff)), n = length(res))
  })
}

extract_proportions_single <- function(exps_list, strat, recommender, metaod_positive = T){
  max_diff <- lookup_max_difference(strat, recommender)
  exps_list <- map(exps_list, function(x) x %>% filter(strategy == strat))
  map_dfr(exps_list, function(x){
    res_metaod <- (x %>% filter(mfs_metaod) %>% pull(result))[[1]] %>% pluck(recommender)
    res_catch22  <- (x %>% filter(mfs_catch22) %>% pull(result))[[1]] %>% pluck(recommender)
    res <- res_metaod - res_catch22
    if(!metaod_positive) res <- -res
    data.frame(event = length(which(res >= max_diff)), n = length(res))
  })
}

get_conf_interval <- function(props){
  if(nrow(distinct(props)) == 1){
    prop <- round(props$event[1]/props$n[1], digits = 2)
    return(data.frame(estimate = prop,
                      lower    = prop,
                      upper    = prop))
  }else{
    props <- props %>% filter(event != 0)
    obj <- metaprop(event = props$event,
                    n = props$n,
                    studlab = 1:nrow(props))
    return(data.frame(estimate = round(boot::inv.logit(obj$TE.common), digits = 2),
               lower    = round(boot::inv.logit(obj$lower.common), digits = 2),
               upper    = round(boot::inv.logit(obj$upper.common), digits = 2)))
  }
}

get_estimate <- function(strat, rec){
  res <- rbind(extract_proportions_single(exps_list = experiments,
                                          strat = strat,
                                          recommender = rec,
                                          metaod_positive =  T) %>% get_conf_interval,
               extract_proportions_single(exps_list = experiments,
                                          strat = strat,
                                          recommender = rec,
                                          metaod_positive =  F) %>% get_conf_interval,
               extract_proportions_both(exps_list = experiments,
                                        strat = strat,
                                        recommender = rec) %>% get_conf_interval)
  res$strategy <- strat
  res$recommender <- rec
  res$type <- c('metaod', 'c22', 'equal')
  res %>% select(strategy, recommender, type, estimate, lower, upper)
}


