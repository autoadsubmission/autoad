# Data loading ----
suppressMessages(library(tidyverse))
library(meta)
library(here)
source(here('paper_scripts', 'utils', 'utils.R'))

experiments <- map(c(7777, 7778, 7779, 7780, 7781), load_experiments, percentiles = TRUE)
names(experiments) <- c('s7777', 's7778', 's7779', 's7780', 's7781')

experiments <- map(experiments,
                   function(x) x %>%
                     select(-metric, -mfs_scaled) %>%
                     filter((mfs_metaod & !mfs_catch22) | (!mfs_metaod & mfs_catch22)))

# 1.MetaOD vs C22 as metafeature sets ----

# We do so with meta-analysis techniques, using proportions as our effect sizes

source(here('paper_scripts', 'utils', 'utils_mfs_comparison.R'))

estimates <- expand.grid(strategy = c('stratified'),
                         recommender = c('hydra', 'regression', 'metaod', 'nearest_neighbor'),
                         stringsAsFactors = F)
estimates <- map2(estimates$strategy, estimates$recommender, get_estimate) %>% bind_rows

# Nearest neighbor in grouped case doesn't have variability, we change it manually
nn_metaod <- (experiments$s7777 %>% filter(strategy == 'grouped', mfs_metaod)  %>% pull(result))[[1]]$nearest_neighbor
nn_c22    <- (experiments$s7777 %>% filter(strategy == 'grouped', !mfs_metaod) %>% pull(result))[[1]]$nearest_neighbor
estimates$estimate[which(estimates$strategy == 'grouped' & estimates$recommender == 'nearest_neighbor' & estimates$type == 'equal')] <- length(which(nn_metaod == nn_c22))/100
estimates$estimate[which(estimates$strategy == 'grouped' & estimates$recommender == 'nearest_neighbor' & estimates$type == 'metaod')] <- length(which(nn_metaod > nn_c22))/100
estimates$estimate[which(estimates$strategy == 'grouped' & estimates$recommender == 'nearest_neighbor' & estimates$type == 'c22')] <- length(which(nn_metaod < nn_c22))/100

estimates$lower[which(estimates$strategy == 'grouped' & estimates$recommender == 'nearest_neighbor' & estimates$type == 'equal')] <- length(which(nn_metaod == nn_c22))/100
estimates$lower[which(estimates$strategy == 'grouped' & estimates$recommender == 'nearest_neighbor' & estimates$type == 'metaod')] <- length(which(nn_metaod > nn_c22))/100
estimates$lower[which(estimates$strategy == 'grouped' & estimates$recommender == 'nearest_neighbor' & estimates$type == 'c22')] <- length(which(nn_metaod < nn_c22))/100

estimates$upper[which(estimates$strategy == 'grouped' & estimates$recommender == 'nearest_neighbor' & estimates$type == 'equal')] <- length(which(nn_metaod == nn_c22))/100
estimates$upper[which(estimates$strategy == 'grouped' & estimates$recommender == 'nearest_neighbor' & estimates$type == 'metaod')] <- length(which(nn_metaod > nn_c22))/100
estimates$upper[which(estimates$strategy == 'grouped' & estimates$recommender == 'nearest_neighbor' & estimates$type == 'c22')] <- length(which(nn_metaod < nn_c22))/100

paste_intervals <- function(estimate, lower, upper){
  paste0('[', lower*100, ', ', estimate*100, ', ', upper*100, ']')
}

estimates %>%
  pivot_wider(id_cols = c('strategy', 'recommender'),
              names_from = type,
              values_from = c(estimate, lower, upper)) %>%
  mutate(metaod = pmap_chr(list(estimate_metaod, lower_metaod, upper_metaod), paste_intervals),
         c22 = pmap_chr(list(estimate_c22, lower_c22, upper_c22), paste_intervals),
         equal = pmap_chr(list(estimate_equal, lower_equal, upper_equal), paste_intervals)) %>%
  select(strategy, recommender, equal, metaod, c22) %>%
  arrange(strategy)
