# Libraries and functions ----
library(autoad)
suppressMessages(library(tidyverse))
library(scmamp)
library(furrr)
library(here)
source(here('paper_scripts', 'utils.R'))

prepare_parallelization()
seed_num <- c(7777, 7778, 7779, 7780, 7781)

# Experiments definition ----
experiments <- expand.grid(
   strategy = c("stratified", "grouped"),
   metric = c('pr_auc'),
   mfs_metaod = c(TRUE, FALSE),
   mfs_catch22 = c(TRUE, FALSE),
   mfs_scaled = c(TRUE),
   seed = seed_num,
   stringsAsFactors = F
   ) %>%
  filter(mfs_metaod | mfs_catch22) %>%
  mutate(cache_folder = pmap_chr(list(strategy, metric, mfs_metaod, mfs_catch22, mfs_scaled, seed), create_cache_folder_path))

walk(experiments$cache_folder, dir.create)

rec_types <- c('global_best', 'u_regression', 'u_regression2', 'regression', 'nearest_neighbor', 'cfact', 'metaod', 'hydra')


# Actual experiment execution ----
pwalk(experiments, function(strategy, metric, mfs_metaod, mfs_catch22, mfs_scaled, seed_num, cache_folder){
  experiment_string <- paste0('Strategy: ', strategy, '\n',
                              'Metric: ', metric, '\n',
                              'Folder: ', cache_folder)
  logger::log_info(experiment_string)
  get_evaluations(recommender_types = rec_types,
                  data_strategy = strategy,
                  metric = metric,
                  mfs_metaod = mfs_metaod,
                  mfs_catch22 = mfs_catch22,
                  mfs_scaled = mfs_scaled,
                  cache_folder = cache_folder,
                  overwrite = FALSE,
                  seed_num = seed_num)
})

