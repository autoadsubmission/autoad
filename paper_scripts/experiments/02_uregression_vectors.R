# Libraries and functions ----
library(autoad)
suppressMessages(library(tidyverse))
library(scmamp)
library(furrr)
library(here)
library(meta)
source(here('paper_scripts', 'utils', 'utils.R'))

# Experiment loading / execution ----
result_path <- here('paper_scripts', 'cache_uregression', 'results.rds')

if(file.exists(result_path)){
  experiments <- expand.grid(
    strategy = c("stratified", "grouped"),
    factors = 2:20,
    seed_num = c(7777, 7778, 7779, 7780, 7781),
    stringsAsFactors = F
  )
  results <- read_rds(result_path)
  experiments$res <- results
  rm(results)
}else{
  prepare_parallelization()

  # Experiments definition ----
  experiments <- expand.grid(
    strategy = c("stratified", "grouped"),
    factors = 2:20,
    seed_num = c(7777, 7778, 7779, 7780, 7781),
    stringsAsFactors = F
  )

  # Actual experiment execution ----
  results <- pmap(list(experiments$strategy, experiments$factors, experiments$seed_num),
                  function(strategy, factors, seed_num){
                    logger::log_info('Strategy: ', strategy, ', factors: ', factors, ', seed number: ', seed_num)
                    dat <- switch(strategy,
                                  "grouped" = get_data_leave_one_provider_out(seed = seed_num,
                                                                              metric = 'pr_auc',
                                                                              mfs_metaod = FALSE,
                                                                              mfs_catch22 = TRUE,
                                                                              mfs_scaled = TRUE)$data,
                                  "stratified" = get_data_stratified_kfolds_cv(seed = seed_num,
                                                                               metric = 'pr_auc',
                                                                               mfs_metaod = FALSE,
                                                                               mfs_catch22 = TRUE,
                                                                               mfs_scaled = TRUE)$data)
                    res <- future_map(dat$splits, function(dat_split){
                      d <- autoad:::get_data_from_split(dat_split)
                      # Apply and evaluate
                      set.seed(seed_num)
                      recommender('u_regression') %>%
                        fit(train_performance = d$train$performance,
                            train_metafeatures = d$train$metafeatures,
                            n_factors = factors) %>%
                        predict(d$test$metafeatures) %>%
                        evaluate_recommendations(., d$test$performance) %>%
                        mutate(provider = rsample::testing(dat_split)$provider,
                               dataset = rsample::testing(dat_split)$dataset,
                               factors = factors)
                      #})
                    }, .progress = F, .options = furrr::furrr_options(seed = TRUE))
                    res
                  })

  experiments$res <- results

  write_rds(experiments, result_path)
}
