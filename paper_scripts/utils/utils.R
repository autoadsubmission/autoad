get_results <- function(cache_folder){
  files <- list.files(cache_folder)
  res <- map(paste0(cache_folder, '/', files), read_rds)
  names(res) <- files
  #Delete files with errors
  bad_files <- numeric()
  for(i in 1:length(res)){
    if(class(res[[i]]) == 'character') bad_files <- c(bad_files, i)
  }
  res[bad_files] <- NULL
  res
}

create_cache_folder_path <- function(strategy, metric, mfs_metaod, mfs_catch22, mfs_scaled, seed){
  path <- here('paper_scripts', 'cache', seed)
 path <- paste0(path, '/', strategy)
 path <- paste0(path, '_', metric)
 if(mfs_metaod)  path <- paste0(path, '_metaod')
 if(mfs_catch22) path <- paste0(path, '_catch22')
 if(mfs_scaled) path <- paste0(path, '_scaled')
 path
 #paste0(path, '/')
}

get_evaluations <- function(recommender_types, data_strategy, metric, mfs_metaod, mfs_catch22, mfs_scaled = FALSE, cache_folder, overwrite, seed_num){
  # Internal functions ----
  log_start <- function(res_path, rec_type, split_num, overwrite){
    if(!overwrite & file.exists(res_path)){
      logger::log_info('Recommender ', rec_type, ' on split ', split_num, ' already done, skipping')
      return(FALSE)
    }else{
      logger::log_info('Testing recommender ', rec_type, ' on split ', split_num)
      return(TRUE)
    }
  }
  internal_procedure <- function(x, cache_folder, rec_type, split_num, seed_num, overwrite){
    res_path <- paste0(cache_folder, '/', rec_type, '_', split_num, '_', seed_num ,'.rds')
    continue <- log_start(res_path, rec_type, split_num, overwrite)
    if(continue){
      res_internal <- tryCatch(analyze_split(dat_split = x,
                                             rec_type = rec_type),
                               error = function(e) return('There was an error'))
      write_rds(res_internal,
                res_path)
      return(res_internal)
    }else{
      return(NULL)
    }
  }
  # Actual code ----
  dat <- switch(data_strategy,
                "grouped" = get_data_leave_one_provider_out(seed = seed_num,
                                                                           metric = metric,
                                                                           mfs_metaod = mfs_metaod,
                                                                           mfs_catch22 = mfs_catch22,
                                                                           mfs_scaled = mfs_scaled)$data,
                "stratified" = get_data_stratified_kfolds_cv(seed = seed_num,
                                                                metric = metric,
                                                                mfs_metaod = mfs_metaod,
                                                                mfs_catch22 = mfs_catch22,
                                                                mfs_scaled = mfs_scaled)$data)
  map(recommender_types, function(rec_type){
    parallelize <- rec_type %in% c('global_best', 'u_regression', 'u_regression2', 'regression', 'nearest_neighbor', 'cfact', 'metaod')
    if(parallelize){
      res <- future_map2(dat$splits, 1:length(dat$splits), function(x, num){
        internal_procedure(x, cache_folder, rec_type, num, seed_num, overwrite)
      }, .progress = F,
      .options = furrr::furrr_options(seed = TRUE))
    }else{
      res <- map2(dat$splits, 1:length(dat$splits), function(x, num){
        internal_procedure(x, cache_folder, rec_type, num, seed_num, overwrite)
      })
    }
    NULL
  })
}

load_experiments <- function(seed_num, percentiles = FALSE){
  if(percentiles){
    experiments_path <- here('paper_scripts', 'cache', paste0('experiments', seed_num, '_percentiles.rds'))
  }else{
    experiments_path <- here('paper_scripts', 'cache', paste0('experiments', seed_num, '.rds'))
  }
  if(file.exists(experiments_path)){
    experiments <- read_rds(experiments_path)
  }else{
    experiments <- expand.grid(
      strategy = c("stratified", "grouped"),
      metric = c('pr_auc'),
      mfs_metaod = c(TRUE, FALSE),
      mfs_catch22 = c(TRUE, FALSE),
      mfs_scaled = c(TRUE),
      stringsAsFactors = F
    ) %>%
      filter(mfs_metaod | mfs_catch22) %>%
      mutate(cache_folder = pmap_chr(list(strategy, metric, mfs_metaod, mfs_catch22, mfs_scaled, seed_num), create_cache_folder_path)) %>%
      mutate(result = map(cache_folder, function(x){
        print(x)
        if(length(list.files(x)) > 0){
          if(percentiles){
            return(get_results(x) %>% autoad:::get_percentile_matrix())
          }else{
            return(get_results(x) %>% autoad:::get_performance_matrix())
          }
        }else{
          return(NA)
        }
      }))
    write_rds(experiments, experiments_path)
    return(experiments)
  }
}
