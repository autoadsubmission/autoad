#' Fit Hydra model recommender
#'
#' Bi-headed model that uses UMAP and clustering to detect if new datasets to output recommendations to are close or far from existing datasets in the
#' meta-feature space. It uses a URegression model for novel datasets and a cFact recommender for existing datasets.
#'
#' @param train_performance A data frame or matrix corresponding one row per dataset and one column per configuration, with performance values on each cell.
#' @param train_metafeatures A a data frame containing the metafeatures of each dataset as columns.
#'
#' @return A list of class `Hydra` containing four elements: `local_head`, a fitted cFact recommender, `global_head`, a fitted URegression recommender, `train_metafeatures`, the
#' internal data frame used to calculate new datasets distances, and `recommender_type`, a character containing the recommender type ('Hydra').
#' @export
#'
#' @examples
#'
#' dat <- get_sample_data(train_test_split = T)
#' hydra <- recommender('hydra')
#' hydra <- hydra %>% fit(dat$train$performance, dat$train$metafeatures)
#' hydra %>% predict(dat$test$metafeatures)
#' @export
fit.hydra <- function(object,
                      train_performance,
                      train_metafeatures,
                      ...){
  local_head <- recommender('cfact') %>%
    fit(train_performance, train_metafeatures)
  global_head <- recommender('u_regression') %>%
    fit(train_performance, train_metafeatures, n_factors = 2)
  res <- list(local_head = local_head,
              global_head = global_head,
              train_metafeatures = train_metafeatures,
              recommender_type = 'Hydra')
  class(res) <- class(object)
  res
}


#' Recommend models for new datasets using Hydra
#'
#' Model recommendation. Issues a single model recommendation per row in `test_metafeatures`.
#'
#' @param object Object obtained from `fit.hydra()`.
#' @param test_metafeatures A a data frame containing the metafeatures of each dataset as columns.
#'
#' @return A character string corresponding to the recommended model per row on `test_metafeatures`.
#' @export
#'
#' @examples
#'
#' dat <- get_data(train_test_split = T)
#' hydra <- recommender('hydra')
#' hydra <- hydra %>% fit(dat$train$performance, dat$train$metafeatures)
#' hydra %>% predict(dat$test$metafeatures)
predict.hydra <- function(object, test_metafeatures){
  train_metafeatures <- object$train_metafeatures
  train_performance <- object$train_performance
  type_reference <- tidyr::tibble(index = 1:(nrow(train_metafeatures) + nrow(test_metafeatures)),
                                  type = c(rep('train', times = nrow(train_metafeatures)),
                                           rep('test', times = nrow(test_metafeatures))))
  metafeatures <- rbind(train_metafeatures, test_metafeatures)
  # Project train + test data
  scaler <- caret::preProcess(x = metafeatures, method = c('zv', 'center', 'scale'))
  scaled_mfs <- predict(scaler, newdata = metafeatures)
  set.seed(7777)
  complete_data_proj <- uwot::umap(scaled_mfs,
                                   n_components = 10,
                                   metric = 'manhattan',
                                   ret_model = F,
                                   scale = 'none')
  # Cluster projected data
  data_clustering <- dbscan::dbscan(x = complete_data_proj, eps = 1, minPts = 5)
  # Create regression models for clusters in which there are points from the test data
  test_clusters <- data_clustering$cluster[type_reference %>% filter(type == 'test') %>% pull(index)]
  recommenders <- map(unique(test_clusters), function(clust_num){
    train_metafeatures_rows <- which(data_clustering$cluster[type_reference %>% filter(type == 'train') %>% pull(index)] == clust_num)
    if(length(train_metafeatures_rows) > 10){
      # With enough samples, we train a regression recommender
      return(object$local_head)
    }else{
      # We don't have enough samples to train a recommender, just return the global recommender
      return(object$global_head)
    }
  })
  names(recommenders) <- paste0('C', unique(test_clusters))
  # Predict, per test point, with corresponding recommender
  map_dfr(unique(test_clusters), function(clust_num){
    samples_index <- which(test_clusters == clust_num)
    dplyr::tibble(index = samples_index,
                  recommendation = recommenders[[paste0('C', clust_num)]] %>% predict(test_metafeatures %>% slice(samples_index)))
  }) %>%
    arrange(index) %>%
    pull(recommendation) -> res
  class(res) <- c('recommendation', 'character')
  res
}



