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

# 2. Recommenders comparison ----

experiments <- map(experiments,
                   function(x) x %>% filter(mfs_catch22 & !mfs_metaod))

estimates <- map_dfr(experiments,
                     function(x){
                       map2_dfr(x$strategy, x$result, function(scenario, y){
                         y %>%
                           pivot_longer(cols = -c(dataset, index)) %>%
                           group_by(name) %>%
                           summarise(perc_mean = mean(value), perc_sd = sd(value), n = n()) %>%
                           mutate(scenario = scenario)
                       })
                     }) %>%
  group_by(name, scenario) %>%
  group_map(., function(x, ...){
    obj <- metamean(n = x$n,
                    mean = x$perc_mean,
                    sd = x$perc_sd)
    tibble(scenario = x$scenario[1],
           recommender = x$name[1],
           estimate = obj$TE.fixed,
           lower = obj$lower.fixed,
           upper = obj$upper.fixed)
  }, .keep = T) %>%
  bind_rows %>%
  arrange(scenario, estimate) %>%
  filter(recommender %in% c('metaod', 'global_best', 'nearest_neighbor', 'regression', 'hydra'))

estimates$recommender <- factor(estimates$recommender,
                                levels = c('hydra', 'regression', 'global_best', 'metaod', 'nearest_neighbor'))
levels(estimates$recommender) <- c('Hydra', 'Regression', 'Global Best', 'MetaOD', 'kNN')
estimates$scenario <- factor(estimates$scenario,
                             levels = c('stratified', 'grouped'))
levels(estimates$scenario) <- c('Scenario I', 'Scenario II')

p1 <- ggplot(estimates) +
  geom_point(aes(x = recommender, y = estimate)) +
  geom_errorbar(aes(x = recommender,
                    ymin = lower,
                    ymax = upper)) +
  facet_wrap(scenario ~ .) +
  xlab('Recommender') +
  ylab('Average Percentile') +
  #ylim(c(40, 100)) +
  cowplot::theme_cowplot() +
  geom_text(aes(x = recommender,
                y = lower - 2,
                label = round(estimate, digits = 1)))


ggsave(filename = 'recommender_comparison.pdf',
       plot = p1,
       device = 'pdf',
       path = here('paper_scripts', 'figures'),
       units = 'cm',
       width = 27,
       height = 10)
