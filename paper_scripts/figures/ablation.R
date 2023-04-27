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

# Ablation study ----


s1 <- map_dfr(experiments, function(x){
  x <- x %>% filter(strategy == 'stratified')
  result <- map(x$result, function(x){
    x <- x %>% select(cfact, hydra, u_regression2)
    names(x) <- c('CFact', 'Hydra', 'UReg(2)')
    x
  })
  result
})

s2 <- map_dfr(experiments, function(x){
  x <- x %>% filter(strategy == 'grouped')
  result <- map(x$result, function(x){
    x <- x %>% select(cfact, hydra, u_regression2)
    names(x) <- c('CFact', 'Hydra', 'UReg(2)')
    x
  })
  result
})

pdf(here('paper_scripts', 'figures', 'ablation_scenario1.pdf'),
    width = 6,
    height = 3)
scmamp::plotCD(s1)
title('Scenario I')
dev.off()

pdf(here('paper_scripts', 'figures', 'ablation_scenario2.pdf'),
    width = 6,
    height = 3)
scmamp::plotCD(s2)
title('Scenario II')
dev.off()


