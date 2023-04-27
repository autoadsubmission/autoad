# Libraries and experiment result loading ----

source(here('paper_scripts', 'experiments', '02_uregression_vectors.R'))

# Analysis ----

experiments$res <- map(experiments$res, bind_rows)
experiments <- experiments %>%
  group_by(strategy, seed_num) %>%
  group_nest()
experiments$perf_matrix <- map(experiments$data, function(x){
  x$res %>%
    bind_rows %>%
    select(dataset, percentile, factors) %>%
    pivot_wider(id_cols = c(dataset),
                names_from = factors,
                values_from = percentile,
                values_fn = mean)
})

# Per strategy we want to compare all factors
# We repeat this five times (one per seed)

# Meta-analysis using average rank as effect size
get_values_for_metamean <- function(df){
  map(df %>% pull(perf_matrix), function(x){
    x %>%
      select(-dataset) %>%
      #scmamp:::rankMatrix() %>%
      as_tibble() %>%
      pivot_longer(cols = everything()) %>%
      rename('factors' = 'name', 'rank' = 'value') %>%
      group_by(factors) %>%
      summarise(n = n(),
                rank_avg = mean(rank),
                rank_sd = sd(rank))
  }) %>% bind_rows()
}
get_estimates <- function(rank_df){
  rank_df %>%
    group_by(factors) %>%
    group_map(., function(x, ...){
      metaobj <- metamean(n = x$n,
                          mean = x$rank_avg,
                          sd = x$rank_sd)
      tibble(factors = x$factors[1],
             estimate = metaobj$TE.fixed,
             lower = metaobj$lower.fixed,
             upper = metaobj$upper.fixed)
    }, .keep = T) %>%
    bind_rows() %>%
    mutate(factors = as.numeric(factors)) %>%
    arrange(factors)
}

ranks_s1 <- experiments %>% filter(strategy == 'stratified') %>% get_values_for_metamean
ranks_s2 <- experiments %>% filter(strategy == 'grouped') %>% get_values_for_metamean


res <- rbind(ranks_s1 %>% get_estimates %>% mutate(scenario = 'Scenario I'),
             ranks_s2 %>% get_estimates %>% mutate(scenario = 'Scenario II'))

cd_s1 <- scmamp:::getNemenyiCD(alpha = 0.05, num.alg = 19, num.problems = 282)
cd_s2 <- scmamp:::getNemenyiCD(alpha = 0.05, num.alg = 19, num.problems = 94)

res$cd <- ifelse(res$scenario == 'Scenario I', cd_s1, cd_s2)


ggplot(res) +
  geom_ribbon(aes(x = factors,
                  ymin = lower,
                  ymax = upper,
                  fill = scenario),
              alpha = 0.2) +
  geom_errorbar(aes(x = factors,
                    ymin = lower,
                    ymax = upper,
                    color = scenario,
                    linetype = scenario)) +
  geom_point(aes(x = factors,
                 y = estimate,
                 color = scenario,
                 shape = scenario),
             size = 3) +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.15, 0.55)) +
  labs(color =  NULL,
       shape = NULL,
       fill = NULL,
       linetype = NULL) +
  xlab('SVD Vectors in URegression') +
  ylab('Average Percentile') -> p1

ggsave(filename = 'uregression_comparison.pdf',
       plot = p1,
       device = 'pdf',
       path = here('experiments', '20230118_paper', 'figures'),
       units = 'cm',
       width = 12,
       height = 7)

