library(tidyverse)
library(autoad)
library(uwot)
library(here)

mfs <- get_precomputed_metafeatures(include_metaod = FALSE,
                                    include_c22 = TRUE)

Source <- mfs$provider
mfs <- mfs %>% select(-(1:4))
set.seed(7777)
proj <- umap(mfs)

p <- ggplot() +
  geom_jitter(aes(x = proj[,1],
                 y = proj[,2],
                 color = Source,
                 shape = Source),
             size = 2,
             alpha = 0.75,
             width = 0.8,
             height = 0.8) +
  xlab('') +
  ylab('') +
  cowplot::theme_cowplot() +
  theme(legend.position = c(0.7, 0.75),
        axis.title = element_blank())

ggsave(filename = 'datasets.pdf',
       plot = p,
       device = 'pdf',
       path = here('paper_scripts', 'figures'),
       units = 'cm',
       width = 12,
       height = 8)
