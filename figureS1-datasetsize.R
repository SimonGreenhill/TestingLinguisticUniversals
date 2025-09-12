require(ggplot2, warn.conflicts=FALSE, quietly=TRUE)
require(dplyr, warn.conflicts=FALSE, quietly=TRUE)
require(readr, warn.conflicts=FALSE, quietly=TRUE)
# attach R.oo (dependency of R.utils) here to shut up the
# really annoying verbose messages
require(R.oo, warn.conflicts=FALSE, quietly=TRUE)
require(R.utils, warn.conflicts=FALSE, quietly=TRUE)  # countLines

BTRESULTSDIR <- "../results/brms/"

theme_set(theme_classic(base_size=14))

df.others <- readr::read_tsv('results/BT_results_summary/results.txt', show_col_types=FALSE)

# cleanup
df.others <- df.others %>%
dplyr::select(Universal = code, Universal.shorter, old_lit_size = `Size..review.of.original.`) 

# remove the 23 universals where we do not have recorded size
df.others <- df.others %>% filter(!is.na(old_lit_size))

if(!file.exists("lgs_per_universal_counts.csv")){
  source("count_samples_sizes.R")
}

df.bt <- read_csv("lgs_per_universal_counts.csv", show_col_types = F) %>% 
  dplyr::filter(Universal %in% df.others$Universal )

p <- ggplot() +
  geom_histogram(data = df.others[c('Universal', 'old_lit_size')], aes(x=old_lit_size),fill = "steelblue", color = "steelblue", alpha = 0.7, bins = 30) +
  geom_histogram(data = df.bt[c('Universal', 'BT_n')], aes(x=BT_n),fill = "tomato", color = "tomato", alpha = 0.7, bins = 30) +
  xlab("Number of Languages") +
  ylab("Frequency")

ggsave('figureS1-datasetsize.png', p, width=7, height=4)
ggsave('figureS1-datasetsize.pdf', p, width=7, height=4)
