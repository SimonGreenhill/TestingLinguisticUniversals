require(ggplot2, warn.conflicts=FALSE, quietly=TRUE)
require(dplyr, warn.conflicts=FALSE, quietly=TRUE)
require(readr, warn.conflicts=FALSE, quietly=TRUE)
# attach R.oo (dependency of R.utils) here to shut up the
# really annoying verbose messages
require(R.oo, warn.conflicts=FALSE, quietly=TRUE)
require(R.utils, warn.conflicts=FALSE, quietly=TRUE)  # countLines

BTRESULTSDIR <- "../results/brms/"

theme_set(theme_classic(base_size=14))

df.others <- readr::read_tsv('../results/results.csv', show_col_types=FALSE)

# cleanup
df.others <- df.others %>%
  select(code, Universal.shorter, `Size (review.of.original)`) %>%
  rename(size=`Size (review.of.original)`)

# remove the 23 universals where we do not have recorded size
df.others <- df.others %>% filter(!is.na(size))


# load from our analyses
filelist <- list.files(BTRESULTSDIR, pattern="BT_data.txt", recursive=TRUE, full.names=TRUE)

# almost as quick as `wc -l`
df.bt <- sapply(filelist, countLines, simplify=TRUE, USE.NAMES=TRUE)
df.bt <- data.frame(filename = names(df.bt), size = df.bt)
df.bt$code <- basename(dirname(df.bt$filename))

rownames(df.bt) <- NULL


df.others$sample <- 'Literature'
df.bt$sample <- 'Grambank'

p <- ggplot() +
  geom_histogram(data = df.others[c('code', 'size', 'sample')], aes(x=size, group=sample),fill = "steelblue", color = "steelblue", alpha = 0.7, bins = 30) +
  geom_histogram(data = df.bt[c('code', 'size', 'sample')], aes(x=size, group=sample),fill = "tomato", color = "tomato", alpha = 0.7, bins = 30) +
  xlab("Number of Languages") +
  ylab("Frequency")

ggsave('figureS1-datasetsize.png', p, width=7, height=4)
ggsave('figureS1-datasetsize.pdf', p, width=7, height=4)
