library(ggplot2)
library(ggrepel)
library(dplyr)
library(patchwork)
library(forcats)

BAYES_FACTOR_THRESHOLD <- 10
base_size = 10
size = point_size = 0.2

# better plot
df.bayestraits <- readr::read_tsv("results/BT_results_summary/results.txt", show_col_types=FALSE) %>%
  dplyr::filter(bmrs_support == "yes") %>% 
  dplyr::select(code, Universal.shorter, 
                Domain_general,  
                median_BF,
                o95HDI_lower, o95HDI_higher, 
                o95HDI_SIG)

pn <- df.bayestraits %>% 
  filter(Domain_general == "narrow word order") %>% 
  ggplot(aes(x = median_BF,
             y =   forcats::fct_reorder(Universal.shorter, o95HDI_lower),
    xmax=o95HDI_higher, xmin=o95HDI_lower,
    color=o95HDI_SIG
    )) +
    geom_pointrange(size = point_size) +
    geom_vline(xintercept=BAYES_FACTOR_THRESHOLD) +
    theme_classic(base_size=base_size) +
    scale_color_manual("Significant", values=c("gray", "steelblue")) +
    ylab(NULL) +
    xlab("Bayes Factor") +
    guides(color="none") +
  ggtitle("Narrow Word Order")


pw <- df.bayestraits %>% 
  filter(Domain_general == "broad word order") %>% 
  ggplot(aes(x = median_BF,
             y =   forcats::fct_reorder(Universal.shorter, o95HDI_lower),
             xmax=o95HDI_higher, xmin=o95HDI_lower,
             color=o95HDI_SIG
  )) +
  geom_pointrange(size = point_size) +
  geom_vline(xintercept=BAYES_FACTOR_THRESHOLD) +
  theme_classic(base_size=base_size) +
  scale_color_manual("Significant", values=c("gray", "steelblue")) +
  ylab(NULL) +
  xlab("Bayes Factor") +
  guides(color="none") +  
  ggtitle("Broad Word Order")


ph <- df.bayestraits %>% 
  filter(Domain_general == "hierarchy") %>% 
  ggplot(aes(x = median_BF,
             y =   forcats::fct_reorder(Universal.shorter, o95HDI_lower),
             xmax=o95HDI_higher, xmin=o95HDI_lower,
             color=o95HDI_SIG
  )) +
  geom_pointrange(size = point_size) +
  geom_vline(xintercept=BAYES_FACTOR_THRESHOLD) +
  theme_classic(base_size=base_size) +
  scale_color_manual("Significant", values=c("gray", "steelblue")) +
  ylab(NULL) +
  xlab("Bayes Factor") +
  guides(color="none") +  
  ggtitle("Hierarchy")

po <- df.bayestraits %>% 
  filter(Domain_general == "other") %>% 
  ggplot(aes(x = median_BF,
             y =   forcats::fct_reorder(Universal.shorter, o95HDI_lower),
             xmax=o95HDI_higher, xmin=o95HDI_lower,
             color=o95HDI_SIG
  )) +
  geom_pointrange(size = point_size) +
  geom_vline(xintercept=BAYES_FACTOR_THRESHOLD) +
  theme_classic(base_size=base_size) +
  scale_color_manual("Significant", values=c("gray", "steelblue")) +
  ylab(NULL) +
  xlab("Bayes Factor") +
  guides(color="none") +  
  ggtitle("Other")

p <- ((pn + ph) / (pw + po)) + plot_layout(heights = c(2.5, 1))

ggsave(filename="figure2-bayesfactors-pointrange.png",width = 22, height = 27, units = "cm", dpi = 300, plot = p)
ggsave(filename="figure2-bayesfactors-pointrange.pdf",width = 22, height = 27, units = "cm", dpi = 300, plot = p)
ggsave(filename="figure2-bayesfactors-pointrange.tiff",width = 22, height = 27, units = "cm", dpi = 300, plot = p)

