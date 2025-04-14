source("requirements.R")

universals_type <- read_tsv("universals_types.tsv", show_col_types = F)

fns <- list.files(path = "results", pattern = "summary_clean\\.\\d+", full.names = T, recursive = T)

df_all <- fns %>% 
#  .[1:1000] %>% 
  map_df(
    function(x) data.table::fread(x, sep = "\t", skip = 1, col.names = c("term", "Estimate" , "Est.Error", "l-95% CI",  "u-95% CI" , "Rhat" ,     "Bulk_ESS" , "Tail_ESS",  "Tree" )
) %>% #the colnames in the summary_clean-files aren't right, the first column should have the name "term", but it's missing so everything is off by one. Hard-coding the colnames like this and skipping the first row fixes it.
      dplyr::mutate(filename = x)) 

df_all$universal_code <- stringr::str_match(df_all$filename, "results/(.*?)/brms")[, 2]

df <- df_all %>% 
  filter(term == "fixed_V3") %>% 
  group_by(universal_code) %>% 
  mutate(median_Estimate = median(Estimate), 
            median_l_95_CI = median(`l-95% CI`), 
            median_u_95_CI = median(`u-95% CI`)) %>%
  ungroup() %>% 
  mutate(support = ifelse(`median_l_95_CI` < 0 & `median_u_95_CI` < 0|
                                     `median_l_95_CI`  > 0 & `median_u_95_CI`  > 0  , "yes (supported)", "no (not supported)")) %>% 
  left_join(universals_type, by = "universal_code")

df$Universal.shorter <- fct_reorder(df$Universal.shorter,  df$median_Estimate)

pn <-   df %>%
  filter(Domain_general == "narrow word order") %>% 
  ggplot(mapping = aes(x = Estimate, y =Universal.shorter, fill = desc(median_Estimate), 
                       alpha = support, linetype = as.factor(desc(support))
                       )) +
  geom_errorbar(aes(xmax = `l-95% CI`,
                    xmin = `u-95% CI`,
                    color = support
                    ), width = 0.2, alpha = 0.3, linewidth = 0.09) +
  ggridges::geom_density_ridges(bandwidth = 0.03, rel_min_height = 0.01, linewidth = 0.09) +
  ggdist::stat_dots(color = "black", linewidth = 0.09) +
  geom_vline(aes(xintercept = 0), linetype="dashed", color = "darkgray", alpha = 1) + 
  theme_light() +
  theme(legend.position = "None",
        axis.text = element_text(size = 4),
        title = element_text(size = 8),
        strip.background = element_rect(color = "black",fill = "white"),
        strip.text = element_text(color = "black"),
        axis.title = element_blank()) +
  scale_color_manual(values = c("darkgrey", "steelblue"))  +
  suppressWarnings(scale_alpha_discrete(range = c(0.3, 1)) ) + #use supress warnings to silence "Using alpha for a discrete variable is not advised.". In this case, it makes sense.
  ggtitle("Narrow Word Order")
 

pw <- df %>%
  filter(Domain_general == "broad word order") %>% 
  ggplot(mapping = aes(x = Estimate, y =Universal.shorter, fill = desc(median_Estimate), 
                       alpha = support, linetype = as.factor(desc(support))
  )) +
  geom_errorbar(aes(xmax = `l-95% CI`,
                    xmin = `u-95% CI`,
                    color = support
  ), width = 0.2, alpha = 0.3, linewidth = 0.09) +
  ggridges::geom_density_ridges(bandwidth = 0.03, rel_min_height = 0.01, linewidth = 0.09) +
  ggdist::stat_dots(color = "black", linewidth = 0.09) +
  geom_vline(aes(xintercept = 0), linetype="dashed", color = "darkgray", alpha = 1) + 
  theme_light() +
  theme(legend.position = "None",
        axis.text = element_text(size = 4),
        title = element_text(size = 8),
        strip.background = element_rect(color = "black",fill = "white"),
        strip.text = element_text(color = "black"),
        axis.title = element_blank()) +
  scale_color_manual(values = c("darkgrey", "steelblue"))  +
  suppressWarnings(scale_alpha_discrete(range = c(0.3, 1)) )  +#use supress warnings to silence "Using alpha for a discrete variable is not advised.". In this case, it makes sense.
  ggtitle("Broad Word Order")

ph <- df %>%
  filter(Domain_general == "hierarchy") %>% 
  ggplot(mapping = aes(x = Estimate, y =Universal.shorter, fill = desc(median_Estimate), 
                       alpha = support, linetype = as.factor(desc(support))
  )) +
  geom_errorbar(aes(xmax = `l-95% CI`,
                    xmin = `u-95% CI`,
                    color = support
  ), width = 0.2, alpha = 0.3, linewidth = 0.09) +
  ggridges::geom_density_ridges(bandwidth = 0.03, rel_min_height = 0.01, linewidth = 0.09) +
  ggdist::stat_dots(color = "black", linewidth = 0.09) +
  geom_vline(aes(xintercept = 0), linetype="dashed", color = "darkgray", alpha = 1) + 
  theme_light() +
  theme(legend.position = "None",
        title = element_text(size = 8),
        axis.text = element_text(size = 4),
        strip.background = element_rect(color = "black",fill = "white"),
        strip.text = element_text(color = "black"),
        axis.title = element_blank()) +
  scale_color_manual(values = c("darkgrey", "steelblue"))  +
  suppressWarnings(scale_alpha_discrete(range = c(0.3, 1)) )  +#use supress warnings to silence "Using alpha for a discrete variable is not advised.". In this case, it makes sense.
  ggtitle("Hierarchy")

po <- df %>%
  filter(Domain_general == "other") %>% 
  ggplot(mapping = aes(x = Estimate, y =Universal.shorter, fill = desc(median_Estimate), 
                       alpha = support, linetype = as.factor(desc(support))
  )) +
  geom_errorbar(aes(xmax = `l-95% CI`,
                    xmin = `u-95% CI`,
                    color = support
  ), width = 0.2, alpha = 0.3, linewidth = 0.09) +
  ggridges::geom_density_ridges(bandwidth = 0.03, rel_min_height = 0.01, linewidth = 0.09) +
  ggdist::stat_dots(color = "black", linewidth = 0.09) +
  geom_vline(aes(xintercept = 0), linetype="dashed", color = "darkgray", alpha = 1) + 
  theme_light() +
  theme(legend.position = "None",
        title = element_text(size = 8),
        axis.text = element_text(size = 4),
        strip.background = element_rect(color = "black",fill = "white"),
        strip.text = element_text(color = "black"),
        axis.title = element_blank()) +
  scale_color_manual(values = c("lightgrey", "#2e59e6"))  +
  suppressWarnings(scale_alpha_discrete(range = c(0.3, 1)) ) + #use supress warnings to silence "Using alpha for a discrete variable is not advised.". In this case, it makes sense.
  ggtitle("Other")


p <- ((pn + pw) / (ph + po)) + plot_layout(heights = c(2.5, 1))
  
ggsave(plot = p, filename = "joyplots_barplot_universals.png", width = 17, height = 17, units = "cm", dpi = 300)

