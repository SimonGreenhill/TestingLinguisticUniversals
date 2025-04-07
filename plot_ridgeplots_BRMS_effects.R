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
  mutate(straddle_zero_95 = ifelse(`l-95% CI` < 0 & `u-95% CI` < 0|
                                     `l-95% CI`  > 0 & `u-95% CI`  > 0  , "no", "yes")) %>% 
  group_by(universal_code) %>% 
  mutate(mean_Estimate = mean(Estimate)) %>% 
  group_by(universal_code, straddle_zero_95) %>% 
  mutate(n = n()) %>% 
  left_join(universals_type, by = "universal_code")

df_prop <- df %>% 
  distinct(Universal.shorter, straddle_zero_95, n) %>% 
  pivot_wider(names_from = straddle_zero_95, values_from = n, id_cols = `Universal.shorter`) %>% 
  mutate(yes = ifelse(is.na(yes), 0, yes)) %>% 
  mutate(no = ifelse(is.na(no), 0, no)) %>% 
  mutate(prop = no / (yes + no)) %>% 
#  dplyr::select(universal_code, prop) %>% 
  mutate(support = ifelse(prop >= 0.9, "yes (supported)", "no (not supported)")) %>% 
  mutate(label = paste0(Universal.shorter, " (", prop * 100, "%)"))

df <- df %>% 
  full_join(df_prop, by = "Universal.shorter") 


df$label <- fct_reorder(df$label,  df$mean_Estimate)

joyplot <-   df %>%
  ggplot(mapping = aes(x = Estimate, y =label, fill = desc(mean_Estimate), 
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
        strip.background = element_rect(color = "black",fill = "white"),
        strip.text = element_text(color = "black"),
        axis.title = element_blank()) +
  scale_color_manual(values = c("red", "darkgrey"))  +
  suppressWarnings(scale_alpha_discrete(range = c(0.3, 1)) ) + #use supress warnings to silence "Using alpha for a discrete variable is not advised.". In this case, it makes sense.
  facet_grid(Domain_general~., scales="free", space="free_y")

joyplot

#barplot <- df %>% 
#  distinct(universal_code,Universal.shorter, label, straddle_zero_95, n, Domain_general) %>% 
#    ggplot(aes(y = label, x = n/100, fill = straddle_zero_95)) +
#    geom_bar( color = "black", stat = "identity") +
#  scale_fill_manual(values = c("#2A5880", "whitesmoke"))  +
#  theme_light() +
#  theme(legend.position = "none", 
#        axis.text.y = element_blank(),
#        strip.background = element_rect(color = "black",fill = "white"),
#        strip.text = element_text(color = "black"),
#        axis.title = element_blank()) +
#  scale_x_continuous(labels = scales::percent) +
#  facet_grid(Domain_general~., scales="free", space="free_y")

#p <- grid.arrange(joyplot, barplot, ncol = 2, widths = c(2, 0.8))
  
ggsave(plot = joyplot, filename = "joyplots_barplot_universals.png", width = 10, height = 25, units = "cm", dpi = 300)

