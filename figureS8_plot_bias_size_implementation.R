library(tidyverse)
library(patchwork)

df <- readr::read_tsv("results/BT_results_summary/results.txt", show_col_types=FALSE) %>% 
  dplyr::select(universal_code = code, 
               Size = `Size..review.of.original.`,
               `Implementation` = `Implementation..review.of.original..1`, 
               Bias.3, supported, brms.support = brmsQ_Fixed_V3_SIG) %>% 
  mutate(supported_new = ifelse(supported == "NOT SIG" & brms.support == "SIG", "supported in sp BRMS, but not in BT", supported)) %>% 
  mutate(supported_new = ifelse(supported_new == "NOT SIG", "not supported", supported_new)) %>% 
  mutate(supported_new = ifelse(supported_new == "SIG", "supported in sp BRMS and BT", supported_new)) 
  
df_plot_S8 <- df %>%
    mutate(Sample_size = ifelse(Size >= 75, ">=75", NA)) %>%
    mutate(Sample_size = ifelse(Size < 75, "<75", Sample_size)) %>%
    mutate(Sample_size = ifelse(is.na(Size), "unclear", Sample_size)) %>%
    mutate(Bias = ifelse(`Bias.3` == "bias towards Eurasia"|
                             `Bias.3` == "exclusively samples one family or area"|
                             `Bias.3` == "bias against the Pacific, PNG, Australia, and/or South America"   , "regional", NA  )) %>%
    mutate(Bias = ifelse(`Bias.3` == "no bias"   , "no bias", Bias  )) %>%
    mutate(Bias = ifelse(is.na(Bias.3)  , "unclear", Bias  )) %>%
    mutate(Implementation = ifelse(Implementation == "less than OK", "imperfect", Implementation )) %>%
    mutate(Implementation = ifelse(Implementation == "OK", "imperfect", Implementation )) %>%
    dplyr::select(universal_code, Sample_size, Bias, Implementation, supported_new, supported)

df_plot_S8$supported_new <- factor(df_plot_S8$supported_new, levels=c("supported in sp BRMS and BT", "supported in sp BRMS, but not in BT", "not supported"))

plot_bar <- function(df, label_legend=FALSE, label_axis=FALSE, label_model=TRUE, var = NULL) {

        ggplot(df, aes({{var}}, group=supported_new, fill=supported_new), alpha = 0.8) +
        geom_bar(color= "black") +
        scale_fill_manual(values=c( "#3a4c40", "#8ccc7c", "#cdcfcc")) +
#        scale_fill_manual(values=c( "#2A2E87", "steelblue", "lightgray")) +
        xlab(NULL) +
        scale_y_continuous(
            #    limits=c(0, length(levels(df$Universal.shorter))),
            breaks=c(0, 20, 40, 60, 80, 100)) +
        coord_flip() +
        theme_classic() +
        theme(title = element_text(size=22),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size = 18),
              axis.title.x = element_blank(),
              legend.title = element_blank(),
              axis.text.y = element_text(size=22),
              legend.position=if (label_legend) 'right' else 'none',
         )
}



p.bias_no_bias <- plot_bar(subset(df_plot_S8, Bias == 'no bias'),  label_legend=FALSE,
         label_axis= TRUE, label_model=FALSE, var = Bias) +
    ggtitle("a. Bias")

p.bias_regional <- plot_bar(subset(df_plot_S8, Bias == 'regional'),label_legend=FALSE, label_axis= TRUE, label_model=FALSE, var = Bias)

p.bias_unclear <- plot_bar(subset(df_plot_S8, Bias == 'unclear'), label_legend=FALSE, label_axis= TRUE, label_model=FALSE, var = Bias)

p.bias <- (p.bias_no_bias  / p.bias_regional / p.bias_unclear)
##

##
p.size_small <- plot_bar(subset(df_plot_S8, Sample_size == '<75'), label_legend=FALSE, label_axis= TRUE, label_model=FALSE, var = Sample_size) +
    ggtitle("b. Sample size")

p.size_large <- plot_bar(subset(df_plot_S8, Sample_size == '>=75'),  label_legend=FALSE, label_axis= TRUE, label_model=FALSE, var = Sample_size)

p.size_unclear <- plot_bar(subset(df_plot_S8, Sample_size == 'unclear'),  label_legend=TRUE, label_axis= TRUE, label_model=FALSE, var = Sample_size)

##

p.implementation_imperfect <- plot_bar(subset(df_plot_S8, Implementation == 'imperfect'), label_legend=FALSE,
                           label_axis= TRUE, label_model=FALSE, var = Implementation) +
    ggtitle("c. Implementation")

p.implementation_perfect <- plot_bar(subset(df_plot_S8, Implementation == 'perfect'), label_legend=FALSE,
                                       label_axis= TRUE, label_model=FALSE, var = Implementation)

p.implementation <- (p.implementation_imperfect  /p.implementation_perfect/ plot_spacer())


p.size <- (p.size_small/p.size_large/p.size_unclear)

##
p <- ( p.bias |p.size |p.implementation)  +
  plot_layout(guides = "collect") & 
  theme(axis.title.x = element_blank())  # No legend in these plots

p

width = 20
height = 8

ggsave(filename="figures_SI8_barplot.png", height=height, width=width, dpi = 300,
       plot = p)

ggsave(filename="figures_SI8_barplot.tiff", height=height, width=width, dpi = 300,
       plot = p)


#grDevices::cairo_pdf(file="figures_SI8_barplot.pdf", height=height, width= width,)
#plot(p)
#x <- dev.off()

#GLM
df_for_glm <- df_plot_S8 %>% 
  filter(supported_new != "supported in BRMS but not in BT") %>% 
  mutate(across(all_of(c("Sample_size", "Bias", "Implementation", "supported")), as.factor))

#df_for_glm$supported <- factor(df_for_glm$supported, levels = c("SIG"   ,  "NOT SIG"))

model <- glm(supported ~ Sample_size + Bias + Implementation, data = df_for_glm, family="binomial")

summary(model)$coefficients %>% 
  as.data.frame() %>% 
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>% 
  rownames_to_column("term") %>% write_tsv("FigureS8_GLM_coef.tsv", na = "", quote = "all")

####################ALL

df_plot_S8 %>% 
  mutate(all = "all") %>% 
ggplot(aes(all, fill=supported_new), alpha = 0.8) +
  geom_bar(color= "black") +
  scale_fill_manual(values=c( "#ae39ed", "#e9aff0", "#cdcfcc")) +
  #        scale_fill_manual(values=c( "#2A2E87", "steelblue", "lightgray")) +
  xlab(NULL) +
  scale_y_continuous(
    #    limits=c(0, length(levels(df$Universal.shorter))),
    breaks=c(0, 50, 100, 150, 191)) +
  coord_flip() +
  theme_classic() +
  theme(title = element_text(size=22),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position='bottom'
  )


ggsave("Figure_results_all.png", width = 7, height = 3)
