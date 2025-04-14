library(tidyverse)
library(patchwork)

df <- readr::read_tsv("results/BT_results_summary/results.txt", show_col_types=FALSE) %>% 
  dplyr::select(universal_code = code, 
               Size = `Size..review.of.original.`,
               `Implementation` = `Implementation..review.of.original..1`, 
               Bias.3, supported)
  
df.brms <- read_tsv("summary/df_brms_sp_posterior.tsv", show_col_types = FALSE) %>% 
  filter(term == "fixed_V3") %>% 
  group_by(universal_code) %>% 
  summarise(median_Estimate = median(Estimate), 
            median_l_95_CI = median(`l-95% CI`), 
            median_u_95_CI = median(`u-95% CI`)) %>%
  ungroup() %>% 
  mutate(brms.support = ifelse(`median_l_95_CI` < 0 & `median_u_95_CI` < 0|
                            `median_l_95_CI`  > 0 & `median_u_95_CI`  > 0  , yes = "yes (supported, 95% CI excludes zero)", no = "no (not supported, 95% CI does not excludes zero)")) %>% 
  dplyr::select(brms.support, universal_code)

df <- df %>% 
  full_join(df.brms)

meta <- df %>%
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
    dplyr::select(universal_code, Sample_size, Bias, Implementation)

df_plot_S8 <- df.brms %>%
    dplyr::select(code, model, Significant) %>%
  mutate(Significant = ifelse(Significant == "yes", "SIG in BRMS", "not SIG in BRMS")) %>%
    left_join(meta,
              by = "code") %>%
  left_join(df %>%
              dplyr::select(code, supported),
            by = "code" ) %>%
  mutate(Sig = ifelse(supported == "SIG", "supported_in_BT", Significant))

df_plot_S8 <- df_plot_S8 %>%
    filter(model == "space+phylogeny")

df_plot_S8$Sig <- factor(df_plot_S8$Sig, levels=c("supported_in_BT", "SIG in BRMS","not SIG in BRMS"))

df_plot_S8$supported <- factor(df_plot_S8$supported, levels=c("SIG", "NOT SIG"))

plot_bar <- function(df, label_legend=FALSE, label_axis=FALSE, label_model=TRUE, var = NULL) {

        ggplot(df, aes({{var}}, group=supported, fill=supported), alpha = 0.8) +
        geom_bar() +
        scale_fill_manual(values=c( "#3a4c40", "lightgray")) +
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
              axis.text.y = element_text(size=22),
              legend.position=if (label_legend) 'top' else 'none',
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

p.size_unclear <- plot_bar(subset(df_plot_S8, Sample_size == 'unclear'),  label_legend=FALSE, label_axis= TRUE, label_model=FALSE, var = Sample_size)

##

p.implementation_imperfect <- plot_bar(subset(df_plot_S8, Implementation == 'imperfect'), label_legend=FALSE,
                           label_axis= TRUE, label_model=FALSE, var = Implementation) +
    ggtitle("c. Implementation")

p.implementation_perfect <- plot_bar(subset(df_plot_S8, Implementation == 'perfect'), label_legend=FALSE,
                                       label_axis= TRUE, label_model=FALSE, var = Implementation)

p.implementation <- (p.implementation_imperfect  /p.implementation_perfect/ plot_spacer())


p.size <- (p.size_small/p.size_large/p.size_unclear)

##
p <- ( p.bias |p.size |p.implementation)


width = 20
height = 8

ggsave(filename="figures_SI8_barplot.png", height=height, width=width, dpi = 300,
       plot = p)

ggsave(filename="figures_SI8_barplot.tiff", height=height, width=width, dpi = 300,
       plot = p)


grDevices::cairo_pdf(file="figures_SI8_barplot.pdf", height=height, width= width,)
plot(p)
x <- dev.off()


df_plot_S8$Significant <- factor(df_plot_S8$Significant, levels = c("SIG in BRMS"   ,  "not SIG in BRMS"))

model <- glm(Significant ~ Sample_size + Bias + Implementation, data = df_plot_S8, family="binomial")
summary(model)


df_plot_S8$supported <- factor(df_plot_S8$supported, levels = c("SIG"   ,  "NOT SIG"))

model <- glm(supported ~ Sample_size + Bias + Implementation, data = df_plot_S8, family="binomial")
summary(model)

df_plot_S8$Sig <- factor(df_plot_S8$Sig, levels = c("not SIG in BRMS"   ,"SIG in BRMS",  "supported_in_BT"))
#model <- glm(Sig ~ Sample_size + Bias + Implementation, data = df_plot_S8)
