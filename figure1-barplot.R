source("requirements.R")


df_sp <- read_tsv("summary/df_brms_sp_posterior.tsv", show_col_types = F) %>% 
  filter(term == "fixed_V3") %>% 
  group_by(universal_code) %>% 
  summarise(median_Estimate = median(Estimate), 
         median_l_95_CI = median(`l-95% CI`), 
         median_u_95_CI = median(`u-95% CI`)) %>%
  ungroup() %>% 
  mutate(support = ifelse(`median_l_95_CI` < 0 & `median_u_95_CI` < 0|
                            `median_l_95_CI`  > 0 & `median_u_95_CI`  > 0  , yes = "yes (supported, 95% CI excludes zero)", no = "no (not supported, 95% CI does not excludes zero)")) %>% 
  dplyr::select("space+phylogeny" = support, universal_code)

df_naive <- read_tsv("summary/df_brms_naive.tsv", show_col_types = F) %>%  
  filter(term == "fixed_V3") %>% 
  dplyr::select("uncontrolled" = brms_support, universal_code)

universals_meta_data <- read_tsv("universals_types.tsv", show_col_types = F) %>% 
  dplyr::select(universal_code, Domain_general)

df.brms <- df_sp %>% 
  full_join(df_naive, by = join_by(universal_code)) %>% 
  reshape2::melt(id.vars = "universal_code") %>% 
  left_join(universals_meta_data, by = "universal_code") %>% 
  rename(model = variable, support = value)
  
## Construct Figure 1
plot_bar <- function(df, title="xx", label_legend=FALSE, label_axis=FALSE, label_model=TRUE) {
    # do this reordering in here so we don't mess up the other plots
    df$model <- factor(df$model, levels=c("space+phylogeny", "uncontrolled"))
    df$support <- factor(df$support, levels=c("yes (supported, 95% CI excludes zero)",
                                          "no (not supported, 95% CI does not excludes zero)"))
    ggplot(df, aes(x = model, group=support, fill=support), alpha = 0.8) +
        geom_bar() +
        scale_fill_manual(values=c("steelblue", "lightgray")) +
        xlab(NULL) +
        scale_y_continuous(
        #    limits=c(0, length(levels(df$Universal.shorter))),
            breaks=seq(0, 190, 25)) +
        coord_flip() +
        ggtitle(title) +
        theme(title = element_text(size=22),
            axis.title=element_text(size=26),
            axis.text = element_text(size = 22),
            legend.position = if (label_legend) "bottom" else "none",  # Control legend visibility
            axis.text.y=if (label_model) element_text(size=22) else element_blank()
        ) +
        ylab(ifelse(label_axis, 'Number of Generalizations', ''))
}

# Define your plots
p.all <- plot_bar(df.brms, "a. Overall", label_legend=TRUE, label_axis=FALSE, label_model=TRUE)
p.hier <- plot_bar(subset(df.brms, Domain_general == 'hierarchy'), "b. Hierarchy", label_legend=FALSE, label_axis=FALSE, label_model=TRUE)
p.bwo <- plot_bar(subset(df.brms, Domain_general == 'broad word order'), "c. Broad Word Order", label_legend=FALSE, label_axis=FALSE, label_model=FALSE)
p.nwo <- plot_bar(subset(df.brms, Domain_general == 'narrow word order'), "d. Narrow Word Order", label_legend=FALSE, label_axis=FALSE, label_model=FALSE)
p.other <- plot_bar(subset(df.brms, Domain_general == 'other'), "e. Other", label_legend=FALSE, label_axis=FALSE, label_model=FALSE)

# Step 1: Combine the plots without individual legends
p.row <- (p.hier | p.bwo | p.nwo | p.other) +
  plot_layout(guides = "collect") & 
  theme(axis.title.x = element_blank())  # No legend in these plots

# Step 2: Create the shared x-axis label
xlab <- ggplot() +
  theme_void() +
  annotate("text", x = 0.5, y = 0.5, label = "Shared X-axis Title", 
           size = 5, fontface = "bold", hjust = 0.5) +
  theme(plot.margin = margin(t = 10))  # Add space before the legend

# Step 3: Combine everything: Main plot, shared x-axis label, and shared legend
p.fig1 <- (wrap_elements(xlab) / p.all / p.row) +
  plot_layout(heights = c(0.1, 1, 1), guides = "collect") &
  theme(legend.position = "bottom", legend.text = element_text(size = 20))  # Collect the legend at the bottom

# Step 4: Display the final plot
p.fig1

width = 20
height = 8

ggsave(filename="figure1-barplot.tiff", height=height, width=width, dpi = 300,
       plot = p.fig1)
ggsave(filename="figure1-barplot.png", height=height, width= width,
       plot = p.fig1)

#grDevices::cairo_pdf(file="figure1-boxplot.pdf", height=height, width= width,)
#plot(p.fig1)
#x <- dev.off()


