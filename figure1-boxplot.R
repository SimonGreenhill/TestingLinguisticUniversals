library(ggplot2)
library(ggrepel)
library(patchwork)
library(Cairo)

source('common.R')

## Construct Figure 1
plot_bar <- function(df, title="xx", label_legend=FALSE, label_axis=FALSE, label_model=TRUE) {
    # do this reordering in here so we don't mess up the other plots
    df$model <- factor(df$model, levels=c("space+phylogeny", "uncontrolled"))
    ggplot(df, aes(model, group=Significant, fill=Significant), alpha = 0.8) +
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
            legend.position=if (label_legend) 'top' else 'none',
            axis.text.y=if (label_model) element_text(size=22) else element_blank()
        ) +
        ylab(ifelse(label_axis, 'Number of Generalizations', ''))
}

p.all <- plot_bar(df.brms, "a. Overall", label_legend=FALSE, label_axis=FALSE, label_model=TRUE)
p.hier <- plot_bar(subset(df.brms, Domain_general == 'hierarchy'), "b. Hierarchy", label_legend=FALSE, label_axis=FALSE, label_model=TRUE)
p.bwo <- plot_bar(subset(df.brms, Domain_general == 'broad word order'), "c. Broad Word Order", label_legend=FALSE, label_axis=FALSE, label_model=FALSE)
p.nwo <- plot_bar(subset(df.brms, Domain_general == 'narrow word order'), "d. Narrow Word Order", label_legend=FALSE, label_axis=FALSE, label_model=FALSE)
p.other <- plot_bar(subset(df.brms, Domain_general == 'other'), "e. Other", label_legend=FALSE, label_axis=FALSE, label_model=FALSE)


p.fig1 <- (p.all / (p.hier | p.bwo | p.nwo | p.other)) +
    plot_layout(guides = 'auto')

width = 18
height = 8

ggsave(filename="figure1-boxplot.tiff", height=height, width=width, dpi = 300,
       plot = p.fig1)
ggsave(filename="figure1-boxplot.png", height=height, width= width,
       plot = p.fig1)

grDevices::cairo_pdf(file="figure1-boxplot.pdf", height=height, width= width,)
plot(p.fig1)
x <- dev.off()


