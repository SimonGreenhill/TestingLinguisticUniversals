#!/usr/bin/env Rscript
library(dplyr)
library(ape)
library(phytools)
library(corHMM)
library(ggplot2)
library(ggtree)
library(treeio)
library(tidytree)
library(patchwork)

#mycolors <- c("#1abc9c", "#3498db", "#8e44ad", "#e67e22")
mycolors <- c(
    "#003f5c", "#f9413e","#ffa600", "#56bec6"
)
labelcolor <- "#003f5c"

args <- commandArgs(trailingOnly=TRUE)
# check for -h or --help
if ((length(args) == 0) || (any(grep("^(--help|-h)$", args))))
{
    cat("usage: ./plot_panel.r [...rds...]", sep="\n")
    quit("no", 1)
}

plots <- list()
for (f in args) {
    plots[[tools::file_path_sans_ext(f)]] <- readRDS(f)
}


# Domain: hierarchy
# # A tibble: 1 × 3
#   code   Universal.shorter   BT_EDGE_BF
#   <chr>  <fct>                    <dbl>
# 1 1053KA agr Dets > agr Mods       190.
#
# Domain: broad word order
# # A tibble: 1 × 3
#   code   Universal.shorter BT_EDGE_BF
#   <chr>  <fct>                  <dbl>
# 1 1152KA Rel-N > N-Rel           78.4
#
# Domain: narrow word order
# # A tibble: 1 × 3
#   code   Universal.shorter               BT_EDGE_BF
#   <chr>  <fct>                                <dbl>
# 1 0344KA VS (tr. verb) > VS (intr. verb)       180.
#
# Domain: other
# # A tibble: 1 × 3
#   code    Universal.shorter      BT_EDGE_BF
#   <chr>   <fct>                       <dbl>
# 1 1431mKA less case > more tense       27.9
#

plots[["0344KA"]] <- plots[["0344KA"]] + ggtitle("A. Narrow Word Order: VS (tr. verb) ⇒ VS (intr. verb)")
plots[["1152KA"]] <- plots[["1152KA"]] + ggtitle("B. Broad Word Order: Rel-N ⇒ N-Rel")
plots[["1053KA"]] <- plots[["1053KA"]] + ggtitle("C. Hierarchy: agr Dets ⇒ agr Mods")
plots[["1431mKA"]] <- plots[["1431mKA"]] + ggtitle("D. Other: less case ⇒ more tense")


p <- (plots[["0344KA"]] | plots[["1152KA"]]) / (plots[["1053KA"]] | plots[["1431mKA"]])
p <- p + plot_layout(guides = 'collect') & theme(legend.position='top')



ggsave("panel.pdf", width=18, height=18)
ggsave("panel.png", width=18, height=18)
