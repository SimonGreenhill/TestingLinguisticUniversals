#!/usr/bin/env Rscript
library(dplyr)
library(ape)
library(phytools)
library(corHMM)
library(ggplot2)
library(ggtree)
library(treeio)
library(tidytree)
library(rcldf)

THRESHOLD <- 10

#mycolors <- c("#1abc9c", "#3498db", "#8e44ad", "#e67e22")
mycolors <- c(
    "#003f5c", "#ffa600", "#56bec6", "#f9413e"
)
labelcolor <- "#003f5c"

args <- commandArgs(trailingOnly=TRUE)
# check for -h or --help
if ((length(args) == 0) || (any(grep("^(--help|-h)$", args))))
{
    cat("usage: ./plot_asr.r file.rds output.pdf", sep="\n")
    quit("no", 1)
}

glot <- cldf(args[[1]])
tree <- readRDS(args[[2]])

# get glottocodes
get_gc <- function(x) strsplit(x, "_")[[1]][[1]]

glottocodes <- data.frame(
    ID = unlist(lapply(tree@phylo$tip.label, get_gc)),
    Taxon = tree@phylo$tip.label
)

glottocodes <- glottocodes %>% left_join(glot$tables$LanguageTable, by='ID')

# get big families
families <- glottocodes %>%
    select(Family_ID) %>%
    filter(is.na(Family_ID) == FALSE) %>%
    group_by(Family_ID) %>%
    count() %>%
    filter(n > THRESHOLD)
# get family names
families <- glot$tables$LanguageTable %>% select(ID, Name) %>%
    right_join(families, by=c("ID"="Family_ID"))


# tag some clades
clades <- data.frame(node=NULL, group=NULL, family=NULL)
for (clade in families$ID) {
    taxa <- glottocodes %>% filter(Family_ID == clade) %>% pull('Taxon')
    node_id <- getMRCA(tree@phylo, taxa)
    clades <- rbind(clades, data.frame(node=node_id, group="D", family=families[families$ID == clade,][['Name']]))
}

t2 <- full_join(tree, clades, by = 'node')

p <- ggtree(t2, layout="circular", branch.length='none', aes(colour = group), size=0.5) +
    geom_cladelab(
        data=subset(t2@extraInfo, is.na(family)==FALSE),
        mapping=aes(node=node, label=family),
        subset=TRUE,
        angle="auto"
    ) +
    scale_color_manual(
        name="state",
        values=mycolors,
        breaks=c("A", "B", "C", "D"),
        labels=c("00", "01", "10", "11")
    ) +
    theme_tree(
        legend.position="top"
    ) +
    guides(colour = guide_legend(override.aes = list(size=20, keyheight=100)))



ggsave(args[[3]], width=10, height=10)
saveRDS(p, args[[4]])
