#!/usr/bin/env Rscript
library(dplyr)
library(ape)
library(phytools)
library(corHMM)
library(ggtree)
library(tidytree)
library(treeio)

NCORES <- 6

recode_to_4state <- function(x){
    if (x == '00') return("A")
    if (x == '01') return("B")
    if (x == '10') return("C")
    if (x == '11') return("D")

    stop(paste("Invalid State: ", x))
}

# pull glottocodes out of string in taxa labels
get_glottocodes <- function(x) strsplit(x, "_")[[1]][[1]]


args <- commandArgs(trailingOnly=TRUE)
# check for -h or --help
if ((length(args) == 0) || (any(grep("^(--help|-h)$", args))))
{
    cat("usage: ./asr.r treefile bayestraits_data output.rds", sep="\n")
    quit("no", 1)
}

tree <- read.nexus(args[[1]])

# prepare data
data <- read.delim(args[[2]], header=FALSE, na.strings=c("?", "-"))
colnames(data) <- c('Glottocode', 'State1', 'State2')
data$Pattern <- paste(data$State1, data$State2, sep="")
data$Pattern <- sapply(data$Pattern, recode_to_4state)

# add taxon labels to data
glottocodes <- data.frame(
    Glottocode = unlist(lapply(tree$tip.label, get_glottocodes)),
    Taxon = tree$tip.label
)
data <- data %>% left_join(glottocodes, by="Glottocode")

# remove things from data not in tree
data <- data[data$Taxon %in% tree$tip.label, ]

# prune tree to remove things not in data
tree <- keep.tip(tree, data$Taxon)

h <- corHMM::corHMM(
    tree, data[,c("Taxon", "Pattern")],
    rate.cat=1,  # not HRM model
    model="ARD",  # all rates are different
    node.states="marginal",  # simpler for visualisation only
    root.p="maddfitz",  # specify maddison/fitzjohn root prior
    lewis.asc.bias=TRUE,  # correct for ascertainment bias
    n.cores=NCORES
)

model = h$solution
model[is.na(h)] <- 0
diag(model) <- -rowSums(model)

#simmap <- makeSimmap(tree=h$phy, data=h$data, model=model, rate.cat=1, nSim=1, nCores=NCORES)

p <- sapply(1:max(h$index.mat, na.rm = TRUE), function(x)
    na.omit(c(h$solution))[na.omit(c(h$index.mat) == x)][1])

s1 <- ancRECON(
    phy = h$phy,
    data = h$data,
    p = p,
    method = "marginal",
    rate.cat = h$rate.cat,
    ntraits = NULL,
    rate.mat = h$index.mat,
    root.p = h$root.p)


nodes <- data.frame(
    node=seq(Ntip(tree) + 1, Ntip(tree) + Nnode(tree) + 1),
    group = NA,
    row.names=seq(Ntip(tree) + 1, Ntip(tree) + Nnode(tree) + 1)
)

grp <- as.character(Ntip(h$phy) + 1 + which(s1$lik.anc.states[, 1] == 1))
nodes[grp, ]$group <- 'A'
grp <- as.character(Ntip(h$phy) + 1 + which(s1$lik.anc.states[, 2] == 1))
nodes[grp, ]$group <- 'B'
grp <- as.character(Ntip(h$phy) + 1 + which(s1$lik.anc.states[, 3] == 1))
nodes[grp, ]$group <- 'C'
grp <- as.character(Ntip(h$phy) + 1 + which(s1$lik.anc.states[, 4] == 1))
nodes[grp, ]$group <- 'D'

# add in tips
nodes <- rbind(nodes, data.frame(
    node=1:Ntip(tree),
    group=setNames(data$Pattern, data$Taxon)
))

t <- full_join(h$phy, nodes, by = 'node')

saveRDS(t, file=args[[3]])