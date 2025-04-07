# Info about Universal. 
# Universal 18. When the descriptive adjective precedes the noun, 
#the demonstrative and the numeral, with overwhelmingly more than chance frequency, do likewise. 

#GB193 on adjectives, GB024 on numeral, GB025 on demonstratives, correlated evolution

# GB193: What is the order of adnominal property word and noun?
# GB025: What is the order of adnominal demonstrative and noun?

# Variety B Dem-N > Num-N
# GB193:1 > GB025:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB193_GB025 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB193", "GB025"))

GB193_GB025_compl <- GB193_GB025[complete.cases(GB193_GB025),]

# prune tree

world_nexus <- read.nexus("EDGE6635forAnneMarie_MCC.nex")

dropdata <- setdiff(GB193_GB025_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB193_GB025_compl_pruned <- GB193_GB025_compl[ !(GB193_GB025_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB193_GB025_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

GB193_GB025_compl_pruned$GB193[GB193_GB025_compl_pruned$GB193 == '2'] <- '0'
GB193_GB025_compl_pruned$GB193[GB193_GB025_compl_pruned$GB193 == '3'] <- '0'

table(GB193_GB025_compl_pruned$GB193)

GB193_GB025_compl_pruned$GB025[GB193_GB025_compl_pruned$GB025 == '2'] <- '0'
GB193_GB025_compl_pruned$GB025[GB193_GB025_compl_pruned$GB025 == '3'] <- '0'

table(GB193_GB025_compl_pruned$GB025)

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB193_GB025_compl_pruned, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

