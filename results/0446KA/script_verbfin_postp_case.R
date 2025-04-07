# Info about Universal. 
# If a language has Verb-final (rigid, or non-rigid) or free word order as (one of) 
#its basic word order(s), 
# then it has postpositions or case affixes.

# GB133 Is a pragmatically unmarked constituent order verb-final for transitive clauses?
# GB131	Is a pragmatically unmarked constituent order verb-initial for transitive clauses?	Harald	1410	Values
# GB132	Is a pragmatically unmarked constituent order verb-medial for transitive clauses?

# GB075	Are there postpositions?
# GB070	Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)?	Jakob	1365	Values
# GB071	Are there morphological cases for pronominal core arguments (i.e. S/A/P)?

# GB133:1 | GB136:0 > GB070:1 | GB075:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

verbfin_postp_case <- subset(x = GB_wide_strict, select = c("Language_ID", "GB133","GB075","GB070","GB136"))

verbfin_postp_case <- verbfin_postp_case[complete.cases(verbfin_postp_case),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(verbfin_postp_case$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

verbfin_postp_case_pruned <- verbfin_postp_case[ !(verbfin_postp_case$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, verbfin_postp_case$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

for(i in 1:nrow(verbfin_postp_case_pruned)){
  if((verbfin_postp_case_pruned$GB133[i] == "1") | (verbfin_postp_case_pruned$GB136[i] == "0") ){verbfin_postp_case_pruned$verbf[i] <- 1}
  else(verbfin_postp_case_pruned$verbf[i] <- 0)
}

for(i in 1:nrow(verbfin_postp_case_pruned)){
  if((verbfin_postp_case_pruned$GB075[i] == '1') | (verbfin_postp_case_pruned$GB070[i] == '1')){verbfin_postp_case_pruned$postp_case[i] <- 1}
  else(verbfin_postp_case_pruned$postp_case[i] <- 0)
}

verbfin_postp_case_pruned2 <- subset(x = verbfin_postp_case_pruned, select = c("Language_ID", "verbf","postp_case"))

#checks
world_nexus_pruned
nrow(verbfin_postp_case_pruned2)
table(verbfin_postp_case_pruned2$verbf, verbfin_postp_case_pruned2$postp_case)

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(verbfin_postp_case_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

