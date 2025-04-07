#Universal 1593. Internally headed relative clauses occur only in languages 
#manifesting null  anaphora (that is, the use of null NPs in place of lexical 
#pronouns etc. in most argument positions). 

#Relevant features
#GB329 Are there internally-headed relative clauses?
# GB522 Can the S or A argument be omitted from a pragmatically unmarked 
#clause when the referent is inferrable from context ("pro-drop" or "null  anaphora")?

#GB329:1 > GB522:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB329_GB522 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB329", "GB522"))

GB329_GB522_compl <- GB329_GB522[complete.cases(GB329_GB522),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB329_GB522_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB329_GB522_compl_pruned <- GB329_GB522_compl[ !(GB329_GB522_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB329_GB522_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 

GB329_GB522_compl_pruned2 <- subset(x = GB329_GB522_compl_pruned, select = c("Language_ID", "GB329", "GB522"))

table(GB329_GB522_compl_pruned2$GB329, GB329_GB522_compl_pruned2$GB522)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB329_GB522_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

