# Info about Universal. 
# If a language has no adjective agreement, then the genitive will precede the noun.

# GB170	Can an adnominal property word agree with the noun in gender/noun class?
# GB184	Can an adnominal property word agree with the noun in number?

# GB065	What is the pragmatically unmarked order of adnominal possessor noun and possessed noun?

# 1	Possessor-Possessed	581
# 2	Possessed-Possessor	510
# 3	both	287
# ?	Not known

# no Adj Agr > Gen-N
# GB170:0 & GB184:0 > GB065:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB170_GB184_GB065 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB170", "GB184", "GB065"))

GB170_GB184_GB065 <- GB170_GB184_GB065[complete.cases(GB170_GB184_GB065),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB170_GB184_GB065$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB170_GB184_GB065_pruned <- GB170_GB184_GB065[ !(GB170_GB184_GB065$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB170_GB184_GB065$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

for(i in 1:nrow(GB170_GB184_GB065_pruned)){
  if((GB170_GB184_GB065_pruned$GB170[i] == '0' &  GB170_GB184_GB065_pruned$GB184[i] == '0')){GB170_GB184_GB065_pruned$agree[i] <- 1}
  else(GB170_GB184_GB065_pruned$agree[i] <- 0)
}

table(GB170_GB184_GB065_pruned$agree, GB170_GB184_GB065_pruned$GB065)
GB170_GB184_GB065_pruned$GB065[GB170_GB184_GB065_pruned$GB065 == '1'] <- '1'
GB170_GB184_GB065_pruned$GB065[GB170_GB184_GB065_pruned$GB065 == '2'] <- '0'
GB170_GB184_GB065_pruned$GB065[GB170_GB184_GB065_pruned$GB065 == '3'] <- '0'

GB170_GB184_GB065_pruned2 <- subset(x = GB170_GB184_GB065_pruned, select = c("Language_ID", "agree", "GB065"))

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB170_GB184_GB065_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")



