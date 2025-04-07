# Info about Universal. 
# Nominal modifiers (such as relative, adjectival, and attributive expressions) 
#follow nouns in VO languages and precede nouns in OV languages

# GB131	Is a pragmatically unmarked constituent order verb-initial for transitive clauses?	Harald	1410	Values
# GB132	Is a pragmatically unmarked constituent order verb-medial for transitive clauses?	Harald	1376	Values
# GB133	Is a pragmatically unmarked constituent order verb-final for transitive clauses?

# GB193	What is the order of adnominal property word and noun?

#0	they cannot be used attributively	23
#1	ANM - N	295
#2	N - ANM	695
#3	both	168
#?	Not known	70

# Variety B ############################################################

#OV > Adj-N
#GB131:0 & ( GB132:1 | GB133:1 ) > GB193:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

VO_GB193 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB193"))

VO_GB193 <- VO_GB193[complete.cases(VO_GB193),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(VO_GB193$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

VO_GB193_pruned <- VO_GB193[ !(VO_GB193$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, VO_GB193$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

for(i in 1:nrow(VO_GB193_pruned)){
  if(VO_GB193_pruned$GB131[i] == '0' &  ( VO_GB193_pruned$GB132[i] == '1' | VO_GB193_pruned$GB133[i] == '1') ){VO_GB193_pruned$OV[i] <- 1}
  else(VO_GB193_pruned$OV[i] <- 0)
}

table(VO_GB193_pruned$OV, VO_GB193_pruned$GB193)
VO_GB193_pruned$GB193[VO_GB193_pruned$GB193 == '2'] <- '0'
VO_GB193_pruned$GB193[VO_GB193_pruned$GB193 == '3'] <- '0'

table(VO_GB193_pruned$OV, VO_GB193_pruned$GB193)

VO_GB193_pruned2 <- subset(x = VO_GB193_pruned, select = c("Language_ID", "OV", "GB193"))


# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(VO_GB193_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


