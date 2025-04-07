#Universal 240. If a language has ergative case marking, it will with far more 
# than chance frequency also have divergent ordering of genitive and adjective 
# attributes (with genitives preceding and with adjectives following their head 
# nouns); if it has divergent genitive and adjective ordering, it will [supposedly 
# with somewhat lower frequency] also have ergative case marking.

#Relevant Grambank features
#GB409 Is there any ergative alignment of flagging?
#GB193 What is the order of adnominal property word and noun?
#GB065 What is the pragmatically unmarked order of adnominal possessor noun and possessed noun?

#Ergative case marking:
#YES for GB409 Is there any ergative alignment of flagging?
#Genitives precede head nouns
#1 for GB065 What is the pragmatically unmarked order of adnominal possessor noun and possessed noun?
#Adjectives follow head nouns
#2 for GB193 What is the order of adnominal property word and noun?

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB409_GB065_GB193 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB409", "GB065", "GB193"))

GB409_GB065_GB193_compl <- GB409_GB065_GB193[complete.cases(GB409_GB065_GB193),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB409_GB065_GB193_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB409_GB065_GB193_compl_pruned <- GB409_GB065_GB193_compl[ !(GB409_GB065_GB193_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB409_GB065_GB193_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile
for(i in 1:nrow(GB409_GB065_GB193_compl_pruned)){
  if((GB409_GB065_GB193_compl_pruned$GB065[i] == '1') & (GB409_GB065_GB193_compl_pruned$GB193[i] == '2'))
     {GB409_GB065_GB193_compl_pruned$Divergent_Order[i] <- 1}
  else(GB409_GB065_GB193_compl_pruned$Divergent_Order[i] <- 0)
}

GB409_GB065_GB193_compl_pruned2 <- subset(x = GB409_GB065_GB193_compl_pruned, select = c("Language_ID", "GB409", "Divergent_Order"))

table(GB409_GB065_GB193_compl_pruned2$GB409, GB409_GB065_GB193_compl_pruned2$Divergent_Order)

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB409_GB065_GB193_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

