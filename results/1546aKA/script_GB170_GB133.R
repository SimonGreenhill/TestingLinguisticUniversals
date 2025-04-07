#Universal 1546. In verb-initial languages, there is probably less agreement of adjectives  
#with common nouns than in verb-final languages, especially case agreement. 

#The Universal will be tested on:
#Version A: verb-final languages and the presence of agreement of adjectives with nouns
#Version B: verb-initial languages and the presence of agreemnet

#Relevant features
#Verb_final - YES for GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?
#While also NO for:
#GB131        Is a pragmatically unmarked constituent order verb-initial for transitive clauses?   
#GB132        Is a pragmatically unmarked constituent order verb-medial for transitive clauses?

#Verb_initial - YES for GB131        Is a pragmatically unmarked constituent order verb-initial for transitive clauses? 
#While also NO for:
#GB132        Is a pragmatically unmarked constituent order verb-medial for transitive clauses?
#GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?

#Agreement of adjectives with nouns - YES for either of:
#GB170	Can an adnominal property word agree with the noun in gender/noun class?
#GB184 Can an adnominal property word agree with the noun in number?

#GB131:0 & GB132:0 & GB133:1 > GB170:1|GB184:1 A

#Version A

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB170_GB133 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB170", "GB184"))

GB170_GB133_compl <- GB170_GB133[complete.cases(GB170_GB133),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB170_GB133_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB170_GB133_compl_pruned <- GB170_GB133_compl[ !(GB170_GB133_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB170_GB133_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 
#GB131:0 & GB132:0 & GB133:1 > GB170:1|GB184:1 A

for(i in 1:nrow(GB170_GB133_compl_pruned)){
  if((GB170_GB133_compl_pruned$GB131[i] == '0') & (GB170_GB133_compl_pruned$GB132[i] == '0') & (GB170_GB133_compl_pruned$GB133[i] == '1')) {GB170_GB133_compl_pruned$Verb_final[i] <- 1}
  else(GB170_GB133_compl_pruned$Verb_final[i] <- 0)
}

for(i in 1:nrow(GB170_GB133_compl_pruned)){
  if((GB170_GB133_compl_pruned$GB170[i] == '1') | (GB170_GB133_compl_pruned$GB184[i] == '1')) {GB170_GB133_compl_pruned$Agreement[i] <- 1}
  else(GB170_GB133_compl_pruned$Agreement[i] <- 0)
}

GB170_GB133_compl_pruned2 <- subset(x = GB170_GB133_compl_pruned, select = c("Language_ID", "Verb_final", "Agreement"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB170_GB133_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

