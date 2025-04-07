# Info about Universal. 
# The Tensedness Universals of Adjective Encoding: A. If a language is tensed, 
#it will have nouny adjectives.
# A language is TENSED, if predicates in main sentences are obligatorily marked for 
#a Past-NonPast distinction by means of bound morphology. 
# GB082	Is there overt morphological marking of present tense on verbs?
# GB083	Is there overt morphological marking on the verb dedicated to past tense?
# GB084	Is there overt morphological marking on the verb dedicated to future tense?

# GB069: Do core adjectives (defined semantically as property concepts; value, shape, age, dimension) used attributively require the same morphological treatment as verbs?
# GB068 Do core adjectives (defined semantically as property concepts such as value, shape, age, dimension) act like verbs in predicative position?

# GB082:1|GB083:1|GB084:1 > GB068:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

tense_uni <- subset(x = GB_wide_strict, select = c("Language_ID", "GB082", "GB083","GB084","GB068"))

for(i in 1:nrow(tense_uni)){
  summ <- sum(c(as.numeric(tense_uni$GB082[i]), 
                as.numeric(tense_uni$GB083[i]), 
                as.numeric(tense_uni$GB084[i])), na.rm = T)
  if(summ > 0 ){tense_uni$morph[i] <- 1}
  else(tense_uni$morph[i] <- 0)
}

tense_uni <- subset(x = tense_uni, select = c("Language_ID", "morph", "GB068"))

tense_uni_compl <- tense_uni[complete.cases(tense_uni),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(tense_uni_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

tense_uni_compl_pruned <- tense_uni_compl[ !(tense_uni_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, tense_uni_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# checks

world_nexus_pruned
nrow(tense_uni_compl_pruned)
table(tense_uni_compl_pruned$morph, tense_uni_compl_pruned$GB068)

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(tense_uni_compl_pruned, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


