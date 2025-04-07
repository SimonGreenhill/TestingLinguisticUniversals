# Info about Universal. 
# Universal 27. If a language is exclusively suffixing, it is postpositional; 
#if it is exclusively prefixing, it is prepositional. 

#GB079	Do verbs have prefixes/proclitics, other than those that only mark A, S or P (do include portmanteau: A & S + TAM)?	
#GB090	Can the S argument be indexed by a prefix/proclitic on the verb in the simple main clause?	
#GB092	Can the A argument be indexed by a prefix/proclitic on the verb in the simple main clause?
#GB094	Can the P argument be indexed by a prefix/proclitic on the verb in the simple main clause?	
#GB430	Can adnominal possession be marked by a prefix on the possessor?
#GB431	Can adnominal possession be marked by a prefix on the possessed noun?

# Variety A, pref - prepositions
#sum(GB079:1,GB090:1,GB092:1,GB094:1,GB430:1,GB431:1) => 2 & 
#sum(GB80:1,GB089:1,GB091:1,GB093:1,GB432:1,GB433:1) = 0 
#> GB074:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

pref_GB074 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB079", "GB090","GB092","GB094","GB430","GB431","GB074",
                                                    "GB080","GB089","GB091","GB093","GB432","GB433"))

for(i in 1:nrow(pref_GB074)){
  sum1 <- sum(c(as.numeric(pref_GB074$GB079[i]), 
                as.numeric(pref_GB074$GB090[i]), 
                as.numeric(pref_GB074$GB092[i]), 
                as.numeric(pref_GB074$GB094[i]),
                as.numeric(pref_GB074$GB430[i]), 
                as.numeric(pref_GB074$GB431[i])), na.rm = T)
  sum2 <- sum(c(as.numeric(pref_GB074$GB080[i]), 
                as.numeric(pref_GB074$GB089[i]), 
                as.numeric(pref_GB074$GB091[i]), 
                as.numeric(pref_GB074$GB093[i]),
                as.numeric(pref_GB074$GB432[i]), 
                as.numeric(pref_GB074$GB433[i])), na.rm = T) 
  if((sum1 >= 2) & (sum2 == 0)){pref_GB074$det[i] <- 1}
  else(pref_GB074$det[i] <- 0)
}

pref_GB074_compl <- subset(x = pref_GB074, select = c("Language_ID", "det", "GB074"))

pref_GB074_compl <- pref_GB074[complete.cases(pref_GB074),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(pref_GB074_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

pref_GB074_compl_pruned <- pref_GB074_compl[ !(pref_GB074_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, pref_GB074_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

pref_GB074_compl_pruned2 <- subset(x = pref_GB074_compl_pruned, select = c("Language_ID", "det","GB074"))

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(pref_GB074_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")



