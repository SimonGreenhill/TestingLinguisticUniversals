# Info about Universal. 
# Universal 27. If a language is exclusively suffixing, it is postpositional; 
#if it is exclusively prefixing, it is prepositional. 

# Variety B suf - postpositions

#GB080	Do verbs have suffixes/enclitics, other than those that only mark A, S or P (do include portmanteau: A & S + TAM)?	
#GB089	Can the S argument be indexed by a suffix/enclitic on the verb in the simple main clause?	
#GB091	Can the A argument be indexed by a suffix/enclitic on the verb in the simple main clause?	
#GB093	Can the P argument be indexed by a suffix/enclitic on the verb in the simple main clause?	
#GB432	Can adnominal possession be marked by a suffix on the possessor?
#GB433	Can adnominal possession be marked by a suffix on the possessed noun?

# sum(GB80:1,GB089:1,GB091:1,GB093:1,GB432:1,GB433:1) => 2 & 
# sum(GB079:1,GB090:1,GB092:1,GB094:1,GB430:1,GB431:1) = 0 
# > GB075:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

suf_GB075 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB080", "GB089","GB091","GB093","GB432","GB433","GB075",
                                                   "GB079","GB090","GB092","GB094","GB430","GB431"))

for(i in 1:nrow(suf_GB075)){
  sum1 <- sum(c(as.numeric(suf_GB075$GB080[i]), 
                as.numeric(suf_GB075$GB089[i]), 
                as.numeric(suf_GB075$GB091[i]), 
                as.numeric(suf_GB075$GB093[i]),
                as.numeric(suf_GB075$GB432[i]), 
                as.numeric(suf_GB075$GB433[i])), na.rm = T)
  sum2 <- sum(c(as.numeric(suf_GB075$GB079[i]), 
                as.numeric(suf_GB075$GB090[i]), 
                as.numeric(suf_GB075$GB092[i]), 
                as.numeric(suf_GB075$GB094[i]),
                as.numeric(suf_GB075$GB430[i]), 
                as.numeric(suf_GB075$GB431[i])), na.rm = T) 
  if((sum1 >= 2) & (sum2 == 0)){suf_GB075$det[i] <- 1}
  else(suf_GB075$det[i] <- 0)
}

suf_GB075_compl <- subset(x = suf_GB075, select = c("Language_ID", "det", "GB075"))

suf_GB075_compl <- suf_GB075[complete.cases(suf_GB075),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(suf_GB075_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

suf_GB075_compl_pruned <- suf_GB075_compl[ !(suf_GB075_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, suf_GB075_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

suf_GB075_compl_pruned2 <- subset(x = suf_GB075_compl_pruned, select = c("Language_ID", "det","GB075"))

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(suf_GB075_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

