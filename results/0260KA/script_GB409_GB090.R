#Universal 260. In ergative languages personal conjugation is of prefixal or prefix-suffixal 
#nature.

#Relevant Grambank features
#Ergative languages
#YES for GB409 Is there any ergative alignment of flagging?
#Prefixes
#YES for prefix features:
#GB090	Can the S argument be indexed by a prefix/proclitic on the verb in the simple main clause?
#GB092	Can the A argument be indexed by a prefix/proclitic on the verb in the simple main clause?
#GB094	Can the P argument be indexed by a prefix/proclitic on the verb in the simple main clause? P argument be indexed by a prefix/proclitic on the verb in the simple main clause?

# the strong version where there are only at least prefixes:
# GB409:1 > GB090:1 | GB092:1 | GB094:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB409_GB090 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB409",  "GB090", "GB092", "GB094"))

for(i in 1:nrow(GB409_GB090)){
  summ <- sum(c(as.numeric(GB409_GB090$GB090[i]), 
                as.numeric(GB409_GB090$GB092[i]), 
                as.numeric(GB409_GB090$GB094[i])), na.rm = T)
  if(summ > 0 ){GB409_GB090$conj[i] <- 1}
  else(GB409_GB090$conj[i] <- 0)
}

GB409_GB090 <- subset(x = GB409_GB090, select = c("Language_ID", "GB409", "conj"))

GB409_GB090_compl <- GB409_GB090[complete.cases(GB409_GB090),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB409_GB090_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB409_GB090_compl_pruned <- GB409_GB090_compl[ !(GB409_GB090_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB409_GB090_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB409_GB090_compl_pruned, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

