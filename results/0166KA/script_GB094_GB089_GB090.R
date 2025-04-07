# Info about Universal. 
# If a verb agrees in prefix with the direct object, 
# then at least some of the verbs agree with the subject.

# GB094	Can the P argument be indexed by a prefix/proclitic on the verb in the simple main clause?

# GB089	Can the S argument be indexed by a suffix/enclitic on the verb in the simple main clause?	
# GB090	Can the S argument be indexed by a prefix/proclitic on the verb in the simple main clause?

# pref-V Obj > V Agr Sbj
#  GB094:1 & GB093:0 > GB089:1 | GB090:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB094_GB089_GB090 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB093","GB094", "GB089", "GB090"))

GB094_GB089_GB090 <- GB094_GB089_GB090[complete.cases(GB094_GB089_GB090),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB094_GB089_GB090$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB094_GB089_GB090 <- GB094_GB089_GB090[ !(GB094_GB089_GB090$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB094_GB089_GB090$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

for(i in 1:nrow(GB094_GB089_GB090)){
  if((GB094_GB089_GB090$GB089[i] == '1' | GB094_GB089_GB090$GB090[i] == '1')){GB094_GB089_GB090$Sindex[i] <- 1}
  else(GB094_GB089_GB090$Sindex[i] <- 0)
}

for(i in 1:nrow(GB094_GB089_GB090)){
  if((GB094_GB089_GB090$GB094[i] == '1' & GB094_GB089_GB090$GB093[i] == '0')){GB094_GB089_GB090$Oindex[i] <- 1}
  else(GB094_GB089_GB090$Oindex[i] <- 0)
}

table(GB094_GB089_GB090$Sindex, GB094_GB089_GB090$Oindex)

GB094_GB089_GB090_2 <- subset(x = GB094_GB089_GB090, select = c("Language_ID", "Oindex", "Sindex"))

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB094_GB089_GB090_2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")



