# Info about Universal. 
# Universal 43. If a language has gender categories in the noun, 
#it has gender categories in the pronoun. 
#  I agree that this is testable, though we don’t specifically ask about how gender 
#is marked on nouns. I would not use the agreement features to target gender marking in the 
#noun (GB170-172, GB198), since they may not capture all situations in which gender is marked 
#on the noun or in the noun phrase. I’d use GB051-054, GB192, and GB321 instead, 
#to determine whether there is a gender system.


# GB030	Is there a gender distinction in independent 3rd person pronouns?
# GB196	Is there a male/female distinction in 2nd person independent pronouns?	Hannah	1119	Values
# GB197	Is there a male/female distinction in 1st person independent pronouns?

# GB051	Is there a gender/noun class system where sex is a factor in class assignment?
# GB052	Is there a gender/noun class system where shape is a factor in class assignment?
# GB053	Is there a gender/noun class system where animacy is a factor in class assignment?
# GB054	Is there a gender/noun class system where plant status is a factor in class assignment?
# GB192	Is there a gender system where a noun's phonological properties are a factor in class assignment?
# GB321	Is there a large class of nouns whose gender/noun class is not phonologically or semantically predictable?

# sum(GB051:1,GB052:1,GB053:1,GB054:1,GB192:1,GB321:1) > 0, sum(GB030:1,GB196:1,GB197:1) > 0 

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

gender <- subset(x = GB_wide_strict, select = c("Language_ID", "GB030","GB196", "GB197", "GB051","GB052","GB053", "GB054", "GB192","GB321"))

for(i in 1:nrow(gender)){
  summ <- sum(c(as.numeric(gender$GB030[i]), 
                as.numeric(gender$GB196[i]), 
                as.numeric(gender$GB197[i])), na.rm = T)
  if(summ > 0 ){gender$prongender[i] <- 1}
  else(gender$prongender[i] <- 0)
}

for(i in 1:nrow(gender)){
  summ <- sum(c(as.numeric(gender$GB051[i]), 
                as.numeric(gender$GB052[i]), 
                as.numeric(gender$GB053[i]), 
                as.numeric(gender$GB054[i]), 
                as.numeric(gender$GB192[i]), 
                as.numeric(gender$GB321[i])), na.rm = T)
  if(summ > 0 ){gender$nomgender[i] <- 1}
  else(gender$nomgender[i] <- 0)
}

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(gender$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

gender_pruned <- gender[ !(gender$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, gender$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

table(gender$prongender,gender$nomgender)

# prepare datafile

gender_pruned2 <- subset(x = gender_pruned, select = c("Language_ID", "nomgender", "prongender"))

table(is.na(gender_pruned2$prongender))
table(is.na(gender_pruned2$nomgender))

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(gender_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


