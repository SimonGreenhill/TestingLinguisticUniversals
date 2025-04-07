#Universal 247. The presence of the category of gender is connected with the 
# development of the morphological opposition of nominative/accusative. In those 
# systems where the special form of accusative is attested, the category of gender
# exists.

# IF there is an accusative case distinct from the nominative, THEN there is a category of gender.

#Relevant Grambank features
#Gender:
#  YES for at least one of the following:
# GB030    Is there a gender distinction in independent 3rd person pronouns?
# GB051    Is there a gender/noun class system where sex is a factor in class assignment?
# GB052    Is there a gender/noun class system where shape is a factor in class assignment?
# GB053    Is there a gender/noun class system where animacy is a factor in class assignment?
# GB054    Is there a gender/noun class system where plant status is a factor in class assignment?
# GB192    Is there a gender system where a noun's phonological properties are a factor in class assignment?
#GB196	Is there a male/female distinction in 2nd person independent pronouns?	Hannah	1457	
#GB197	Is there a male/female distinction in 1st person independent pronouns?
# GB321    Is there a large class of nouns whose gender/noun class is not phonologically or semantically predictable?

#Cases (morphological opposition of nominal accusative):
#YES for one of the following:
#GB070        Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)?
#GB071	Are there morphological cases for pronominal core arguments (i.e. S/A/P)?

#GB408:1 > GB030|GB051|GB052|GB053|GB054|GB192|GB196|GB197|GB321:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB070_GB051 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB408", "GB030", 
                                                     "GB051", "GB052", "GB053", "GB054", "GB192", 
                                                     "GB196", "GB197", "GB321"))

for(i in 1:nrow(GB070_GB051)){
  summ <- sum(c(as.numeric(GB070_GB051$GB030[i]), 
                as.numeric(GB070_GB051$GB051[i]), 
                as.numeric(GB070_GB051$GB052[i]), 
                as.numeric(GB070_GB051$GB053[i]), 
                as.numeric(GB070_GB051$GB054[i]), 
                as.numeric(GB070_GB051$GB192[i]), 
                as.numeric(GB070_GB051$GB321[i]), 
                as.numeric(GB070_GB051$GB196[i]), 
                as.numeric(GB070_GB051$GB197[i])), na.rm = T)
  if(summ > 0 ){GB070_GB051$Gender[i] <- 1}
  else(GB070_GB051$Gender[i] <- 0)
}

GB070_GB051 <- subset(x = GB070_GB051, select = c("Language_ID", "GB408", "Gender"))

GB070_GB051_compl <- GB070_GB051[complete.cases(GB070_GB051),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB070_GB051_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB070_GB051_compl_pruned <- GB070_GB051_compl[ !(GB070_GB051_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB070_GB051_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

table(GB070_GB051_compl_pruned$GB408, GB070_GB051_compl_pruned$Gender)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB070_GB051_compl_pruned, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


