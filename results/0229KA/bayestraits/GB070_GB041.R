#Universal 229. If nominals inflect for number, they also inflect for case.

#Relevant features
#Case - YES for either of:
#GB070	Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)?
#GB072	Are there morphological cases for oblique non-pronominal NPs (i.e. not S/A/P)?

#Number (inflections only) - YES for at least one of the following:
# GB041    Are there several nouns (more than three) which are suppletive for number?
# GB042    Is there productive overt morphological singular marking on nouns?   
# GB043    Is there productive morphological dual marking on nouns?   
# GB044    Is there productive morphological plural marking on nouns?
# GB165    Is there productive morphological trial marking on nouns?
# GB166    Is there productive morphological paucal marking on nouns?

#GB041|GB042|GB043|GB044|GB165|GB166:1 > GB070|GB072:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB070_GB041 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB070", "GB072", "GB041", "GB042", "GB043", "GB044", "GB165", "GB166"))

#GB041|GB042|GB043|GB044|GB165|GB166:1 > GB070|GB072:1

for(i in 1:nrow(GB070_GB041)){
  summ <- sum(c(as.numeric(GB070_GB041$GB041[i]), as.numeric(GB070_GB041$GB042[i]), as.numeric(GB070_GB041$GB043[i]), as.numeric(GB070_GB041$GB044[i]), as.numeric(GB070_GB041$GB165[i]), as.numeric(GB070_GB041$GB166[i])), na.rm = T)
  if(summ > 0 ){GB070_GB041$Number[i] <- 1}
  else(GB070_GB041$Number[i] <- 0)
}

GB070_GB041 <- subset(x = GB070_GB041, select = c("Language_ID", "GB070", "GB072", "Number"))

GB070_GB041_compl <- GB070_GB041[complete.cases(GB070_GB041),]

# prune tree

world_nexus <- read.nexus("EDGE6635forAnneMarie100.nex")

dropdata <- setdiff(GB070_GB041_compl$Language_ID, world_nexus[[1]]$tip.label)

# remove data that is not in the tree:

GB070_GB041_compl_pruned <- GB070_GB041_compl[ !(GB070_GB041_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus[[1]]$tip.label, GB070_GB041_compl$Language_ID)

world_nexus_pruned <- vector(length = 100, mode = "list")
for(i in 1:100)
{world_nexus_pruned[[i]] <- drop.tip(world_nexus[[i]], droptips)
}

# checks 
nrow(GB070_GB041_compl_pruned)
setdiff(GB070_GB041_compl_pruned$Language_ID, world_nexus_pruned[[1]]$tip.label)
setdiff(world_nexus_pruned[[1]]$tip.label, GB070_GB041_compl_pruned$Language_ID)
length(world_nexus_pruned[[1]]$tip.label)

# prepare datafile
for(i in 1:nrow(GB070_GB041_compl_pruned)){
  if((GB070_GB041_compl_pruned$GB070[i] == '1') | (GB070_GB041_compl_pruned$GB072[i] == '1')){GB070_GB041_compl_pruned$Case[i] <- 1}
  else(GB070_GB041_compl_pruned$Case[i] <- 0)
}

GB070_GB041_compl_pruned2 <- subset(x = GB070_GB041_compl_pruned, select = c("Language_ID", "Number", "Case"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB070_GB041_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.trees")



