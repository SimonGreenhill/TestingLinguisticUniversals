#Universal 231
#If constituent order is rigid (or if there is no case inflection), then the gradation of
# adjectives is expressed by function words; if constituent order is flexible 
# (or if there is case inflection), then the gradation of adjectives is expressed 
# inflectionally.

#Relevant Grambank questions:
#GB275 Is there a bound comparative degree marker on the property word in a comparative construction? 
#GB070 Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)? 
#GB072 Are there morphological cases for oblique non-pronominal NPs (i.e. not S/A/P)?
#GB136	Is the order of core argument (i.e. S/A/P) constituents fixed?

# will need to do both seperately, so a A & B version of this universal

#B
#Gradation expressed inflectionally:
#YES for GB275 Is there a bound comparative degree marker on the property word in a comparative construction?

#Case inflection is present:
# YES for at least one of the following:
#GB070 Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)? 
#GB072 Are there morphological cases for oblique non-pronominal NPs (i.e. not S/A/P)?

#Flexible word order:
#NO for:
#GB136	Is the order of core argument (i.e. S/A/P) constituents fixed?

#(GB070: 1 | GB072: 1) | GB136: 0 >  GB275: 1

#B
library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB136_GB275 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB136", "GB275", "GB070","GB072"))

GB136_GB275_compl <- GB136_GB275[complete.cases(GB136_GB275),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB136_GB275_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB136_GB275_compl_pruned <- GB136_GB275_compl[ !(GB136_GB275_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB136_GB275_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

#prepare datafile for B version
for(i in 1:nrow(GB136_GB275_compl_pruned)){
  if((GB136_GB275_compl_pruned$GB136[i] == '0') |  (GB136_GB275_compl_pruned$GB070[i] == '1' | GB136_GB275_compl_pruned$GB072[i] == '1')){GB136_GB275_compl_pruned$Flexible[i] <- 1}
  else(GB136_GB275_compl_pruned$Flexible[i] <- 0)
}

GB136_GB275_compl_pruned2 <- subset(x = GB136_GB275_compl_pruned, select = c("Language_ID", "Flexible", "GB275"))

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB136_GB275_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")



