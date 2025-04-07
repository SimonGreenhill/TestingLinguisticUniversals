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

#A 
#Gradation expressed with function words:
#YES for at least one of the following: 
#GB276	Is there a non-bound comparative degree marker modifying the property word in a comparative construction?
#GB273	Is there a comparative construction with a standard marker that elsewhere has neither a locational meaning nor a 'surpass/exceed' meaning?
#GB265	Is there a comparative construction that includes a form that elsewhere means 'surpass, exceed'?
#GB266	Is there a comparative construction that employs a marker of the standard which elsewhere has a locational meaning?	

#No case:
#NO for both of the following:
#GB070 Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)? 
#GB072 Are there morphological cases for oblique non-pronominal NPs (i.e. not S/A/P)?

#Rigid constituent order:
#YES for GB136	Is the order of core argument (i.e. S/A/P) constituents fixed?

#(GB070: 0 & GB072: 0 )| GB136: 1 >  GB276|GB273|GB265|GB166: 1

#A
library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB136_GB275 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB136", "GB265", "GB166", "GB273", "GB276", "GB070","GB072"))

for(i in 1:nrow(GB136_GB275)){
  summ <- sum(c(as.numeric(GB136_GB275$GB265[i]), as.numeric(GB136_GB275$GB166[i]), as.numeric(GB136_GB275$GB273[i]), as.numeric(GB136_GB275$GB276[i])), na.rm = T)
  if(summ > 0 ){GB136_GB275$Function_Words[i] <- 1}
  else(GB136_GB275$Function_Words[i] <- 0)
}

GB136_GB275 <- subset(x = GB136_GB275, select = c("Language_ID", "GB136", "GB070", "GB072", "Function_Words"))

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


# prepare datafile for A version 
for(i in 1:nrow(GB136_GB275_compl_pruned)){
  if((GB136_GB275_compl_pruned$GB136[i] == '1') |  (GB136_GB275_compl_pruned$GB070[i] == '0' & GB136_GB275_compl_pruned$GB072[i] == '0')){GB136_GB275_compl_pruned$Fixed[i] <- 1}
  else(GB136_GB275_compl_pruned$Fixed[i] <- 0)
}

GB136_GB275_compl_pruned2 <- subset(x = GB136_GB275_compl_pruned, select = c("Language_ID","Fixed", "Function_Words"))

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB136_GB275_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")



