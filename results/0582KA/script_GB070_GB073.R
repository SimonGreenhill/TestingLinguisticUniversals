#Universal 582 (used to be 585 in the old version). If there is any inflection 
#in nouns, there are also some inflections in pronouns.

#Relevant features
#Nominal inflection:
#GB042	Is there productive overt morphological singular marking on nouns?
#  GB043	Is there productive morphological dual marking on nouns?
#  GB044	Is there productive morphological plural marking on nouns?
#  GB051 Is there a gender/noun class system where sex is a factor in class assignment?
#GB052 Is there a gender/noun class system where shape is a factor in class assignment?
#GB053 Is there a gender/noun class system where animacy is a factor in class assignment?
#GB054 Is there a gender/noun class system where plant status is a factor in class assignment?
#  GB070 Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)?
#  GB072 Are there morphological cases for oblique non-pronominal NPs (i.e. not S/A/P)?
#  GB165	Is there productive morphological trial marking on nouns?
#  GB166	Is there productive morphological paucal marking on nouns?
#GB192 Is there a gender system where a noun's phonological properties are a factor in class assignment?
#GB321 Is there a large class of nouns whose gender/noun class is not phonologically or semantically predictable?
#GB430	Can adnominal possession be marked by a prefix on the possessor?	
#GB431	Can adnominal possession be marked by a prefix on the possessed noun?	
#  GB432	Can adnominal possession be marked by a suffix on the possessor?
#  GB433	Can adnominal possession be marked by a suffix on the possessed noun?

#ProNominal inflection:
# GB071	Are there morphological cases for pronominal core arguments (i.e. S/A/P)?
# GB073	Are there morphological cases for independent oblique personal pronominal arguments (i.e. not S/A/P)?

# (GB042:1|GB043:1|GB044:1|GB165:1|GB166:1|GB430:1|GB431:1|GB432:1|GB433:1) > 
# (GB030:1|GB071:1|GB073:1|GB196:1|GB197:1)

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB070_GB071 <- subset(x = GB_wide_strict, select 
                      = c("Language_ID", "GB042", "GB043", "GB044",
                      "GB165", "GB166",  "GB430", "GB431", 
                      "GB432", "GB433", "GB030","GB071","GB073","GB196","GB197"))

#condition 1:
#(GB042:1|GB043:1|GB044:1|GB165:1|GB166:1|GB430:1|GB431:1|GB432:1|GB433:1) > 0
for(i in 1:nrow(GB070_GB071)){
  summ <- sum(c(as.numeric(GB070_GB071$GB042[i]), 
                as.numeric(GB070_GB071$GB043[i]), 
                as.numeric(GB070_GB071$GB044[i]), 
                as.numeric(GB070_GB071$GB165[i]), 
                as.numeric(GB070_GB071$GB166[i]), 
                as.numeric(GB070_GB071$GB430[i]), 
                as.numeric(GB070_GB071$GB431[i]), 
                as.numeric(GB070_GB071$GB432[i]), 
                as.numeric(GB070_GB071$GB433[i])), na.rm = T)
  if(summ > 0 ){GB070_GB071$Nouns_Inflection[i] <- 1}
  else(GB070_GB071$Nouns_Inflection[i] <- 0)
}

#condition 2:
#(GB030:1|GB071:1|GB073:1|GB196:1|GB197:1) > 0
for(i in 1:nrow(GB070_GB071)){
  summ <- sum(c(as.numeric(GB070_GB071$GB030[i]), 
                as.numeric(GB070_GB071$GB071[i]), 
                as.numeric(GB070_GB071$GB073[i]), 
                as.numeric(GB070_GB071$GB196[i]), 
                as.numeric(GB070_GB071$GB197[i])), na.rm = T)
  if(summ > 0 ){GB070_GB071$ProNouns_Inflection[i] <- 1}
  else(GB070_GB071$ProNouns_Inflection[i] <- 0)
}

GB070_GB071_2 <- subset(x = GB070_GB071, select = c("Language_ID", "Nouns_Inflection", "ProNouns_Inflection"))

GB070_GB071_compl <- GB070_GB071_2[complete.cases(GB070_GB071_2),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB070_GB071_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB070_GB071_pr_co_pruned <- GB070_GB071_compl[ !(GB070_GB071_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB070_GB071_pr_co_pruned$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB070_GB071_pr_co_pruned, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

