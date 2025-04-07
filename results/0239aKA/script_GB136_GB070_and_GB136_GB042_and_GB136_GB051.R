#Universal 239. If constituent order is free, all potentially declinable kinds of 
# nominal words in relationships of determination – nouns, pronouns, adjectives 
# (including articles) – are declined at least for case, the cardinal relational category, 
# but presumably for number and gender as well.

#Relevant Grambank features:
#GB136	Is the order of core argument (i.e. S/A/P) constituents fixed?
#Case pronouns: GB071 Are there morphological cases for pronominal core arguments (i.e. S/A/P)?
#  Case nouns: GB070 Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)?
  
#  Agreement with number:
#  Number on nouns: GB044 Is there productive morphological plural marking on nouns?
#  Number on  articles: GB186 Can an article agree with the noun in number?
#  Number on adjectives: GB184 Can an adnominal property word agree with the noun in number?
  
#  Agreement with gender (the presence of gender in noun is already implied):
# GB170        Can an adnominal property word agree with the noun in gender/noun class?
# GB172        Can an article agree with the noun in gender/noun class?
# GB196        Is there a male/female distinction in 2nd person independent pronouns?        
# GB197        Is there a male/female distinction in 1st person independent pronouns?
# GB030        Is there a gender distinction in independent 3rd person pronouns?

#Free constituent order:
#NO for GB136	Is the order of core argument (i.e. S/A/P) constituents fixed?

#The feature can be tested in several parts

#Version A: Free constituent order and case in nouns and pronouns

#Free constituent order:
#NO for GB136	Is the order of core argument (i.e. S/A/P) constituents fixed?
#Cases in nouns AND pronouns
#YES for at least one pronominal case feature and YES for at least one non-pronominal case feature:
#GB071 Are there morphological cases for pronominal core arguments (i.e. S/A/P)?
#GB073	Are there morphological cases for independent oblique personal pronominal arguments (i.e. not S/A/P)?
#GB072	Are there morphological cases for oblique non-pronominal NPs (i.e. not S/A/P)?
#GB070 Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)?

#Version A
#GB136:0 > GB070|GB072: 1 & GB071|GB073: 1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB136_GB070 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB136", "GB070", "GB071", "GB072", "GB073"))

GB136_GB070_compl <- GB136_GB070[complete.cases(GB136_GB070),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB136_GB070_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB136_GB070_compl_pruned <- GB136_GB070_compl[ !(GB136_GB070_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB136_GB070_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile for A version 
for(i in 1:nrow(GB136_GB070_compl_pruned)){
  if(GB136_GB070_compl_pruned$GB136[i] == '0'){GB136_GB070_compl_pruned$Free[i] <- 1}
  else(GB136_GB070_compl_pruned$Free[i] <- 0)
}

for(i in 1:nrow(GB136_GB070_compl_pruned)){
  if((GB136_GB070_compl_pruned$GB070[i] == '1' | GB136_GB070_compl_pruned$GB072[i] == '1') & (GB136_GB070_compl_pruned$GB071[i] == '1' | GB136_GB070_compl_pruned$GB073[i] == '1')){GB136_GB070_compl_pruned$Case[i] <- 1}
  else(GB136_GB070_compl_pruned$Case[i] <- 0)
}

GB136_GB070_compl_pruned2 <- subset(x = GB136_GB070_compl_pruned, select = c("Language_ID", "Free", "Case"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB136_GB070_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


