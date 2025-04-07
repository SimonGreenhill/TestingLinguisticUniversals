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

#Version B: Free constituent order and number in nouns, adjectives, and articles
#Free constituent order:
#NO for GB136	Is the order of core argument (i.e. S/A/P) constituents fixed?
#Number is declined on adjectives, and articles:
#YES for both of the following: 
#GB186 Can an article agree with the noun in number?
#GB184 Can an adnominal property word agree with the noun in number?
#Number is declined on nouns:
#YES for at least one of the following
#GB044 Is there productive morphological plural marking on nouns?
#GB042        Is there productive overt morphological singular marking on nouns?
#GB043        Is there productive morphological dual marking on nouns?
#GB044        Is there productive morphological plural marking on nouns?        
#GB165        Is there productive morphological trial marking on nouns?        
#GB166        Is there productive morphological paucal marking on nouns?
  
# GB136:0 > GB186: 1 & GB184: 1 & (GB042|GB043|GB044|GB165|GB166:1)
#Version B
library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB136_GB042 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB136", "GB042", "GB043", 
                                                     "GB044", "GB165", "GB166", "GB184", "GB186"))

for(i in 1:nrow(GB136_GB042)){
  summ <- sum(c(as.numeric(GB136_GB042$GB042[i]), as.numeric(GB136_GB042$GB043[i]), as.numeric(GB136_GB042$GB044[i], as.numeric(GB136_GB042$GB165[i]), as.numeric(GB136_GB042$GB166[i]))), na.rm = T)
  if(summ > 0 ){GB136_GB042$Number[i] <- 1}
  else(GB136_GB042$Number[i] <- 0)
}

GB136_GB042 <- subset(x = GB136_GB042, select = c("Language_ID", "GB184", "GB186", "GB136", "Number"))

GB136_GB042_compl <- GB136_GB042[complete.cases(GB136_GB042),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB136_GB042_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB136_GB042_compl_pruned <- GB136_GB042_compl[ !(GB136_GB042_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB136_GB042_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile for B version 
for(i in 1:nrow(GB136_GB042_compl_pruned)){
  if(GB136_GB042_compl_pruned$GB136[i] == '0'){GB136_GB042_compl_pruned$Free[i] <- 1}
  else(GB136_GB042_compl_pruned$Free[i] <- 0)
}

for(i in 1:nrow(GB136_GB042_compl_pruned)){
  if((GB136_GB042_compl_pruned$GB186[i] == '1') & (GB136_GB042_compl_pruned$GB184[i] == '1') & (GB136_GB042_compl_pruned$Number[i] == '1')){GB136_GB042_compl_pruned$Number_Noun_Adj_Art[i] <- 1}
  else(GB136_GB042_compl_pruned$Number_Noun_Adj_Art[i] <- 0)
}

GB136_GB042_compl_pruned2 <- subset(x = GB136_GB042_compl_pruned, select = c("Language_ID", "Free", "Number_Noun_Adj_Art"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB136_GB042_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")




