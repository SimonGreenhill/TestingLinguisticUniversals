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

#Verion C: Free constituent order and gender in adjectives, articles, and pronouns 
#Free constituent order:
#NO for GB136	Is the order of core argument (i.e. S/A/P) constituents fixed?

#Gender marked on pronouns:
#YES at least for one of the following:
## GB196  Is there a male/female distinction in 2nd person independent pronouns?        
# GB197 Is there a male/female distinction in 1st person independent pronouns?
# GB030 Is there a gender distinction in independent 3rd person pronouns?

#Gender marked on adjectives and articles:
#YES for both of the following:
# GB170 Can an adnominal property word agree with the noun in gender/noun class?
# GB172 Can an article agree with the noun in gender/noun class?

#Gender marked on nouns:
#YES for at least one of the following:
#GB051        Is there a gender/noun class system where sex is a factor in class assignment?
#GB052        Is there a gender/noun class system where shape is a factor in class assignment?
#  GB053        Is there a gender/noun class system where animacy is a factor in class assignment?
#  GB054        Is there a gender/noun class system where plant status is a factor in class assignment?
#  GB192        Is there a gender system where a noun's phonological properties are a factor in class assignment?
#GB321        Is there a large class of nouns whose gender/noun class is not phonologically or semantically predictable?

# GB136:0 > GB170: 1 & GB172: 1 & (GB051|GB052|GB053|GB054|GB192|GB321: 1)

#Version C
library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB136_GB051 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB051", "GB052", "GB053", 
                                                     "GB054", "GB192", "GB321", "GB136",  
                                                     "GB170", "GB172"))

for(i in 1:nrow(GB136_GB051)){
  summ <- sum(c(as.numeric(GB136_GB051$GB051[i]), as.numeric(GB136_GB051$GB052[i]), as.numeric(GB136_GB051$GB053[i]), as.numeric(GB136_GB051$GB054[i]), (GB136_GB051$GB192[i]), (GB136_GB051$GB321[i])), na.rm = T)
  if(summ > 0 ){GB136_GB051$Gender[i] <- 1}
  else(GB136_GB051$Gender[i] <- 0)
}

GB136_GB051 <- subset(x = GB136_GB051, select = c("Language_ID", "GB136", "GB170", "GB172", "Gender"))


GB136_GB051_compl <- GB136_GB051[complete.cases(GB136_GB051),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB136_GB051_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB136_GB051_compl_pruned <- GB136_GB051_compl[ !(GB136_GB051_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB136_GB051_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile for C version 
for(i in 1:nrow(GB136_GB051_compl_pruned)){
  if(GB136_GB051_compl_pruned$GB136[i] == '0'){GB136_GB051_compl_pruned$Free[i] <- 1}
  else(GB136_GB051_compl_pruned$Free[i] <- 0)
}

for(i in 1:nrow(GB136_GB051_compl_pruned)){
  if((GB136_GB051_compl_pruned$GB170[i] == '1') & (GB136_GB051_compl_pruned$GB172[i] == '1') & (GB136_GB051_compl_pruned$Gender[i] == '1')){GB136_GB051_compl_pruned$Gender_Noun_Pronoun_Adj_Art[i] <- 1}
  else(GB136_GB051_compl_pruned$Gender_Noun_Pronoun_Adj_Art[i] <- 0)
}

GB136_GB051_compl_pruned2 <- subset(x = GB136_GB051_compl_pruned, select = c("Language_ID", "Free", "Gender_Noun_Pronoun_Adj_Art"))

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB136_GB051_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


