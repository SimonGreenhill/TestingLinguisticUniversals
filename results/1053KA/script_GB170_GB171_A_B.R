#Universal 1053. If determiners agree within NPs, modifiers are likelier 
#also to agree than not to agree.

#The universal can be tested separately for number and gender
#XX has merged these into one instead of split by gender vs. number

#Determiners:
#GB171        Can an adnominal demonstrative agree with the noun in gender/noun class?
#  GB172        Can an article agree with the noun in gender/noun class?        
#  GB198        Can an adnominal numeral agree with the noun in gender/noun class?
#  GB185        Can an adnominal demonstrative agree with the noun in number?
#  GB186        Can an article agree with the noun in number?

  
#  Modifiers:
#  GB170        Can an adnominal property word agree with the noun in gender/noun class?
#  GB184        Can an adnominal property word agree with the noun in number?

#GB171|GB172|GB198|GB185|GB186:1 > GB170|GB184:1

library(ape)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB170_GB171 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB170", "GB171", "GB172", 
                                                     "GB184","GB185","GB186","GB198"))

for(i in 1:nrow(GB170_GB171)){
  summ <- sum(c(as.numeric(GB170_GB171$GB171[i]), 
                as.numeric(GB170_GB171$GB172[i]), 
                as.numeric(GB170_GB171$GB198[i]), 
                as.numeric(GB170_GB171$GB185[i]), 
                as.numeric(GB170_GB171$GB186[i])), na.rm = T)
  if(summ > 0 ){GB170_GB171$det[i] <- 1}
  else(GB170_GB171$det[i] <- 0)
}

for(i in 1:nrow(GB170_GB171)){
  summ <- sum(c(as.numeric(GB170_GB171$GB170[i]), 
                as.numeric(GB170_GB171$GB184[i])), na.rm = T)
  if(summ > 0 ){GB170_GB171$mod[i] <- 1}
  else(GB170_GB171$mod[i] <- 0)
}

GB170_GB171 <- subset(x = GB170_GB171, select = c("Language_ID", "det", "mod"))

GB170_GB171_compl <- GB170_GB171[complete.cases(GB170_GB171),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB170_GB171_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB170_GB171_compl_pruned <- GB170_GB171_compl[ !(GB170_GB171_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB170_GB171_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB170_GB171_compl_pruned, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


