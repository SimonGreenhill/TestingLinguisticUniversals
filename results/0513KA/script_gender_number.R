# Info about Universal. 
# Universal 36. If a language has the category of gender, it always has the category of number. 

#gender
# GB030	Is there a gender distinction in independent 3rd person pronouns?
# GB051	Is there a gender/noun class system where sex is a factor in class assignment?
# GB052	Is there a gender/noun class system where shape is a factor in class assignment?
# GB053	Is there a gender/noun class system where animacy is a factor in class assignment?
# GB054	Is there a gender/noun class system where plant status is a factor in class assignment?
# GB192	Is there a gender system where a noun's phonological properties are a factor in class assignment?
# GB321	Is there a large class of nouns whose gender/noun class is not phonologically or semantically predictable?

# not used
# GB170: Can an adnominal property word agree with the noun in gender/noun class?
# GB171: Can an adnominal demonstrative agree with the noun in gender/noun class?
# GB172: Can an article agree with the noun in gender/noun class?
# GB198	Can an adnominal numeral agree with the noun in gender/noun class?

#number
# GB041	Are there several nouns (more than three) which are suppletive for number?
# GB042	Is there productive overt morphological singular marking on nouns?	
# GB043	Is there productive morphological dual marking on nouns?	
# GB044	Is there productive morphological plural marking on nouns?
# GB165	Is there productive morphological trial marking on nouns?
# GB166	Is there productive morphological paucal marking on nouns?
# GB316	Is singular number regularly marked in the noun phrase by a dedicated phonologically free element?
# GB317	Is dual number regularly marked in the noun phrase by a dedicated phonologically free element?
# GB318	Is plural number regularly marked in the noun phrase by a dedicated phonologically free element?
# GB319	Is trial number regularly marked in the noun phrase by a dedicated phonologically free element?
# GB320	Is paucal number regularly marked in the noun phrase by a dedicated phonologically free element?

# not used
# GB184: Can an adnominal property word agree with the noun in number?
# GB185: Can an adnominal demonstrative agree with the noun in number?
# GB186: Can an article agree with the noun in number?

#sum(GB030:1,GB051:1,GB052:1,GB053:1,GB054:1,GB192:1,GB321:1, GB198:1) > 0, 
#> sum(GB041:1,GB042:1,GB043:1,GB044:1,GB165:1,GB166:1,GB316:1,GB317:1,GB318:1,GB319:1,GB320:1) > 0

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

gender_number <- subset(x = GB_wide_strict, select = c("Language_ID", "GB030", "GB051","GB052","GB053", "GB054", "GB192","GB321","GB198",
                                                       "GB041","GB042","GB043","GB044","GB165","GB166","GB316","GB317","GB318","GB319","GB320"))

for(i in 1:nrow(gender_number)){
  summ <- sum(c(as.numeric(gender_number$GB030[i]), 
                as.numeric(gender_number$GB051[i]), 
                as.numeric(gender_number$GB052[i]), 
                as.numeric(gender_number$GB053[i]), 
                as.numeric(gender_number$GB054[i]), 
                as.numeric(gender_number$GB192[i]), 
                as.numeric(gender_number$GB321[i]),
                as.numeric(gender_number$GB198[i])), na.rm = T)
  if(summ > 0 ){gender_number$gender[i] <- 1}
  else(gender_number$gender[i] <- 0)
}

for(i in 1:nrow(gender_number)){
  summ <- sum(c(as.numeric(gender_number$GB041[i]), 
                as.numeric(gender_number$GB042[i]), 
                as.numeric(gender_number$GB043[i]), 
                as.numeric(gender_number$GB044[i]), 
                as.numeric(gender_number$GB165[i]), 
                as.numeric(gender_number$GB166[i]), 
                as.numeric(gender_number$GB316[i]), 
                as.numeric(gender_number$GB317[i]), 
                as.numeric(gender_number$GB318[i]), 
                as.numeric(gender_number$GB319[i]), 
                as.numeric(gender_number$GB320[i])), na.rm = T)
  if(summ > 0 ){gender_number$number[i] <- 1}
  else(gender_number$number[i] <- 0)
}

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(gender_number$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

gender_number_pruned <- gender_number[ !(gender_number$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, gender_number$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

gender_number_pruned2 <- subset(x = gender_number_pruned, select = c("Language_ID", "gender","number"))

# checks

table(gender_number_pruned2$gender, gender_number_pruned2$number)
table(is.na(gender_number_pruned2$gender))
table(is.na(gender_number_pruned2$number))

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(gender_number_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


