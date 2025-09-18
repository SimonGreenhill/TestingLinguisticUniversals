#Universal 564 (used to be 566 in the old version). If the Dual extends to nouns, 
# it also extends to pronouns.

#Relevant features
#Pronouns and nouns with dual can be found under following features (limitation: 
#in the case of pronouns, those are not only dual forms but also unit augemnted 
#forms)

#Pronouns
# YES for GB031 Is there a dual or unit augmented form (in addition to plural 
#or augmented) for all person categories in the pronoun system?
#Nouns
#YES for at least one of the following:
#GB043 Is there productive morphological dual marking on nouns?        
#GB317 Is dual number regularly marked in the noun phrase by a dedicated 
#phonologically free element?

# GB043|GB317:1 > GB031:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB031_GB043 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB031", "GB043", "GB317"))

GB031_GB043_compl <- GB031_GB043[complete.cases(GB031_GB043),]

# prepare datafile 
#GB043|GB317:1 > GB031:1
for(i in 1:nrow(GB031_GB043_compl)){
  if((GB031_GB043_compl$GB043[i] == '1') | (GB031_GB043_compl$GB317[i] == '1')) {GB031_GB043_compl$Dual_Noun[i] <- 1}
  else(GB031_GB043_compl$Dual_Noun[i] <- 0)
}

GB031_GB043_compl2 <- subset(x = GB031_GB043_compl, select = c("Language_ID", "Dual_Noun", "GB031"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB031_GB043_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

