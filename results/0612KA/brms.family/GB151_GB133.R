#Universal 612 (used to be 615 in the old version). If there is Switch-Reference 
# marking, languages will mostly be verb-final.

#Relevant features
#GB151        Is there an overt verb marker dedicated to signalling coreference or noncoreference between the subject of one clause and an argument of an adjacent clause ("switch reference")?
#Verb-final: 
#YES for  GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?
#NO for GB131 and GB132
#GB151:1 > GB131:0 & GB132:0 & GB133:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB151_GB133 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB151", "GB131", "GB132", "GB133"))

GB151_GB133_compl <- GB151_GB133[complete.cases(GB151_GB133),]

# prepare datafile 
for(i in 1:nrow(GB151_GB133_compl)){
  if((GB151_GB133_compl$GB131[i] == '0') & (GB151_GB133_compl$GB132[i] == '0') & (GB151_GB133_compl$GB133[i] == '1')) {GB151_GB133_compl$Verb_final[i] <- 1}
  else(GB151_GB133_compl$Verb_final[i] <- 0)
}

GB151_GB133_compl2 <- subset(x = GB151_GB133_compl, select = c("Language_ID", "GB151", "Verb_final"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB151_GB133_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


