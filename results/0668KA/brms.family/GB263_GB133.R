#Universal 668 (used to be 671 in the old version). Most question particles 
#occur in sentence-initial (or enclitic to the initial constituent) or in 
#sentence-final position. Question particles almost always occur finally in 
#SOV languages and show a greater tendency to occur initially in other types.

# IF basic word order is SOV, THEN question particles almost always occur sentence finally.
# IF basic word order is not SOV, THEN question particles show a greater tendency than with basic order SOV to occur sentence initially.

#Relevant features
#Verb-final languages (limitation: not only SOV, but also OSV):
#  GB133 Is a pragmatically unmarked constituent order verb-final for transitive clauses?
#  Other types of word orders:
#  GB131 Is a pragmatically unmarked constituent order verb-initial for transitive clauses?
#  GB132 Is a pragmatically unmarked constituent order verb-medial for transitive clauses?
#  
#  Question particle position:
#  GB263 Is there a clause-final polar interrogative particle?        
#  GB262 Is there a clause-initial polar interrogative particle?

#Version A: sentence-final particle + verb-final word order
#GB131:0 & GB132:0 & GB133:1 > GB263:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB263_GB133 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB263", "GB131", "GB132", "GB133"))

GB263_GB133_compl <- GB263_GB133[complete.cases(GB263_GB133),]

# prepare datafile 
#GB131:0 & GB132:0 & GB133:1 > GB263:1
for(i in 1:nrow(GB263_GB133_compl)){
  if((GB263_GB133_compl$GB131[i] == '0') & (GB263_GB133_compl$GB132[i] == '0') & (GB263_GB133_compl$GB133[i] == '1')) {GB263_GB133_compl$Verb_final[i] <- 1}
  else(GB263_GB133_compl$Verb_final[i] <- 0)
}

GB263_GB133_compl2 <- subset(x = GB263_GB133_compl, select = c("Language_ID", "Verb_final", "GB263"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB263_GB133_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


