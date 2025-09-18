#Universal 822 (used to be 825 in the old version). Correlatives are limited to 
#verb-final languages, and in fact, are largely limited to "loose" verb-final 
#ones, namely which permit some NPs, especially "heavy" ones to occur 
#to the right of the verb without any special effect of foregrounding or 
#backgrounding.

#IF there are correlatives, THEN basic order is loosely verb-final.

#Relevant features
#GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?
#  GB330        Are there correlative relative clauses?

#GB330:1 > GB131:0 & GB132:0 & GB133:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB330_GB133 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB330", "GB131", "GB132", "GB133"))

GB330_GB133_compl <- GB330_GB133[complete.cases(GB330_GB133),]

# prepare datafile 

for(i in 1:nrow(GB330_GB133_compl)){
  if((GB330_GB133_compl$GB131[i] == '0') & (GB330_GB133_compl$GB132[i] == '0') & (GB330_GB133_compl$GB133[i] == '1')) {GB330_GB133_compl$Verb_final[i] <- 1}
  else(GB330_GB133_compl$Verb_final[i] <- 0)
}

GB330_GB133_compl2 <- subset(x = GB330_GB133_compl, select = c("Language_ID", "GB330", "Verb_final"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB330_GB133_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

