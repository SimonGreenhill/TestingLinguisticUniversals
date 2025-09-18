#Universal 1372. A lexically distinct form of verb HAVE is generally missing in verb 
#peripheral languages (i.e. SOV, VOS). That is, a verb HAVE is generally confined to SVO languages.

#Relevant features
#GB250        Can predicative possession be expressed with a transitive 'habeo' verb?
#GB131        Is a pragmatically unmarked constituent order verb-initial for transitive clauses?
#  GB132        Is a pragmatically unmarked constituent order verb-medial for transitive clauses?        
#GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?

#GB250:1 > GB131:0 & GB132:1 & GB133:0

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB250_GB133 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB250"))

GB250_GB133_compl <- GB250_GB133[complete.cases(GB250_GB133),]

# prepare datafile 
#GB250:1 > GB131:0 & GB132:1 & GB133:0

for(i in 1:nrow(GB250_GB133_compl)){
  if((GB250_GB133_compl$GB131[i] == '0') & (GB250_GB133_compl$GB132[i] == '1') & (GB250_GB133_compl$GB133[i] == '0')) {GB250_GB133_compl$SVO[i] <- 1}
  else(GB250_GB133_compl$SVO[i] <- 0)
}

GB250_GB133_compl2 <- subset(x = GB250_GB133_compl, select = c("Language_ID", "GB250", "SVO"))

table(GB250_GB133_compl2$SVO, GB250_GB133_compl2$GB250)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB250_GB133_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

