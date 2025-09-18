#Universal 982 (used to be 986 in the old version). If a language has a 
#locative comparative, then it is either SOV or VSO.

#GB266        Is there a comparative construction that employs a marker 
#of the standard which elsewhere has a locational meaning?

#Word order
#YES for either of the following while also NO for GB132:
#  GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses? (limitation: not only SOV but also OSV)
#GB131        Is a pragmatically unmarked constituent order verb-initial for transitive clauses?(limitation: not only VSO but also VOS)

#GB266:1 > GB131:1 | GB133:1 & GB132:0 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB133_GB266 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB266"))

GB133_GB266_compl <- GB133_GB266[complete.cases(GB133_GB266),]

# prepare datafile 
#GB266:1 > GB131:1 | GB133:1 & GB132:0 

for(i in 1:nrow(GB133_GB266_compl)){
  if(((GB133_GB266_compl$GB131[i] == '1') | (GB133_GB266_compl$GB133[i] == '1')) & (GB133_GB266_compl$GB132[i] == '0')) {GB133_GB266_compl$Word_Order[i] <- 1}
  else(GB133_GB266_compl$Word_Order[i] <- 0)
}

GB133_GB266_compl2 <- subset(x = GB133_GB266_compl, select = c("Language_ID", "GB266", "Word_Order"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB133_GB266_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


