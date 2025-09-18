#Universal 983 (used to be 987 in the old version). If a language has an exceed comparative, then it is SVO.

#Relevant features:
#An exceed comparative
#GB265 Is there a comparative construction that includes a form that elsewhere means 'surpass, exceed'?
  
  #Word order
  #YES for GB132 Is a pragmatically unmarked constituent order verb-medial for transitive clauses?:
  #while NO for both:
  #  GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses? 
  #GB131        Is a pragmatically unmarked constituent order verb-initial for transitive clauses?

#GB265:1 > GB131:0 & GB132:1 & GB133:0

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB132_GB265 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB265"))

GB132_GB265_compl <- GB132_GB265[complete.cases(GB132_GB265),]

# prepare datafile 
#GB265:1 > GB131:0 & GB132:1 & GB133:0

for(i in 1:nrow(GB132_GB265_compl)){
  if((GB132_GB265_compl$GB131[i] == '0') & (GB132_GB265_compl$GB132[i] == '1') & (GB132_GB265_compl$GB133[i] == '0')) {GB132_GB265_compl$SVO[i] <- 1}
  else(GB132_GB265_compl$SVO[i] <- 0)
}

GB132_GB265_compl2 <- subset(x = GB132_GB265_compl, select = c("Language_ID", "GB265", "SVO"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB132_GB265_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


