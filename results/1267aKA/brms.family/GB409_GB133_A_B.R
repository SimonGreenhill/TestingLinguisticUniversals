#Universal 1267. Ergative systems are found only in SOV and VSO languages. SVO languages are never ergative.

#IF alignment is ergative, THEN basic order is not SVO.
#IF basic order is SVO, THEN alignment is not ergative; 

#Relevant features
#GB131        Is a pragmatically unmarked constituent order verb-initial for transitive clauses?
#  GB132        Is a pragmatically unmarked constituent order verb-medial for transitive clauses?        
#  GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?
#  GB409        Is there any ergative alignment of flagging?

#Version A: SOV and VSO + ergative : GB409:1 > GB131|GB133:1 & GB132:0

#Version A

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB409_GB133 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB409"))

GB409_GB133_compl <- GB409_GB133[complete.cases(GB409_GB133),]

# prepare datafile 
#GB409:1 > GB131|GB133:1 & GB132:0
for(i in 1:nrow(GB409_GB133_compl)){
  if(((GB409_GB133_compl$GB131[i] == '1') | (GB409_GB133_compl$GB133[i] == '1')) & (GB409_GB133_compl$GB132[i] == '0')) {GB409_GB133_compl$VSO_SOV[i] <- 1}
  else(GB409_GB133_compl$VSO_SOV[i] <- 0)
}


GB409_GB133_compl2 <- subset(x = GB409_GB133_compl, select = c("Language_ID", "GB409", "VSO_SOV"))

table(GB409_GB133_compl2$VSO_SOV, GB409_GB133_compl2$GB409)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB409_GB133_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

