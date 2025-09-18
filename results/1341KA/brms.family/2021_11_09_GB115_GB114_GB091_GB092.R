#Universal 1341. "If a language has a verbal reciprocal/reflexive, it must have an overt AGR marker.
#Standardized IF there is a verbal reciprocal/reflexive, THEN there must be verb agreement at least with subject."

#verbal reciprocal/reflexive > verb agreement with subject

#GB115:1 | GB114:1 > GB091:1 | GB092:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB115", "GB114", "GB091", "GB092"))

data_frame_compl <- data_frame[complete.cases(data_frame),]

# prepare datafile

#making sure the first set of conditions is met: GB115:1 | GB114:1 
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB115[i] == 1 | data_frame_compl$GB114[i] == 1)
  {data_frame_compl$Condition_1[i] <- 1}
  else(data_frame_compl$Condition_1[i] <- 0)
}

#making sure the second set of conditions is met: GB091:1 | GB092:1
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB091[i] == 1 | data_frame_compl$GB092[i] == 1)
  {data_frame_compl$Condition_2[i] <- 1}
  else(data_frame_compl$Condition_2[i] <- 0)
}

# write files
data_frame_compl2 <- subset(x = data_frame_compl, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

