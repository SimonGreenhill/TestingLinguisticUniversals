#Universal 439. Non-accusative alignment may be associated with verb-initial order.

#non-accusative alignment > V-initial

#GB408:0 & GB409:1 > GB131:1 & GB132:0 & GB133:0 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB408", "GB409", "GB131", "GB132", "GB133"))

data_frame_compl <- data_frame[complete.cases(data_frame),]

# prepare datafile

#making sure the first set of conditions is met: GB408:0 & GB409:1
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB408[i] == 0 & data_frame_compl$GB409[i] == 1)
  {data_frame_compl$Condition_1[i] <- 1}
  else(data_frame_compl$Condition_1[i] <- 0)
}

#making sure the second set of conditions is met: GB131:1 & GB132:0 & GB133:0 
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB131[i] == 1 & data_frame_compl$GB132[i] == 0 & data_frame_compl$GB133[i] == 0)
  {data_frame_compl$Condition_2[i] <- 1}
  else(data_frame_compl$Condition_2[i] <- 0)
}

# write files
data_frame_compl2 <- subset(x = data_frame_compl, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)
