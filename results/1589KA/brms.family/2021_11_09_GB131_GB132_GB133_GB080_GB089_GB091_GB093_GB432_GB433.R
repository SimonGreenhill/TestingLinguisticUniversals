#Universal 1589. Verb-final languages exhibit a very strong postposing tendency which leads to their preponderance of suffixes.

#V-final > postposing & suffixes

#GB131:0 & GB132:0 & GB133:1 > GB080:1 & sum(GB089|GB091|GB093|GB432|GB433) > 0

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB080", "GB089", "GB091", "GB093", "GB432", "GB433"))

data_frame_compl <- data_frame[complete.cases(data_frame),]

# prepare datafile

#making sure the first set of conditions is met: GB131:0 & GB132:0 & GB133:1 
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB131[i] == 0 & data_frame_compl$GB132[i] == 0 & data_frame_compl$GB133[i] == 1)
  {data_frame_compl$Condition_1[i] <- 1}
  else(data_frame_compl$Condition_1[i] <- 0)
}

#making sure the second set of conditions is met: GB080:1 & sum(GB089|GB091|GB093|GB432|GB433) > 0
for(i in 1:nrow(data_frame_compl)){
  summ <- sum(c(as.numeric(data_frame_compl$GB089[i]), as.numeric(data_frame_compl$GB091[i]), as.numeric(data_frame_compl$GB093[i]), as.numeric(data_frame_compl$GB432[i]), as.numeric(data_frame_compl$GB433[i])), na.rm = T)
  if(summ > 2 &  data_frame_compl$GB080[i] == 1){data_frame_compl$Condition_2[i] <- 1}
  else(data_frame_compl$Condition_2[i] <- 0)
}

# write files
data_frame_compl2 <- subset(x = data_frame_compl, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


