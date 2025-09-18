#Universal 1741. There is more prefixing on verb than on noun. If a language has any prefixes on noun, it will also have prefixes on verb with considerably more than chance frequency.

#Gen prefix > verbal prefixes

#GB430:1 | GB431:1 > sum(GB079:1,GB090:1, GB092:1, GB094:1) > 0

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB430", "GB431", "GB079", "GB090", "GB092", "GB094" ))

data_frame_compl <- data_frame[complete.cases(data_frame),]

# prepare datafile

#making sure the first set of conditions is met: GB430:1 | GB431:1
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB430[i] == 1 | data_frame_compl$GB431[i] == 1)
  {data_frame_compl$Condition_1[i] <- 1}
  else(data_frame_compl$Condition_1[i] <- 0)
}


#making sure the second set of conditions is met: sum(GB079:1,GB090:1, GB092:1, GB094:1) > 0
for(i in 1:nrow(data_frame_compl)){
  summ <- sum(c(as.numeric(data_frame_compl$GB079[i]), as.numeric(data_frame_compl$GB090[i]), as.numeric(data_frame_compl$GB092[i]), as.numeric(data_frame_compl$GB094[i])), na.rm = T)
  if(summ > 0 ){data_frame_compl$Condition_2[i] <- 1}
  else(data_frame_compl$Condition_2[i] <- 0)
}

# write files
data_frame_compl2 <- subset(x = data_frame_compl, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


