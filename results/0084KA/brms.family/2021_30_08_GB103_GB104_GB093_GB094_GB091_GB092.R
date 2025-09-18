#Universal 84. If in a language the verb agrees with anything, it agrees with some or all subjects.

#verb agreement > agreement S

#(GB103:1 | GB104: 1 | GB093:1 | GB094:1) > (GB091:1 | GB092: 1)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB103", "GB104", "GB093", "GB094", "GB091", "GB092"))

data_frame_compl <- data_frame[complete.cases(data_frame),]

# prepare datafile

# (GB103:1 | GB104: 1 | GB093:1 | GB094:1)
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB103[i] == 1 | data_frame_compl$GB104[i] == 1 | data_frame_compl$GB093[i] == 1 | data_frame_compl$GB094[i] == 1)
  {data_frame_compl$Condition_1[i] <- 1}
  else(data_frame_compl$Condition_1[i] <- 0)
}

# (GB091:1 | GB092: 1)
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB091[i] == 1 | data_frame_compl$GB092[i] == 1)
  {data_frame_compl$Condition_2[i] <- 1}
  else(data_frame_compl$Condition_2[i] <- 0)
}

# write files

data_frame_compl2 <- subset(x = data_frame_compl, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)
