#Universal 331b. If the prefix position is the only possible one for a subject-affix, then  the object-affix also occupies this position in all forms (with the  exception of Wolio) or at least in some of them (no exceptions).

#s prefix > o prefix | o suffix (weak)

#(GB092:1 & GB091:0) > GB094:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB092", "GB091", "GB094"))

data_frame_compl <- data_frame[complete.cases(data_frame),]

# prepare datafile

#making sure the first set of conditions is met: (GB092:1 & GB091:0)
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB092[i] == 1 & data_frame_compl$GB091[i] == 0)
  {data_frame_compl$Condition_1[i] <- 1}
  else(data_frame_compl$Condition_1[i] <- 0)
}

#making sure the second set of conditions is met: GB094:1
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB094[i] == 1)
  {data_frame_compl$Condition_2[i] <- 1}
  else(data_frame_compl$Condition_2[i] <- 0)
}

# write files
data_frame_compl2 <- subset(x = data_frame_compl, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

