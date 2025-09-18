#Universal 332. If the suffix position is the only possible one for an object-affix,  then the subject-affix also occupies this position at least in some  cases.

#o suffix > s suffix

#GB094:0 & GB093:1 > GB091:1

# GB093	Can the P argument be indexed by a suffix/enclitic on the verb in the simple main clause?
# GB094	Can the P argument be indexed by a prefix/proclitic on the verb in the simple main clause?
# GB091	Can the A argument be indexed by a suffix/enclitic on the verb in the simple main clause?

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB093", "GB091", "GB094"))

data_frame_compl <- data_frame[complete.cases(data_frame),]

# prepare datafile

#making sure the first set of conditions is met: GB094:0 & GB093:1 
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB094[i] == 0 & data_frame_compl$GB093[i] == 1)
  {data_frame_compl$Condition_1[i] <- 1}
  else(data_frame_compl$Condition_1[i] <- 0)
}

#making sure the second set of conditions is met: GB091:1
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB091[i] == 1)
  {data_frame_compl$Condition_2[i] <- 1}
  else(data_frame_compl$Condition_2[i] <- 0)
}

table(data_frame_compl$Condition_1, data_frame_compl$Condition_2)

# write files
data_frame_compl2 <- subset(x = data_frame_compl, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

