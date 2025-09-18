#Universal 1621. If a language has adjectives, then the numeral tends to modify the noun directly (that is, in most languages that have adjectives the numeral does not require the occurrence of a sortal classifier), but not vice versa. 
#Adj > ¬sortal classifiers

#GB193:1|2|3 | GB068:1 | GB069:1 > GB057:0

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB193", "GB068", "GB069", "GB057"))

data_frame_compl <- data_frame[complete.cases(data_frame),]

# prepare datafile

#making sure the first set of conditions is met: GB193:1|2|3 | GB068:1 | GB069:1
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB193[i] == 1 | data_frame_compl$GB193[i] == 2 | data_frame_compl$GB193[i] == 3 | data_frame_compl$GB068[i] == 1 | data_frame_compl$GB069[i] == 1)
  {data_frame_compl$Condition_1[i] <- 1}
  else(data_frame_compl$Condition_1[i] <- 0)
}

#making sure the second set of conditions is met: GB057:0
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB057[i] == 0)
  {data_frame_compl$Condition_2[i] <- 1}
  else(data_frame_compl$Condition_2[i] <- 0)
}

# write files
data_frame_compl2 <- subset(x = data_frame_compl, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


