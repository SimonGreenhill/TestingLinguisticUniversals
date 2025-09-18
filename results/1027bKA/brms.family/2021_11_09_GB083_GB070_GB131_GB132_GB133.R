#Universal 1027b. "There are correlations between following parameters:
#VO word order, Non-tensedness, Non-casedness, WITH-Construction, Non-Absolute Converb.
#Standardized IF VO, THEN Non-tensedness, Non-casedness, WITH-Construction, and Non-Absolute Converb.
#IF Non-tensedness, THEN VO, Non-casedness, WITH-Construction, and Non-Absolute Converb.
#IF Non-casedness, THEN VO, Non-tensedness, WITH-Construction, and Non-Absolute Converb.
#IF AND-construction, THEN VO, Non-tensedness, Non-casedness, and Non-Absolute Converb.
#IF Non-Absolute Converb, THEN VO, Non-tensedness, Non-casedness, and WITH-Construction."

#¬morph tense -> ¬case | VO

#GB083:0 > ((GB131:1 | GB132:1) & GB133:0)  | GB070:0

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB083", "GB070", "GB131", "GB132", "GB133"))

data_frame_compl <- data_frame[complete.cases(data_frame),]

# prepare datafile

#making sure the first set of conditions is met: GB083:0

for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB083[i] == 0)
  {data_frame_compl$Condition_1[i] <- 1}
  else(data_frame_compl$Condition_1[i] <- 0)
}

#making sure the second set of conditions is met: ((GB131:1 | GB132:1) & GB133:0)  | GB070:0

for(i in 1:nrow(data_frame_compl)){
  if(((data_frame_compl$GB131[i] == 1 | data_frame_compl$GB132[i] == 1) & data_frame_compl$GB133[i] == 0) | data_frame_compl$GB070[i] == 0)
  {data_frame_compl$Condition_2[i] <- 1}
  else(data_frame_compl$Condition_2[i] <- 0)
}

# write files
data_frame_compl2 <- subset(x = data_frame_compl, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


