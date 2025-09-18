#Universal 1028a. "There are correlations between following parameters:
#OV word order, Tensedness, Casedness, AND-Construction, Absolute Converb.
#IF basic word order OV, THEN Tensedness, Casedness, AND-Construction, and Absolute Converb.
#IF Tensedness, THEN basic word order OV, Casedness, AND-Construction, and Absolute Converb.
#IF Casedness, THEN basic word order OV, Tensedness, AND-Construction, and Absolute Converb.
#IF AND-construction, THEN basic word order OV, Tensedness, Casedness, and Absolute Converb.
#IF Absolute Converb, THEN basic word order OV, Tensedness, Casedness, and AND-Construction."

#OV -> morph tense | case

#GB131:0 & (GB132:1 | GB133:1) > GB083:1|GB070:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB083", "GB070"))

data_frame_compl <- data_frame[complete.cases(data_frame),]

# prepare datafile

#making sure the first set of conditions is met: GB131:0 & (GB132:1 | GB133:1)
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB131[i] == 0 & (data_frame_compl$GB132[i] == 1 | data_frame_compl$GB133[i] == 1))
  {data_frame_compl$Condition_1[i] <- 1}
  else(data_frame_compl$Condition_1[i] <- 0)
}


#making sure the second set of conditions is met: GB083:1|GB070:1
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB083[i] == 1 | data_frame_compl$GB070[i] == 1)
  {data_frame_compl$Condition_2[i] <- 1}
  else(data_frame_compl$Condition_2[i] <- 0)
}

# write files
data_frame_compl2 <- subset(x = data_frame_compl, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

