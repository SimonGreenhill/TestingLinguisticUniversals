#Universal 1544. "In verb-initial languages, verbal case marking is attested to a very significant degree. IF word order is verb-initial, THEN there is relational marking (agreement, applicative, etc.) associated with the verb.
#Standardized IF word order is verb-initial, THEN there is relational marking (agreement, applicative, etc.) associated with the verb. "

#V-initial > verbal case-marking

#GB131:1 & GB132:0 & GB133:0 > GB103:1 | GB104:1 | GB108:1 | GB092:1| GB094:1 | GB091:1 | GB093:1 | GB089: 1 | GB090: 1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB103", "GB104", "GB108", "GB092", "GB094", "GB091", "GB093", "GB089", "GB090"))

data_frame_compl <- data_frame[complete.cases(data_frame),]

# prepare datafile

#making sure the first set of conditions is met: GB131:1 & GB132:0 & GB133:0
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB131[i] == 1 & data_frame_compl$GB132[i] == 0 & data_frame_compl$GB133[i] == 0)
  {data_frame_compl$Condition_1[i] <- 1}
  else(data_frame_compl$Condition_1[i] <- 0)
}


#making sure the second set of conditions is met: GB103:1 | GB104:1 | GB108:1 | GB092:1| GB094:1 | GB091:1 | GB093:1 | GB089: 1 | GB090: 1
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB103[i] == 1 | data_frame_compl$GB104[i] == 1 | data_frame_compl$GB108[i] == 1 | data_frame_compl$GB092[i] == 1 | data_frame_compl$GB094[i] == 1 | data_frame_compl$GB091[i] == 1 | data_frame_compl$GB093[i] == 1 | data_frame_compl$GB089[i] == 1 | data_frame_compl$GB090[i] == 1)
  {data_frame_compl$Condition_2[i] <- 1}
  else(data_frame_compl$Condition_2[i] <- 0)
}

# write files
data_frame_compl2 <- subset(x = data_frame_compl, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

