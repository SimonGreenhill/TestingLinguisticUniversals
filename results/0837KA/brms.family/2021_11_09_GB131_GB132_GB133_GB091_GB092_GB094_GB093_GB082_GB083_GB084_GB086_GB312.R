#Universal 837. "VSO languages tend to have Person-Number inflection. Standardized: IF basic order is VSO, THEN there is very likely to be verbal person-number inflection or both person-number and tense-aspect-mood inflection, but not verbal tense-aspect-mood without person-number inflection."

#VSO > person & number inflection

#GB131:1 & GB132:0 & GB133:0 > (GB091:1 | GB092:1 | GB093:1 | GB094:1) | ((GB091:1 | GB092:1 | GB093:1 | GB094:1) & (GB082:1 | GB083: 1| GB084:1 | GB086:1 | GB312:1))


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB091", "GB092", "GB094", "GB093", "GB082", "GB083", "GB084", "GB086", "GB312"))

data_frame_compl <- data_frame[complete.cases(data_frame),]

# prepare datafile

#making sure the first set of conditions is met: GB131:1 & GB132:0 & GB133:0
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB131[i] == 1 & data_frame_compl$GB132[i] == 0 & data_frame_compl$GB133[i] == 0)
  {data_frame_compl$Condition_1[i] <- 1}
  else(data_frame_compl$Condition_1[i] <- 0)
}

#making sure the second set of conditions is met: (GB091:1 | GB092:1 | GB093:1 | GB094:1) | ((GB091:1 | GB092:1 | GB093:1 | GB094:1) & (GB082:1 | GB083: 1| GB084:1 | GB086:1 | GB312:1))
for(i in 1:nrow(data_frame_compl)){
  if((data_frame_compl$GB091[i] == 1 | data_frame_compl$GB092[i] == 1 | data_frame_compl$GB093[i] == 1 | data_frame_compl$GB094[i] == 1) | ((data_frame_compl$GB091[i] == 1 | data_frame_compl$GB092[i] == 1 | data_frame_compl$GB093[i] == 1 | data_frame_compl$GB094[i] == 1) & (data_frame_compl$GB082[i] == 1 | data_frame_compl$GB083[i] == 1 | data_frame_compl$GB084[i] == 1 | data_frame_compl$GB086[i] == 1 | data_frame_compl$GB312[i] == 1)))
  {data_frame_compl$Condition_2[i] <- 1}
  else(data_frame_compl$Condition_2[i] <- 0)
}

# write files
data_frame_compl2 <- subset(x = data_frame_compl, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

