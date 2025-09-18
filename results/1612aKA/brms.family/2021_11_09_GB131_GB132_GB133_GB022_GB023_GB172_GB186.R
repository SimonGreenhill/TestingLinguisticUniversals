#Universal 1612a. "In SOV languages, when the expected order of Modifier-Noun does not occur,  agreement [between noun and modifier] tends to appear.

#SOV & N-Art > N-Art agr

#GB131:0 & GB132:0 & GB133:1 & GB022:0 & GB023:1 > GB172:1 | GB186:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB022", "GB023", "GB172", "GB186"))

data_frame_compl <- data_frame[complete.cases(data_frame),]

# prepare datafile

#making sure the first set of conditions is met: GB131:0 & GB132:0 & GB133:1 & GB022:0 & GB023:1
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB131[i] == 0 & data_frame_compl$GB132[i] == 0 & data_frame_compl$GB133[i] == 1 & data_frame_compl$GB022[i] == 1 & data_frame_compl$GB023[i] == 1)
  {data_frame_compl$Condition_1[i] <- 1}
  else(data_frame_compl$Condition_1[i] <- 0)
}

#making sure the second set of conditions is met: GB172:1 | GB186:1
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB172[i] == 1 | data_frame_compl$GB186[i] == 1)
  {data_frame_compl$Condition_2[i] <- 1}
  else(data_frame_compl$Condition_2[i] <- 0)
}

# write files
data_frame_compl2 <- subset(x = data_frame_compl, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


