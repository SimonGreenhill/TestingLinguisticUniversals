#Universal 175. If a language has Prep word order, then if the genitive follows the noun, the relative clause follows the noun.

#Adp-N & N-Gen > N-Rel

#GB074:1 & GB065:2 > GB327:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB074", "GB065", "GB327"))

data_frame_compl <- data_frame[complete.cases(data_frame),]

# prepare datafile

#making sure the first set of conditions is met: GB074:1 & GB065:2
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB074[i] == 1 & data_frame_compl$GB065[i] == 2)
  {data_frame_compl$Condition_1[i] <- 1}
  else(data_frame_compl$Condition_1[i] <- 0)
}


#making sure the second set of conditions is met: GB327:1
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB327[i] == 1)
  {data_frame_compl$Condition_2[i] <- 1}
  else(data_frame_compl$Condition_2[i] <- 0)
}

# write files
data_frame_compl2 <- subset(x = data_frame_compl, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)
