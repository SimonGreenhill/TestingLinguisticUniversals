#Universal 73. If the demonstrative determiner follows the noun, then the adjective follows the noun.

#N-Dem > N-Adj

#GB025:2 > GB193:2

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB193", "GB025"))

data_frame_compl <- data_frame[complete.cases(data_frame),]

# prepare datafile

# GB025:2 
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB025[i] == 2)
  {data_frame_compl$Condition_1[i] <- 1}
  else(data_frame_compl$Condition_1[i] <- 0)
}

#GB193:2
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB193[i] == 2)
  {data_frame_compl$Condition_2[i] <- 1}
  else(data_frame_compl$Condition_2[i] <- 0)
}

# write files

data_frame_compl2 <- subset(x = data_frame_compl, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)
