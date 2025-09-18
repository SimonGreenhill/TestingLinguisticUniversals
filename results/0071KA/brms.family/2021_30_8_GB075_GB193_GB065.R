#Universal 71. If a language has Postp word order, and if the adjective precedes the noun, then the genitive precedes the noun.

#N-Adp & Adj-N > Gen-N

#GB075:1 & GB193:1 > GB065:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB075", "GB193", "GB065"))

data_frame_compl <- data_frame[complete.cases(data_frame),]


# prepare datafile

#making sure the first set of conditions is met
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB075[i] == 1 & data_frame_compl$GB193[i] == 1)
  {data_frame_compl$Condition_1[i] <- 1}
  else(data_frame_compl$Condition_1[i] <- 0)
}

# setting keeping GB065:1 and setting other options to 0
data_frame_compl$GB065[data_frame_compl$GB065 == '3'] <- '0'
data_frame_compl$GB065[data_frame_compl$GB065 == '2'] <- '0'

# write files

data_frame_compl2 <- subset(x = data_frame_compl, select = c("Language_ID", "Condition_1","GB065"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

