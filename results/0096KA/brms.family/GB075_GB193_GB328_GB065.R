#Universal 96. IF there are postpositions, THEN (IF adjectives precede nouns or relatives precede nouns, THEN demonstratives preced nouns and numerals precede nouns and possessives preced nouns) and (IF demonstratives precede nouns or numerals precede nouns or possessives precede nouns, THEN genitives precede nouns).

#N-Adp & (Adj-N | Rel-N) > PossPron-N

#GB075:1 & (GB193:1 |  GB328:1) > GB065:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB075", "GB193", "GB328", "GB065"))

data_frame_compl <- data_frame[complete.cases(data_frame),]

# prepare datafile

#making sure the first set of conditions is met: GB075:1 & (GB193:1 |  GB328:1)
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB075[i] == 1 & (data_frame_compl$GB193[i] == 1 | data_frame_compl$GB328[i] == 1))
  {data_frame_compl$Condition_1[i] <- 1}
  else(data_frame_compl$Condition_1[i] <- 0)
}

#making sure the second set of conditions is met: GB065:1
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB065[i] == 1)
  {data_frame_compl$Condition_2[i] <- 1}
  else(data_frame_compl$Condition_2[i] <- 0)
}

# write files

data_frame_compl2 <- subset(x = data_frame_compl, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)
