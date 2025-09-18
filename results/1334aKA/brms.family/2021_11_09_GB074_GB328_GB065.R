#Universal 1334a. "Prepositional Noun-Modifier Hierarchy (PrNMH): If a language is prepositional, then
#if Relative clause Noun order then Genitive Noun order,
#if Genitive Noun order then Adjective Noun order, and
#if Adjective Noun order then Demonstrative Noun order."

#Adp-N & Rel-N > Gen-N

#GB074:1 & GB328:1 > GB065:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB074", "GB328", "GB065"))

data_frame_compl <- data_frame[complete.cases(data_frame),]

# prepare datafile

#making sure the first set of conditions is met: GB074:1 & GB328:1
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB074[i] == 1 & data_frame_compl$GB328[i] == 1 )
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

