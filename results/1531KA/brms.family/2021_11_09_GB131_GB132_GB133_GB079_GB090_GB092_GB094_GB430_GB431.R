#Universal 1531. Verb-initial languages evidence significant prefixing, though normally there is some suffixing as well. There is a possibility of ambi-fixing (discontinuous affixes), and a somewhat greater than chance tendency for discontinuous demonstratives. 

#V-initial > prefixes

#GB131:1 & GB132:0 & GB133:0 > sum(GB079 & GB090 & GB092 & GB094 & GB430 & GB431) => 2

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB079", "GB090", "GB092", "GB094", "GB430", "GB431"))

data_frame_compl <- data_frame[complete.cases(data_frame),]

# prepare datafile

#making sure the first set of conditions is met: GB131:1 & GB132:0 & GB133:0 
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB131[i] == 1 & data_frame_compl$GB132[i] == 0 & data_frame_compl$GB133[i] == 0)
  {data_frame_compl$Condition_1[i] <- 1}
  else(data_frame_compl$Condition_1[i] <- 0)
}


#making sure the second set of conditions is met: sum(GB079 & GB090 & GB092 & GB094 & GB430 & GB431) => 2
for(i in 1:nrow(data_frame_compl)){
  summ <- sum(c(as.numeric(data_frame_compl$GB079[i]), as.numeric(data_frame_compl$GB090[i]), as.numeric(data_frame_compl$GB092[i]), as.numeric(data_frame_compl$GB094[i]), as.numeric(data_frame_compl$GB430[i]), as.numeric(data_frame_compl$GB431[i])), na.rm = T)
  if(summ >= 2 ){data_frame_compl$Condition_2[i] <- 1}
  else(data_frame_compl$Condition_2[i] <- 0)
}

# write files
data_frame_compl2 <- subset(x = data_frame_compl, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

