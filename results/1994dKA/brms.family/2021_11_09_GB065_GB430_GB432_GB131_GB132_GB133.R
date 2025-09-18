#Universal 1994d. Standardized IF the attributive noun (genitive, esp. when morphologically unmarked)  precedes the head noun, THEN (i) nominal affixes (for case, number, gender/class) and adpositions are postposed (suffixes, postpositions), (ii) pronominal possessives precede the head noun, (iii) attributive adjectives precede the head noun, (iv) verbal affixes (person, number)  and pronouns (for subject) precede the verb, (v) direct objects (accusative, esp. when morphologically unmarked) precede the verb.

#non-affixed Gen-N > OV

#GB065:1 & GB430:0 & GB432:0 > GB131:0 & (GB132:1 | GB133:1) 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB065", "GB430", "GB432", "GB131", "GB132", "GB133"))

data_frame_compl <- data_frame[complete.cases(data_frame),]

# prepare datafile

#making sure the first set of conditions is met: GB065:1 & GB430:0 & GB432:0 
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB065[i] == 1 & data_frame_compl$GB430[i] == 0 & data_frame_compl$GB432[i] == 0)
  {data_frame_compl$Condition_1[i] <- 1}
  else(data_frame_compl$Condition_1[i] <- 0)
}

#making sure the second set of conditions is met: GB131:0 & (GB132:1 | GB133:1) 
for(i in 1:nrow(data_frame_compl)){
  if(data_frame_compl$GB131[i] == 0 & (data_frame_compl$GB132[i] == 1 | data_frame_compl$GB133[i] == 1))
  {data_frame_compl$Condition_2[i] <- 1}
  else(data_frame_compl$Condition_2[i] <- 0)
}

# write files
data_frame_compl2 <- subset(x = data_frame_compl, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


