#Universal 506. If a language has inflection, it always has derivation.

#inflection > word formation

#sum(GB042, GB043, GB044, GB051, GB052, GB053, GB054, GB070, GB072, GB082, GB083, GB084, GB086, GB089, GB090, GB091, GB092, GB093, GB094, GB103, GB104, GB107, GB108, GB113, GB114, GB115, GB119, GB120, GB121, GB147, GB148, GB149, GB155, GB165, GB166, GB192, GB286, GB298, GB312, GB321, GB322, GB323, GB430, GB431, GB432, GB433) >=1 > sum(GB047, GB048, GB049) >=1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB042", "GB043", "GB044", "GB051", "GB052", "GB053", "GB054", "GB070", "GB072", "GB082", "GB083", "GB084", "GB086", "GB089", "GB090", "GB091", "GB092", "GB093", "GB094", "GB103", "GB104", "GB107", "GB108", "GB113", "GB114", "GB115", "GB119", "GB120", "GB121", "GB147", "GB148", "GB149", "GB155", "GB165", "GB166", "GB192", "GB286", "GB298", "GB312", "GB321", "GB322", "GB323", "GB430", "GB431", "GB432", "GB433", "GB047", "GB048", "GB049"))

data_frame_compl <- data_frame[complete.cases(data_frame),]

# prepare datafile

#making sure the first set of conditions is met: sum(GB042, GB043, GB044, GB051, GB052, GB053, GB054, GB070, GB072, GB082, GB083, GB084, GB086, GB089, GB090, GB091, GB092, GB093, GB094, GB103, GB104, GB107, GB108, GB113, GB114, GB115, GB119, GB120, GB121, GB147, GB148, GB149, GB155, GB165, GB166, GB192, GB286, GB298, GB312, GB321, GB322, GB323, GB430, GB431, GB432, GB433) >=1
for(i in 1:nrow(data_frame_compl)){
  summ <- sum(c(as.numeric(data_frame_compl$GB042[i]), as.numeric(data_frame_compl$GB043[i]), 
                as.numeric(data_frame_compl$GB044[i]), as.numeric(data_frame_compl$GB051[i]), 
                as.numeric(data_frame_compl$GB052[i]), as.numeric(data_frame_compl$GB053[i]), 
                as.numeric(data_frame_compl$GB054[i]), as.numeric(data_frame_compl$GB070[i]), 
                as.numeric(data_frame_compl$GB072[i]), as.numeric(data_frame_compl$GB082[i]), 
                as.numeric(data_frame_compl$GB083[i]), as.numeric(data_frame_compl$GB084[i]), 
                as.numeric(data_frame_compl$GB086[i]), as.numeric(data_frame_compl$GB089[i]), 
                as.numeric(data_frame_compl$GB090[i]), as.numeric(data_frame_compl$GB091[i]), 
                as.numeric(data_frame_compl$GB092[i]), as.numeric(data_frame_compl$GB093[i]), 
                as.numeric(data_frame_compl$GB094[i]), as.numeric(data_frame_compl$GB103[i]), 
                as.numeric(data_frame_compl$GB104[i]), as.numeric(data_frame_compl$GB107[i]), 
                as.numeric(data_frame_compl$GB108[i]), as.numeric(data_frame_compl$GB113[i]),
                as.numeric(data_frame_compl$GB114[i]), as.numeric(data_frame_compl$GB115[i]), 
                as.numeric(data_frame_compl$GB119[i]), as.numeric(data_frame_compl$GB120[i]), 
                as.numeric(data_frame_compl$GB121[i]), as.numeric(data_frame_compl$GB147[i]), 
                as.numeric(data_frame_compl$GB148[i]), as.numeric(data_frame_compl$GB149[i]), 
                as.numeric(data_frame_compl$GB155[i]), as.numeric(data_frame_compl$GB165[i]),
                as.numeric(data_frame_compl$GB166[i]), as.numeric(data_frame_compl$GB192[i]), 
                as.numeric(data_frame_compl$GB286[i]), as.numeric(data_frame_compl$GB298[i]), 
                as.numeric(data_frame_compl$GB312[i]), as.numeric(data_frame_compl$GB321[i]), 
                as.numeric(data_frame_compl$GB322[i]), as.numeric(data_frame_compl$GB323[i]), 
                as.numeric(data_frame_compl$GB430[i]), as.numeric(data_frame_compl$GB431[i]),
                as.numeric(data_frame_compl$GB432[i]), as.numeric(data_frame_compl$GB433[i])
                ), na.rm = T)
  if(summ >= 1 ){data_frame_compl$Condition_1[i] <- 1}
  else(data_frame_compl$Condition_1[i] <- 0)
}

#making sure the second set of conditions is met: sum(GB047, GB048, GB049) >=1
for(i in 1:nrow(data_frame_compl)){
  summ <- sum(c(as.numeric(data_frame_compl$GB047[i]), as.numeric(data_frame_compl$GB048[i]), as.numeric(data_frame_compl$GB049[i])), na.rm = T)
  if(summ >= 1 ){data_frame_compl$Condition_2[i] <- 1}
  else(data_frame_compl$Condition_2[i] <- 0)
}

# write files
data_frame_compl2 <- subset(x = data_frame_compl, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

