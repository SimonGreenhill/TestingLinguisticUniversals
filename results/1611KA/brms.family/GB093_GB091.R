#Universal 1611. All languages with verb-patient agreement, regardless of type, 
#also have verb agreement with the agent as well.

#Relevant features
#GB094        Can the P argument be indexed by a prefix/proclitic on the verb in the simple main clause?
#GB093        Can the P argument be indexed by a suffix/enclitic on the verb in the simple main clause?
  
#GB092        Can the A argument be indexed by a prefix/proclitic on the verb in the simple main clause?
#GB091        Can the A argument be indexed by a suffix/enclitic on the verb in the simple main clause?

#GB094:1 | GB093:1 > GB092:1 | GB091:1


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB093_GB091 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB091", "GB092", "GB093", "GB094"))

GB093_GB091_compl <- GB093_GB091[complete.cases(GB093_GB091),]

# prepare datafile 

for(i in 1:nrow(GB093_GB091_compl)){
  if((GB093_GB091_compl$GB093[i] == '1') | (GB093_GB091_compl$GB094[i] == '1')) {GB093_GB091_compl$Patient_Agreement[i] <- 1}
  else(GB093_GB091_compl$Patient_Agreement[i] <- 0)
}

for(i in 1:nrow(GB093_GB091_compl)){
  if((GB093_GB091_compl$GB091[i] == '1') | (GB093_GB091_compl$GB092[i] == '1')) {GB093_GB091_compl$Agent_Agreement[i] <- 1}
  else(GB093_GB091_compl$Agent_Agreement[i] <- 0)
}

GB093_GB091_compl2 <- subset(x = GB093_GB091_compl, select = c("Language_ID", "Patient_Agreement", "Agent_Agreement"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB093_GB091_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)




