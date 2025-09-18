#Universal 1610. In languages with verb agreement, patient verb agreement appears more 
#frequently in verb-peripheral languages [i.e. verb-initial and verb-final languages] than 
#in verb-medial languages.

#Relevant features
#YES for either of
#GB131        Is a pragmatically unmarked constituent order verb-initial for transitive clauses?
#  GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?

#NO for GB132        Is a pragmatically unmarked constituent order verb-medial for transitive clauses?
  
# Patient verb agreement:
#GB094        Can the P argument be indexed by a prefix/proclitic on the verb in the simple main clause?
#GB093        Can the P argument be indexed by a suffix/enclitic on the verb in the simple main clause?

# GB094|GB093:1 > (GB131:1 | GB133:1) & GB132:0 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB093_GB094 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB093", "GB094"))

GB093_GB094_compl <- GB093_GB094[complete.cases(GB093_GB094),]


# prepare datafile 

for(i in 1:nrow(GB093_GB094_compl)){
  if(((GB093_GB094_compl$GB131[i] == '1') | (GB093_GB094_compl$GB133[i] == '1')) & (GB093_GB094_compl$GB132[i] == '0')) {GB093_GB094_compl$Verb_peripheral[i] <- 1}
  else(GB093_GB094_compl$Verb_peripheral[i] <- 0)
}

for(i in 1:nrow(GB093_GB094_compl)){
  if((GB093_GB094_compl$GB093[i] == '1') | (GB093_GB094_compl$GB094[i] == '1')) {GB093_GB094_compl$Patient_Agreement[i] <- 1}
  else(GB093_GB094_compl$Patient_Agreement[i] <- 0)
}

GB093_GB094_compl2 <- subset(x = GB093_GB094_compl, select = c("Language_ID", "Patient_Agreement", "Verb_peripheral"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB093_GB094_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)




