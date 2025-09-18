#Universal 357 (used to be 358 in the old version). If in a dominant word order 
#VS the only possible forms are the forms of the V-s conjugation, then the 
#forms like V-o are also the only possible ones.

#languages with VS word order:
#GB131 Is a pragmatically unmarked constituent order verb-initial for transitive clauses?
  
#Subject-agreement markers are V-s in the following languages:
#GB091 Can the A argument be indexed by a suffix/enclitic on the verb in the simple main clause?
  
#Languages with V-o marking:
#GB093 Can the P argument be indexed by a suffix/enclitic on the verb in the simple main clause?

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB131_GB093 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB091","GB092","GB093", "GB094"))

GB131_GB093_compl <- GB131_GB093[complete.cases(GB131_GB093),]

# prepare datafile 
#languages with VS word order:
#GB131 Is a pragmatically unmarked constituent order verb-initial for transitive clauses?

#Subject-agreement markers are V-s in the following languages:
#GB091 Can the A argument be indexed by a suffix/enclitic on the verb in the simple main clause?

#Languages with V-o marking:
#GB093 Can the P argument be indexed by a suffix/enclitic on the verb in the simple main clause?

for(i in 1:nrow(GB131_GB093_compl)){
  if((GB131_GB093_compl$GB131[i] == '1') & (GB131_GB093_compl$GB132[i] == '0') & 
     (GB131_GB093_compl$GB133[i] == '0') & (GB131_GB093_compl$GB091[i] == '1') & 
     (GB131_GB093_compl$GB092[i] == '0')) {GB131_GB093_compl$VS_Vs[i] <- 1}
  else(GB131_GB093_compl$VS_Vs[i] <- 0)
}

for(i in 1:nrow(GB131_GB093_compl)){
  if((GB131_GB093_compl$GB093[i] == '1') & (GB131_GB093_compl$GB094[i] == '0')) {GB131_GB093_compl$Parg[i] <- 1}
  else(GB131_GB093_compl$Parg[i] <- 0)
}

GB131_GB093_compl2 <- subset(x = GB131_GB093_compl, select = c("Language_ID", "VS_Vs", "Parg"))

# checks
nrow(GB131_GB093_compl2)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB131_GB093_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

