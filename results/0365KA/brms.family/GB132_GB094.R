#Universal 365 (used to be 366 in the old version). If the verb agrees with the subject and object 
#and there is at least one o-V form, then the dominant order is SV.

#Relevant features
#Picking languages with SV word order (= not verb-initial) - YES for either of them:
#GB132 Is a pragmatically unmarked constituent order verb-medial for transitive clauses?
#GB133 Is a pragmatically unmarked constituent order verb-final for transitive clauses?

#Languages with verbs that agree with objects (o-V):
#  GB094 Can the P argument be indexed by a prefix/proclitic on the verb in the simple main clause?

#Langauges with verbs that agree with subjects:
#  GB091 Can the A argument be indexed by a suffix/enclitic on the verb in the simple main clause?
#  GB092 Can the A argument be indexed by a prefix/proclitic on the verb in the simple main clause?

# GB094:1 & (GB091:1 | GB092:1) > (GB132:1 | GB133:1) & GB131:0


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB132_GB094 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB094", "GB091", "GB092"))

GB132_GB094_compl <- GB132_GB094[complete.cases(GB132_GB094),]

# prepare datafile 

#IF 1 for 
#  GB091 Can the A argument be indexed by a suffix/enclitic on the verb in the simple main clause?
#OR
#  GB092 Can the A argument be indexed by a prefix/proclitic on the verb in the simple main clause?
#AND 1 for
#  GB094 Can the P argument be indexed by a prefix/proclitic on the verb in the simple main clause?
#THEN
#YES for either of them:
#GB132 Is a pragmatically unmarked constituent order verb-medial for transitive clauses?
#GB133 Is a pragmatically unmarked constituent order verb-final for transitive clauses?

for(i in 1:nrow(GB132_GB094_compl)){
  if(((GB132_GB094_compl$GB091[i] == '1') | (GB132_GB094_compl$GB092[i] == '1')) & (GB132_GB094_compl$GB094[i] == '1')) {GB132_GB094_compl$Agreement[i] <- 1}
  else(GB132_GB094_compl$Agreement[i] <- 0)
}

for(i in 1:nrow(GB132_GB094_compl)){
  if((GB132_GB094_compl$GB131[i] == '0') & ((GB132_GB094_compl$GB132[i] == '1') | (GB132_GB094_compl$GB133[i] == '1'))) {GB132_GB094_compl$SV[i] <- 1}
  else(GB132_GB094_compl$SV[i] <- 0)
}

GB132_GB094_compl2 <- subset(x = GB132_GB094_compl, select = c("Language_ID", "Agreement", "SV"))

# checks

nrow(GB132_GB094_compl2)
table(GB132_GB094_compl2$Agreement, GB132_GB094_compl2$SV)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB132_GB094_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


  