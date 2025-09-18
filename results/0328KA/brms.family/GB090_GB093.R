#Universal 328 (used to be 329 in the old version). If a verb agrees with a subject in prefix, 
# then verbs have affixes or clitics which agree with the direct object.

#Relevant features:
#Subject
#GB090 Can the S argument be indexed by a prefix/proclitic on the verb in the simple main clause?
#GB092 Can the A argument be indexed by a prefix/proclitic on the verb in the simple main clause?
#Object:
#GB093 Can the P argument be indexed by a suffix/enclitic on the verb in the simple main clause?
#GB094 Can the P argument be indexed by a prefix/proclitic on the verb in the simple main clause?
#GB090:1|GB092:1 > GB093:1|GB094:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB090_GB093 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB092", "GB093", "GB094"))

GB090_GB093_compl <- GB090_GB093[complete.cases(GB090_GB093),]

# prepare datafile 

for(i in 1:nrow(GB090_GB093_compl)){
  if((GB090_GB093_compl$GB093[i] == '1') | (GB090_GB093_compl$GB094[i] == '1')) {GB090_GB093_compl$Object[i] <- 1}
  else(GB090_GB093_compl$Object[i] <- 0)
}

GB090_GB093_compl2 <- subset(x = GB090_GB093_compl, select = c("Language_ID", "GB092", "Object"))

# checks

nrow(GB090_GB093_compl2)
table(GB090_GB093_compl2$Object, GB090_GB093_compl2$GB092)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB090_GB093_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

