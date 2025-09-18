#Universal 1415. If heads of possessive constructions agree with 
#their possessors in a given language then verbs agree with subjects 
#in that language.
#Standardized. IF heads of possessive constructions (=possessees) 
#agree with their possessors, THEN verbs agree with subjects.

#Relevant features

#GB089 Can the S argument be indexed by a suffix/enclitic on the verb in the simple main clause?
#GB090 Can the S argument be indexed by a prefix/proclitic on the verb in the simple main clause?
#  GB091 Can the A argument be indexed by a suffix/enclitic on the verb in the simple main clause?
#  GB092 Can the A argument be indexed by a prefix/proclitic on the verb in the simple main clause?
  
#  GB433        Can adnominal possession be marked by a suffix on the possessed noun?
#  GB431        Can adnominal possession be marked by a prefix on the possessed noun?

#GB431:1|GB433:1 > GB091|GB092:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB433_GB089 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB091", "GB092", "GB433", "GB431"))

GB433_GB089_compl <- GB433_GB089[complete.cases(GB433_GB089),]

# prepare datafile 
#GB431:1|GB433:1 > GB091|GB092:1

for(i in 1:nrow(GB433_GB089_compl)){
  if( (GB433_GB089_compl$GB091[i] == '1') | (GB433_GB089_compl$GB092[i] == '1')) {GB433_GB089_compl$Verbal_Agreement[i] <- 1}
  else(GB433_GB089_compl$Verbal_Agreement[i] <- 0)
}

for(i in 1:nrow(GB433_GB089_compl)){
  if( (GB433_GB089_compl$GB433[i] == '1') | (GB433_GB089_compl$GB431[i] == '1')) {GB433_GB089_compl$Possessive_Agreement[i] <- 1}
  else(GB433_GB089_compl$Possessive_Agreement[i] <- 0)
}

GB433_GB089_compl2 <- subset(x = GB433_GB089_compl, select = c("Language_ID", "Possessive_Agreement", "Verbal_Agreement"))

table(GB433_GB089_compl2$Verbal_Agreement, GB433_GB089_compl2$Possessive_Agreement)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB433_GB089_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

