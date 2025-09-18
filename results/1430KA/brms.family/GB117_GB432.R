#Universal 1430. If a language has possessive suffixes on nouns, most commonly 
#it will lack overt copula form (of 'to be') in the present tense. 

#Relevant features

#Possessive suffixes - YES for either of the folllowing:
#GB432        Can adnominal possession be marked by a suffix on the possessor?
#GB433        Can adnominal possession be marked by a suffix on the possessed noun?

#The lack of overt copula - "NO" for:
# GB117 Is there a copula for predicate nominals?

#GB432:1 | GB433:1 > GB117:0

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB117_GB432 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB117", "GB432", "GB433"))

GB117_GB432_compl <- GB117_GB432[complete.cases(GB117_GB432),]

# prepare datafile 
#GB432:1 | GB433:1 > GB117:0

for(i in 1:nrow(GB117_GB432_compl)){
  if((GB117_GB432_compl$GB432[i] == '1') | (GB117_GB432_compl$GB433[i] == '1')) {GB117_GB432_compl$Possessive_suffixes[i] <- 1}
  else(GB117_GB432_compl$Possessive_suffixes[i] <- 0)
}

for(i in 1:nrow(GB117_GB432_compl)){
  if(GB117_GB432_compl$GB117[i] == '0') {GB117_GB432_compl$No_Copula[i] <- 1}
  else(GB117_GB432_compl$No_Copula[i] <- 0)
}


GB117_GB432_compl2 <- subset(x = GB117_GB432_compl, select = c("Language_ID", "Possessive_suffixes", "No_Copula"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB117_GB432_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

