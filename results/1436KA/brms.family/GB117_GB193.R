#Universal 1436. If in a language the adjective follows the noun, there is an overt copula 
#form (of 'to be') in the present tense form. 

#Relevant features
#Copula
#YES for GB117 Is there a copula for predicate nominals?
#The adjective follows the noun:
#"2" for GB193 What is the order of adnominal property word and noun?
#GB193:2 > GB117:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB117_GB193 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB117", "GB193"))

GB117_GB193_compl <- GB117_GB193[complete.cases(GB117_GB193),]

# prepare datafile 
#GB193:2 > GB117:1

for(i in 1:nrow(GB117_GB193_compl)){
  if(GB117_GB193_compl$GB193[i] == '2') {GB117_GB193_compl$N_Adj[i] <- 1}
  else(GB117_GB193_compl$N_Adj[i] <- 0)
}


GB117_GB193_compl2 <- subset(x = GB117_GB193_compl, select = c("Language_ID", "N_Adj", "GB117"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB117_GB193_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


