# Universal 572 (used to be 575 in the old version). If a language has obligatory 
# marking of (in)definiteness, then it has obligatory marking of nominal plurality 
# (but not vice versa).

#Relevant features:
#Marking of (in)definiteness
#GB020        Are there definite or specific articles?        
#  GB021        Do indefinite nominals commonly have indefinite articles?
  
#  Plurality (noun)
# GB041    Are there several nouns (more than three) which are suppletive for number?
# GB044    Is there productive morphological plural marking on nouns?
# GB318    Is plural number regularly marked in the noun phrase by a dedicated phonologically free element?

# GB020|GB021:1 >  GB041|GB044|GB318:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB020_GB041 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB020", "GB021", "GB041", "GB044", "GB318"))

for(i in 1:nrow(GB020_GB041)){
  summ <- sum(c(as.numeric(GB020_GB041$GB020[i]), 
                as.numeric(GB020_GB041$GB021[i])), na.rm = T)
  if(summ > 0 ){GB020_GB041$def[i] <- 1}
  else(GB020_GB041$def[i] <- 0)
}

for(i in 1:nrow(GB020_GB041)){
  summ <- sum(c(as.numeric(GB020_GB041$GB041[i]), 
                as.numeric(GB020_GB041$GB044[i]), 
                as.numeric(GB020_GB041$GB318[i])), na.rm = T)
  if(summ > 0 ){GB020_GB041$plur[i] <- 1}
  else(GB020_GB041$plur[i] <- 0)
}

GB020_GB041 <- subset(x = GB020_GB041, select = c("Language_ID", "def", "plur"))

GB020_GB041_compl <- GB020_GB041[complete.cases(GB020_GB041),]

# checks
nrow(GB020_GB041_compl)
table(GB020_GB041_compl$def, GB020_GB041_compl$plur)

#write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB020_GB041_compl, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


