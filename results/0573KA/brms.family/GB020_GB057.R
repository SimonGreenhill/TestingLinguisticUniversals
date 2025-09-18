#Universal 573 (used to be 576 in the old version). If a language has obligatory 
#marking of (in)definiteness, then it has no obligatory marking of numeral 
#classification (but not vice versa).

#Relevant features
#Marking of (in)definiteness
#GB020        Are there definite or specific articles?        
#  GB021        Do indefinite nominals commonly have indefinite articles?
#Presence of classifiers
#  GB057        Are there numeral classifiers?

#if GB020|GB021:1 > GB057:0

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB020_GB057 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB020", "GB021", "GB057"))

GB020_GB057_compl <- GB020_GB057[complete.cases(GB020_GB057),]

# prepare datafile
##if GB020|GB021:1 > GB057:0
for(i in 1:nrow(GB020_GB057_compl)){
  if((GB020_GB057_compl$GB020[i] == '1') | (GB020_GB057_compl$GB021[i] == '1')){GB020_GB057_compl$Definiteness[i] <- 1}
  else(GB020_GB057_compl$Definiteness[i] <- 0)
}

GB020_GB057_compl2 <- subset(x = GB020_GB057_compl, select = c("Language_ID", "Definiteness", "GB057"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB020_GB057_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

