#Universal 1349. Numeral classifiers occur in the languages where plural is weakly
#developed.

#Relevant features
#GB057        Are there numeral classifiers?
#GB044        Is there productive morphological plural marking on nouns?        
#GB318        Is plural number regularly marked in the noun phrase by a dedicated phonologically free element?

#GB044:0 & GB318:0 > GB057:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB057_GB044 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB057", "GB044", "GB318"))

GB057_GB044_compl <- GB057_GB044[complete.cases(GB057_GB044),]

# prepare datafile 

for(i in 1:nrow(GB057_GB044_compl)){
  if((GB057_GB044_compl$GB044[i] == '0') & (GB057_GB044_compl$GB318[i] == '0')) {GB057_GB044_compl$No_plural[i] <- 1}
  else(GB057_GB044_compl$No_plural[i] <- 0)
}

GB057_GB044_compl2 <- subset(x = GB057_GB044_compl, select = c("Language_ID", "GB057", "No_plural"))

table(GB057_GB044_compl2$GB057, GB057_GB044_compl2$No_plural)

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB057_GB044_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

