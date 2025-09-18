#Universal 577 (used to be 580 in the old version). If any inflectional category 
#is expressed through suppletion in nouns, it is also expressed suppletively in 
#pronouns or verbs.

#Relevant features (can be tested for number only):
#GB041        Are there several nouns (more than three) which are suppletive for 
#             number?
#GB109        Is there verb suppletion for participant number?
# GB041:1 > GB109:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB041_GB109 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB041", "GB109"))

GB041_GB109_compl <- GB041_GB109[complete.cases(GB041_GB109),]

# prepare datafile

GB041_GB109_compl2 <- subset(x = GB041_GB109_compl, select = c("Language_ID", "GB041", "GB109"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB041_GB109_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

