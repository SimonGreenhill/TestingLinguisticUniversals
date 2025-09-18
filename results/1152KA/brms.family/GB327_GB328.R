#Universal 1152. For any language, preposed relative clause implies postposed relative clause.

#Relevant features
#GB327        Can the relative clause follow the noun?        
#GB328        Can the relative clause precede the noun?
#GB328:1 > GB327:1

# GB328:1 > GB327:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB327_GB328 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB327", "GB328"))

GB327_GB328_compl <- GB327_GB328[complete.cases(GB327_GB328),]

# prepare datafile 

GB327_GB328_compl2 <- subset(x = GB327_GB328_compl, select = c("Language_ID",  "GB328", "GB327"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB327_GB328_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


