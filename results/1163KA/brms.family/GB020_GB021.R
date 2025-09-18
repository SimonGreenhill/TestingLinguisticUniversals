#Universal 1163. If a language has a grammaticalized indefinite article, it is 
#likely to also have a definite article, while the reverse does not necessarily 
#hold true.

#Relevant features
#GB020        Are there definite or specific articles?
#GB021        Do indefinite nominals commonly have indefinite articles?

#  GB021:1 >  GB020:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB020_GB021 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB020", "GB021"))

GB020_GB021_compl <- GB020_GB021[complete.cases(GB020_GB021),]

# prepare datafile 

GB020_GB021_compl2 <- subset(x = GB020_GB021_compl, select = c("Language_ID", "GB021", "GB020"))
table(GB020_GB021_compl2$GB020, GB020_GB021_compl2$GB021)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB020_GB021_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


