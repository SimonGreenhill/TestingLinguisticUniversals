#Universal 2020. A system of switch-reference marking is found only in languages with an 
#accusative syntax.

#Relevant features:
#GB151 Is there an overt verb marker dedicated to signalling coreference or  noncoreference between the subject of one clause and an argument of an adjacent clause ("switch reference")?
#  GB408 Is there any accusative alignment of flagging?

#GB151:1 > GB408:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB151_GB408 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB151", "GB408"))

GB151_GB408_compl <- GB151_GB408[complete.cases(GB151_GB408),]

# prepare datafile

GB151_GB408_compl2 <- subset(x = GB151_GB408_compl, select = c("Language_ID", "GB151", "GB408"))

table(GB151_GB408_compl2$GB151, GB151_GB408_compl2$GB408)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB151_GB408_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


