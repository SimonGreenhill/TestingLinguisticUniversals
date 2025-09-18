#Universal 1593. Internally headed relative clauses occur only in languages 
#manifesting null  anaphora (that is, the use of null NPs in place of lexical 
#pronouns etc. in most argument positions). 

#Relevant features
#GB329 Are there internally-headed relative clauses?
# GB522 Can the S or A argument be omitted from a pragmatically unmarked 
#clause when the referent is inferrable from context ("pro-drop" or "null  anaphora")?

#GB329:1 > GB522:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB329_GB522 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB329", "GB522"))

GB329_GB522_compl <- GB329_GB522[complete.cases(GB329_GB522),]

# prepare datafile 

GB329_GB522_compl2 <- subset(x = GB329_GB522_compl, select = c("Language_ID", "GB329", "GB522"))

table(GB329_GB522_compl2$GB329, GB329_GB522_compl2$GB522)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB329_GB522_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


