#Universal 1653. If a language is non-tensed, the nominal predicate does not require a copula. 

#Relevant features:

#Non-tensed languages - NO for all of the following:
# GB082        Is there overt morphological marking of present tense on verbs?        
#GB083        Is there overt morphological marking on the verb dedicated to past tense?        
#GB084        Is there overt morphological marking on the verb dedicated to future tense?        
#GB110        Is there verb suppletion for tense or aspect?           
#GB121        Can tense be marked by an inflecting word ("auxiliary verb")?        
#GB521        Can tense be marked by a non-inflecting word ("auxiliary particle")?
#Copula:
#GB117 Is there a copula for predicate nominals?

# GB083:0 > GB117:0


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB117_GB082 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB083", "GB117"))

GB117_GB082_compl <- GB117_GB082[complete.cases(GB117_GB082),]

table(GB117_GB082_compl$GB083, GB117_GB082_compl$GB117)

GB117_GB082_compl$GB083[GB117_GB082_compl$GB083 == '0'] <- 'no'
GB117_GB082_compl$GB083[GB117_GB082_compl$GB083 == '1'] <- '0'
GB117_GB082_compl$GB083[GB117_GB082_compl$GB083 == 'no'] <- '1'

GB117_GB082_compl$GB117[GB117_GB082_compl$GB117 == '0'] <- 'no'
GB117_GB082_compl$GB117[GB117_GB082_compl$GB117 == '1'] <- '0'
GB117_GB082_compl$GB117[GB117_GB082_compl$GB117 == 'no'] <- '1'

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB117_GB082_compl, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


