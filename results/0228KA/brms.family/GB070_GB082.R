#Universal 228. If nouns inflect for case, then verbs inflect for some inflectional category.

#Relevant features:

#Case - YES for either of:
#GB070	Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)?
#GB072	Are there morphological cases for oblique non-pronominal NPs (i.e. not S/A/P)?

#Verbal inflection presence:
#GB111        Are there conjugation classes?
#GB079        Do verbs have prefixes/proclitics, other than those that only mark A, S or P (do include portmanteau: A & S + TAM)?     
#GB090        Can the S argument be indexed by a prefix/proclitic on the verb in the simple main clause?        
#  GB092        Can the A argument be indexed by a prefix/proclitic on the verb in the simple main clause?        
#  GB094        Can the P argument be indexed by a prefix/proclitic on the verb in the simple main clause?        
#  GB080        Do verbs have suffixes/enclitics, other than those that only mark A, S or P (do include portmanteau: A & S + TAM)?
#  GB089        Can the S argument be indexed by a suffix/enclitic on the verb in the simple main clause?        
#  GB091        Can the A argument be indexed by a suffix/enclitic on the verb in the simple main clause?
#  GB093        Can the P argument be indexed by a suffix/enclitic on the verb in the simple main clause?
#GB119        Can mood be marked by an inflecting word ("auxiliary verb")? 
#  GB120        Can aspect be marked by an inflecting word ("auxiliary verb")?      
#  GB121        Can tense be marked by an inflecting word ("auxiliary verb")?        
#  GB298        Can standard negation be marked by an inflecting word ("auxiliary verb")?
#  GB082        Is there overt morphological marking of present tense on verbs?
#  GB083        Is there overt morphological marking on the verb dedicated to past tense?
#  GB084        Is there overt morphological marking on the verb dedicated to future tense?
#GB312	Is there overt morphological marking on the verb dedicated to mood?
#GB086	Is a morphological distinction between perfective and imperfective aspect available on verbs?

#GB070:1|GB072:1 > GB111|GB079|GB090|GB092|GB094|GB080|GB089|GB091|GB093|GB119|GB120|GB121|GB298|GB082|GB083|GB084|GB312|GB086:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB070_GB082 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB070", "GB072","GB111", "GB079", "GB090",  "GB092", "GB094", "GB080",  "GB089", "GB091", "GB093",  "GB119", "GB120", "GB121", "GB298", "GB082", "GB083", "GB084",  "GB312","GB086"))

for(i in 1:nrow(GB070_GB082)){
  summ <- sum(c(as.numeric(GB070_GB082$GB312[i]),
                as.numeric(GB070_GB082$GB086[i]),
                as.numeric(GB070_GB082$GB111[i]), 
                as.numeric(GB070_GB082$GB079[i]), 
                as.numeric(GB070_GB082$GB080[i]), 
                as.numeric(GB070_GB082$GB090[i]),
                as.numeric(GB070_GB082$GB089[i]),
                as.numeric(GB070_GB082$GB091[i]), 
                as.numeric(GB070_GB082$GB092[i]), 
                as.numeric(GB070_GB082$GB093[i]), 
                as.numeric(GB070_GB082$GB094[i]), 
                as.numeric(GB070_GB082$GB119[i]), 
                as.numeric(GB070_GB082$GB121[i]), 
                as.numeric(GB070_GB082$GB120[i]), 
                as.numeric(GB070_GB082$GB298[i]), 
                as.numeric(GB070_GB082$GB082[i]), 
                as.numeric(GB070_GB082$GB083[i]), 
                as.numeric(GB070_GB082$GB084[i])), na.rm = T)
  if(summ > 0 ){GB070_GB082$Verbal_Inflection[i] <- 1}
  else(GB070_GB082$Verbal_Inflection[i] <- 0)
}

GB070_GB082 <- subset(x = GB070_GB082, select = c("Language_ID", "GB070", "GB072", "Verbal_Inflection"))

GB070_GB082_compl <- GB070_GB082[complete.cases(GB070_GB082),]

# prepare datafile
for(i in 1:nrow(GB070_GB082_compl)){
  if((GB070_GB082_compl$GB070[i] == '1') | (GB070_GB082_compl$GB072[i] == '1')){GB070_GB082_compl$Case[i] <- 1}
  else(GB070_GB082_compl$Case[i] <- 0)
}

GB070_GB082_compl2 <- subset(x = GB070_GB082_compl, select = c("Language_ID", "Case", "Verbal_Inflection"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB070_GB082_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)
