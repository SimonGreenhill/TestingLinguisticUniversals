#UNiversal 230. IF constituent order is rigid, THEN cases are absent, and vice versa; ; 
#If cases are absent, THEN the use of prepositions is extensive, and vice versa;; 
#IF order is rigid, THEN the use of prepositions is extensive, and vice versa. : 
#IF constituent order is flexible, THEN cases are present, and vice versa; ; 
#IF cases are present, THEN the use of prepositions is sparse, and vice versa; ; 
#IF order is flexible, THEN the use of prepositions is sparse, and vice versa.

#Version E: Cases + no prepositions
#Cases - YES for either of the following:
#GB070	Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)?
#  GB072	Are there morphological cases for oblique non-pronominal NPs (i.e. not S/A/P)?
#No prepositions - NO:
#GB074	Are there prepositions?
# GB070:1 & GB072:1 > GB074:0

#Version E

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB070_GB074_GB136 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB070", "GB072", "GB074"))

GB070_GB074_GB136_compl <- GB070_GB074_GB136[complete.cases(GB070_GB074_GB136),]

# prepare datafile
# GB070:1 & GB072:1 > GB074:0

for(i in 1:nrow(GB070_GB074_GB136_compl)){
  if((GB070_GB074_GB136_compl$GB070[i] == '1') & (GB070_GB074_GB136_compl$GB072[i] == '1')){GB070_GB074_GB136_compl$Case[i] <- 1}
  else(GB070_GB074_GB136_compl$Case[i] <- 0)
}

for(i in 1:nrow(GB070_GB074_GB136_compl)){
  if(GB070_GB074_GB136_compl$GB074[i] == '0'){GB070_GB074_GB136_compl$No_Prepositions[i] <- 1}
  else(GB070_GB074_GB136_compl$No_Prepositions[i] <- 0)
}

GB070_GB074_GB136_compl2 <- subset(x = GB070_GB074_GB136_compl, select = c("Language_ID", "Case", "No_Prepositions"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB070_GB074_GB136_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)




