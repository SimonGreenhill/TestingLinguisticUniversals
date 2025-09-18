#UNiversal 230. IF constituent order is rigid, THEN cases are absent, and vice versa; ; 
#If cases are absent, THEN the use of prepositions is extensive, and vice versa;; 
#IF order is rigid, THEN the use of prepositions is extensive, and vice versa. : 
#IF constituent order is flexible, THEN cases are present, and vice versa; ; 
#IF cases are present, THEN the use of prepositions is sparse, and vice versa; ; 
#IF order is flexible, THEN the use of prepositions is sparse, and vice versa.


#Version D: Flexible constituent word order + cases
# "#IF constituent order is flexible, THEN cases are present, and vice versa; ; "
#Flexible WO - NO:
#GB136	Is the order of core argument (i.e. S/A/P) constituents fixed?
#Cases - YES for either of the following:
#GB070	Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)?
#  GB072	Are there morphological cases for oblique non-pronominal NPs (i.e. not S/A/P)?
#GB136:0 > GB070:1 | GB072:1

#Version D


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB070_GB074_GB136 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB070", "GB072", "GB136"))

GB070_GB074_GB136_compl <- GB070_GB074_GB136[complete.cases(GB070_GB074_GB136),]

# prepare datafile
#GB136:0 > GB070:1 | GB072:1
for(i in 1:nrow(GB070_GB074_GB136_compl)){
  if((GB070_GB074_GB136_compl$GB070[i] == '1') | (GB070_GB074_GB136_compl$GB072[i] == '1')){GB070_GB074_GB136_compl$Case[i] <- 1}
  else(GB070_GB074_GB136_compl$Case[i] <- 0)
}

for(i in 1:nrow(GB070_GB074_GB136_compl)){
  if(GB070_GB074_GB136_compl$GB136[i] == '0'){GB070_GB074_GB136_compl$Flexible_WO[i] <- 1}
  else(GB070_GB074_GB136_compl$Flexible_WO[i] <- 0)
}

GB070_GB074_GB136_compl2 <- subset(x = GB070_GB074_GB136_compl, select = c("Language_ID",  "Flexible_WO", "Case"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB070_GB074_GB136_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

