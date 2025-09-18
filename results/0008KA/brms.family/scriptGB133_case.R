# Info about Universal. 
# Universal 41. If in a language the verb follows both the nominal subject and 
#nominal object as the dominant order, the language almost always has a case system. 

# GB133: Is a pragmatically unmarked constituent order verb-final for transitive clauses?  
# GB070:Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)?  
# GB071 Are there morphological cases for pronominal core arguments (i.e. S/A/P)?
# GB072	Are there morphological cases for oblique non-pronominal NPs (i.e. not S/A/P)?
# GB073	Are there morphological cases for oblique independent personal pronouns (i.e. not S/A/P)?

#GB133:1 > GB070|GB071|GB072|GB073:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB133_case <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132","GB133", "GB070","GB071","GB072","GB073"))

GB133_case_compl <- GB133_case[complete.cases(GB133_case),]

# prepare datafile
for(i in 1:nrow(GB133_case_compl)){
  if(GB133_case_compl$GB070[i] == 1 | GB133_case_compl$GB071[i] == 1 | GB133_case_compl$GB072[i] == 1 | GB133_case_compl$GB073[i] == 1){GB133_case_compl$case[i] <- 1}
  else(GB133_case_compl$case[i] <- 0)
}

for(i in 1:nrow(GB133_case_compl)){
  if(GB133_case_compl$GB131[i] == 0 & GB133_case_compl$GB132[i] == 0 & GB133_case_compl$GB133[i] == 1){GB133_case_compl$worder[i] <- 1}
  else(GB133_case_compl$worder[i] <- 0)
}

GB133_case_compl2 <- subset(x = GB133_case_compl, select = c("Language_ID", "worder","case"))

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB133_case_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)
