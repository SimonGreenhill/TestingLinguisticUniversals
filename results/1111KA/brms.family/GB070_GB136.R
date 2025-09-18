#Universal 1111. Lack of inflectional morphology implies fixed word order of direct nominal 
#arguments. The converse is not true, hardly even a tendency.

#IF case inflection on direct nominal arguments gets lost, THEN their word order becomes fixed, but not vice versa.

#Relevant features
#GB136        Is the order of core argument (i.e. S/A/P) constituents fixed?
#GB070 Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)?
#GB072	Are there morphological cases for oblique non-pronominal NPs (i.e. not S/A/P)?	

#GB070:0 & GB072:0 > GB136:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB070_GB136 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB136","GB070", "GB072"))

GB070_GB136_compl <- GB070_GB136[complete.cases(GB070_GB136),]

# prepare datafile 

for(i in 1:nrow(GB070_GB136_compl)){
  if((GB070_GB136_compl$GB070[i] == 0) & (GB070_GB136_compl$GB072[i] == 0))
  {GB070_GB136_compl$Condition_2[i] <- 1}
  else(GB070_GB136_compl$Condition_2[i] <- 0)
}

GB070_GB136_compl2 <- subset(x = GB070_GB136_compl, select = c("Language_ID", "Condition_2", "GB136"))
table(GB070_GB136_compl2$Condition_2, GB070_GB136_compl2$GB136)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB070_GB136_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

