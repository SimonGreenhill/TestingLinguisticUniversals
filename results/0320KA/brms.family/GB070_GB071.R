#320 (used to be 321 in the old version). If personal pronouns have an opposition of the syntactic 
# forms of nominative and accusative, then this opposition is relevant for nouns as well.
#GB070        Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)?
#0	absent	1073
#1	present	601
#?	Not known	68

#GB071        Are there morphological cases for pronominal core arguments (i.e. S/A/P)?
#0	absent	843
#1	present	709
#?	Not known	88

#GB408: Is there any accusative alignment of flagging?
#0	absent	677
#1	present	521
#?	Not known	60

# GB071:1 & GB408: 1 > GB070:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB070_GB071 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB070", "GB071","GB408"))

GB070_GB071_compl <- GB070_GB071[complete.cases(GB070_GB071),]

for(i in 1:nrow(GB070_GB071_compl)){
  if((GB070_GB071_compl$GB071[i] == '1') & (GB070_GB071_compl$GB408[i] == '1')){GB070_GB071_compl$AC_Noun[i] <- 1}
  else(GB070_GB071_compl$AC_Noun[i] <- 0)
}

GB070_GB071_compl2 <- subset(x = GB070_GB071_compl, select = c("Language_ID", "GB070", "AC_Noun"))

# checks

nrow(GB070_GB071_compl2)
table(GB070_GB071_compl2$AC_Noun, GB070_GB071_compl2$GB070)

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB070_GB071_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

