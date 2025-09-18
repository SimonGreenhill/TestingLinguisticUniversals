#Universal 1550. In verb-initial languages the dominant order of relative clauses is always 
#postnominal. 

#Relevant features
#Verb_initial - YES for GB131        Is a pragmatically unmarked constituent order verb-initial for transitive clauses? 
#While also NO for:
#GB132        Is a pragmatically unmarked constituent order verb-medial for transitive clauses?
#GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?

#Postnominal relative clauses:
#GB327 Can the relative clause follow the noun?

#GB131:1 & GB132:0 & GB133:0 > GB327:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB131_GB327 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB327"))

GB131_GB327_compl <- GB131_GB327[complete.cases(GB131_GB327),]

# prepare datafile 
#GB131:1 & GB132:0 & GB133:0 > GB327:1

for(i in 1:nrow(GB131_GB327_compl)){
  if((GB131_GB327_compl$GB131[i] == '1') & (GB131_GB327_compl$GB132[i] == '0') & (GB131_GB327_compl$GB133[i] == '0')) {GB131_GB327_compl$Verb_initial[i] <- 1}
  else(GB131_GB327_compl$Verb_initial[i] <- 0)
}

GB131_GB327_compl2 <- subset(x = GB131_GB327_compl, select = c("Language_ID", "Verb_initial", "GB327"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB131_GB327_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


