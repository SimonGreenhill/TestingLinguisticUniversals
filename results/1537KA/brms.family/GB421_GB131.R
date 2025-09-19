#Universal 1537. In verb-initial languages, subordinate markers such 
#as complementizers, nominalizers, and subordinate conjunctions 
#precede their clauses. 

#Verb-initial - YES for
#GB131        Is a pragmatically unmarked constituent order verb-initial for transitive clauses?
#While NO for 
#GB132        Is a pragmatically unmarked constituent order verb-medial for transitive clauses?
#GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?

#Complementizers precedes the clause
#GB421 Is there a preposed complementizer in complements of verbs of thinking and/or knowing?

#GB131:1 & GB132:0 & GB133:0 > GB421:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB421_GB131 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB421"))

GB421_GB131_compl <- GB421_GB131[complete.cases(GB421_GB131),]

# prepare datafile 
#GB131:1 & GB132:0 & GB133:0 > GB421:1

for(i in 1:nrow(GB421_GB131_compl)){
  if((GB421_GB131_compl$GB131[i] == '1') & (GB421_GB131_compl$GB132[i] == '0') & (GB421_GB131_compl$GB133[i] == '0')) {GB421_GB131_compl$Verb_initial[i] <- 1}
  else(GB421_GB131_compl$Verb_initial[i] <- 0)
}

GB421_GB131_compl2 <- subset(x = GB421_GB131_compl, select = c("Language_ID", "Verb_initial", "GB421"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB421_GB131_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)



