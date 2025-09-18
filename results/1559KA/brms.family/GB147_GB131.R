#Universal 1599. Verb-initial languages always have a passive voice and it is almost 
#always marked in the verbal morphology (rather than by a serial verb construction as in 
#Chinese, for example). 

#Relevant features
#Verb-initial - YES for
#GB131        Is a pragmatically unmarked constituent order verb-initial for transitive clauses?
#While NO for 
#GB132        Is a pragmatically unmarked constituent order verb-medial for transitive clauses?
#GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?

#Morphological passive marked on verb:
#GB147 Is there a morphological passive marked on the lexical verb?

#GB131:1 & GB132:0 & GB133:0 > GB147:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB147_GB131 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB147"))

GB147_GB131_compl <- GB147_GB131[complete.cases(GB147_GB131),]

# prepare datafile 
#GB131:1 & GB132:0 & GB133:0 > GB147:1

for(i in 1:nrow(GB147_GB131_compl)){
  if((GB147_GB131_compl$GB131[i] == '1') & (GB147_GB131_compl$GB132[i] == '0') & (GB147_GB131_compl$GB133[i] == '0')) {GB147_GB131_compl$Verb_initial[i] <- 1}
  else(GB147_GB131_compl$Verb_initial[i] <- 0)
}

GB147_GB131_compl2 <- subset(x = GB147_GB131_compl, select = c("Language_ID", "Verb_initial", "GB147"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB147_GB131_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


