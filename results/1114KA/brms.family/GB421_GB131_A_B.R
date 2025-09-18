#Universal 1114. VO languages are exceptionlessly Comp-initial. OV languages exemplify both 
#final complementizers and initial complementizers.

#IF basic word order is VO, THEN complementizers are clause-initial.
#OR, BY CONTRAPOSITION:
#IF complementizers are clause-final, THEN basic word order is OV.

#Relevant features
#GB421        Is there a preposed complementizer in complements of verbs of thinking and/or knowing?
#GB422        Is there a postposed complementizer in complements of verbs of thinking and/or knowing?
  
#GB131        Is a pragmatically unmarked constituent order verb-initial for transitive clauses?
#GB132 Is a pragmatically unmarked constituent order verb-medial for transitive clauses?
#GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?

#Version A: VO languages are exceptionlessly Comp-initial GB131:1|GB132:1 & GB133:0 > GB421:1
#VO languages:
#YES for either of #GB131 Is a pragmatically unmarked constituent order verb-initial for transitive clauses?
#OR GB132 Is a pragmatically unmarked constituent order verb-medial for transitive clauses?
#While also NO for GB133

#Comp-initial:
#YES for GB421 Is there a preposed complementizer in complements of verbs of thinking and/or knowing?

#(GB131:1 | GB132:1) & GB133:0  > GB421:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")
GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB421_GB131 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB421"))

GB421_GB131_compl <- GB421_GB131[complete.cases(GB421_GB131),]

# prepare datafile 
# GB131:1|GB132:1 & GB133:0 > GB421:1

for(i in 1:nrow(GB421_GB131_compl)){
  if(((GB421_GB131_compl$GB131[i] == '1') | (GB421_GB131_compl$GB132[i] == '1')) & (GB421_GB131_compl$GB133[i] == '0')) {GB421_GB131_compl$VO[i] <- 1}
  else(GB421_GB131_compl$VO[i] <- 0)
}

GB421_GB131_compl2 <- subset(x = GB421_GB131_compl, select = c("Language_ID", "VO", "GB421"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB421_GB131_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


