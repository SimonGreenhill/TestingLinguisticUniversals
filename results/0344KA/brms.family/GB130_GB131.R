#Universal 344 (used to be 345 in the old version). If the order VS is dominant
# with transitive verbs, then it will be dominant with intransitive verbs as well.

#relevant features
#VS in transitive clauses if "1" for:
#GB131: Is a pragmatically unmarked constituent order verb-initial for transitive clauses? (limitation: this might include not only VSO but also VOS languages)

#VS in intransitive clauses if "2" for:
#GB130: What is the pragmatically unmarked order of S and V in intransitive clauses?


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB130_GB131 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB130", "GB131"))

GB130_GB131_compl <- GB130_GB131[complete.cases(GB130_GB131),]

# prepare datafile 

for(i in 1:nrow(GB130_GB131_compl)){
  if(GB130_GB131_compl$GB130[i] == '2') {GB130_GB131_compl$VS_Intransitive[i] <- 1}
  else(GB130_GB131_compl$VS_Intransitive[i] <- 0)
}

GB130_GB131_compl2 <- subset(x = GB130_GB131_compl, select = c("Language_ID", "GB131", "VS_Intransitive"))

# checks

nrow(GB130_GB131_compl2)
table(GB130_GB131_compl2$GB131, GB130_GB131_compl2$VS_Intransitive)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB130_GB131_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

