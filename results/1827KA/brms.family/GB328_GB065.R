#Universal 1827. If a language shows the order RelNoun or AdjNoun in its noun phrase, 
#it is very likely to also show PossNoun, but not vice versa. 

#Relevant features
#PossNoun - 1 (YES): possessor-possessed
#GB065 What is the pragmatically unmarked order of adnominal possessor noun and possessed noun?
#AdjNoun - 1 (YES):
# GB193 What is the order of adnominal property word and noun?
#RelNoun - YES for:
# GB328  Can the relative clause precede the noun?

#GB328:1 | GB193:1 > GB065:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB328_GB065 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB065", "GB193", "GB328"))

GB328_GB065_compl <- GB328_GB065[complete.cases(GB328_GB065),]

# prepare datafile

for(i in 1:nrow(GB328_GB065_compl)){
  if((GB328_GB065_compl$GB193[i] == '1') | (GB328_GB065_compl$GB328[i] == '1')){GB328_GB065_compl$RelNoun_AdjNoun[i] <- 1}
  else(GB328_GB065_compl$RelNoun_AdjNoun[i] <- 0)
}

for(i in 1:nrow(GB328_GB065_compl)){
if((GB328_GB065_compl$GB065[i] == '1')){GB328_GB065_compl$PossNoun[i] <- 1}
else(GB328_GB065_compl$PossNoun[i] <- 0)
}

GB328_GB065_compl2 <- subset(x = GB328_GB065_compl, select = c("Language_ID", "RelNoun_AdjNoun", "PossNoun"))

table(GB328_GB065_compl2$RelNoun_AdjNoun, GB328_GB065_compl2$PossNoun)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB328_GB065_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


