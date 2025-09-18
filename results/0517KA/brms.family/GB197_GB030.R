#Universal 517 (used to be 519 in the old version). If a language has gender distinctions in the 1st person, it always has gender distinctions in the 2nd or 3rd person, or in both.

#Relevant features

#if     
#YES for GB197        Is there a male/female distinction in 1st person independent pronouns?

#then YES for at least one of the conditions below: 
#GB030        Is there a gender distinction in independent 3rd person pronouns?
#GB196        Is there a male/female distinction in 2nd person independent pronouns? 

#GB197:1 > GB030:1 | GB196:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB197_GB030 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB030", "GB196", "GB197"))

GB197_GB030_compl <- GB197_GB030[complete.cases(GB197_GB030),]

# prepare datafile 

for(i in 1:nrow(GB197_GB030_compl)){
  if((GB197_GB030_compl$GB030[i] == '1') | (GB197_GB030_compl$GB196[i] == '1')) {GB197_GB030_compl$Gender_2_3[i] <- 1}
  else(GB197_GB030_compl$Gender_2_3[i] <- 0)
}

GB197_GB030_compl2 <- subset(x = GB197_GB030_compl, select = c("Language_ID", "GB197", "Gender_2_3"))

# checks
nrow(GB197_GB030_compl2)
table(GB197_GB030_compl2$GB197, GB197_GB030_compl2$Gender_2_3)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB197_GB030_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)


