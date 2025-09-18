#Universal 1142. Passivization is prominent in SVO languages, but not at 
#all in OV languages.
#Standardized - IF there is a passive, THEN basic word order will be SVO rather than (S)OV.
# passive > SVO

#Relevant features
#Passive present (passivisation)
#GB147        Is there a morphological passive marked on the lexical verb?      
#  GB304        Can the agent be expressed overtly in a passive clause?
#  GB302        Is there a phonologically free passive marker ("particle" or "auxiliary")?
  
 # GB131	Is a pragmatically unmarked constituent order verb-initial for transitive clauses?
 # GB132        Is a pragmatically unmarked constituent order verb-medial for transitive clauses?        
# GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?

#Testing the claim for two word orders: SVO (Version A) and SOV (Version B)
#GB147:1 | GB304:1 | GB302:1 > GB131:0 & GB132:1 & GB133:0
#GB147:0 & GB304:0 & GB302:0 > GB131:0 & GB132:0 & GB133:1

#Version A
#GB147:1 | GB304:1 | GB302:1 > GB131:0 & GB132:1 & GB133:0

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB147_GB132 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB147", "GB302", "GB304"))

GB147_GB132_compl <- GB147_GB132[complete.cases(GB147_GB132),]

# prepare datafile 
#GB147:1 | GB304:1 | GB302:1 > GB131:0 & GB132:1 & GB133:0

for(i in 1:nrow(GB147_GB132_compl)){
  if((GB147_GB132_compl$GB131[i] == '0') & (GB147_GB132_compl$GB132[i] == '1') & (GB147_GB132_compl$GB133[i] == '0')) {GB147_GB132_compl$Verb_medial[i] <- 1}
  else(GB147_GB132_compl$Verb_medial[i] <- 0)
}

for(i in 1:nrow(GB147_GB132_compl)){
  if(GB147_GB132_compl$GB147[i] == '1' | GB147_GB132_compl$GB302[i] == '1' | GB147_GB132_compl$GB304[i] == '1') {GB147_GB132_compl$Passive[i] <- 1}
  else(GB147_GB132_compl$Passive[i] <- 0)
}


GB147_GB132_compl2 <- subset(x = GB147_GB132_compl, select = c("Language_ID", "Passive", "Verb_medial"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB147_GB132_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)




