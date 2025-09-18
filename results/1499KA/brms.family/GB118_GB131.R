#Universal 1499. Languages with serial verb constructions tend to have VO ordering with verb serialization and one object per verb (but some permit two objects); they tend to be isolating, with little inflectional morphology. Syllable structure is typically simple, and many have lexical and/or grammatical tone distinctions. 

#Relevant features
#Serial verb constructions - YES:
#GB118 Are there serial verb constructions?

#VO - YES for either of:
#GB131        Is a pragmatically unmarked constituent order verb-initial for transitive clauses?
#GB132        Is a pragmatically unmarked constituent order verb-medial for transitive clauses?
#While also NO for:
#GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?

#GB118:1 > sum(GB070|GB072|GB051|GB052|GB053|GB054|GB192|GB321|GB042|
          #GB043|GB044|GB165|GB166|GB430|GB431|GB432|GB433|GB119|GB120|
          #GB121|GB298|GB080|GB089|GB091|GB093|GB094|GB082|GB083|GB084|GB312|GB086) < 2 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB118_GB131 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB118","GB070","GB072","GB051",
                                              "GB052","GB053","GB054","GB192","GB321","GB042","GB043",
                                              "GB044","GB165","GB166","GB430","GB431","GB432","GB433","GB119","GB120",
                                              "GB121","GB298","GB080","GB089","GB091","GB093","GB094","GB082","GB083",
                                              "GB084","GB312","GB086"))
# prepare datafile 

for(i in 1:nrow(GB118_GB131)){
  summ <- sum(c(as.numeric(GB118_GB131$GB070[i]), 
                as.numeric(GB118_GB131$GB072[i]), 
                as.numeric(GB118_GB131$GB051[i]), 
                as.numeric(GB118_GB131$GB052[i]),
                as.numeric(GB118_GB131$GB053[i]), 
                as.numeric(GB118_GB131$GB054[i]), 
                as.numeric(GB118_GB131$GB192[i]), 
                as.numeric(GB118_GB131$GB321[i]), 
                as.numeric(GB118_GB131$GB042[i]), 
                as.numeric(GB118_GB131$GB043[i]), 
                as.numeric(GB118_GB131$GB044[i]), 
                as.numeric(GB118_GB131$GB165[i]), 
                as.numeric(GB118_GB131$GB166[i]), 
                as.numeric(GB118_GB131$GB430[i]), 
                as.numeric(GB118_GB131$GB431[i]), 
                as.numeric(GB118_GB131$GB432[i]), 
                as.numeric(GB118_GB131$GB433[i]), 
                as.numeric(GB118_GB131$GB119[i]), 
                as.numeric(GB118_GB131$GB120[i]), 
                as.numeric(GB118_GB131$GB121[i]), 
                as.numeric(GB118_GB131$GB298[i]), 
                as.numeric(GB118_GB131$GB080[i]),
                as.numeric(GB118_GB131$GB089[i]), 
                as.numeric(GB118_GB131$GB091[i]), 
                as.numeric(GB118_GB131$GB093[i]), 
                as.numeric(GB118_GB131$GB094[i]), 
                as.numeric(GB118_GB131$GB082[i]), 
                as.numeric(GB118_GB131$GB083[i]),
                as.numeric(GB118_GB131$GB084[i]), 
                as.numeric(GB118_GB131$GB312[i]), 
                as.numeric(GB118_GB131$GB086[i])), na.rm = T)
  if(summ <= 0 ){GB118_GB131$infl[i] <- 1}
  else(GB118_GB131$infl[i] <- 0)
}

GB118_GB1312 <- subset(x = GB118_GB131, select = c("Language_ID", "GB118", "infl"))

GB118_GB131_compl <- GB118_GB1312[complete.cases(GB118_GB1312),]

table(GB118_GB131_compl$infl, GB118_GB131_compl$GB118)

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB118_GB131_compl, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)
