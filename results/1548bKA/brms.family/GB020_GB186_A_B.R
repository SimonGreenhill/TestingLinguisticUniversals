#Universal 1548. In verb-initial languages, the existence of several articles (definite,  
#indefinite, specific, plural, proper noun) is much more common than in verb-final languages. 

#IF word order is verb-initial, THEN thhe existence of several articles (definite, indefinite, 
#specific, plural, proper noun) is much more common than if word order is verb-final.

#Relevant features

#Relevant features
#Verb_final - YES for GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?
#While also NO for:
#GB131        Is a pragmatically unmarked constituent order verb-initial for transitive clauses?   
#GB132        Is a pragmatically unmarked constituent order verb-medial for transitive clauses?

#Verb_initial - YES for GB131        Is a pragmatically unmarked constituent order verb-initial for transitive clauses? 
#While also NO for:
#GB132        Is a pragmatically unmarked constituent order verb-medial for transitive clauses?
#GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?

#Several articles - YES for at least two:
#GB020 Are there definite or specific articles?
#GB021 Do indefinite nominals commonly have indefinite articles?
#GB172 Can an article agree with the noun in gender/noun class? 
#GB186 Can an article agree with the noun in number?

#The Universal will be tested on:
#Version B: verb-initial languages and the existence of several articles
#  GB131:1 & GB132:0 & GB133:0 > sum(GB020,GB021,GB172,GB186) >= 2

#Version B

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB020_GB186 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB020", "GB021", "GB172", "GB186"))

for(i in 1:nrow(GB020_GB186)){
  summ <- sum(c(as.numeric(GB020_GB186$GB020[i]), as.numeric(GB020_GB186$GB021[i]), as.numeric(GB020_GB186$GB172[i]), as.numeric(GB020_GB186$GB186[i])), na.rm = T)
  if(summ >= 2 ){GB020_GB186$Several_Articles[i] <- 1}
  else(GB020_GB186$Several_Articles[i] <- 0)
}

GB020_GB186_c <- subset(x = GB020_GB186, select = c("Language_ID", "GB131", "GB132", "GB133", "Several_Articles"))

GB020_GB186_compl <- GB020_GB186_c[complete.cases(GB020_GB186_c),]

# prepare datafile 

for(i in 1:nrow(GB020_GB186_compl)){
  if((GB020_GB186_compl$GB131[i] == '1') & (GB020_GB186_compl$GB132[i] == '0') & (GB020_GB186_compl$GB133[i] == '0')) {GB020_GB186_compl$Verb_initial[i] <- 1}
  else(GB020_GB186_compl$Verb_initial[i] <- 0)
}

GB020_GB186_compl2 <- subset(x = GB020_GB186_compl, select = c("Language_ID", "Verb_initial", "Several_Articles"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB020_GB186_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)



