#Universal 386 (used to be 387 in the old version). If a language has nominal inflection, it also 
# has verbal inflection.

#Nominal inflection:
#  GB070 Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)?
#  GB072 Are there morphological cases for oblique non-pronominal NPs (i.e. not S/A/P)?
#  GB051 Is there a gender/noun class system where sex is a factor in class assignment?
#GB052 Is there a gender/noun class system where shape is a factor in class assignment?
#GB053 Is there a gender/noun class system where animacy is a factor in class assignment?
#GB054 Is there a gender/noun class system where plant status is a factor in class assignment?
#GB192 Is there a gender system where a noun's phonological properties are a factor in class assignment?
#GB321 Is there a large class of nouns whose gender/noun class is not phonologically or semantically predictable?
#GB042	Is there productive overt morphological singular marking on nouns?
#  GB043	Is there productive morphological dual marking on nouns?
#  GB044	Is there productive morphological plural marking on nouns?
#  GB165	Is there productive morphological trial marking on nouns?
#  GB166	Is there productive morphological paucal marking on nouns?
#GB430	Can adnominal possession be marked by a prefix on the possessor?	
#GB431	Can adnominal possession be marked by a prefix on the possessed noun?	
#  GB432	Can adnominal possession be marked by a suffix on the possessor?
#  GB433	Can adnominal possession be marked by a suffix on the possessed noun?

#Presence of inflections on verbs (or at least on auxiliaries):
#GB119        Can mood be marked by an inflecting word ("auxiliary verb")?        
#GB120        Can aspect be marked by an inflecting word ("auxiliary verb")?        
#GB121        Can tense be marked by an inflecting word ("auxiliary verb")?        
#GB298        Can standard negation be marked by an inflecting word ("auxiliary verb")?
#GB080 Do verbs have suffixes/enclitics, other than those that only mark A, S or P (do include portmanteau: A & S + TAM)?
#GB089 Can the S argument be indexed by a suffix/enclitic on the verb in the simple main clause? 
#GB091 Can the A argument be indexed by a suffix/enclitic on the verb in the simple main clause? 
#GB093 Can the P argument be indexed by a suffix/enclitic on the verb in the simple main clause? 
#GB079 Do verbs have prefixes/proclitics, other than those that only mark A, S or P (do include portmanteau: A & S + TAM)?
#GB090 Can the S argument be indexed by a prefix/proclitic on the verb in the simple main clause? 
#GB092 Can the A argument be indexed by a prefix/proclitic on the verb in the simple main clause? 
#GB094 Can the P argument be indexed by a prefix/proclitic on the verb in the simple main clause? 
#GB082	Is there overt morphological marking of present tense on verbs?
#GB083	Is there overt morphological marking on the verb dedicated to past tense?	
#GB084	Is there overt morphological marking on the verb dedicated to future tense?
#GB312	Is there overt morphological marking on the verb dedicated to mood?
#GB086	Is a morphological distinction between perfective and imperfective aspect available on verbs?

#sum(GB042:1, GB043:1, GB044:1,GB165:1, GB166:1, GB430:1, GB431:1, GB432:1, GB433:1) > 0 > 
#sum(GB119:1, GB120:1, GB121:1, GB298:1, GB089:1, GB090:1, GB091:1, GB092:1, GB093:1, GB094:1, 
#GB082:1, GB083:1, GB084:1,  GB312:1, GB086:1, GB107:1, GB108:1, GB103:1, GB104:1) > 0

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB070_GB119 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB042", "GB043", "GB044","GB165", "GB166", 
                                                     "GB430", "GB431", "GB432", "GB433",
                                                     "GB119", "GB120", "GB121", "GB298",
                                                     "GB089", "GB090",  "GB091", "GB092","GB093",  "GB094",
                                                     "GB082", "GB083", "GB084", "GB312", 
                                                     "GB086", "GB107",  "GB108", "GB103","GB104"))

# condition 1: sum(GB042:1, GB043:1, GB044:1,GB165:1, GB166:1, GB430:1, GB431:1, GB432:1, GB433:1) > 0
for(i in 1:nrow(GB070_GB119)){
  summ <- sum(c(as.numeric(GB070_GB119$GB042[i]), 
                as.numeric(GB070_GB119$GB043[i]), 
                as.numeric(GB070_GB119$GB044[i]), 
                as.numeric(GB070_GB119$GB165[i]), 
                as.numeric(GB070_GB119$GB166[i]), 
                as.numeric(GB070_GB119$GB430[i]), 
                as.numeric(GB070_GB119$GB431[i]), 
                as.numeric(GB070_GB119$GB432[i]), 
                as.numeric(GB070_GB119$GB433[i])), na.rm = T)
  if(summ > 0 ){GB070_GB119$Nouns_Inflection[i] <- 1}
  else(GB070_GB119$Nouns_Inflection[i] <- 0)
}

#condition 2; #sum(GB119:1, GB120:1, GB121:1, GB298:1, GB089:1, GB090:1, GB091:1, GB092:1, GB093:1, GB094:1, 
#GB082:1, GB083:1, GB084:1,  GB312:1, GB086:1, GB107:1, GB108:1, GB103:1, GB104:1) > 0

for(i in 1:nrow(GB070_GB119)){
  summ <- sum(c(as.numeric(GB070_GB119$GB119[i]), 
                as.numeric(GB070_GB119$GB120[i]), 
                as.numeric(GB070_GB119$GB121[i]), 
                as.numeric(GB070_GB119$GB298[i]), 
                as.numeric(GB070_GB119$GB089[i]), 
                as.numeric(GB070_GB119$GB090[i]), 
                as.numeric(GB070_GB119$GB091[i]),
                as.numeric(GB070_GB119$GB092[i]), 
                as.numeric(GB070_GB119$GB093[i]),
                as.numeric(GB070_GB119$GB094[i]), 
                as.numeric(GB070_GB119$GB082[i]), 
                as.numeric(GB070_GB119$GB083[i]), 
                as.numeric(GB070_GB119$GB084[i]), 
                as.numeric(GB070_GB119$GB312[i]),
                as.numeric(GB070_GB119$GB086[i]), 
                as.numeric(GB070_GB119$GB107[i]), 
                as.numeric(GB070_GB119$GB108[i]), 
                as.numeric(GB070_GB119$GB103[i]), 
                as.numeric(GB070_GB119$GB104[i])), na.rm = T)
  if(summ > 0 ){GB070_GB119$Verbs_Inflection[i] <- 1}
  else(GB070_GB119$Verbs_Inflection[i] <- 0)
}

GB070_GB119 <- subset(x = GB070_GB119, select = c("Language_ID", "Nouns_Inflection", "Verbs_Inflection"))

GB070_GB119_compl <- GB070_GB119[complete.cases(GB070_GB119),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB070_GB119_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB070_GB119_compl_pruned <- GB070_GB119_compl[ !(GB070_GB119_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB070_GB119_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB070_GB119_compl_pruned, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

