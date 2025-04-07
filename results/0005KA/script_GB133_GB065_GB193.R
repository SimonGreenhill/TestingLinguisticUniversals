# Universal 5. If a language has dominant SOV order and the genitive follows the governing noun, 
# then the adjective likewise follows the noun. 

#GB133 on clause final V; but no way to say anything about ‘dominance’; 
# GB065 on adnominal possession; GB193 on adjectives
#GB065:
#1	Possessor-Possessed	581
#2	Possessed-Possessor	510
#3	both	287
#?	Not known	50

#GB193 Values - Feature GB193: What is the order of adnominal property word and noun?
#0	they cannot be used attributively	23
#1	ANM - N	295
#2	N - ANM	695
#3	both	168
#?	Not known	70

# the  genitive follows the governing noun:
# 1 for GB065	What is the pragmatically unmarked order of adnominal possessor noun and possessed noun?

# V final order is determined when:
# YES for GB133 Is a pragmatically unmarked constituent order verb-final for transitive clauses?

# noun-adjective order is when:
# 2 for GB193: What is the order of adnominal property word and noun? is the only option

#GB133:1 & GB065:1 > GB193:2

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131","GB132","GB133", "GB065", "GB193"))

data_frame_compl <- data_frame[complete.cases(data_frame),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(data_frame_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

data_frame_compl_pruned <- data_frame_compl[ !(data_frame_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, data_frame_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# checks 
nrow(data_frame_compl_pruned)
setdiff(data_frame_compl_pruned$Language_ID, world_nexus_pruned$tip.label)
setdiff(world_nexus_pruned$tip.label, data_frame_compl_pruned$Language_ID)
length(world_nexus_pruned$tip.label)

# prepare datafile

# combining GB133 and GB065 in one:
for(i in 1:nrow(data_frame_compl_pruned)){
  if(data_frame_compl_pruned$GB131[i] == 0 &
     data_frame_compl_pruned$GB132[i] == 0 &
    data_frame_compl_pruned$GB133[i] == 1 & 
     data_frame_compl_pruned$GB065[i] == 1)
  {data_frame_compl_pruned$GB133_GB065[i] <- 1}
  else(data_frame_compl_pruned$GB133_GB065[i] <- 0)
}

# setting GB193 to 2 and setting other options to 0
data_frame_compl_pruned$GB193[data_frame_compl_pruned$GB193 == '1'] <- '0'
data_frame_compl_pruned$GB193[data_frame_compl_pruned$GB193 == '3'] <- '0'
data_frame_compl_pruned$GB193[data_frame_compl_pruned$GB193 == '2'] <- '1'

# write files

data_frame_compl_pruned2 <- subset(x = data_frame_compl_pruned, select = c("Language_ID", "GB133_GB065","GB193"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


