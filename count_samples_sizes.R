library(tidyverse)

fns <- list.files(path = "results/", pattern = "BT_data.txt", recursive = T, full.names = T)

fns <- fns[str_detect(string = fns, pattern = "bayestraits", negate = T)]

df_BT_n <- data.frame(Glottocode = as.character(), 
                 Universal = as.character())

for(fn in fns){
  #  fn <- fns[1]
  
  df_spec <- read_tsv(file = fn, show_col_types = F, col_names = c("Glottocode", "Var1", "Var2")) %>% 
    dplyr::select(Glottocode) %>% 
    mutate(Universal = fn %>% str_replace("/BT_data.txt", "") %>% 
             str_replace("results//", "")
    )
  
  df_BT_n<- df_BT_n%>% full_join(df_spec, by = join_by(Glottocode, Universal))  
  
}

df_BT_n_summed <- df_BT_n %>% 
  group_by(Universal) %>% 
  summarise(BT_n = n())



##counting ns in brms_spatfam

universals_type <- read_tsv("universals_types.tsv", show_col_types = F)

fns <- list.dirs(path = "2023_08_24_no_tree_fam_control_bfcluster", full.names = T, recursive = T)

fns <- fns[basename(fns) %in% universals_type$universal_code]

glottolog_langs <- read_tsv("output/processed_data/glottolog_4.3_languages.tsv", show_col_types = F)



df_fam_n <- data.frame(Glottocode = as.character(), 
                 Universal = as.character())

for(fn in fns){

#fn <- fns[1]
    datfra <- read.table(file = paste0(fn, "/BT_data.txt")) %>% 
      dplyr::select(Glottocode = V1)
  
  datfra$Longitude <- glottolog_langs$Longitude[match(datfra$Glottocode, glottolog_langs$Glottocode)]
  datfra$Latitude <- glottolog_langs$Latitude[match(datfra$Glottocode, glottolog_langs$Glottocode)]
  datfra$macroarea <- glottolog_langs$Macroarea[match(datfra$Glottocode, glottolog_langs$Glottocode)]
  datfra$Family_ID <- glottolog_langs$Family_ID[match(datfra$Glottocode, glottolog_langs$Glottocode)]
  
  datfra <- datfra[!is.na(datfra$Longitude),]
  datfra <- datfra[!is.na(datfra$Latitude),]
  datfra <- datfra[!is.na(datfra$Family_ID),]
  datfra <- datfra[!datfra$Family_ID == "",] # no isolates allowed
  datfra <- datfra[!is.na(datfra$Family_ID),] # no isolates allowed
  
  fams <- as.vector(sort(table(datfra$Family_ID)))
  names(fams) <- names(sort(table(datfra$Family_ID)))
  too_small <- fams[fams < 5]
  datfra <- datfra[!datfra$Family_ID %in% names(too_small),] # no small families allowed  
  
  df_spec <- data.frame(Glottocode =  datfra$Glottocode, 
                        Universal = basename(fn))

  df_fam_n  <- df_fam_n %>% full_join(df_spec, by = join_by(Glottocode, Universal))  
}

df_fam_n_summed <- df_fam_n %>% 
  group_by(Universal) %>% 
  summarise(fam_n = n())


joined <- df_BT_n_summed %>% 
  full_join(df_fam_n_summed, by = "Universal")

joined %>% 
  write_csv("lgs_per_universal_counts.csv", na = "")
