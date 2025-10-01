library(tidyverse)

# load the sizes of our BayesTraits analyses:
fns <- Sys.glob("results/*/BT_data.txt")

df_BT <- read_tsv(fns, show_col_types = FALSE, col_names = c("Glottocode", "Var1", "Var2"), id="Filename")
# extract universal
df_BT <- df_BT |> mutate(Universal = basename(dirname(Filename)))

# summarise language count for each universal analysis
df_BT_n <- df_BT %>%
    group_by(Universal) %>%
    summarise(BT_n = n())


# calculate family level analysis, these have different data sizes:
#  i.e. the same as above, without any isolates or familes of size N
glottolog_langs <- readr::read_delim('summary/glottolog_4.8_languages.tsv', show_col_types = FALSE)

fns <- Sys.glob("results/*/brms.family/BT_data.txt")
df_fam <- read_tsv(fns, show_col_types = FALSE, col_names = c("Glottocode", "Var1", "Var2"), id="Filename")
# extract universal
df_fam <- df_fam |> mutate(Universal = basename(dirname(dirname(Filename))))


# can't just get nrow here as there is preprocessing code in the brms_family code
# to remove isolates and languages with no location data.
df_fam_n <- data.frame(Universal = as.character(), fam_n=as.numeric())


for (universal in unique(df_fam$Universal)) {
    f <- df_fam |>
        # select the rows for this feature
        filter(Universal == universal) |>
        # merge in required info from glottolog
        left_join(
            glottolog |> select(Language_ID, Family_ID, Longitude, Latitude, Macroarea),
            join_by(Glottocode==Language_ID)
        )
    # now remove the isolates,
    f <- f |> filter(!is.na(Family_ID)) |>
        # and things without locations
        filter(!is.na(Longitude) | !is.na(Latitude))


    fams <- as.vector(sort(table(f$Family_ID)))
    names(fams) <- names(sort(table(f$Family_ID)))
    too_small <- fams[fams < 5]

    f <- f |> filter(Family_ID %in% names(too_small) == FALSE) # no small families allowed

    df_fam_n <- rbind(df_fam_n, data.frame(Universal = universal, fam_n=nrow(f)))

}



# summarise language count for each universal analysis
df_fam_n <- df_fam %>%
    group_by(Universal) %>%
    summarise(fam_n = n())



for(fn in fns){

#fn <- fns[105]
    datfra <- read.table(file = paste0(fn, "/BT_data.txt")) %>%
      dplyr::select(Glottocode = V1)

  datfra$Longitude <- glottolog_langs$Longitude[match(datfra$Glottocode, glottolog_langs$Glottocode)]
  datfra$Latitude <- glottolog_langs$Latitude[match(datfra$Glottocode, glottolog_langs$Glottocode)]
  datfra$macroarea <- glottolog_langs$Macroarea[match(datfra$Glottocode, glottolog_langs$Glottocode)]
  datfra$Family_ID <- glottolog_langs$Family_ID[match(datfra$Glottocode, glottolog_langs$Glottocode)]
### 1074
  datfra <- datfra[!is.na(datfra$Longitude),]
  nrow(datfra)
  datfra <- datfra[!is.na(datfra$Latitude),]
  nrow(datfra)
  datfra <- datfra[!is.na(datfra$Family_ID),]
  nrow(datfra)  # 1036
  datfra <- datfra[!datfra$Family_ID == "",] # no isolates allowed
  datfra <- datfra[!is.na(datfra$Family_ID),] # no isolates allowed
    nrow(datfra)
    ### 1036
  fams <- as.vector(sort(table(datfra$Family_ID)))
  names(fams) <- names(sort(table(datfra$Family_ID)))
  too_small <- fams[fams < 5]
  length(too_small) # 106

  datfra <- datfra[!datfra$Family_ID %in% names(too_small),] # no small families allowed
    # 872
  df_spec <- data.frame(Glottocode =  datfra$Glottocode,
                        Universal = '1667bKA')

  df_fam_n  <- df_fam_n %>% full_join(df_spec, by = join_by(Glottocode, Universal))
}

df_fam_n_summed <- df_fam_n %>%
  group_by(Universal) %>%
  summarise(fam_n = n())


joined <- df_BT_n_summed %>%
  full_join(df_fam_n_summed, by = "Universal")

joined %>%
  write_csv("summary/lgs_per_universal_counts.csv", na = "")
