library(tidyverse)

glottolog_langs <- readr::read_csv('results/Glottolog_Languages.csv', show_col_types = FALSE)

# load the sizes of our BayesTraits analyses:
fns <- Sys.glob("results/*/BT_data.txt")

df_BT <- read_tsv(fns, show_col_types = FALSE, col_names = c("Glottocode", "Var1", "Var2"), id="Filename")
# extract universal
df_BT <- df_BT |> mutate(Universal = basename(dirname(Filename)))

# summarise language count for each universal analysis
df_BT_n <- df_BT |>
  left_join(glottolog_langs, by = c("Glottocode" = "glottocode")) |> 
  dplyr::filter(!is.na(longitude)) |> #removing languages with missing longitude or latitude data, just as in brms_spatphylo.R
  dplyr::filter(!is.na(longitude)) |>
    group_by(Universal, Filename) |>
    summarise(uncontrolled_spatfphylo_n = n(), .groups = "drop")

# calculate family level analysis, these have different data sizes:
#  i.e. the same as above, without any isolates or familes of size N
#fam n
df_spatfam_n <- data.frame(Universal = unique(basename(dirname(df_BT$Filename))),
                       Filename = unique(df_BT$Filename),
                       spatfam_n = as.numeric(NA))

for (universal in unique(df_BT$Filename)) {

datfra <- read.table(file = universal)
  
datfra$ID <- datfra$V1
datfra$ID2 <- datfra$ID
datfra$Longitude <- glottolog_langs$longitude[match(datfra$ID, glottolog_langs$glottocode)]
datfra$Latitude <- glottolog_langs$latitude[match(datfra$ID, glottolog_langs$glottocode)]
datfra$macroarea <- glottolog_langs$macroarea[match(datfra$ID, glottolog_langs$glottocode)]

datfra$family <- glottolog_langs$Family_name[match(datfra$ID, glottolog_langs$glottocode)]

datfra <- datfra[!is.na(datfra$Longitude),]
datfra <- datfra[!is.na(datfra$Latitude),]

# remove isolates
old <- nrow(datfra)
datfra <- datfra[!is.na(datfra$family),]
datfra <- datfra[!datfra$family == "",]
datfra <- datfra[!datfra$family == "Isolate",]  # should not be any of these, but just in case.

# check we don't have isolate-as-family problem
stopifnot('isolate' %in% tolower(datfra$family) == FALSE)
stopifnot('Isolate' %in% tolower(datfra$family) == FALSE)
stopifnot('' %in% tolower(datfra$family) == FALSE)

# remove small families (less than 5 members)
fams <- as.vector(sort(table(datfra$family)))
names(fams) <- names(sort(table(datfra$family)))
too_small <- fams[fams < 5]
datfra <- datfra[!datfra$family %in% names(too_small),]

df_spatfam_n[df_spatfam_n$Filename == universal, "spatfam_n"] <- nrow(datfra)

}

joined <- df_BT_n |>
  full_join(df_spatfam_n, by = join_by(Universal, Filename))

joined |>
  write_csv("summary/lgs_per_universal_counts.csv", na = "")


df <- read_tsv("SI Data 1/results.txt", show_col_types = F)
colnames_order <- colnames(df)
df <- df |>
  dplyr::select(-main_n, -fam_n)

joined |>
  dplyr::rename(main_n = uncontrolled_spatfphylo_n, fam_n = spatfam_n) |>
  dplyr::select(code = Universal, main_n, fam_n) |>
  full_join(df, by = "code") |>
  dplyr::select(all_of(colnames_order)) |>
  write.table(file = "SI Data 1/results.txt", quote = FALSE, 
              sep = "\t", row.names = F)



  


