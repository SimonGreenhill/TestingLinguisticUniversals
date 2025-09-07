#This script takes the values and languages tables from a cldf-release and combines then and transforms them to a wide data format from a long. It does not take into account the parameter or code tables.

source("requirements.R")

if(!file.exists("output/processed_data/glottolog_4.3_languages.tsv")){

  dir <- paste0("../data/glottolog")
  if(!dir.exists(dir)){
    dir.create(dir)}
    
    if(!file.exists("../data/glottolog/cldf/codes.csv")){
      SH.misc::get_zenodo_dir(url = "https://zenodo.org/records/4061165/files/glottolog/glottolog-cldf-v4.3.zip", 
                              exdir= "../data/glottolog/")
  }
  
# fetching Glottolog v4.3 from Zenodo using rcldf (requires internet)
glottolog_rcldf_obj <- rcldf::cldf("../data/glottolog/", load_bib = F)

ValueTable_wide <- glottolog_rcldf_obj$tables$ValueTable %>% 
  reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "Value")
  
glottolog_rcldf_obj$tables$LanguageTable %>% 
  dplyr::rename(Language_level_ID = Language_ID, Language_ID = ID) %>% 
  full_join(ValueTable_wide, by = "Language_ID") %>% 
  write_tsv("output/processed_data/glottolog_4.3_languages.tsv")
}