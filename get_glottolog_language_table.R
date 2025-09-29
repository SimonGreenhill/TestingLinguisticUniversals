#This script takes the values and languages tables from a cldf-release and combines then and transforms them to a wide data format from a long. It does not take into account the parameter or code tables.

library(tidyverse)
library(reshape2)

if(!file.exists("summary/glottolog_4.8_languages.tsv")){

# fetching Glottolog v4.3 from Zenodo using rcldf (requires internet)
glottolog_rcldf_obj <- rcldf::cldf("https://zenodo.org/records/8131091/files/glottolog/glottolog-cldf-v4.8.zip", load_bib = F)

ValueTable_wide <- glottolog_rcldf_obj$tables$ValueTable %>% 
  reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "Value")
  
glottolog_rcldf_obj$tables$LanguageTable %>% 
  dplyr::rename(Language_level_ID = Language_ID, Language_ID = ID) %>% 
  dplyr::full_join(ValueTable_wide, by = "Language_ID") %>% 
  readr::write_tsv("summary/glottolog_4.8_languages.tsv")
    }
  
