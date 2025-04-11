source("requirements.R")

#this script takes the information about coef estimate, upper and lower 95% etc stored in the results files called "summary_clean". Unfortunately, due to how summary() reports information, there can be some problems with how the files are printed. Sometimes, some columns "run over" to new rows below, there can be trouble with the colnames and separators. This script relies on a function, stack_summary_clean_tables, and runs this over different sets of these files. Different accomodations are made, files that have an uncommon formatted are separated out to "odd_ones" and read in again but with slightl different settings. Then all the content is merged. The final product is a data-frame for all results for the sp models on the posteriors ("output/proccessed_data/df_brms_sp_posterior.tsv"), naive models ("output/proccessed_data/df_brms_naive.tsv") and the models with only family and macroarea as control predictors ("output/proccessed_data/df_brms_family_macroarea.tsv"). These resulting data-frames can then be used for the plotting etc.

universals_type <- read_tsv("universals_types.tsv", show_col_types = F)
universals_type$Universal.shorter <- stringr::str_replace_all(universals_type$Universal.shorter, ">", "⇒")

#function for stacking the summary tables (do not use for ranef-tables, only summary_clean)
stack_summary_clean_tables <- function(fns = NULL, col_names = NULL, nrows = "all", sep = NULL){
  
  if(nrows == "all"){
 output <-  fns %>% 
    map_df(
      function(x) data.table::fread(x, sep = sep, skip = 1, 
                                    col.names = col_names
      ) %>% #the colnames in the summary_clean-files aren't right, the first column should have the name "term", but it's missing so everything is off by one. Hard-coding the colnames like this and skipping the first row fixes it.
        dplyr::mutate(filename = x)) 
  }

  if(nrows != "all"){
    output<-  fns %>% 
      map_df(
        function(x) data.table::fread(x, sep = sep, skip = 1, nrows= nrows,
                                      col.names = col_names
        ) %>% #the colnames in the summary_clean-files aren't right, the first column should have the name "term", but it's missing so everything is off by one. Hard-coding the colnames like this and skipping the first row fixes it.
        dplyr::mutate(filename = x)) 
  }
  
  output$universal_code <- stringr::str_match(output$filename, "results/(.*?)/brms")[, 2]
  output
   }

       
#sp posteriors brms

df_all_posteriors <- stack_summary_clean_tables(fns = list.files(path = "results", pattern = "summary_clean\\.\\d+", full.names = T, recursive = T), col_names = c("term", "Estimate" , "Est.Error", "l-95% CI",  "u-95% CI" , "Rhat" ,     "Bulk_ESS" , "Tail_ESS",  "Tree" ), sep = "\t")

df_all_posteriors %>% 
  mutate(brms_support = ifelse(`l-95% CI` < 0 & `u-95% CI` < 0|
                                 `l-95% CI`  > 0 & `u-95% CI`  > 0  , "yes (supported, 95% CI excludes zero)", "no (not supported, 95% CI does not excludes zero)")) %>% 
  left_join(universals_type, by = join_by(universal_code)) %>% 
  write_tsv("output/proccessed_data/df_brms_sp_posterior.tsv", quote = "all", na = "") 

#naive

fns = list.files(path = "results", pattern = "summary_clean_uncontrolled.txt", full.names = T, recursive = T)
odd_ones_out <- "results/1560KA/brms.single/summary_clean_uncontrolled.txt"
fns <-  setdiff(fns, odd_ones_out)

df_all_naive <- stack_summary_clean_tables(fns = fns, nrows = 2, col_names =  c("term", "Estimate", "Est.Error",   "l-95% CI",   "u-95% CI",     "Rhat", "Bulk_ESS"), sep = " ")

odd_one_out_df <- stack_summary_clean_tables(fns = odd_ones_out, nrows = 2, col_names =  c("term", "Estimate", "Est.Error",   "l-95% CI",   "u-95% CI",     "Rhat"), sep = " ")

df_all_naive <- full_join(df_all_naive, odd_one_out_df, by = join_by(term, Estimate, Est.Error, `l-95% CI`, `u-95% CI`, Rhat, filename, universal_code))

df_all_naive %>% 
  mutate(brms_support = ifelse(`l-95% CI` < 0 & `u-95% CI` < 0|
                                 `l-95% CI`  > 0 & `u-95% CI`  > 0  , "yes (supported, 95% CI excludes zero)", "no (not supported, 95% CI does not excludes zero)")) %>% 
  left_join(universals_type, by = join_by(universal_code)) %>% 
  write_tsv("output/proccessed_data/df_brms_naive.tsv", quote = "all", na = "") 

# family_id as control isntead of phylogeny
fns = list.files(path = "results/sensitivity_no_tree_fam_control/", pattern = "summary_clean.txt", full.names = T, recursive = T)
odd_ones_out <- c("results/sensitivity_no_tree_fam_control//batch03/0093KA/summary_clean.txt",
                  "results/sensitivity_no_tree_fam_control//batch07/0238bKA/summary_clean.txt",
                  "results/sensitivity_no_tree_fam_control//batch08/0239bKA/summary_clean.txt",
                  "results/sensitivity_no_tree_fam_control//batch09/0357KA/summary_clean.txt",
                  "results/sensitivity_no_tree_fam_control//batch10/0504aKA/summary_clean.txt",
                  "results/sensitivity_no_tree_fam_control//batch11/0569KA/summary_clean.txt",
                  "results/sensitivity_no_tree_fam_control//batch11/0572KA/summary_clean.txt",
                  "results/sensitivity_no_tree_fam_control//batch14/1163KA/summary_clean.txt",
                  "results/sensitivity_no_tree_fam_control//batch15/1334cKA/summary_clean.txt",
                  "results/sensitivity_no_tree_fam_control//batch15/1427KA/summary_clean.txt",
                  "results/sensitivity_no_tree_fam_control//batch17/1550KA/summary_clean.txt",
                  "results/sensitivity_no_tree_fam_control//batch18/1611KA/summary_clean.txt"
                  )
fns <-  setdiff(fns, odd_ones_out)

df_all_family <- stack_summary_clean_tables(fns = fns, nrows =9, col_names =  c("term", "Estimate", "Est.Error",   "l-95% CI",   "u-95% CI",     "Rhat"), sep = " ")

df_all_family_odd_ones <- stack_summary_clean_tables(fns = fns, nrows =9, col_names =  c("term", "Estimate", "Est.Error",   "l-95% CI",   "u-95% CI",     "Rhat"), sep = " ")

df_all_family <- full_join(df_all_family, df_all_family_odd_ones, by = join_by(term, Estimate, Est.Error, `l-95% CI`, `u-95% CI`, Rhat, filename, universal_code))

df_all_family$universal_code <- stringr::str_match(df_all_family$filename, "results/sensitivity_no_tree_fam_control//batch\\d+/(.*?)/summary_clean\\.txt")[, 2]

df_all_family %>% 
  mutate(brms_support = ifelse(`l-95% CI` < 0 & `u-95% CI` < 0|
                            `l-95% CI`  > 0 & `u-95% CI`  > 0  , "yes (supported, 95% CI excludes zero)", "no (not supported, 95% CI does not excludes zero)")) %>% 
  left_join(universals_type, by = join_by(universal_code)) %>% 
  write_tsv("output/proccessed_data/df_brms_family_macroarea.tsv", quote = "all", na = "") 



