
########### Read in brms results with family controls

sum_filesQ <- list.files(pattern = "summary_clean.txt", path = "results",full.names = TRUE, recursive = TRUE)
sum_filesQ <- sum_filesQ[grepl("brms.family", sum_filesQ)]
sum_filesQ2 <- lapply(sum_filesQ, read.csv)
sums <- data.frame(sum_filesQ)

row.names(sums) <- sum_filesQ

# parse the files
for(i in 1:length(sum_filesQ2)){
  if(nrow(sum_filesQ2[[i]]) == 19){
    splitted0 <- strsplit(sums[i,1], "/")
    sums$KA_code[i] <- splitted0[[1]][2]
    splitted1 <- strsplit(sum_filesQ2[[i]][1,], "\\s+")
    sums$Fixed_Intercept_Estimate[i] <- splitted1[[1]][2]
    sums$Fixed_Intercept_Est_Error[i] <- splitted1[[1]][3]
    sums$Fixed_Intercept_low_95_CI[i] <- splitted1[[1]][4]
    sums$Fixed_Intercept_upp_95_CI[i] <- splitted1[[1]][5]
    sums$Fixed_Intercept_Rhat[i] <- splitted1[[1]][6]
    splitted2 <- strsplit(sum_filesQ2[[i]][2,], "\\s+")
    sums$Fixed_V3_Estimate[i] <- splitted2[[1]][2]
    sums$Fixed_V3_Est_Error[i] <- splitted2[[1]][3]
    sums$Fixed_V3_low_95_CI[i] <- splitted2[[1]][4]
    sums$Fixed_V3_upp_95_CI[i] <- splitted2[[1]][5]
    sums$Fixed_V3_Rhat[i] <- splitted2[[1]][6]
    splitted3 <- strsplit(sum_filesQ2[[i]][3,], "\\s+")
    sums$Random_sd_Intercept_V1_Estimate[i] <- splitted3[[1]][2]
    sums$Random_sd_Intercept_V1_Est_Error[i] <- splitted3[[1]][3]
    sums$Random_sd_Intercept_V1_low_95_CI[i] <- splitted3[[1]][4]
    sums$Random_sd_Intercept_V1_upp_95_CI[i] <- splitted3[[1]][5]
    sums$Random_sd_Intercept_V1_Rhat[i] <- splitted3[[1]][6]
    splitted4 <- strsplit(sum_filesQ2[[i]][4,], "\\s+")
    sums$Random_sd_Intercept_family_Estimate[i] <- splitted4[[1]][2]
    sums$Random_sd_Intercept_family_Est_Error[i] <- splitted4[[1]][3]
    sums$Random_sd_Intercept_family_low_95_CI[i] <- splitted4[[1]][4]
    sums$Random_sd_Intercept_family_upp_95_CI[i] <- splitted4[[1]][5]
    sums$Random_sd_Intercept_family_Rhat[i] <- splitted4[[1]][6]
    splitted5 <- strsplit(sum_filesQ2[[i]][5,], "\\s+")
    sums$Random_sd_Intercept_V3_family_Estimate[i] <- splitted5[[1]][2]
    sums$Random_sd_Intercept_V3_family_Est_Error[i] <- splitted5[[1]][3]
    sums$Random_sd_Intercept_V3_family_low_95_CI[i] <- splitted5[[1]][4]
    sums$Random_sd_Intercept_V3_family_upp_95_CI[i] <- splitted5[[1]][5]
    sums$Random_sd_Intercept_V3_family_Rhat[i] <- splitted5[[1]][6]
    splitted6 <- strsplit(sum_filesQ2[[i]][6,], "\\s+")
    sums$Random_cor_Intercept_V3_family_Estimate[i] <- splitted6[[1]][2]
    sums$Random_cor_Intercept_V3_family_Est_Error[i] <- splitted6[[1]][3]
    sums$Random_cor_Intercept_V3_family_low_95_CI[i] <- splitted6[[1]][4]
    sums$Random_cor_Intercept_V3_family_upp_95_CI[i] <- splitted6[[1]][5]
    sums$Random_cor_Intercept_V3_family_Rhat[i] <- splitted6[[1]][6]
    splitted7 <- strsplit(sum_filesQ2[[i]][7,], "\\s+")
    sums$Random_sd_Intercept_macro_Estimate[i] <- splitted7[[1]][2]
    sums$Random_sd_Intercept_macro_Est_Error[i] <- splitted7[[1]][3]
    sums$Random_sd_Intercept_macro_low_95_CI[i] <- splitted7[[1]][4]
    sums$Random_sd_Intercept_macro_upp_95_CI[i] <- splitted7[[1]][5]
    sums$Random_sd_Intercept_macro_Rhat[i] <- splitted7[[1]][6]
    splitted8 <- strsplit(sum_filesQ2[[i]][8,], "\\s+")
    sums$Random_sd_Intercept_V3_macro_Estimate[i] <- splitted8[[1]][2]
    sums$Random_sd_Intercept_V3_macro_Est_Error[i] <- splitted8[[1]][3]
    sums$Random_sd_Intercept_V3_macro_low_95_CI[i] <- splitted8[[1]][4]
    sums$Random_sd_Intercept_V3_macro_upp_95_CI[i] <- splitted8[[1]][5]
    sums$Random_sd_Intercept_V3_macro_Rhat[i] <- splitted8[[1]][6]
    splitted9 <- strsplit(sum_filesQ2[[i]][9,], "\\s+")
    sums$Random_cor_Intercept_V3_macro_Estimate[i] <- splitted9[[1]][2]
    sums$Random_cor_Intercept_V3_macro_Est_Error[i] <- splitted9[[1]][3]
    sums$Random_cor_Intercept_V3_macro_low_95_CI[i] <- splitted9[[1]][4]
    sums$Random_cor_Intercept_V3_macro_upp_95_CI[i] <- splitted9[[1]][5]
    sums$Random_cor_Intercept_V3_macro_Rhat[i] <- splitted9[[1]][6]
    splitted11 <- strsplit(sum_filesQ2[[i]][11,], "\\s+")
    sums$Fixed_Intercept_Bulk_ESS[i] <- splitted11[[1]][2]
    sums$Fixed_Intercept_Tail_ESS[i] <- splitted11[[1]][3]
    splitted12 <- strsplit(sum_filesQ2[[i]][12,], "\\s+")
    sums$Fixed_V3_Bulk_ESS[i] <- splitted12[[1]][2]
    sums$Fixed_V3_Tail_ESS[i] <- splitted12[[1]][3]
    splitted13 <- strsplit(sum_filesQ2[[i]][13,], "\\s+")
    sums$Random_sd_Intercept_V1_BulkESS[i] <- splitted13[[1]][2]
    sums$Random_sd_Intercept_V1_Tail_ESS[i] <- splitted13[[1]][3]
    splitted14 <- strsplit(sum_filesQ2[[i]][14,], "\\s+")
    sums$Random_sd_Intercept_family_Bulk_ESS[i] <- splitted14[[1]][2]
    sums$Random_sd_Intercept_family_Tail_ESS[i] <- splitted14[[1]][3]
    splitted15 <- strsplit(sum_filesQ2[[i]][15,], "\\s+")
    sums$Random_sd_Intercept_V3_familyy_Bulk_ESS[i] <- splitted15[[1]][2]
    sums$Random_sd_Intercept_V3_family_Tail_ESS[i] <- splitted15[[1]][3]
    splitted16 <- strsplit(sum_filesQ2[[i]][16,], "\\s+")
    sums$Random_cor_Intercept_V3_family_Bulk_ESS[i] <- splitted16[[1]][2]
    sums$Random_cor_Intercept_V3_family_Tail_ESS[i] <- splitted16[[1]][3]
    splitted17 <- strsplit(sum_filesQ2[[i]][17,], "\\s+")
    sums$Random_sd_Intercept_macro_Bulk_ESS[i] <- splitted17[[1]][2]
    sums$Random_sd_Intercept_macro_Tail_ESS[i] <- splitted17[[1]][3]
    splitted18 <- strsplit(sum_filesQ2[[i]][18,], "\\s+")
    sums$Random_sd_Intercept_V3_macro_Bulk_ESS[i] <- splitted18[[1]][2]
    sums$Random_sd_Intercept_V3_macro_Tail_ESS[i] <- splitted18[[1]][3]
    splitted19 <- strsplit(sum_filesQ2[[i]][19,], "\\s+")
    sums$Random_cor_Intercept_V3_macro_Bulk_ESS[i] <- splitted19[[1]][2]
    sums$Random_cor_Intercept_V3_macro_Tail_ESS[i] <- splitted19[[1]][3]
  }
}

table(is.na(sums)) # FALSE

### adding a column on significance of the main fixed effect
for(i in 1:nrow(sums)){
  if(sums$Fixed_V3_low_95_CI[i] <= 0 & sums$Fixed_V3_upp_95_CI[i] <= 0){
    sums$cor_sig[i] <- "SIG"
  }
  else if(sums$Fixed_V3_low_95_CI[i] >= 0 & sums$Fixed_V3_upp_95_CI[i] >= 0){
    sums$cor_sig[i] <- "SIG"
  }
  else
    sums$cor_sig[i] <- "NOT SIG"
}

table(sums$cor_sig)

write.table(sums, file = "0processig_output-with_NAs.text", sep = "\t", quote = F)

# adding relevant columns to main data file

dat_tab <- read.csv(file = "SI Data 1/results.txt", sep = '\t')

dat_tab$FAM_Estimate <- sums$Fixed_V3_Estimate[match(dat_tab$code, sums$KA_code)]
dat_tab$FAM_low_95_CI <- sums$Fixed_V3_low_95_CI[match(dat_tab$code, sums$KA_code)]
dat_tab$FAM_upp_95_CI <- sums$Fixed_V3_upp_95_CI[match(dat_tab$code, sums$KA_code)]
dat_tab$FAM_SIG <- sums$cor_sig[match(dat_tab$code, sums$KA_code)]

table(dat_tab$FAM_SIG, dat_tab$SPAPHY_SIG)

write.table(dat_tab, file = "SI Data 1/results.txt", quote = F, sep = "\t", row.names = F)

library(DescTools)
cor.test(dat_tab$SPAPHY_median_Estimate, as.numeric(dat_tab$FAM_Estimate), alternative = "t", method = "spearman")
SpearmanRho(dat_tab$SPAPHY_median_Estimate, as.numeric(dat_tab$FAM_Estimate), , conf.level=0.95)





