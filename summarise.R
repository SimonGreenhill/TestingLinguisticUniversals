#!/usr/bin/env Rscript

library(readr)

load <- function(dirname, what="summary", include_single = FALSE) {
  
    patterns <- list(
        "summary" = "summary_clean",
        "macroarea_V3" = "ranef_macroarea_V3",
        "macroarea_intercept" = "ranef_macroarea_intercept",
        "phylo" = "ranef_phylo.",
        "spatial" = "ranef_spatial"
    )
    
    p <- patterns[[match(what, names(patterns))]]
    if (is.null(p)) { stop("Invalid filetype") }

    if(include_single == F){
      dirname <- file.path(dirname, "brms.posterior")
    }
        
    files <- list.files(path = dirname, pattern=p, full.names=TRUE, recursive = TRUE)
  
    if (include_single == FALSE & length(files) != 100) {
        warning(sprintf("only %d files found in %s (expected 100)", length(files), dirname))
    }
    

    if(what == "summary"){
        colnames  <- c("term", "Estimate", "Est.Error", "l-95% CI", "u-95% CI" , "Rhat", "Bulk_ESS", "Tail_ESS", "Tree")
        o <- readr::read_tsv(files, id='Filename', show_col_types=FALSE, skip=1, col_names=colnames)
    }
    if(what == "macroarea_V3"| what == "macroarea_intercept"){
        colnames <- c("macroarea", "Estimate",	"Est.Error",	"Q2.5",	"Q97.5")
        o <- readr::read_tsv(files, id='Filename', show_col_types=FALSE, skip=1, col_names=colnames)
    }
    if(what == "phylo"| what == "spatial"){
        colnames <- c("glottocode", "Estimate",	"Est.Error",	"Q2.5",	"Q97.5")
        o <- readr::read_tsv(files, id='Filename', show_col_types=FALSE, skip=1, col_names=colnames)
    }
    o
}

results <- load(dirname = 'results/1827KA', what = 'summary')
phylo <- load('results/1827KA/', what = 'phylo')


