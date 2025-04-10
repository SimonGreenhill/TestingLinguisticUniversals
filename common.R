source("requirements.R")

BAYES_FACTOR_THRESHOLD <- 10

df <- readr::read_tsv('results/BT_results_summary/results.txt', show_col_types=FALSE)
df$Diff <- df$uncon_Fixed_V3_Estimate - df$brmsQ_Fixed_V3_Estimate

# replace > with =>
df$Universal.shorter <- stringr::str_replace_all(df$Universal.shorter, ">", "⇒")

read.bayestraits <- function(file.name, ...) {
    if (!file.exists(file.name)) stop(sprintf("file %s does not exist", file.name))
    conn <- file(file.name, "r")
    on.exit(close(conn))
    start <- 0
    repeat {
        line <- readLines(conn, 1)
        if (startsWith(line, 'Iteration\t')) { break }
        start <- start + 1

        if (start > 200) { stop("unable to find starting block")}  # safety valve
    }
    readr::read_delim(
        file.name, delim="\t",
        skip=start,
        na=c("--", ""),
        trim_ws=TRUE,
        name_repair="minimal",  # this ignores the warnings for the trailing empty column
        show_col_types=FALSE,
        lazy = FALSE,
        ...)
}




get_results <- function(df, model1='uncon', label1='space+phylogeny') {

    .get <- function(df, m1, m2, values_to='value') {
        df %>% select(code, Domain_general, Universal.shorter, !!m1, !!m2) %>%
            rename(uncontrolled=!!m1, brms=!!m2) %>%
            tidyr::pivot_longer(cols=c(uncontrolled, brms), names_to="model", values_to=values_to)
    }

    df_mean <- .get(df, sprintf('%s_Fixed_V3_Estimate', model1), 'brmsQ_Fixed_V3_Estimate', 'value')
    df_lower <- .get(df, sprintf('%s_Fixed_V3_low_95_CI', model1), 'brmsQ_Fixed_V3_low_95_CI', 'lower')
    df_upper <- .get(df, sprintf('%s_Fixed_V3_upp_95_CI', model1), 'brmsQ_Fixed_V3_upp_95_CI', 'upper')

    dfx <- df_mean %>%
        left_join(df_lower, by=join_by(code, Universal.shorter, model, Domain_general)) %>%
        left_join(df_upper, by=join_by(code, Universal.shorter, model, Domain_general))

    # figure out which models cross zero.
    dfx$includes_zero <- dplyr::between(rep(0, nrow(dfx)), dfx$lower, dfx$upper)

    dfx$Significant <- if_else(dfx$includes_zero, 'no', 'yes')

    # label models better.
    dfx$model[dfx$model == 'brms'] <- label1

    # order things properly
    dfx$model <- factor(dfx$model, levels=c("uncontrolled", label1))
    dfx$Universal.shorter <- factor(dfx$Universal.shorter, levels=sort(unique(dfx$Universal.shorter), decreasing=TRUE))
    dfx$Domain_general <- factor(dfx$Domain_general, levels=c('broad word order', 'narrow word order', 'hierarchy', 'other'))

    dfx$Significant <- factor(dfx$Significant, levels=c("yes", "no"))

    dfx
}




df.brms <- get_results(df, model1="uncon", label1="space+phylogeny")
df.brms.fam <- get_results(df, model1="FAM", label1="space+family")



df.bayestraits <- df %>%
    select(Universal.shorter, BT_EDGE_BF) %>%
    right_join(df.brms, by="Universal.shorter") %>%
    filter(Significant == 'yes') %>%
    filter(model == 'space+phylogeny')

df.bayestraits$Universal.shorter <- factor(
    df.bayestraits$Universal.shorter,
    levels=df.bayestraits[order(df.bayestraits$BT_EDGE_BF),]$Universal.shorter
)


