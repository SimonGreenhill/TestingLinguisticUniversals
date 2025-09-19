library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(patchwork)
library(ggridges)

theme_set(theme_classic(base_size=18))

BAYESTRAITS_DIR <- file.path("results/")

RATES <- list(
  'q12' = 'not harmonic',           # 00 -> 01
  'q21' = 'harmonic (absent)',      # 01 -> 00
  'q24' = 'harmonic (present)',     # 01 -> 11
  'q42' = 'not harmonic',           # 11 -> 01
  'q34' = 'harmonic (present)',     # 10 -> 11
  'q43' = 'not harmonic',           # 11 -> 10
  'q31' = 'harmonic (absent)',      # 10 -> 00
  'q13' = 'not harmonic'            # 00 -> 10
)

RATE_PAIRS <- list(
    # second is always change to harmonic state
    c('q13', 'q31', 'to harmonic (absent)'), # NH->HA
    c('q12', 'q21', 'to harmonic (absent)'), # NH->HA
    c('q42', 'q24', 'to harmonic (present)'), # NH->HP
    c('q43', 'q34', 'to harmonic (present)')  # NH->HP
)

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


 fns <-  list.files(BAYESTRAITS_DIR, include.dirs=TRUE, full.names=TRUE)
 fns <- fns[ !(fns %in% c("results//brms.R"           ,   "results//BT_results_summary"             ,
"results//Glottolog_Languages.csv"     ,    "results//languages.csv"  ,                
"results//sensitivity_no_tree_fam_control", "results//spatiaLvarcov_decay.R"     ,     
"results//varcov.spatial_function.R" , "results//brms_spatphylo.R"))]


 
results <- NULL
for (dirname in  fns) {
    if (dir.exists(dirname)) {
        filename <- file.path(dirname, 'dep', "BT_data.txt.Log.txt.gz")
        bt <- read.bayestraits(filename)
        bt$code <- basename(dirname)
        results <- rbind(results, bt)
    }
}

results.long <- results %>%
    select(c('code', names(RATES))) %>%
    tidyr::gather(rate, value, names(RATES)) %>%
     as.data.frame()   # tibbles are annoying

results.long$harmonic <- unlist(RATES[results.long$rate])


ggplot(results.long, aes(x=value, y=code, fill=harmonic)) +
    geom_density_ridges(alpha = 0.6, bandwidth = 0.017) +
    xlim(0, 1) +
    theme(legend.position="top") +
    ylab(NULL) +
    scale_fill_discrete(type = c("deeppink2","darkmagenta", "cornflowerblue"), name="")

ggsave('figure4-harmonic_1.png', width=6, height=20)

# figure out if proportion of rates is higher for harmonic changes
is_tendency <- function(i, df, qDisharmonic, qHarmonic) {
    df[[i, qHarmonic]] > df[[i, qDisharmonic]]
}

tendencies <- NULL
for (r in RATE_PAIRS) {
    print(r)
    df <- data.frame(
        vDisharmonic = r[[1]],
        vHarmonic = r[[2]],
        qDisharmonic = results[[r[[1]]]],
        qHarmonic = results[[r[[2]]]],
        code = results$code,
        harmonic=sapply(1:nrow(results), is_tendency, df=results, qDisharmonic=r[[1]], qHarmonic=r[[2]]),
        type = r[[3]]
    )
    tendencies <- rbind(tendencies, df)
}

# convert to proportions
tendencies.proportions <- tendencies %>%
    group_by(code, harmonic, type) %>%
    summarise(n=n()) %>%
    mutate(freq = n / sum(n))

# merge in category information
tendencies.proportions <- tendencies.proportions %>%
    left_join(
        df.bayestraits %>% select(code, Universal.shorter, Domain_general),
        by="code")

plot_bar <- function(df, title="xx") {
    # figure out ordering (sort by proportion of times the harmonic rate was larger)
    #factor_order <- df %>% filter(harmonic) %>% arrange(-freq) %>% pull(code)
    #df$code <- factor(df$code, levels=factor_order)

    ggplot(df, aes(code, freq, fill=type)) +
        geom_bar(stat="identity", position="fill") +
        geom_hline(yintercept=.50) +
        coord_flip() +
        xlab(NULL) +
        theme(legend.position="top") +
        scale_fill_manual("Larger Rate", values=c('tomato', 'steelblue')) +
        facet_grid() +
        scale_y_continuous(labels = scales::percent) +
        ggtitle(title)
}

# plot by universal:

p.nwo <- plot_bar(subset(tendencies.proportions, Domain_general == 'narrow word order'), "a. Narrow Word Order")
p.bwo <- plot_bar(subset(tendencies.proportions, Domain_general == 'broad word order'), "b. Broad Word Order")
p.hier <- plot_bar(subset(tendencies.proportions, Domain_general == 'hierarchy'), "c. Hierarchy")
p.other <- plot_bar(subset(tendencies.proportions, Domain_general == 'other'), "d. Other")

p.nwo <- p.nwo + ylab(NULL)
p.bwo <- p.bwo + ylab(NULL)
p.hier <- p.hier + ylab("Frequency")
p.other <- p.other + ylab("Frequency")


p.fig <- ((p.nwo + p.bwo) / (p.hier + p.other)) +
    plot_layout(heights = c(1.5, 1), guides="collect") &
    theme(legend.position="top")

ggsave('figure4-harmonic_2.png', width=12, height=18)


# plot overall patterns
tendencies.proportions$Domain_general <- factor(
    tendencies.proportions$Domain_general,
    levels=rev(c("broad word order", "narrow word order", "hierarchy", "other"))
)

ggplot(subset(tendencies.proportions, harmonic), aes(freq, Domain_general, fill=stat(x))) +
    geom_density_ridges_gradient(scale=1.2, rel_min_height=0.01) +
    geom_vline(xintercept=0.5, color="#333333") +
    scale_fill_viridis_c(direction = -1, option="B", guide = "none") +
    xlim(0, 1) +
    xlab("Proportion of Harmonic Rates > Disharmonic Rates") +
    ylab(NULL)

ggsave('figure4-harmonic_3.png', width = 11, height = 9)

## add an 'overall' bar
#tendencies.proportions.overall <- rbind(
#    tendencies.proportions,
#    tendencies.proportions %>%
#        mutate(Domain_general = 'overall')
#)

tendencies.proportions.domains <- tendencies %>%
    left_join(df.bayestraits %>% select(code, Universal.shorter, Domain_general), by="code") %>%
    group_by(Domain_general, harmonic) %>%
    summarise(n=n()) %>%
    mutate(freq = n / sum(n))


better_names <- data.frame(
    Domain_general = c('hierarchy', 'narrow word order', 'broad word order', 'other'),
    Domain = c('Hierarchy', 'Narrow Word Order', 'Broad Word Order', 'Other')
)


tendencies.proportions.domains <- tendencies.proportions.domains %>% left_join(better_names, by = join_by(Domain_general))

tendencies.proportions.domains$Domain <- factor(tendencies.proportions.domains$Domain, levels=rev(better_names$Domain))


ggplot(tendencies.proportions.domains, mapping = aes(x = Domain, y = freq, fill = harmonic)) +
    geom_bar(alpha = 0.8, position="stack", stat="identity") +
    geom_hline(yintercept=0.5, color='#333333') +
    scale_fill_discrete(type = c("tomato","steelblue"), name="", labels=c(
        'Disharmonic Rates Greater', # FALSE
        'Harmonic Rates Greater' # TRUE
    )) +
    scale_y_continuous(labels = scales::percent) +
    theme(
        axis.title.x = element_blank() ,
        axis.title.y = element_blank(),
        legend.position = "bottom") +
    coord_flip()

ggsave('figure4-harmonic_4.png', width = 8, height = 4)


# significant tendencies?

# add in Domain_general to tendencies
tendencies <- df.brms %>%
    select(code, Domain_general) %>%
    unique() %>%
    right_join(tendencies, join_by(code==code))

# summarise by median as don't want to overcount.
tendencies.summary <- tendencies %>%
    group_by(code, Domain_general, harmonic, type) %>%
    summarise(qDisharmonicMedian=median(qDisharmonic, na.rm=TRUE), qHarmonicMedian=median(qHarmonic, na.rm=TRUE))


pdf('rates.pdf')
# Are the median harmonic rates larger than the disharmonic ones?
for (dg in unique(tendencies.summary$Domain_general)) {
    cat(dg, "\n")
    dg.rates <- subset(tendencies.summary, Domain_general==dg)

    w <- wilcox.test(dg.rates$qHarmonicMedian, dg.rates$qDisharmonicMedian, paired=TRUE, alternative='greater')

    p.d <- ggplot(dg.rates, aes(x=qDisharmonicMedian)) + geom_histogram(fill="tomato") +
        scale_x_log10(limits=c(0.001, 100)) +
        ggtitle(dg)
    p.h <- ggplot(dg.rates, aes(x=qHarmonicMedian)) + geom_histogram(fill="steelblue") +
        scale_x_log10(limits=c(0.001, 100))

    p.s <- ggplot(dg.rates, aes(x=qDisharmonicMedian, y=qHarmonicMedian)) +
        geom_point() +
        geom_abline(intercept=0) +
        geom_smooth() +
        scale_x_log10(limits=c(0.001, 100)) +
        scale_y_log10(limits=c(0.001, 100)) +
        ggtitle(dg)

    x <- (p.s | p.d / p.h) +  plot_layout(guides='collect')

    print(x)
    print(w)

}

x <- dev.off()

