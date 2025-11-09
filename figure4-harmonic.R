library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(patchwork)
library(ggridges)
library(bayestraitr)
library(stringr)




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

theme_set(theme_classic(base_size=18))

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


df_categories <- read_tsv("universals_types.tsv", show_col_types=FALSE)

# we only want the BayesTraits results for the brms significant
btres <- read_tsv('results/BT_results_summary/results.txt', show_col_types=FALSE)

significant <- btres |> filter(supported=='SIG') |> pull(code)


print("DEBUG DEBUG DEBUG")
significant <- read_csv('~/Desktop/fig4-old.csv', show_col_types=FALSE) |> pull('OLD')

pp <- function(code, bt) {
    for (q in c("q12", "q13", "q21", "q24", "q31", "q34", "q42", "q43")) {
        cat(sprintf("%s\t%s\t%d\t%0.3f\t%0.3f\n", code, q, nrow(bt), median(bt[[q]]), sd(bt[[q]])))
    }
}


results <- NULL
for (f in Sys.glob("results/*/bayestraits/dep/BT_data.txt.Log.txt.gz")) {
    code <- basename(dirname(dirname(dirname(f))))
    # only load supported BayesTraits results
    if (code %in% significant) {
        bt <- read.bayestraits(f)
        bt$code <- code
        results <- rbind(results, bt)
        pp(code, bt)
    } else {
        cat(sprintf("Ignoring %s - not significant\n", code))
    }
}


is_tendency <- function(df, qDisharmonic, qHarmonic) { df[[qHarmonic]] > df[[qDisharmonic]] }

tendencies <- do.call(rbind, lapply(RATE_PAIRS, function(r) {
    data.frame(
        vDisharmonic = r[[1]],
        vHarmonic    = r[[2]],
        qDisharmonic = results[[r[[1]]]],
        qHarmonic    = results[[r[[2]]]],
        code         = results$code,
        harmonic     = is_tendency(results, qDisharmonic = r[[1]], qHarmonic = r[[2]]),
        type         = r[[3]]
    )
}))


# convert to proportions and merge in category information
tendencies.proportions <- tendencies |>
    group_by(code, harmonic, type) |>
    summarise(n=n()) |>
    mutate(freq = n / sum(n)) |>
    left_join(
      df_categories |> select(code = universal_code, Universal.shorter, Domain_general),
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

ggsave('figure4-harmonic_grouped.png', width=12, height=18, plot = p.fig)


# plot overall patterns
tendencies.proportions$Domain_general <- factor(
    tendencies.proportions$Domain_general,
    levels=rev(c("broad word order", "narrow word order", "hierarchy", "other"))
)


tendencies.proportions.domains <- tendencies %>%
  left_join(
    df_categories %>% select(code = universal_code, Domain_general),
    by="code") %>%  # merge in category information
  group_by(Domain_general, harmonic) %>%
    summarise(n=n()) %>%
    mutate(freq = n / sum(n)) %>%
  ungroup()


better_names <- data.frame(
    Domain_general = c('hierarchy', 'narrow word order', 'broad word order', 'other'),
    Domain = c('Hierarchy', 'Narrow Word Order', 'Broad Word Order', 'Other')
)


tendencies.proportions.domains <- tendencies.proportions.domains %>% left_join(better_names, by = join_by(Domain_general))

tendencies.proportions.domains$Domain <- factor(tendencies.proportions.domains$Domain, levels=rev(better_names$Domain))

p <- ggplot(tendencies.proportions.domains, mapping = aes(x = Domain, y = freq, fill = harmonic)) +
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


ggsave('figure4-harmonic.png', width = 8, height = 4, plot = p)

grDevices::cairo_pdf(file="figure4-harmonic.pdf", height = 4, width = 8)
plot(p)
x <- dev.off()


# significant tendencies?

# add in Domain_general to tendencies
tendencies <- df_categories %>%
  select(code = universal_code, Domain_general) %>%
    unique() %>%
    right_join(tendencies, join_by(code==code))

# summarise by median as don't want to overcount.
tendencies.summary <- tendencies %>%
    group_by(code, Domain_general, harmonic, type) %>%
    summarise(qDisharmonicMedian=median(qDisharmonic, na.rm=TRUE), qHarmonicMedian=median(qHarmonic, na.rm=TRUE))


# Are the median harmonic rates larger than the disharmonic ones?
sink('figure4-harmonic.txt')
for (dg in unique(tendencies.summary$Domain_general)) {
    cat(dg, "\n")
    dg.rates <- subset(tendencies.summary, Domain_general==dg)
    w <- wilcox.test(dg.rates$qHarmonicMedian, dg.rates$qDisharmonicMedian, paired=TRUE, alternative='greater')
    print(w)
}
sink()
