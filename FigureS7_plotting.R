source("requirements.R")

df <- read_tsv("results/BT_results_summary/results.txt", show_col_types = F) %>% 
  dplyr::filter(brmsQ_Fixed_V3_SIG == "SIG") %>% 
  dplyr::select(code, Universal.shorter, Domain_general, 
                "brmsQ_Fixed_V3_Estimate"         ,      "brmsQ_Fixed_V3_low_95_CI"  ,           
                "brmsQ_Fixed_V3_upp_95_CI"     ,         "brmsQ_Fixed_V3_SIG"     ,              
                BT_median_BF_Estimate = median_BF,
                BT_low_95 = o95HDI_lower,
                BT_upp_95 = o95HDI_higher, 
                BT_SIG = o95HDI_SIG) %>% 
  reshape2::melt(id.vars = c("code", "Universal.shorter", "Domain_general")) %>% 
  mutate(model = ifelse(str_detect(variable, "brmsQ"), "brms (sp + macroarea)", NA)) %>% 
  mutate(model = ifelse(str_detect(variable, "BT"), "BT", model)) %>% 
  mutate(statistic = ifelse(str_detect(variable, "Estimate"), "Estimate", NA)) %>%
  mutate(statistic = ifelse(str_detect(variable, "low_95"), "low_95", statistic)) %>%
  mutate(statistic = ifelse(str_detect(variable, "upp_95"), "upp_95", statistic)) %>%
  mutate(statistic = ifelse(str_detect(variable, "SIG"), "SIG", statistic)) %>% 
  pivot_wider(names_from = "statistic", id_cols = c("code", "Universal.shorter", "Domain_general", "model")) %>% 
  mutate("Estimate" = as.numeric(Estimate), 
         "low_95" = as.numeric(low_95),
         "upp_95" = as.numeric(upp_95)) %>% 
  mutate(v_intercept = ifelse(model == "BT", 10, 0))

point_size = 0.15
base_size = 10


df$model <- factor(df$model, levels = c("brms (sp + macroarea)", "BT"))
df$Universal.shorter <- fct_reorder(df$Universal.shorter, df$Estimate)

pn <- df %>% 
  filter(Domain_general == "narrow word order") %>% 
  ggplot(aes(x = Estimate,
             y =   Universal.shorter,
             xmax=upp_95, xmin=low_95,
             color= SIG
  )) +
  geom_pointrange(size = point_size) +
  geom_vline(mapping = aes(xintercept=v_intercept)) +
  theme_classic(base_size=base_size) +  
  theme(axis.title = element_blank()) +
  scale_color_manual("Significant", values=c("gray", "steelblue")) +
  guides(color="none") +
  facet_grid(~model, scales = "free_x") +
  ggtitle("Narrow Word Order")


pw <- df %>% 
  filter(Domain_general == "broad word order") %>% 
  ggplot(aes(x = Estimate,
             y =   Universal.shorter,
             xmax=upp_95, xmin=low_95,
             color= SIG
  )) +
  geom_pointrange(size = point_size) +
  geom_vline(mapping = aes(xintercept=v_intercept)) +
  theme_classic(base_size=base_size) +  
  theme(axis.title = element_blank()) +
  scale_color_manual("Significant", values=c("gray", "steelblue")) +
  guides(color="none") +
  facet_grid(~model, scales = "free_x") +
  ggtitle("Broad Word Order")

ph <- df %>% 
  filter(Domain_general == "hierarchy") %>% 
  ggplot(aes(x = Estimate,
             y =   Universal.shorter,
             xmax=upp_95, xmin=low_95,
             color= SIG
  )) +
  geom_pointrange(size = point_size) +
  geom_vline(mapping = aes(xintercept=v_intercept)) +
  theme_classic(base_size=base_size) +
  theme(axis.title = element_blank()) +
  scale_color_manual("Significant", values=c("gray", "steelblue")) +
  guides(color="none") +
  facet_grid(~model, scales = "free_x") +
  ggtitle("Hierarchy")

po <- df %>% 
  filter(Domain_general == "other") %>% 
  ggplot(aes(x = Estimate,
             y =   Universal.shorter,
             xmax=upp_95, xmin=low_95,
             color= SIG
  )) +
  geom_pointrange(size = point_size) +
  geom_vline(mapping = aes(xintercept=v_intercept)) +
  theme_classic(base_size=base_size) +
  theme(axis.title = element_blank()) +
  scale_color_manual("Significant", values=c("gray", "steelblue")) +
  guides(color="none") +
  facet_grid(~model, scales = "free_x") +
  ggtitle("Other")

p <- ((pn + pw) / (ph + po)) + plot_layout(heights = c(2.5, 1))


width = 10
height = 12

ggsave(filename="FigureS7.png", height=height, width= width,
       plot = p)

