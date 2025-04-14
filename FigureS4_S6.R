source("requirements.R")

universals_type <- read_tsv("universals_types.tsv", show_col_types = F)

df <- read_tsv("results/BT_results_summary/results.txt", show_col_types = F) %>% 
  dplyr::select(code, Universal.shorter, Domain_general, 
                "brmsQ_Fixed_V3_Estimate"         ,      "brmsQ_Fixed_V3_low_95_CI"  ,           
                "brmsQ_Fixed_V3_upp_95_CI"     ,         "brmsQ_Fixed_V3_SIG"     ,              
                "uncon_Fixed_V3_Estimate"     ,          "uncon_Fixed_V3_low_95_CI"  ,           
                "uncon_Fixed_V3_upp_95_CI"    ,          "uncon_Fixed_V3_SIG"       ,        
                "FAM_Fixed_V3_Estimate"        ,         "FAM_Fixed_V3_low_95_CI"   ,     
                "FAM_Fixed_V3_upp_95_CI"       ,         "FAM_Fixed_V3_SIG"     ) %>% 
  reshape2::melt(id.vars = c("code", "Universal.shorter", "Domain_general")) %>% 
  mutate(model = ifelse(str_detect(variable, "brmsQ"), "sp + macroarea", NA)) %>% 
  mutate(model = ifelse(str_detect(variable, "FAM"), "spatial + macroarea + family", model)) %>% 
  mutate(model = ifelse(str_detect(variable, "uncon"), "uncontrolled", model)) %>% 
  mutate(statistic = ifelse(str_detect(variable, "Estimate"), "Estimate", NA)) %>%
  mutate(statistic = ifelse(str_detect(variable, "low_95"), "low_95", statistic)) %>%
  mutate(statistic = ifelse(str_detect(variable, "upp_95"), "upp_95", statistic)) %>%
  mutate(statistic = ifelse(str_detect(variable, "SIG"), "SIG", statistic)) %>% 
  pivot_wider(names_from = "statistic", id_cols = c("code", "Universal.shorter", "Domain_general", "model")) %>% 
  mutate("Estimate" = as.numeric(Estimate), 
         "low_95" = as.numeric(low_95),
         "upp_95" = as.numeric(upp_95))
  
point_size = 0.15
base_size = 10

df$model <- factor(df$model, levels = c("uncontrolled", "sp + macroarea", "spatial + macroarea + family"))

pn <- df %>% 
  filter(Domain_general == "narrow word order") %>% 
  filter(model != "spatial + macroarea + family") %>% 
  ggplot(aes(x = Estimate,
             y =   forcats::fct_reorder(Universal.shorter, low_95),
             xmax=upp_95, xmin=low_95,
             color= SIG
  )) +
  geom_pointrange(size = point_size) +
  geom_vline(xintercept=0) +
  theme_classic(base_size=base_size) +
  theme(axis.title = element_blank()) +
  scale_color_manual("Significant", values=c("gray", "steelblue")) +
  scale_x_continuous(breaks = c(-3, -2,-1,  0, 1,2,3, 4, 5, 6, 7, 8)) +
  guides(color="none") +
  facet_grid(~model) +
  ggtitle("Narrow Word Order")

pw <- df %>% 
  filter(Domain_general == "broad word order") %>% 
  filter(model != "spatial + macroarea + family") %>% 
  ggplot(aes(x = Estimate,
             y =   forcats::fct_reorder(Universal.shorter, low_95),
             xmax=upp_95, xmin=low_95,
             color= SIG
  )) +
  geom_pointrange(size = point_size) +
  geom_vline(xintercept=0) +
  theme_classic(base_size=base_size) +  
  theme(axis.title = element_blank()) +
  scale_color_manual("Significant", values=c("gray", "steelblue")) +
  scale_x_continuous(breaks = c(-3, -2,-1,  0, 1,2,3, 4, 5, 6, 7, 8)) +
  guides(color="none") +
  facet_grid(~model) +
  ggtitle("Broad Word Order")

ph <- df %>% 
  filter(Domain_general == "hierarchy") %>% 
  filter(model != "spatial + macroarea + family") %>% 
  ggplot(aes(x = Estimate,
             y =   forcats::fct_reorder(Universal.shorter, low_95),
             xmax=upp_95, xmin=low_95,
             color= SIG
  )) +
  geom_pointrange(size = point_size) +
  geom_vline(xintercept=0) +
  theme_classic(base_size=base_size) +
  theme(axis.title = element_blank()) +
  scale_color_manual("Significant", values=c("gray", "steelblue")) +
  scale_x_continuous(breaks = c(-3, -2,-1,  0, 1,2,3, 4, 5, 6, 7, 8)) +
  guides(color="none") +
  facet_grid(~model) +
  ggtitle("Hierarchy")

po <- df %>% 
  filter(Domain_general == "other") %>% 
  filter(model != "spatial + macroarea + family") %>% 
  ggplot(aes(x = Estimate,
             y =   forcats::fct_reorder(Universal.shorter, low_95),
             xmax=upp_95, xmin=low_95,
             color= SIG
  )) +
  geom_pointrange(size = point_size) +
  geom_vline(xintercept=0) +
  theme_classic(base_size=base_size) +
  theme(axis.title = element_blank()) +
  scale_color_manual("Significant", values=c("gray", "steelblue")) +
  scale_x_continuous(breaks = c(-3, -2,-1,  0, 1,2,3, 4, 5, 6, 7, 8)) +
  guides(color="none") +
  facet_grid(~model) +
  ggtitle("Other")

p <- ((pn + pw) / (ph + po)) + plot_layout(heights = c(2.5, 1))


width = 10
height = 12

ggsave(filename="Figure4.png", height=height, width= width,
       plot = p)

#########FIG 6
pn <- df %>% 
  filter(Domain_general == "narrow word order") %>% 
  filter(model != "uncontrolled") %>% 
  ggplot(aes(x = Estimate,
             y =   forcats::fct_reorder(Universal.shorter, low_95),
             xmax=upp_95, xmin=low_95,
             color= SIG
  )) +
  geom_pointrange(size = point_size) +
  geom_vline(xintercept=0) +
  theme_classic(base_size=base_size) +
  theme(axis.title = element_blank()) +
  scale_color_manual("Significant", values=c("gray", "steelblue")) +
  scale_x_continuous(breaks = c(-3, -2,-1,  0, 1,2,3, 4, 5, 6, 7, 8)) +
  guides(color="none") +
  facet_grid(~model) +
  ggtitle("Narrow Word Order")

pw <- df %>% 
  filter(Domain_general == "broad word order") %>% 
  filter(model != "uncontrolled") %>% 
  ggplot(aes(x = Estimate,
             y =   forcats::fct_reorder(Universal.shorter, low_95),
             xmax=upp_95, xmin=low_95,
             color= SIG
  )) +
  geom_pointrange(size = point_size) +
  geom_vline(xintercept=0) +
  theme_classic(base_size=base_size) +  
  theme(axis.title = element_blank()) +
  scale_color_manual("Significant", values=c("gray", "steelblue")) +
  scale_x_continuous(breaks = c(-3, -2,-1,  0, 1,2,3, 4, 5, 6, 7, 8)) +
  guides(color="none") +
  facet_grid(~model) +
  ggtitle("Broad Word Order")

ph <- df %>% 
  filter(Domain_general == "hierarchy") %>%   
  filter(model != "uncontrolled") %>% 
  ggplot(aes(x = Estimate,
             y =   forcats::fct_reorder(Universal.shorter, low_95),
             xmax=upp_95, xmin=low_95,
             color= SIG
  )) +
  geom_pointrange(size = point_size) +
  geom_vline(xintercept=0) +
  theme_classic(base_size=base_size) +
  theme(axis.title = element_blank()) +
  scale_color_manual("Significant", values=c("gray", "steelblue")) +
  scale_x_continuous(breaks = c(-3, -2,-1,  0, 1,2,3, 4, 5, 6, 7, 8)) +
  guides(color="none") +
  facet_grid(~model) +
  ggtitle("Hierarchy")

po <- df %>% 
  filter(Domain_general == "other") %>% 
  filter(model != "uncontrolled") %>% 
  ggplot(aes(x = Estimate,
             y =   forcats::fct_reorder(Universal.shorter, low_95),
             xmax=upp_95, xmin=low_95,
             color= SIG
  )) +
  geom_pointrange(size = point_size) +
  geom_vline(xintercept=0) +
  theme_classic(base_size=base_size) +
  theme(axis.title = element_blank()) +
  scale_color_manual("Significant", values=c("gray", "steelblue")) +
  scale_x_continuous(breaks = c(-3, -2,-1,  0, 1,2,3, 4, 5, 6, 7, 8)) +
  guides(color="none") +
  facet_grid(~model) +
  ggtitle("Other")

p <- ((pn + pw) / (ph + po)) + plot_layout(heights = c(2.5, 1))

width = 10
height = 12

ggsave(filename="Figure6.png", height=height, width= width,
       plot = p)
