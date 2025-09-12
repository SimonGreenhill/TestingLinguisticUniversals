source("requirements.R")

glottolog_df <- read_tsv("output/processed_data/glottolog_4.3_languages.tsv", show_col_types = F) %>% 
  dplyr::select(Glottocode, Longitude, Latitude) %>% 
  mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude)) #shifting the longlat to match the shifted centering of the map

fns <- list.files(path = "results/", pattern = "BT_data.txt", recursive = T, full.names = T)

fns <- fns[str_detect(string = fns, pattern = "bayestraits", negate = T)]

df <- data.frame(Glottocode = as.character(), 
                Universal = as.character())

for(fn in fns){
#  fn <- fns[1]
  
  df_spec <- read_tsv(file = fn, show_col_types = F, col_names = c("Glottocode", "Var1", "Var2")) %>% 
    dplyr::select(Glottocode) %>% 
    mutate(Universal = fn %>% str_replace("/BT_data.txt", "") %>% 
             str_replace("results//", "")
             )

df <- df %>% full_join(df_spec, by = join_by(Glottocode, Universal))  
  
}


df_summed <- df %>% 
  group_by(Glottocode) %>% 
  summarise(n = n()) %>% 
  left_join(glottolog_df, by = "Glottocode") %>% 
  dplyr::filter(!is.na(Longitude))

#fetching datasets
world <- ggplot2::map_data('world2', 
                           wrap=c(-25,335), #rewrapping the worldmap, i.e. shifting the center. I prefer this to world2 because I like to adjust the wrapping a bit differently, and world2 results in polygons leaking
                           ylim=c(-55,90)) #cutting out antarctica (not obligatory) and the northermost part where there are no language points in glottolog

lakes <- ggplot2:: map_data("lakes", 
                            wrap=c(-25,335), 
                            col="white", border="gray",  
                            ylim=c(-55,90))

#Basemap
basemap <- ggplot() +
  geom_polygon(data=world, aes(x=long, #plotting the landmasses
                               y=lat,group=group),
               colour="gray90",
               fill="gray90", size = 0.5) +
  geom_polygon(data=lakes, aes(x=long,#plotting lakes
                               y=lat,group=group),
               colour="gray90",
               fill="white", size = 0.3)  +
  theme(#all of theme options are set such that it makes the most minimal plot, no legend, not grid lines etc
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.line = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_blank(),
    axis.ticks = element_blank())   +
  coord_map(projection = "vandergrinten") + #a non-rectangular world map projection that is a decen compromise between area and distances accuracy
  ylim(-60,95) #cutting out antarctica (not obligatory) 


p <- basemap +
  geom_jitter(data = df_summed, mapping = aes(x = Longitude, y = Latitude, fill = n, color = n), 
              shape = 21, alpha = 0.7, size = 0.9) +
  viridis::scale_fill_viridis(direction = -1) +
  viridis::scale_color_viridis(direction = -1)

ggsave("output/plots/FigureS2_map_coverage.png",  height = 7, width = 9, units = "in", dpi = 400, plot = p)
ggsave("output/plots/FigureS2_map_coverage.pdf",  height = 7, width = 9, units = "in", dpi = 400, plot = p)
ggsave("output/plots/FigureS2_map_coverage.tiff",  height = 7, width = 9, units = "in", dpi = 400, plot = p)

