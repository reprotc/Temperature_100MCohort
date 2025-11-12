################################################################################
################################### Figures ####################################
########################## 100M Brazilian cohort ###############################
################################################################################
if(!require(geobr))      install.packages("geobr");      library(geobr)
if(!require(ggplot2))    install.packages("ggplot2");    library(ggplot2)
if(!require(dplyr))      install.packages("dplyr");      library(dplyr)
if(!require(readr))      install.packages("readr");      library(readr)
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(arrow))      install.packages("arrow");      library(arrow)
################################################################################
setwd("~/ANALYSIS/DATA")
results_root <- "/home/taisa.cortes/ANALYSIS/RESULTS/CC_noRH_main/"
################################################################################
koppen <- fread("koppen_deaths.csv")
names(koppen)

municipalities <- read_municipality(year = 2018)
regions <- read_region(year = 2018)

municipalities <- municipalities %>%
  mutate(CODMUNRES = substr(code_muni, 1, 6))

municipalities$CODMUNRES <- as.character(municipalities$CODMUNRES)
koppen$CODMUNRES <- substr(koppen$CODMUNRES, 1, 6)
koppen$CODMUNRES <- as.character(koppen$CODMUNRES)

map_data <- merge(municipalities, koppen, by = "CODMUNRES")

koppen_colors <- c(
  "Af"  = "#08306B",
  "Am"  = "#2171B5",
  "As"  = "#9ECAE1",
  "Aw"  = "#4292C6",
  "BSh" = "#FFD700",
  "Cfa" = "#9ACD32",
  "Cwa" = "#ADFF2F",
  "Cfb" = "#2E8B57",
  "Cwb" = "#66CDAA"
)

koppen_labels <- c(
  "Af"  = "Af - Tropical Rainforest",
  "Am"  = "Am - Tropical Monsoon",
  "As"  = "As - Tropical Savanna (dry summer)",
  "Aw"  = "Aw - Tropical Savanna (dry winter)",
  "BSh" = "BSh - Hot Semi-Arid",
  "Cfa" = "Cfa - Humid Subtropical",
  "Cwa" = "Cwa - Dry-winter Humid Subtropical",
  "Cfb" = "Cfb - Oceanic",
  "Cwb" = "Cwb - Dry-winter Subtropical Highland"
)

map_plot <- ggplot() +
  geom_sf(data = map_data, aes(fill = Koppen), color = "grey50", size = 0.1, na.rm = TRUE) +
  geom_sf(data = map_data %>% dplyr::filter(is.na(Koppen)), fill = "lightgrey", color = "grey50", size = 0.1) +
  geom_sf(data = regions, fill = NA, color = "black", size = 0.5) +
  scale_fill_manual(
    name   = "Climate (Köppen-Geiger)",
    values = koppen_colors,
    labels = koppen_labels,
    breaks = names(koppen_labels),
    na.value = "lightgrey",
    guide = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  theme_minimal() +
  labs(title = "Brazilian Municipalities",
       subtitle = "Köppen-Geiger Climate Classification",
       x = "Longitude", y = "Latitude") +
  coord_sf() +
  theme(
    legend.position = "bottom",
    plot.margin     = margin(10, 20, 90, 20)
  ) +
  annotate("text", x = -65, y = -2,  label = "North",       color = "black", size = 4) +
  annotate("text", x = -40, y = -15, label = "Northeast",    color = "black", size = 4) +
  annotate("text", x = -55, y = -15, label = "Central-West", color = "black", size = 4) +
  annotate("text", x = -45, y = -25, label = "Southeast",    color = "black", size = 4) +
  annotate("text", x = -50, y = -30, label = "South",        color = "black", size = 4)

ggsave("map_brazil_municipalities_koppen_regions_colored_final2.png",
       plot = map_plot, width = 13, height = 11, dpi = 300)

print(map_plot)