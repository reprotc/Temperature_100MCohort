################################################################################
##################################Figures####################################### 
############################Temperature-mortality###############################
###########################100M Brazilian cohort################################
################################################################################
# Load necessary R packages. Packages are installed if not already available.
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(cowplot)) install.packages("cowplot"); library(cowplot)

################################################################################
dir_save="~/Documents/PAPER_COORTE_FINAL/SAIDA_MODELOS/"
files= list.files(path= dir_save, pattern = "main_model", full.names = T)
dados1=fread(files[1])
dados2=fread(files[2])
dados3=fread(files[3])
dados4=fread(files[4])
dados5=fread(files[5])
dados6=fread(files[6])
dados=rbind(dados1, dados2, dados3, dados4, dados5, dados6)
rm(dados1, dados2, dados3, dados4, dados5, dados6)

################################################################################
unique(dados$Group)
#Set the order of categories on the Y-axis
ordem_grupos= rev(c("Men", "Women", "Under 60 years", "60 years or older",
                    "White", "Black", "Indigenous", 
                    "White older adults", "Black older adults", "Indigenous older adults",
                    "Income 1st tertile", "Income 2nd tertile", "Income 3rd tertile", 
                    "Urban residence", "Rural residence",                              
                    "Having all essential utilities","Lacking one or more essential utilities", 
                    "Masonry multi-room houses","Non-masonry single-room houses"))

dados=subset(dados, dados$Group %in% ordem_grupos)
#Set the order of groups on the X-axis
ordem_regioes <- c("Brazil", "North", "Northeast", "Central-West", "Southeast", "South") 
dados$Region <- factor(dados$Region, levels = ordem_regioes)  

#Percentage increase in Risk with 95%CI
dados <- dados %>%
  mutate(Group = factor(Group, levels = ordem_grupos),  
         perc_increase = ifelse(RR >= 1, (RR - 1) * 100, (1 - RR) * -100),
         IC_low_perc = ifelse(allRRlow >= 1, (allRRlow - 1) * 100, (1 - allRRlow) * -100), # IC inferior
         IC_high_perc = ifelse(allRRhigh >= 1, (allRRhigh - 1) * 100, (1 - allRRhigh) * -100), # IC superior
         Type = factor(Type, levels = c("Cold", "Heat")))


dados <- dados %>%
  mutate(label_text = paste0(round(perc_increase, 1), "% (", 
                             round(IC_low_perc, 1), "% - ", 
                             round(IC_high_perc, 1), "%)"))

# Normalize the percentage increase values using a log transformation
# to reduce the influence of extreme values and improve color scaling in the heatmap
transform_log <- function(x) {
  x_adjusted <- pmax(x, 0)  
  x_log <- log(x_adjusted + 1)  
  return(x_log)
}

dados$perc_increase2 <- transform_log(dados$perc_increase)
summary(dados$perc_increase2)

################################################################################
dados <- dados %>%
  mutate(text_color = ifelse(perc_increase > 100, "white", "black"))

heatmap_cold<- ggplot(dados %>% filter(Type == "Cold"), 
                                  aes(x = Region, y = Group, fill = perc_increase2)) +
  geom_tile(color = "gray90", width = 1, height = 1) +  
  geom_text(aes(label = paste0(round(perc_increase, 1), "% (", 
                               round(IC_low_perc, 1), ": ", 
                               round(IC_high_perc, 1), ")"),
                color = text_color),  
            size = 2.3, check_overlap = TRUE) +  
  scale_color_identity() +  
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Blues")) +  
  theme_minimal() +
  labs(title = "", x = "", y = "") +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8, face = "bold"),
    panel.grid = element_blank(),  
    legend.position = "none",  
    plot.margin = margin(-0.5, -0.5, -0.5, -0.5, "cm"),
    aspect.ratio = 1.2
  )


heatmap_cold

ggsave(paste0(dir_save, "heatmap_cold_final.svg"), 
       plot = heatmap_cold, width = 8, height = 6, dpi = 800)
###############################################################################
heatmap_heat <- ggplot(dados %>% filter(Type == "Heat"), 
                                   aes(x = Region, y = Group, fill = perc_increase2)) +
  geom_tile(color = "gray90", width = 1, height = 1) +  
  geom_text(aes(label = paste0(round(perc_increase, 1), "% (", 
                               round(IC_low_perc, 1), ": ", 
                               round(IC_high_perc, 1), ")"),
                color = text_color),  
            size = 2.3, check_overlap = TRUE) +  
  scale_color_identity() +  
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds")) +  
  theme_minimal() +
  labs(title = "", x = "", y = "") +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8, face = "bold"),
    panel.grid = element_blank(),  
    legend.position = "none",  
    plot.margin = margin(-0.5, -0.5, -0.5, -0.5, "cm"),
    aspect.ratio = 1.2
  )


heatmap_heat

ggsave(paste0(dir_save, "heatmap_heat_final.svg"), 
       plot = heatmap_heat, width = 8, height = 6, dpi = 600)
###############################################################################
