################################################################################
##################################Figures####################################### 
############################Temperature-mortality###############################
###########################100M Brazilian cohort################################
################################################################################
# Load necessary R packages. Packages are installed if not already available.
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(patchwork)) install.packages("patchwork"); library(patchwork)
################################################################################
dir_save <- "~/Documents/PAPER_COORTE_FINAL/SAIDA_MODELOS/GRAFICOS_TABELAS/"
diretorios_causas <- list(
  "Brazil" = "~/Documents/PAPER_COORTE_FINAL/SAIDA_MODELOS/BRAZIL",
  "North" = "~/Documents/PAPER_COORTE_FINAL/SAIDA_MODELOS/NORTE",
  "Northeast" = "~/Documents/PAPER_COORTE_FINAL/SAIDA_MODELOS/NORDESTE",
  "Central-West" = "~/Documents/PAPER_COORTE_FINAL/SAIDA_MODELOS/CENTRO_OESTE",
  "Southeast" = "~/Documents/PAPER_COORTE_FINAL/SAIDA_MODELOS/SUDESTE",
  "South" = "~/Documents/PAPER_COORTE_FINAL/SAIDA_MODELOS/SUL"
)

#read percentiles values for heat and cold by region
percentiles <- fread("~/Documents/PAPER_COORTE_FINAL/SAIDA_MODELOS/cold_heat_percentiles.csv")

################################################################################
process_subgroup <- function(files) {
  data_list <- lapply(files, function(file) {
    df <- fread(file)
    df <- df[, -1, with = FALSE]  
    names(df)[1] <- "Temperature"  
    df$Temperature <- as.numeric(df$Temperature)  
    return(df)
  })
  combined_data <- rbindlist(data_list, use.names = TRUE, fill = TRUE)
  return(combined_data)
}

###################################################################################3
generate_plot <- function(data, region) {
  percentiles_region <- percentiles[[region]]
  data <- data[!is.na(data$Temperature) & !is.na(data$allRRfit)]
  
 
  min_temp <- floor(min(data$Temperature, na.rm = TRUE))
  max_temp <- ceiling(max(data$Temperature, na.rm = TRUE))

  p1 <- percentiles_region[1]
  p99 <- percentiles_region[2]
  
  breaks_base <- seq(min_temp, max_temp, length.out = 2)
  breaks_x <- sort(unique(round(c(breaks_base, p1, p99), 0)))
  
ggplot(data, aes(x = Temperature, y = allRRfit)) +
    geom_line(size = 1.0, color = "#800000") +
    geom_ribbon(aes(ymin = allRRlow, ymax = allRRhigh), fill = "#800000", alpha = 0.15) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
    coord_cartesian(ylim = c(0.8, 2.0)) +
    scale_x_continuous(
      limits = c(min_temp, max_temp),
      breaks = breaks_x
    ) +
    geom_hline(yintercept = 1, linetype = "solid", color = "black") +
    geom_vline(xintercept = p1, linetype = "dashed", color = "black") +
    geom_vline(xintercept = p99, linetype = "dashed", color = "black") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16),
      panel.border = element_rect(color = "grey", fill = NA, size = 1.2),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 16),
      axis.title = element_text(size = 16)
    ) +
    labs(title = paste0(region), x = "Temperature (°C)", y = "RR")
}

################################################################################
plot_list <- list()
for (region in names(diretorios_causas)) {
  region_causas <- list.files(path = diretorios_causas[[region]], 
                              pattern = "^RR\\.pool_perc_all_.*_count_all.*\\.csv$", full.names = TRUE)
  region_data <- process_subgroup(region_causas)
  
  plot_list[[region]] <- generate_plot(region_data, region)
}

combined_plot <- wrap_plots(plot_list, ncol = 3)
ggsave(filename = paste0(dir_save, "all_regions_curves.svg"), 
       plot = combined_plot, width = 20, height = 15, dpi = 600, device = "svg")

