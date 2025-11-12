################################################################################
#################################### Figure ####################################
########################## 100M Brazilian cohort ###############################
################################################################################

if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(patchwork)) install.packages("patchwork"); library(patchwork)

################################################################################
# Directories
dirs <- list(
  save        = "C:/Users/taisa/OneDrive/Desktop/Analysis_EI/GRAPHS/",
  age         = "C:/Users/taisa/OneDrive/Desktop/Analysis_EI/RESULTS/CC_noRH/AGE",
  sex         = "C:/Users/taisa/OneDrive/Desktop/Analysis_EI/RESULTS/CC_noRH/SEX",
  race        = "C:/Users/taisa/OneDrive/Desktop/Analysis_EI/RESULTS/CC_noRH/RACE",
  urban_rural = "C:/Users/taisa/OneDrive/Desktop/Analysis_EI/RESULTS/CC_noRH/URBAN_RURAL",
  sanitation  = "C:/Users/taisa/OneDrive/Desktop/Analysis_EI/RESULTS/CC_noRH/SANITATION",
  income      = "C:/Users/taisa/OneDrive/Desktop/Analysis_EI/RESULTS/CC_noRH/INCOME"
)

################################################################################
# Parameters
params <- list(
  p1  = 11.1,
  p99 = 30.3,
  min_temp = -3.1,
  max_temp = 35
)

################################################################################
# Combine results for each group (csvs files)
process_subgroup <- function(path, patterns, groups, label) {
  files <- unlist(lapply(patterns, function(p) list.files(path, pattern = p, full.names = TRUE)))
  data <- lapply(seq_along(files), function(i) {
    df <- fread(files[i])
    names(df)[1] <- "Temperature"
    df$Group <- groups[i]
    df$Type2 <- label
    df
  })
  rbindlist(data, use.names = TRUE, fill = TRUE)
}

################################################################################
# Data preparation per subgroup
region_race <- process_subgroup(
  dirs$race,
  patterns = c("overall.*death_count_white.*\\.csv$", "overall.*death_count_black.*\\.csv$"),
  groups   = c("White", "Black"),
  label    = "Race/Color"
)

region_sex <- process_subgroup(
  dirs$sex,
  patterns = c("overall.*total_deaths_female.*\\.csv$", "overall.*total_deaths_male.*\\.csv$"),
  groups   = c("Women", "Men"),
  label    = "Sex"
)

region_age <- process_subgroup(
  dirs$age,
  patterns = c("overall.*total_deaths_under60.*\\.csv$", "overall.*total_deaths_above60.*\\.csv$"),
  groups   = c("Under 60 years", "60 years or older"),
  label    = "Age"
)

region_urban_rural <- process_subgroup(
  dirs$urban_rural,
  patterns = c("overall.*death_count_urban.*\\.csv$", "overall.*death_count_rural.*\\.csv$"),
  groups   = c("Urban", "Rural"),
  label    = "Residential location"
)

region_sanitation <- process_subgroup(
  dirs$sanitation,
  patterns = c("overall.*death_count_with_all_sanitation.*\\.csv$", 
               "overall.*death_count_missing_some_sanitation.*\\.csv$"),
  groups   = c("Having all essential utilities", "Lacking one or more"),
  label    = "Essential utilities"
)

region_income <- process_subgroup(
  dirs$income,
  patterns = c("overall.*death_count_income_1_tertile_2011.*\\.csv$",
               "overall.*death_count_income_2_tertile_2011.*\\.csv$",
               "overall.*death_count_income_3_tertile_2011.*\\.csv$"),
  groups   = c("Income 1st Tertile", "Income 2nd Tertile", "Income 3rd Tertile"),
  label    = "Income"
)

################################################################################
# Generic plotting function
plot_subgroup <- function(data, colors, title, filename) {
  p <- ggplot(data, aes(x = Temperature, y = allRRfit, color = Group)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = allRRlow, ymax = allRRhigh, fill = Group), alpha = 0.15, color = NA) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    coord_cartesian(ylim = c(0.8, 1.8)) +
    scale_y_continuous(breaks = seq(0.8, 2.0, 0.2), expand = expansion(mult = 0, add = 0)) +
    scale_x_continuous(
      limits = c(params$min_temp, params$max_temp),
      breaks = c(params$min_temp, params$p1, params$p99, params$max_temp)
    ) +
    geom_hline(yintercept = 1, color = "black") +
    geom_vline(xintercept = c(params$p1, params$p99), linetype = "dashed", color = "black") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10),
      panel.border = element_rect(color = "grey", fill = NA, size = 1.2),
      panel.grid = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.key.size = unit(0.5, "cm"),
      legend.text = element_text(size = 9),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10)
    ) +
    labs(title = paste(title, "(Brazil)"),
         x = "Temperature (°C)",
         y = "RR")
  
  ggsave(file.path(dirs$save, paste0(filename, ".svg")), plot = p, width = 8, height = 6, dpi = 300)
  return(p)
}

################################################################################
# Generate plots
plots <- list(
  race        = plot_subgroup(region_race, c("White"="#4DAF4A", "Black"="#4D4D4D"), "Race/Color", "race"),
  sex         = plot_subgroup(region_sex, c("Women"="#800000", "Men"="#4DAF4A"), "Sex", "sex"),
  age         = plot_subgroup(region_age, c("Under 60 years"="#4DAF4A", "60 years or older"="#800000"), "Age", "age"),
  urban_rural = plot_subgroup(region_urban_rural, c("Urban"="#800000", "Rural"="#4DAF4A"), "Residential location", "urban_rural"),
  sanitation  = plot_subgroup(region_sanitation, c("Having all essential utilities"="#4DAF4A", "Lacking one or more"="#800000"), "Essential utilities", "sanitation"),
  income      = plot_subgroup(region_income, c("Income 1st Tertile"="#800000", "Income 2nd Tertile"="#4D4D4D", "Income 3rd Tertile"="#4DAF4A"), "Income", "income")
)

################################################################################
# Combine and save all subgroup plots
final_page <- wrap_plots(plots, ncol = 3)
ggsave(file.path(dirs$save, "brazil_subgroups_final.svg"), plot = final_page, width = 18, height = 12, dpi = 300)
#check
final_page
