################################################################################
################################## Analysis ####################################
########################## 100M Brazilian cohort ###############################
################################################################################
if(!require(dplyr))      install.packages("dplyr");      library(dplyr)
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(dlnm))       install.packages("dlnm");       library(dlnm)
if(!require(splines))    install.packages("splines");    library(splines)
if(!require(gnm))        install.packages("gnm");        library(gnm)
if(!require(lubridate))  install.packages("lubridate");  library(lubridate)
if(!require(arrow))      install.packages("arrow");      library(arrow)
################################################################################
#Code adapted from:  
#https://github.com/aureliotobias/casecrossover
#https://pmc.ncbi.nlm.nih.gov/articles/PMC10880144/
#https://github.com/gasparrini
################################################################################
setwd("~/ANALYSIS/DATA")
results_root <- "/home/taisa.cortes/ANALYSIS/RESULTS/CC_noRH_main/"
################################################################################
#Model parameters
region   <- "Brazil_"
varper   <- c(25,75)
varfun   <- "ns"
lagfun   <- "ns"
lag_num  <- as.numeric(21)
lag_knot <- logknots(lag_num, nk = 3, fun = "ns")
mmt <- 26.1  
# Fixed MMT (26.1°C) for Brazil and all subgroups to allow a single baseline value 
#in the effect modification analysis; however, this approach has limitations
#https://www.sciencedirect.com/science/article/abs/pii/S2950509725001406
################################################################################
#File tag
base_tag <- function(subgroup) paste0("cc_lag", lag_num, "_p", varper[1], "_", varper[2], "_", subgroup)

#Data
data_all <- arrow::read_parquet("total_contagem_agre_final_with_temp_meta.parquet")

data_all$dow <- lubridate::wday(data_all$Date, label = TRUE, abbr = TRUE, week_start = 1)
data_all$stratum_full <- paste(
  month(data_all$Date),
  year(data_all$Date),
  data_all$dow,
  data_all$CODMUNRES,
  sep = ":"
)


data_all <- data_all %>% arrange(CODMUNRES, Date)
head(data_all)

#Subgroups to analyze
big_groups <- list(
  TOTAL        = c("count_all"),
  SEX          = c("total_deaths_male", "total_deaths_female"),
  AGE          = c("total_deaths_under60", "total_deaths_above60"),
  RACE         = c("death_count_white", "death_count_black"),
  URBAN_RURAL  = c("death_count_urban", "death_count_rural"),
  SANITATION   = c("death_count_with_all_sanitation", "death_count_missing_some_sanitation"),
  INCOME       = c("death_count_income_1_tertile_2011", 
                   "death_count_income_2_tertile_2011", 
                   "death_count_income_3_tertile_2011"))

#Run analysis function for all subgroups
run_analysis <- function(subgroup, group_name) {
  out_dir <- file.path(results_root, group_name)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  needed   <- c("CODMUNRES", subgroup, "Daily_mean_temperature_XAVIER",
                "count_sub", "stratum_full", "dow")
  data_sub <- dplyr::select(data_all, dplyr::all_of(needed))
  
  names(data_sub)[names(data_sub) == "Daily_mean_temperature_XAVIER"]  <- "temp"
  names(data_sub)[names(data_sub) == "Daily_relative_humidity_XAVIER"] <- "humid"
  names(data_sub)[names(data_sub) == subgroup]                          <- "y"
  
  data_sub <- data_sub %>%
    dplyr::group_by(stratum_full) %>%
    dplyr::mutate(count_stratum_sub = sum(count_sub, na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  ranges     <- range(data_sub$temp, na.rm = TRUE)
  knots_per  <- quantile(data_sub$temp, probs = varper/100, na.rm = TRUE)
  knots_hot  <- quantile(data_sub$temp, probs = 99/100,  na.rm = TRUE)
  knots_cold <- quantile(data_sub$temp, probs = 1/100,   na.rm = TRUE)
  
  basis.temp  <- crossbasis(
    data_sub$temp,  lag = lag_num,
    argvar = list(fun = varfun, knots = knots_per),
    arglag = list(fun = lagfun, knots = lag_knot))
  
  model <- gnm(
    y ~ basis.temp,
    data = data_sub,
    family = quasipoisson(),
    eliminate = factor(stratum_full),
    subset = count_stratum_sub > 0,
    na.action = "na.exclude")
 
  cp_pool_cen_mct <- crosspred(
    basis.temp, model, model.link = "log",
    from = ranges[1], to = ranges[2], by = 0.1, cen = mmt)
  
  cp_pool_hot  <- crosspred(
    basis.temp, model, model.link = "log",
    from = ranges[1], to = ranges[2], at = knots_hot,  cen = mmt)
  
  cp_pool_cold <- crosspred(
    basis.temp, model, model.link = "log",
    from = ranges[1], to = ranges[2], at = knots_cold, cen = mmt)
  
  pdf(file = file.path(out_dir, paste0("overall_cc_", base_tag(subgroup), ".pdf")))
  plot(cp_pool_cen_mct, "overall", ci = "area",
       xlab = "Temperature ºC", ylab = "RR", ylim = c(0.8, 1.8),
       main = paste0("Brazil — ", subgroup), col = "red")
  abline(v = knots_cold, lty = 2, lwd = 1)
  abline(v = knots_hot,  lty = 2, lwd = 1)
  dev.off()
  
  RR.pool <- cbind(unlist(cp_pool_cen_mct[c("allRRfit")]),
                   unlist(cp_pool_cen_mct[c("allRRlow")]),
                   unlist(cp_pool_cen_mct[c("allRRhigh")]))
  RR.pool <- as.data.frame(RR.pool)
  names(RR.pool) <- c("allRRfit", "allRRlow", "allRRhigh")
  RR.pool <- cbind(predvar = cp_pool_cen_mct[["predvar"]], RR.pool)
  write.csv(RR.pool, file = file.path(out_dir, paste0("RR_overall_cc_", base_tag(subgroup), ".csv")), row.names = FALSE)
  
  RR.pool_hot <- cbind(unlist(cp_pool_hot[c("allRRfit")]),
                       unlist(cp_pool_hot[c("allRRlow")]),
                       unlist(cp_pool_hot[c("allRRhigh")]))
  RR.pool_hot <- as.data.frame(RR.pool_hot)
  names(RR.pool_hot) <- c("allRRfit", "allRRlow", "allRRhigh")
  RR.pool_hot <- cbind(predvar = cp_pool_hot[["predvar"]], RR.pool_hot)
  RR.pool_hot$mmt <- mmt
  write.csv(RR.pool_hot, file = file.path(out_dir, paste0("RR_hot_cc_", base_tag(subgroup), ".csv")), row.names = FALSE)
  
  RR.pool_cold <- cbind(unlist(cp_pool_cold[c("allRRfit")]),
                        unlist(cp_pool_cold[c("allRRlow")]),
                        unlist(cp_pool_cold[c("allRRhigh")]))
  RR.pool_cold <- as.data.frame(RR.pool_cold)
  names(RR.pool_cold) <- c("allRRfit", "allRRlow", "allRRhigh")
  RR.pool_cold <- cbind(predvar = cp_pool_cold[["predvar"]], RR.pool_cold)
  RR.pool_cold$mmt <- mmt
  write.csv(RR.pool_cold, file = file.path(out_dir, paste0("RR_cold_cc_", base_tag(subgroup), ".csv")), row.names = FALSE)
  
  rm(model, basis.temp, cp_pool_cen_mct, cp_pool_cold, cp_pool_hot,
     RR.pool, RR.pool_cold, RR.pool_hot, data_sub, ranges, knots_per, knots_hot, knots_cold)
  gc()
}

for (grp in names(big_groups)) {
  message(sprintf(">>> Grupo: %s", grp))
  
  out_dir <- file.path(results_root, grp)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  vars <- big_groups[[grp]]
  for (i in seq_along(vars)) {
    sg <- vars[i]
    message(sprintf(i, length(vars), sg))
    run_analysis(sg, grp)
  }
}

