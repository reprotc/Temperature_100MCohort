################################################################################
##################################Main analysis#################################
############################Temperature-mortality###############################
###########################100M Brazilian cohort################################
################################################################################
# Working paper by Cortes et al.
################################################################################
# Load necessary R packages. Packages are installed if not already available.
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(dlnm)) install.packages("dlnm"); library(dlnm)
if(!require(splines)) install.packages("splines"); library(splines)
if(!require(survival)) install.packages("survival"); library(survival)
if(!require(gnm)) install.packages("gnm"); library(gnm)
################################################################################
# Define the data directory
data_dir <- "/mnt/Dados/TaisaCortes/"
setwd("/mnt/Dados/TaisaCortes/DATA_FINAL")


data_final <- fread("aggregate_death_with_exposure_NOV.csv")
data_final$stratum_full=paste(year(data_final$Date), month(data_final$Date), weekdays(data_final$Date), data_final$CODMUNRES, sep = ":")
data_final=subset(data_final, !is.na(data_final$Daily_mean_temperature_XAVIER))
data_final=data_final%>% group_by(stratum_full)%>% mutate(count_stratum=(sum(count_all)))
data_final=subset(data_final, data_final$count_stratum>0)
data_final= data_final %>% arrange(CODMUNRES, Date)

group="total_deaths_masculino"
names(data_final)
data_final=subset(data_final, select=c("total_deaths_masculino", "Daily_mean_temperature_XAVIER", 
                                       "Daily_relative_humidity_XAVIER", "stratum_full"))
gc()


################################################################################
# Setting parameters for the final model.
varper = c(25,75)
varfun = "ns"
lagfun = "ns"
lag_num=as.numeric(21)
lag_knot = logknots(lag_num, nk=3, fun= "ns")

ranges= range(data_final$Daily_mean_temperature_XAVIER)
knots_per=quantile(data_final$Daily_mean_temperature_XAVIER, probs = varper/100, na.rm = T)

# Create crossbasis for temperature and humidity
basis.temp <- crossbasis(data_final$Daily_mean_temperature_XAVIER, lag=lag_num,
                         argvar = list(fun = varfun,  knots= knots_per), arglag = list(fun = lagfun, knots=lag_knot))

basis.humid <- crossbasis(data_final$Daily_relative_humidity_XAVIER, lag=lag_num,
                          argvar = list(fun = varfun,  df=3), arglag = list(fun = lagfun, knots=lag_knot))


# Conditional quasi-poisson regression model 
model <- gnm(total_deaths_masculino~ basis.temp + basis.humid,
             data = data_final, family = quasipoisson(), eliminate= factor(stratum_full))

mmt=26.1

cp_pool_cen_mct <- crosspred(basis.temp,model, model.link = "log",
                             from=ranges[1], to=ranges[2], by=0.1, cen=mmt)

###Table
RR.pool =cbind(unlist(cp_pool_cen_mct[c("allRRfit")]), unlist(cp_pool_cen_mct[c("allRRlow")]), unlist(cp_pool_cen_mct[c("allRRhigh")]))
RR.pool=as.data.frame(OR.pool)
names(RR.pool)=c("allRRfit", "allRRlow", "allRRhigh")
RR.pool = cbind(cp_pool_cen_mct[["predvar"]], OR.pool)
write.csv(RR.pool, file= paste0(data_dir, "RR.pool_perc_all_","lag", lag_num,"_", varfun, "_",
                                paste(varper[1],varper[2],  varper[3], sep = "_"), "_", df_exposure, 
                                lagfun,"_",region,"_",group, ".csv"))
rm(model,data_final, basis.humid, basis.temp, cp_pool_cen_mmt)
gc()
############################################################################################################################################
#(same code for other subgroups....)