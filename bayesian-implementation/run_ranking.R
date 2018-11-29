
library(rstan)
library(data.table)
library(ggplot2)
library(shinystan)
library(rethinking)
# devtools::install_github("rmcelreath/rethinking")

#set working directory and source functions
setwd("~/github/ranking-ties/")
source("bayesian-implementation/functions.R")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)


############################
######### Get Data #########
############################

#read in data
results1718 = fread('data/pl_results_17_18.csv')
results_files = list.files('data/')
all_results = lapply(results_files, function(r) fread(paste0("data/", r)))
names(all_results) <- paste0('season', gsub('pl_results|[.]csv','',results_files))

# make model data
all_model_data = lapply(all_results, makeModelData)


#################################
######### Model Fitting #########
#################################


######### Fit 17-18 season models  #########
data1718 = all_model_data[['season_17_18']]
all_1718_models = fitAllModels(data1718)

lapply(names(all_1718_models), function(f){
  print(f)
  temp = all_1718_models[[f]]
  saveRDS(temp, file = paste0('bayesian-implementation/model_fits/s1718_', f, '.rds'))
})

######### Fit 18-19 season models  #########
data1819 = all_model_data[['season_18_19']]
all_1819_models = fitAllModels(data1819)

lapply(names(all_1819_models), function(f){
  print(f)
  temp = all_1819_models[[f]]
  saveRDS(temp, file = paste0('bayesian-implementation/model_fits/s1819_', f, '.rds'))
})


######### Fit 16-17 season models  #########
data1617 = all_model_data[['season_16_17']]
all_1617_models = fitAllModels(data1617)

lapply(names(all_1617_models), function(f){
  print(f)
  temp = all_1617_models[[f]]
  saveRDS(temp, file = paste0('bayesian-implementation/model_fits/s1617_', f, '.rds'))
})


######### Fit 16-17 season models  #########
data1516 = all_model_data[['season_15_16']]
all_1516_models = fitAllModels(data1516)

lapply(names(all_1516_models), function(f){
  print(f)
  temp = all_1516_models[[f]]
  saveRDS(temp, file = paste0('bayesian-implementation/model_fits/s1516_', f, '.rds'))
})




# 
# 
# fit_rk_mult = stan(file = 'bayesian-implementation/rao-kupper-mult.stan'
#                     , data = makeRKData(data1718, sigma_hat = 0.4, t = 1)
#                     , chains = 4
#                     , warmup = 1000
#                     , iter = 2000
#                     , cores = 2
#                     , refresh = 0)
# print(fit_rk_mult)
# 
# 
# fit_davidson_power_1718 = stan(file = 'bayesian-implementation/davidson-power.stan'
#                    , data = makeDavidsonData(data1718, sigma_hat = 0.4, t = 1)
#                    , chains = 4
#                    , warmup = 1000
#                    , iter = 2000
#                    , cores = 2
#                    , refresh = 0)
# print(fit_davidson_power_1718)
# 
# 
# colSums(extract(fit_davidson_power)$lambda)/4000
# 
# summary(fit_davidson_power)
# 
# 
# print(fit_davidson_beaver)
