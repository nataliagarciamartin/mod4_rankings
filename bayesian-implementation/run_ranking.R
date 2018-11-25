
library(rstan)
library(data.table)
library(ggplot2)

#set working directory and source functions
setwd("~/github/ranking-ties/")
source("bayesian-implementation/functions.R")


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
names(all_1718_models) = c("Davidson", "Davidson-Beaver", "Rao-Kupper", "Rao-Kupper Mult")

lapply(names(all_1718_models), function(f){
  print(f)
  temp = all_1718_models[[f]]
  save(temp, file = paste0('bayesian-implementation/model_fits/s1718_', f, '.RData'))
})

######### Fit 18-19 season models  #########
data1819 = all_model_data[['season_18_19']]
all_1819_models = fitAllModels(data1819)

lapply(names(all_1819_models), function(f){
  print(f)
  temp = all_1819_models[[f]]
  save(temp, file = paste0('bayesian-implementation/model_fits/s1819_', f, '.RData'))
})


######### Fit 16-17 season models  #########
data1617 = all_model_data[['season_16_17']]
all_1617_models = fitAllModels(data1617)

lapply(names(all_1617_models), function(f){
  print(f)
  temp = all_1617_models[[f]]
  save(temp, file = paste0('bayesian-implementation/model_fits/s1617_', f, '.RData'))
})


######### Fit 16-17 season models  #########
data1516 = all_model_data[['season_15_16']]
all_1516_models = fitAllModels(data1516)

lapply(names(all_1516_models), function(f){
  print(f)
  temp = all_1516_models[[f]]
  save(temp, file = paste0('bayesian-implementation/model_fits/s1516_', f, '.RData'))
})



############################
######### Analysis #########
############################


all_fit_sums = lapply(all_fits, getFit)
#all_fit_sums = Reduce(function(...) merge(..., all=T, by = 'param'), all_fit_sums)
all_fit_sums


all_fit_sums = do.call(rbind, lapply(names(all_fit_sums), function(x) cbind(model = x, all_fit_sums[[x]])))

all_fit_sums[, team_num := as.numeric(gsub('alpha[[]|[]]', '', param))]
all_fit_sums = merge(all_fit_sums, teams, by.x = 'team_num', by.y = 'num', all = T)

ggplot(all_fit_sums[grepl('alpha', param)], aes(x = name, y = mean, color = model)) + 
  geom_point() + theme(axis.text.x = element_text(angle = 45 , hjust = 1))





all_fit_sums_1819 = lapply(all_1819_models, getFit)

all_fit_sums_1819 = do.call(rbind, lapply(names(all_fit_sums_1819), function(x) cbind(model = x, all_fit_sums_1819[[x]])))

all_fit_sums_1819[, team_num := as.numeric(gsub('alpha[[]|[]]', '', param))]
all_fit_sums_1819 = merge(all_fit_sums_1819, data1819$teams, by.x = 'team_num', by.y = 'num', all = T)


ggplot(all_fit_sums_1819[grepl('alpha', param)], aes(x = name, y = mean, color = model)) + 
  geom_point() + theme(axis.text.x = element_text(angle = 45 , hjust = 1))

