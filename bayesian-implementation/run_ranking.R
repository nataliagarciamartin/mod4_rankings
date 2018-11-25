
library(rstan)
library(data.table)
library(ggplot2)

#set working directory and source functions
setwd("~/github/ranking-ties/")
source("bayesian-implementation/ranking_functions.R")


#read in data
results1718 = fread('data/pl_results_17_18.csv')
results_files = list.files('data/')
all_results = lapply(results_files, function(r) fread(paste0("data/", r)))
names(all_results) <- paste0('season', gsub('pl_results|[.]csv','',results_files))

# make model data
all_model_data = lapply(all_results, makeModelData)



######### Fit 17-18 season models  #########
data1718 = all_model_data[['season_17_18']]
teams = data1718$teams




names(all_fits) = c("Davidson", "Davidson-Beaver", "Rao-Kupper", "Rao-Kupper Mult")
all_fits = list(fit_davidson, fit_davidson_beaver, fit_RK, fit_RK_mult)


all_fit_sums = lapply(all_fits, getFit)
#all_fit_sums = Reduce(function(...) merge(..., all=T, by = 'param'), all_fit_sums)
all_fit_sums


all_fit_sums = do.call(rbind, lapply(names(all_fit_sums), function(x) cbind(model = x, all_fit_sums[[x]])))

all_fit_sums[, team_num := as.numeric(gsub('alpha[[]|[]]', '', param))]
all_fit_sums = merge(all_fit_sums, teams, by.x = 'team_num', by.y = 'num', all = T)

ggplot(all_fit_sums[grepl('alpha', param)], aes(x = name, y = mean, color = model)) + 
  geom_point() + theme(axis.text.x = element_text(angle = 45 , hjust = 1))






######### Fit 18-19 season models  #########
data1819 = all_model_data[['season_18_19']]
all_1819_models = fitAllModels(data1819)

all_fit_sums_1819 = lapply(all_1819_models, getFit)
names(all_fit_sums_1819) = c("Davidson", "Davidson-Beaver", "Rao-Kupper", "Rao-Kupper Mult")
all_fit_sums_1819 = do.call(rbind, lapply(names(all_fit_sums_1819), function(x) cbind(model = x, all_fit_sums_1819[[x]])))

all_fit_sums_1819[, team_num := as.numeric(gsub('alpha[[]|[]]', '', param))]
all_fit_sums_1819 = merge(all_fit_sums_1819, data1819$teams, by.x = 'team_num', by.y = 'num', all = T)


ggplot(all_fit_sums_1819[grepl('alpha', param)], aes(x = name, y = mean, color = model)) + 
  geom_point() + theme(axis.text.x = element_text(angle = 45 , hjust = 1))

