
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


# check model fit

launch_shinystan(fit_davidson)


fit_davidson = stan(file = 'bayesian-implementation/davidson.stan'
                    , data = makeDavidsonData(data1718)
                    , chains = 4
                    , warmup = 1000
                    , iter = 2000
                    , cores = 2
                    , refresh = 0)
print(fit_davidson)

fit_davidson_beaver = stan(file = 'bayesian-implementation/davidson-beaver.stan'
                    , data = makeDavidsonData(data1718, t = 1)
                    , chains = 4
                    , warmup = 1000
                    , iter = 2000
                    , cores = 2
                    , refresh = 0)
print(fit_davidson_beaver)


fit_RK = stan(file = 'bayesian-implementation/rao-kupper.stan'
                    , data = makeRKData(data1718)
                    , chains = 4
                    , warmup = 1000
                    , iter = 2000
                    , cores = 2
                    , refresh = 0)
print(fit_RK)

fit_RK_mult = stan(file = 'bayesian-implementation/rao-kupper-mult.stan'
                   , data = makeRKData(data1718, t = 1)
                   , chains = 4
                   , warmup = 1000
                   , iter = 2000
                   , cores = 2
                   , refresh = 0)
print(fit_RK_mult)


actual = makeDavidsonData(data1718)$result

pred_davidson = getPostPred(model = fit_davidson, actual = actual)
pred_davidson_beaver = getPostPred(model = fit_davidson_beaver, actual = actual)
pred_rk = getPostPred(model = fit_RK, actual = actual)
pred_rk_mult = getPostPred(model = fit_RK_mult, actual = actual)


#classification rate

getPredStats(fit_davidson, actual = actual)




ggplot(data.frame(num_draws = num_draws)) + 
  geom_histogram(aes(num_draws)) + 
  geom_vline(xintercept = sum(actual == 3))


ggplot(data.frame(home_wins = home_wins)) + 
  geom_histogram(aes(home_wins)) + 
  geom_vline(xintercept = sum(actual == 1))



all_fit_sums = lapply(all_1516_models, getFit)
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

lapply(all_1718_models, print)
