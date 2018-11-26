
############################
######### Analysis #########
############################


#load models
fit = load('bayesian-implementation/model_fits/s1718_Rao-Kupper Mult.RData')
fit_s1718_rkmult = get(fit)
rm(fit)

# check model fit

launch_shinystan(fit_davidson)


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