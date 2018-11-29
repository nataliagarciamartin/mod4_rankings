
############################
######### Analysis #########
############################

options(knitr.kable.NA = '-')

#load models
fit = load('bayesian-implementation/model_fits/s1718_Rao-Kupper Mult.rds')
fit_s1718_rkmult = get(fit)
rm(fit)

# check model fit

#### Post predictive power
actual = makeDavidsonData(data1718)$result

pred_davidson = getPredStats(model = all_1718_models[['Davidson']], actual = actual)
pred_davidson_beaver = getPredStats(model = all_1718_models[['Davidson-Beaver']], actual = actual)
pred_davidson_power = getPredStats(model = all_1718_models[['Davidson Power']], actual = actual)
pred_rk = getPredStats(model = all_1718_models[['Rao-Kupper']], actual = actual)
pred_rk_mult = getPredStats(model = all_1718_models[['Rao-Kupper Mult']], actual = actual)

all_1718_pred = lapply(all_1718_models, getPredStats, actual = actual)
class_rate = unlist(lapply(all_1718_pred, function(c) c$class_rate))

kable(data.frame(Model = names(class_rate)
                   , 'Classification Rate' = paste(round(class_rate * 100,2), '%')), format = 'latex', booktabs = T)

###### param summary
param_summary = Reduce(function(x,y) merge(x,y, all = T, by = 'param'), 
       lapply(all_1718_models, function(m){
         fit_sum = summary(m)$summary
         fit_sum = fit_sum[!grepl('outcome|alpha|result|lp', rownames(fit_sum)),]
         lambda = data.frame(param = data1718$teams$name
                             , est = fit_sum[grepl('lambda', rownames(fit_sum)),1])
         rownames(lambda) = NULL
         
         others = fit_sum[!grepl('lambda', rownames(fit_sum)),1]
         others = data.frame(param = names(others), est = others)
         
         rbind(lambda, others)
}))
names(param_summary) = c('param', names(all_1718_models))


param_summary = param_summary[order(1:26 <= 20, param_summary[, 3], decreasing = T),]
kable(param_summary[-26, ], format = 'latex', booktabs = TRUE, digits = 3, row.names = F)


## next season

param_summary_1819 = Reduce(function(x,y) merge(x,y, all = T, by = 'param'), 
                       lapply(all_1819_models, function(m){
                         fit_sum = summary(m)$summary
                         fit_sum = fit_sum[!grepl('outcome|alpha|result|lp', rownames(fit_sum)),]
                         lambda = data.frame(param = paste0('lambda[', str_pad(1:20, width = 2, pad = '0'),']')
                                             , est = fit_sum[grepl('lambda', rownames(fit_sum)),1])
                         rownames(lambda) = NULL
                         
                         #others = fit_sum[!grepl('lambda', rownames(fit_sum)),1]
                         #others = data.frame(param = names(others), est = others)
                         
                         #rbind(lambda, others)
                         lambda
                       }))
names(param_summary_1819) = c('param', names(all_1819_models))

param_summary_1819 = cbind(team = data1819$teams$name, param_summary_1819)
rankings_1819 = data.frame('Rank' = 1:20
  ,'Davidson-Beaver' = param_summary_1819[order(param_summary_1819[, 4], decreasing = T),1]
                           , 'Davidson Power' = param_summary_1819[order(param_summary_1819[, 5], decreasing = T),1]
                           )

kable(rankings_1819, format = 'latex', booktabs = TRUE, digits = 2, row.names = F)



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





###### gamma dist

K = 20
sigma_hat = 0.35
x = seq(0,1, length.out = 500)
d = data.frame(x = x, dens = dgamma(x, shape = 2*K, rate = 2*K/sigma_hat^2))
ggplot(d, aes(x = x, y = dens)) + geom_density(stat = 'identity')



fit_davidson_power_1718 = all_1718_models[['Davidson Power']]
extract(fit_davidson_power_1718)$sigma
ggplot(data.frame(sigma = extract(fit_davidson_power_1718)$sigma), aes(x = sigma)) + geom_density() +
  ggtitle("Marginal posterior distribution of sigma")

fit_dp_1718_lambda = extract(fit_davidson_power_1718)$lambda
fit_dp_1718_lambda = data.frame(fit_dp_1718_lambda)
colnames(fit_dp_1718_lambda) = data1718$teams$name

ggplot(fit_dp_1718_lambda) + 
  geom_density(aes(x = get('Liverpool'), fill = 'Liverpool'), alpha = 0.3) +
  geom_density(aes(x = get('Man United'), fill = 'Man United'), alpha = 0.3) +
  geom_density(alpha = 0.3, aes(x = get('Man City'), fill = 'Man City')) +
  scale_fill_manual(values = c("Man City" = 'lightblue', "Man United" = "red", "Liverpool" = "yellow")) +
  ggtitle(expression(paste("Marginal posterior density of ", log(alpha)))) + 
  xlab(expression(log(alpha)))

plot_data = data.frame(t(apply(fit_dp_1718_lambda, 2, summary)))
colnames(plot_data) = c('min', 'q1', 'med','mean','q3','max')
plot_data$team = rownames(plot_data)
plot_data$team = factor(plot_data$team, levels = plot_data[order(plot_data_db$med, decreasing = T),]$team)


pdf(file = "bayesian-implementation/plots/s1718_davidsonpower_lambda.pdf", width = 8, height = 5)
ggplot(plot_data) +
  geom_segment(aes(x = team, xend = team, y = min, yend = max)) +
  geom_segment(aes(x = team, xend = team, y = q1, yend = q3), size = 1.5) +
  geom_point(aes(x = team, y = med)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab(expression(log(alpha))) +
  ggtitle(expression(paste("Posterior distributions of ",log(alpha), " - Davidson Power Model")))
dev.off()


#########



fit_davidson_beaver_1718 = all_1718_models[['Davidson-Beaver']]
extract(fit_davidson_beaver_1718)$sigma
ggplot(data.frame(sigma = extract(fit_davidson_beaver_1718)$sigma), aes(x = sigma)) + geom_density() +
  ggtitle("Marginal posterior distribution of sigma")

fit_db_1718_lambda = extract(fit_davidson_beaver_1718)$lambda
fit_db_1718_lambda = data.frame(fit_db_1718_lambda)
colnames(fit_db_1718_lambda) = data1718$teams$name

ggplot(fit_db_1718_lambda) + 
  geom_density(aes(x = get('Liverpool'), fill = 'Liverpool'), alpha = 0.3) +
  geom_density(aes(x = get('Man United'), fill = 'Man United'), alpha = 0.3) +
  geom_density(alpha = 0.3, aes(x = get('Man City'), fill = 'Man City')) +
  scale_fill_manual(values = c("Man City" = 'lightblue', "Man United" = "red", "Liverpool" = "yellow")) +
  ggtitle(expression(paste("Marginal posterior density of ", log(alpha)))) + 
  xlab(expression(log(alpha)))

plot_data_db = data.frame(t(apply(fit_db_1718_lambda, 2, summary)))
colnames(plot_data_db) = c('min', 'q1', 'med','mean','q3','max')
plot_data_db$team = rownames(plot_data_db)
plot_data_db$team = factor(plot_data_db$team, levels = plot_data_db[order(plot_data_db$med, decreasing = T),]$team)


pdf(file = "bayesian-implementation/plots/s1718_davidsonbeaver_lambda.pdf", width = 8, height = 5)
ggplot(plot_data_db) +
  geom_segment(aes(x = team, xend = team, y = min, yend = max)) +
  geom_segment(aes(x = team, xend = team, y = q1, yend = q3), size = 1.5) +
  geom_point(aes(x = team, y = med)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab(expression(log(alpha))) +
  ggtitle(expression(paste("Posterior distributions of ",log(alpha), " - Davidson-Beaver")))
dev.off()




##### other parameter comparison

all_params_1718 = rbindlist(lapply(names(all_1718_models), function(m){
  m_extract = extract(all_1718_models[[m]])
  params = names(m_extract)[!grepl("lambda|lp|gamma_bar|outcome_probs|result_hat",names(m_extract))]
  
  post_params = data.frame(matrix(nrow = 4000, ncol = length(params)))
  for(p in 1:length(params)){
    post_params[, p] = m_extract[[params[p]]]
  }
  names(post_params) = params
  cbind(model = m, post_params)
}), use.names = T, fill = T)


pdf(file = "bayesian-implementation/plots/post_param_dists.pdf", height = 5, width = 8)
multiplot(
ggplot(all_params_1718, aes(fill = model)) + 
  geom_density(aes(x = delta), alpha = 0.3) +
  theme_classic() +
  ggtitle(expression(paste("Posterior dist of ", delta))) + 
  scale_fill_manual(values = c("#7fc97f", "#beaed4", "#fdc086"))


, ggplot(all_params_1718[grepl('Davidson', all_params_1718$model)], aes(fill = model)) + 
  geom_density(aes(x = sigma), alpha = 0.3) +
  theme_classic() +
  ggtitle(expression(paste("Posterior dist of ", sigma)))+ 
  scale_fill_manual(values = c("#7fc97f", "#beaed4", "#fdc086"))


, ggplot(all_params_1718[grepl('Davidson', all_params_1718$model)], aes(fill = model)) + 
  geom_density(aes(x = theta), alpha = 0.3) +
  theme_classic() +
  ggtitle(expression(paste("Posterior dist of ", theta)))+ 
  scale_fill_manual(values = c("#beaed4", "#fdc086"))


, ggplot(all_params_1718[grepl('Davidson', all_params_1718$model)], aes(fill = model)) + 
  geom_density(aes(x = beta), alpha = 0.3) +
  theme_classic() +
  ggtitle(expression(paste("Posterior dist of ", beta)))+ 
  scale_fill_manual(values = c("#fdc086")) +
  geom_vline(xintercept = mean(all_params_1718$beta, na.rm = T), lty = 2) +
  annotate("text", x = 0.3, y = 4, label = expression(paste(beta^PM, " = 0.14")))
, cols = 2)
dev.off()




###### home advantage

pdf(file = "bayesian-implementation/plots/ppd_home_wins.pdf", width = 10, height = 4)
multiplot(
ggplot(data.frame(home_wins = all_1718_pred[['Davidson']]$home_wins)) + 
  geom_histogram(aes(x = home_wins)) +
  geom_vline(xintercept = sum(actual == 1), lty = 2, color = 'red') +
  theme_classic() +
  xlab("Number of home wins") +
  ggtitle("PPD # home wins \nDavidson")

,ggplot(data.frame(home_wins = all_1718_pred[['Davidson-Beaver']]$home_wins)) + 
  geom_histogram(aes(x = home_wins)) +
  geom_vline(xintercept = sum(actual == 1), lty = 2, color = 'red') +
  theme_classic() +
  xlab("Number of home wins") +
  ggtitle("PPD # home wins \nDavidson-Beaver")

,ggplot(data.frame(home_wins = all_1718_pred[['Davidson Power']]$home_wins)) + 
  geom_histogram(aes(x = home_wins)) +
  geom_vline(xintercept = sum(actual == 1), lty = 2, color = 'red') +
  theme_classic() +
  xlab("Number of home wins") +
  ggtitle("PPD # home wins \nDavidson Power")
, cols = 3)
dev.off()

