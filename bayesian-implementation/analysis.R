
############################
######### Analysis #########
############################

options(knitr.kable.NA = '-')

library(gridExtra)

#load models
fit = load('bayesian-implementation/model_fits/s1718_Rao-Kupper Mult.rds')
fit_s1718_rkmult = get(fit)
rm(fit)

##########################
##### check model fit ####
##########################

#### Post predictive power
actual = makeDavidsonData(data1718)$result

## get PPD stats
all_1718_pred = lapply(all_1718_models, getPredStats, actual = actual)

## classfication rate
class_rate = unlist(lapply(all_1718_pred, function(c) c$class_rate))

# export table
kable(data.frame(Model = names(class_rate)
                   , 'Classification Rate' = paste(round(class_rate * 100,2), '%')), format = 'latex', booktabs = T)

#### Aux test stats

### Home wins
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


#### Draws
pdf(file = "bayesian-implementation/plots/ppd_draws.pdf", width = 10, height = 4)
multiplot(
  ggplot(data.frame(n_draws = all_1718_pred[['Davidson']]$n_draws)) + 
    geom_histogram(aes(x = n_draws)) +
    geom_vline(xintercept = sum(actual == 3), lty = 2, color = 'red') +
    theme_classic() +
    xlab("Number of draws") +
    ggtitle("PPD # draws \nDavidson")
  
  ,ggplot(data.frame(n_draws = all_1718_pred[['Davidson-Beaver']]$n_draws)) + 
    geom_histogram(aes(x = n_draws)) +
    geom_vline(xintercept = sum(actual == 3), lty = 2, color = 'red') +
    theme_classic() +
    xlab("Number of draws") +
    ggtitle("PPD # draws \nDavidson-Beaver")
  
  ,ggplot(data.frame(n_draws = all_1718_pred[['Davidson Power']]$n_draws)) + 
    geom_histogram(aes(x = n_draws)) +
    geom_vline(xintercept = sum(actual == 3), lty = 2, color = 'red') +
    theme_classic() +
    xlab("Number of draws") +
    ggtitle("PPD # draws \nDavidson Power")
  , cols = 3)
dev.off()



###################
# POSTERIOR DISTS #
###################


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


#### PLOT
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
    annotate("text", x = 0.4, y = 4, label = expression(paste(beta^PM, " = 0.2")))
  , cols = 2)
dev.off()




###################
#### BETA DISTS ###
###################

# get all file names
all_dp_files = list.files('bayesian-implementation/model_fits')[grepl('Power',list.files('bayesian-implementation/model_fits'))]

all_dp_models = list()
for(i in 1:length(all_dp_files)){
  season = gsub("[_Davidson power.rds]", "", all_dp_files[i])
  file = readRDS(paste0('bayesian-implementation/model_fits/',all_dp_files[i]))
  all_dp_models[[season]] = file
}


names(all_dp_models) = unlist(lapply(names(all_dp_models), function(n) unlist(strsplit(n, "_"))[1]))


### get beta dists
all_params_dp = rbindlist(lapply(names(all_dp_models), function(m){
  m_extract = extract(all_dp_models[[m]])
  params = names(m_extract)[!grepl("lambda|lp|gamma_bar|outcome_probs|result_hat",names(m_extract))]
  
  post_params = data.frame(matrix(nrow = 4000, ncol = length(params)))
  for(p in 1:length(params)){
    post_params[, p] = m_extract[[params[p]]]
  }
  names(post_params) = params
  cbind(model = m, post_params)
}), use.names = T, fill = T)


all_params_dp


#### PLOT
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}



plot_delta = ggplot(all_params_dp) + 
  geom_density(aes(x = delta, fill = model), alpha = 0.3, color = 'white') +
  theme_classic() +
  ggtitle(expression(paste("Posterior dist of ", delta))) +
  theme(legend.position = 'bottom') +
  geom_density(aes(x = delta, lty = 'ALL')) +
  geom_vline(xintercept = mean(all_params_dp$delta, na.rm = T), lty = 2, color = 'red') +
  annotate(geom = 'text', x = 1.1, y = 3.8, label = expression(paste(delta^PM, "= 0.95")))


plot_sigma = ggplot(all_params_dp) + 
  geom_density(aes(x = sigma, fill = model), alpha = 0.3, color = 'white') +
  theme_classic() +
  ggtitle(expression(paste("Posterior dist of ", sigma))) +
  theme(legend.position = 'bottom') +
  geom_density(aes(x = sigma, lty = 'ALL')) +
  geom_vline(xintercept = mean(all_params_dp$sigma, na.rm = T), lty = 2, color = 'red') +
  annotate(geom = 'text', x = 0.4, y = 8, label = expression(paste(sigma^PM, "= 0.34")))


plot_theta = ggplot(all_params_dp) + 
  geom_density(aes(x = theta, fill = model), alpha = 0.3, color = 'white') +
  theme_classic() +
  ggtitle(expression(paste("Posterior dist of ", theta))) +
  geom_density(aes(x = theta, lty = 'ALL')) +
  geom_vline(xintercept = mean(all_params_dp$theta, na.rm = T), lty = 2, color = 'red') +
  annotate(geom = 'text', x = 2, y = 2.3, label = expression(paste(theta^PM, "= 1.7")))


plot_beta = ggplot(all_params_dp) + 
  geom_density(aes(x = beta, fill = model), alpha = 0.3, color = 'white') +
  theme_classic() +
  ggtitle(expression(paste("Posterior dist of ", beta))) +
  geom_density(aes(x = beta, lty = 'ALL')) +
  geom_vline(xintercept = mean(all_params_dp$beta, na.rm = T), lty = 2, color = 'red') +
  annotate(geom = 'text', x = 0.55, y = 3, label = expression(paste(beta^PM, "= 0.4")))



pdf(file = "bayesian-implementation/plots/db_allseasons_post_param_dists.pdf", height = 3, width = 8)
grid.arrange(arrangeGrob(plot_delta + theme(legend.position="none"),
                         plot_sigma + theme(legend.position="none"),
                         plot_theta + theme(legend.position="none"),
                         plot_beta + theme(legend.position="none"),
                         nrow=1),
             g_legend(plot_delta), nrow=2,heights=c(10, 1))
dev.off()
################################




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
kable(param_summary[-26, 1:4], format = 'latex', booktabs = TRUE, digits = 3, row.names = F)




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





###########
# LAMBDAS #
###########
data1718$teams

lambdas1718_dp = extract(all_1718_models[['Davidson Power']])$lambda
colnames(lambdas1718_dp) = data1718$teams$name
lambdas1718_db = extract(all_1718_models[['Davidson-Beaver']])$lambda
colnames(lambdas1718_db) = data1718$teams$name

plot_data = rbind(cbind(model = 'Davidson Power', melt(lambdas1718_dp))
                  , cbind(model = 'Davidson-Beaver', melt(lambdas1718_db)))
names(plot_data) = c('model', 'iterations', 'team', 'lambda')

pdf(file = "bayesian-implementation/plots/s1718_lambdas.pdf", width = 8, height = 4)
ggplot(plot_data) +
  geom_boxplot(aes(x = team, y = lambda, color = model), outlier.size = 0) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="bottom") +
  ylab(expression(log(alpha))) + 
  xlab("Team") +
  ggtitle(expression(paste("Posterior distributions of ",log(alpha))))
dev.off()






###################
# NEXT SEASON #
###################

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




       