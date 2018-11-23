
library(rstan)
library(data.table)

setwd("~/github/ranking-ties")

#read in data
results1718 = fread('data/pl_results_17_18.csv')

# cleaning
teams = data.table(name = results1718[, sort(unique(HomeTeam))])
teams[, num := .I]

results1718_simple = results1718[, .(Date = as.Date(x = Date, '%d/%m/%y')
                                   , HomeTeam = as.numeric(factor(HomeTeam, levels = teams$name))
                                   , AwayTeam = as.numeric(factor(AwayTeam, levels = teams$name))
                                   , FTR
                                   , home_win = as.numeric(FTR == 'H')
                                   , away_win = as.numeric(FTR == 'A')
                                   , draw = as.numeric(FTR == 'D')
                                   , match_num = .I
                                   )]


all_pairs = data.table(rbind(t(combn(teams$num, 2)), t(combn(sort(teams$num, decreasing = T), 2))))
setnames(all_pairs, old = names(all_pairs), new = c('team_i', 'team_j'))


all_pairs = all_pairs[, n_ij := 0]
all_pairs = all_pairs[, n_i_ij := 0]
all_pairs = all_pairs[, n_j_ij := 0]
all_pairs = all_pairs[, n_0_ij := 0]

# first half of the matches
home_temp = merge(all_pairs, results1718_simple, by.x = c('team_i', 'team_j'), by.y = c('HomeTeam', 'AwayTeam'), all.x = T)
all_pairs = home_temp[, .(team_i
                          , team_j
                          , n_ij = n_ij + 1
                          , n_i_ij = n_i_ij + home_win
                          , n_j_ij = n_j_ij + away_win
                          , n_0_ij = n_0_ij + draw
)]

# other half of the matches
away_temp = merge(all_pairs, results1718_simple, by.x = c('team_i', 'team_j'), by.y = c('AwayTeam', 'HomeTeam'), all.x = T)
all_pairs = away_temp[, .(team_i
                          , team_j
                          , n_ij = n_ij + 1
                          , n_i_ij = n_i_ij + away_win
                          , n_j_ij = n_j_ij + home_win
                          , n_0_ij = n_0_ij + draw
)]


# total number of times another team was preferred over team i
all_pairs[, .(d_i = sum(n_j_ij - n_i_ij)), by = team_i]


# check that it worked
all_pairs[team_i == 1]
all_pairs[team_j == 1]

all_pairs[, b_ij := n_i_ij + n_0_ij]

N = sum(all_pairs$n_ij)/2



results1718_simple[home_win == 1, result := 1]
results1718_simple[away_win == 1, result := 2]
results1718_simple[draw == 1, result := 3]

teams
data_davidson = list(
  K = nrow(teams)
  , N = nrow(results1718_simple)
  , team_i = results1718_simple$HomeTeam
  , team_j = results1718_simple$AwayTeam
  , result = results1718_simple$result
  , alpha = rep(1, nrow(teams))/nrow(teams)
  , nu_prior = 1
  , eta_prior = 1
)

fit_davidson = stan(file = 'bayesian-implementation/davidson.stan'
                    , data = data_davidson
                    , chains = 4
                    , warmup = 1000
                    , iter = 2000
                    , cores = 2
                    , refresh = 0)

print(fit_davidson)

fit_davidson_ex = extract(fit_davidson)


names(extract(fit_davidson))

dim(fit_davidson_ex$lambda)

cbind(c('nu','eta',teams$name, 'lp'), summary(fit_davidson)$summary)




data_RK = list(
  K = nrow(teams)
  , N = nrow(results1718_simple)
  , team_i = results1718_simple$HomeTeam
  , team_j = results1718_simple$AwayTeam
  , result = results1718_simple$result
  , alpha = rep(1, nrow(teams))/nrow(teams)
  , theta_prior = 1.5
)

fit_RK = stan(file = 'bayesian-implementation/rao-kupper.stan'
                    , data = data_RK
                    , chains = 4
                    , warmup = 1000
                    , iter = 2000
                    , cores = 2
                    , refresh = 0)

print(fit_RK)


cbind(c('theta_bar',teams$name,'theta', 'lp'), summary(fit_RK)$summary)
