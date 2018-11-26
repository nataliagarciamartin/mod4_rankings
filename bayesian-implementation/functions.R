

######### Prep Data ###########


## General data
makeModelData = function(raw_data){
  setDT(raw_data)
  
  # make teams data
  teams = data.table(name = raw_data[, sort(unique(HomeTeam))])
  teams[, num := .I]
  
  # make matches data
  data = raw_data[, .(Date = as.Date(x = Date, '%d/%m/%y')
                      , HomeTeam = as.numeric(factor(HomeTeam, levels = teams$name))
                      , AwayTeam = as.numeric(factor(AwayTeam, levels = teams$name))
                      , home_win = as.numeric(FTR == 'H')
                      , away_win = as.numeric(FTR == 'A')
                      , draw = as.numeric(FTR == 'D')
  )]
  
  data[home_win == 1, result := 1]
  data[away_win == 1, result := 2]
  data[draw == 1, result := 3]
  
  # make league table
  homeresult = data[, .(wins_home = sum(home_win), loss_home = sum(away_win), draws_home = sum(draw)), by = .(team = HomeTeam)]
  awayresult = data[, .(wins_away = sum(away_win), loss_away = sum(home_win), draws_away = sum(draw)), by = .(team = AwayTeam)]
  
  standings = merge(homeresult, awayresult, by = 'team')
  standings[, points := 3 * (wins_home + wins_away) + (draws_home + draws_away)]
  standings = merge(teams, standings, by.x = 'num', by.y = 'team')
  
  #return all data tables
  return(list(games = data, standings = standings[order(points, decreasing = T)], teams = teams))
}



## Make data for Davidson
makeDavidsonData = function(model_data, sigma_hat = NULL, d = NULL, b = NULL, t = NULL){
  
  #pull oout data tables
  teams = model_data$teams
  games = model_data$games
  
  #flat prior if prior is null
  if(is.null(sigma_hat)){
    sigma_hat = 0.2
  }
  
  return(list(
    K = nrow(teams)
    , N = nrow(games)
    , team_i = games$HomeTeam
    , team_j = games$AwayTeam
    , result = games$result
    , sigma_hat = sigma_hat
    , d = ifelse(is.null(d), 1, d)
    , b = ifelse(is.null(b), 1, b)
    , t = t
  ))
}



## Make data for RK
makeRKData = function(model_data, sigma_hat = NULL, g = NULL, t = NULL){
  
  #pull oout data tables
  teams = model_data$teams
  games = model_data$games
  
  #flat prior if prior is null
  if(is.null(sigma_hat)){
    sigma_hat = 0.3
  }
  
  return(list(
    K = nrow(teams)
    , N = nrow(games)
    , team_i = games$HomeTeam
    , team_j = games$AwayTeam
    , result = games$result
    , sigma_hat = sigma_hat
    , g = ifelse(is.null(g), 1, g)
    , t = t
  ))
}



######### Function to fit all Stan models ########
fitAllModels = function(data, which_models = 'all'){
  
  results = list()
  
  #### DAVIDSON - 1/3 power
  if('davidson' %in% which_models | which_models == 'all'){
    cat(paste0("\n",Sys.time(),"\tFitting Davidson...\n"))
    fit_davidson = stan(file = 'bayesian-implementation/davidson.stan'
                        , data = makeDavidsonData(data)
                        , chains = 4
                        , warmup = 1000
                        , iter = 2000
                        , cores = 2
                        , refresh = 0)
    print(fit_davidson)
    
    results[["Davidson"]] = fit_davidson
  }

  
  
  #### DAVIDSON-BEAVER
  if('davidson-beaver' %in% which_models | which_models == 'all'){
    cat(paste0("\n",Sys.time(),"\tFitting Davidson-Beaver...\n"))
    fit_davidson_beaver = stan(file = 'bayesian-implementation/davidson-beaver.stan'
                               , data = makeDavidsonData(data, t = 1)  #need to specify prior for t
                               , chains = 4
                               , warmup = 1000
                               , iter = 2000
                               , cores = 2
                               , refresh = 0)
    print(fit_davidson_beaver)
    
    results[["Davidson-Beaver"]] = fit_davidson_beaver
  }
  
  #### DAVIDSON - power param
  if('davidson-power' %in% which_models | which_models == 'all'){
    cat(paste0("\n",Sys.time(),"\tFitting Davidson power...\n"))
    fit_davidson_power = stan(file = 'bayesian-implementation/davidson-power.stan'
                              , data = makeDavidsonData(data, t = 1)
                              , chains = 4
                              , warmup = 1000
                              , iter = 2000
                              , cores = 2
                              , refresh = 0)
    print(fit_davidson_power)
    
    results[["Davidson Power"]] = fit_davidson_power
  }
  
  
  ## RAO-KUPPER
  if('rao-kupper' %in% which_models | which_models == 'all'){
    cat(paste0("\n",Sys.time(),"\tFitting Rao-Kupper\n"))
    fit_RK = stan(file = 'bayesian-implementation/rao-kupper.stan'
                  , data = makeRKData(data)
                  , chains = 4
                  , warmup = 1000
                  , iter = 2000
                  , cores = 2
                  , refresh = 0)
    print(fit_RK)
    
    results[["Rao-Kupper"]] = fit_RK
  }
  
  # RAO-KUPPER WITH HOME ADVANTAGE
  if('rao-kupper-mult' %in% which_models | which_models == 'all'){
    cat(paste0("\n",Sys.time(),"\tFitting Rao-Kupper Mult\n"))
    fit_RK_mult = stan(file = 'bayesian-implementation/rao-kupper-mult.stan'
                       , data = makeRKData(data, t = 1)
                       , chains = 4
                       , warmup = 1000
                       , iter = 2000
                       , cores = 2
                       , refresh = 0)
    print(fit_RK_mult)
    
    results[["Rao-Kupper Mult"]] = fit_RK_mult
  }
  
  return(results)
}



##### Extract data from fits #####
getFit = function(stan_fit){
  # extract param estimates
  fit_sum = summary(stan_fit)$summary[, 1:3]
  fit_sum = setDT(data.frame(param = rownames(fit_sum), fit_sum))
  
  return(fit_sum)
}




##### Get posterior predictions #####

getPredStats = function(model, actual){
  predicted = extract(model)$result_hat
  
  by_game = rbindlist(lapply(1:length(actual), function(g){
    data.frame(actual = actual[g]
               , 'win_i' = sum(predicted[,g] == 1)
               , 'win_j' = sum(predicted[,g] == 2)
               , 'draw' = sum(predicted[,g] == 3)
               , n_correct = sum(predicted[,g] == actual[g])
               , prop_correct = mean(predicted[,g] == actual[g])
    )
  }))
  
  class_rate = sum(by_game$n_correct)/(4000*nrow(by_game))
  
  n_draws = apply(predicted, 1, function(g) sum(g == 3))
  
  home_wins = apply(predicted, 1, function(g) sum(g == 1))
  
  
  standings = apply(extract(fit_davidson)$result_hat, 1, function(g) {
    points_i = (g == 1) * 3 + (g == 3) * 1 
    points_j = (g == 2) * 3 + (g == 3) * 1
    
    data.table(rbind(cbind(team_i = data1718$games$HomeTeam, points = points_i)
                     , cbind(team_i = data1718$games$AwayTeam, points = points_j)))
  })
  
  top_team = lapply(standings, function(x) {
    x[, .(points = sum(points)), by = team_i][order(points, decreasing = T)][1,]
  })
  
  return(list(by_game = by_game
              , class_rate = class_rate
              , n_draws = n_draws
              , home_wins = home_wins
              , top_team = top_team$team_i
              , top_team_points = top_team$points
              ))
}
