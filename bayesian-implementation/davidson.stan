// adapted from https://opisthokonta.net/?p=1656



data {
  int<lower = 1> K;     // number of teams
  int<lower = 1> N;     // number of games
  
  int team_i[N];  // indicator for team i
  int team_j[N];  // indicator for team j
  int result[N];  // 1 if i wins, 2 is j wins and 3 if draw
  
  // Prior dist params
  vector[K] alpha;     // prior for lambda
  real nu_prior;    //prior param for nu
  real eta_prior;   //prior param for eta
}

parameters {
  // draw param
  real<lower = 0> nu;           
  
  // power param
  real<lower = 0> eta;
  
  // vector of strength param
  simplex[K] lambda;  
  
}



model {
  
  vector[3] outcome_probs[N];
  real mean_strength;
  real denom;
  
  // exp prior for nu
  nu ~ exponential(nu_prior);
  
  // exp prior for eta
  eta ~ exponential(eta_prior);
  
  // dirichlet prior for lambda
  lambda ~ dirichlet(alpha);
  
  //loop over all the games
  for(r in 1:N){
    
    mean_strength = nu * (team_i[r] * team_j[r]) ^ eta;
    denom = lambda[team_i[r]] + lambda[team_j[r]] + mean_strength;
    
    outcome_probs[r][1] = lambda[team_i[r]] / denom;
    outcome_probs[r][2] = lambda[team_j[r]] / denom;
    outcome_probs[r][3] = mean_strength / denom;
    
    result[r] ~ categorical(outcome_probs[r]);
  }
}



