// adapted from https://opisthokonta.net/?p=1656



data {
  int<lower = 1> K;     // number of teams
  int<lower = 1> N;     // number of games
  
  int team_i[N];  // indicator for team i
  int team_j[N];  // indicator for team j
  int result[N];  // 1 if i wins, 2 is j wins and 3 if draw
  
  // Prior dist params
  vector[K] a;     // hyperparam vector for alpha prior
  real d;    // hyperparam for delta prevalence of draws parameter
  real b;   //hyperparam for beta power parameter
  real t;         // hyperparam for home advantage param
}

parameters {
  // draw param
  real<lower = 0> delta;           
  
  // power param
  real<lower = 0> beta;
  
  // vector of strength param
  simplex[K] alpha;  
  
  // home advantage param
  real<lower = 0> theta;
  
}



model {
  
  vector[3] outcome_probs[N];
  real mean_strength;
  real denom;
  real alpha_i;
  real alpha_j;
  
  // exp prior for delta (draws)
  delta ~ exponential(d);
  
  // exp prior for beta (power)
  beta ~ exponential(b);
  
  // dirichlet prior for alpha (strength)
  alpha ~ dirichlet(a);
  
  // exp prior for beta (power)
  theta ~ exponential(t);
  
  //loop over all the games
  for(r in 1:N){
    
    alpha_i = alpha[team_i[r]];
    alpha_j = alpha[team_j[r]];
    
    mean_strength = delta * (theta * alpha_i * alpha_j) ^ beta;
    denom = (theta * alpha_i) + alpha_j + mean_strength;
    
    outcome_probs[r][1] = theta * alpha_i / denom;
    outcome_probs[r][2] = alpha_j / denom;
    outcome_probs[r][3] = mean_strength / denom;
    
    result[r] ~ categorical(outcome_probs[r]);
  }
}



