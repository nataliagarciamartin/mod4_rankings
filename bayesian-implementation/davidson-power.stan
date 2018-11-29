// adapted from https://opisthokonta.net/?p=1656



data {
  int<lower = 1> K;     // number of teams
  int<lower = 1> N;     // number of games
  
  int team_i[N];  // indicator for team i
  int team_j[N];  // indicator for team j
  int result[N];  // 1 if i wins, 2 is j wins and 3 if draw
  
  // Prior dist params
  real sigma_hat; //hyperparam for lambda dist
  real d;    // hyperparam for delta prevalence of draws parameter
  real b;   //hyperparam for beta power parameter
  real t;   //hyperparam for theta home adv parameter
}

parameters {
  // draw param
  real<lower = 0> delta;           
  
  // power param
  real<lower = 0> beta;
  
  // home advantage param
  real<lower = 0> theta;
  
  // vector of strength param
  vector[K] lambda;  
  
  real sigma;
  
  
}


transformed parameters{
  vector[3] outcome_probs[N];
  vector[K] alpha;
  
  alpha = exp(lambda);
  
  for(r in 1:N){
    real mean_strength;
    real denom;
    real alpha_i;
    real alpha_j;
    
    alpha_i = alpha[team_i[r]];
    alpha_j = alpha[team_j[r]];
    
    mean_strength = delta * (alpha_i * alpha_j) ^ beta;
    denom = theta * alpha_i + alpha_j + mean_strength;
    
    outcome_probs[r][1] = theta * alpha_i / denom;
    outcome_probs[r][2] = alpha_j / denom;
    outcome_probs[r][3] = mean_strength / denom;
    
  }
}


model {
  matrix[K, K] Sigma;
  
  // exp prior for delta (draws)
  delta ~ exponential(d);
  
  // exp prior for beta (power)
  beta ~ exponential(b);
  
  // exp prior for theta (home adv)
  theta ~ exponential(t);
  
  // gamma hyperprior for variance of lambdas
  sigma ~ gamma(2*K, 2*K/sigma_hat^2);
  Sigma = diag_matrix(rep_vector(sigma ^ 2, K));
  
  // multivariate norm prior for alpha (strength)
  lambda ~ multi_normal(rep_vector(0, K), Sigma);
  
  //loop over all the games
  for(r in 1:N){
    result[r] ~ categorical(outcome_probs[r]);
  }
}


generated quantities{
  int result_hat[N];
  
  for(r in 1:N){
    result_hat[r] = categorical_rng(outcome_probs[r]);
  }
}




