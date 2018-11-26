// adapted from https://opisthokonta.net/?p=1656



data {
  int<lower = 1> K;     // number of teams
  int<lower = 1> N;     // number of games
  
  int team_i[N];  // indicator for team i
  int team_j[N];  // indicator for team j
  int result[N];  // 1 if i wins, 2 is j wins and 3 if draw
  
  // hyperparams
  real d;    // hyperparam for delta prevalence of draws parameter
  real sigma_hat;  // hyperparam for covariance matrix for strength params
}

parameters {
  // draw param
  real<lower = 0> delta;           
  
  // alpha
  vector[K] lambda;
  
  //sigma
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
    
    mean_strength = delta * (alpha_i * alpha_j) ^ (0.33333);
    denom = alpha_i + alpha_j + mean_strength;
    
    outcome_probs[r][1] = alpha_i / denom;
    outcome_probs[r][2] = alpha_j / denom;
    outcome_probs[r][3] = mean_strength / denom;
    
  }
}


model {
  matrix[K, K] Sigma;
  
  // exp prior for delta (draws)
  delta ~ exponential(d);
  
  // multivariate norm prior for alpha (strength)
  sigma ~ gamma(2*K, 2*K/sigma_hat^2);
  Sigma = diag_matrix(rep_vector(sigma ^ 2, K));
  
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




