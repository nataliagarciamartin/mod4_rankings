// adapted from https://opisthokonta.net/?p=1656


data {
  int<lower = 1> K;     // number of teams
  int<lower = 1> N;     // number of games
  
  int team_i[N];  // indicator for team i
  int team_j[N];  // indicator for team j
  int result[N];  // 1 if i wins, 2 is j wins and 3 if draw
  
  // Prior dist params
  vector[K] a;      // hyperparam for param alpha prior
  real g;           // hyperparam for param gamma prior
}

parameters {
  // draw param
  real<lower = 0> gamma_bar;
  
  // vector of strength param
  simplex[K] alpha;  
  
}

transformed parameters{
  real<lower = 1> gamma;
  vector[3] outcome_probs[N];
  
  gamma = gamma_bar + 1;
  
  for(r in 1:N){
    real alpha_i;
    real alpha_j;
  
    alpha_i = alpha[team_i[r]];
    alpha_j = alpha[team_j[r]];
    
    outcome_probs[r][1] = alpha_i / (alpha_i + alpha_j * gamma);
    outcome_probs[r][2] = alpha_j / (alpha_i * gamma + alpha_j);
    outcome_probs[r][3] = (gamma ^ 2 - 1) * alpha_i * alpha_j/((alpha_i + alpha_j * gamma) * (alpha_i * gamma + alpha_j));
    
  }
  
}

model {
  
  // exp prior for nu
  gamma_bar ~ exponential(g);
  
  // dirichlet prior for alpha
  alpha ~ dirichlet(a);
  
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
