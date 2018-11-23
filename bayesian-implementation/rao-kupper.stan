// adapted from https://opisthokonta.net/?p=1656


data {
  int<lower = 1> K;     // number of teams
  int<lower = 1> N;     // number of games
  
  int team_i[N];  // indicator for team i
  int team_j[N];  // indicator for team j
  int result[N];  // 1 if i wins, 2 is j wins and 3 if draw
  
  // Prior dist params
  vector[K] alpha;     // prior for lambda
  real theta_prior;    //prior param for theta
}

parameters {
  // draw param
  real<lower = 0> theta_bar;
  
  // vector of strength param
  simplex[K] lambda;  
  
}

transformed parameters{
  real<lower = 1> theta;
  theta = theta_bar + 1;
}



model {
  
  vector[3] outcome_probs[N];
  real lambda_i;
  real lambda_j;
  
  // exp prior for nu
  theta_bar ~ exponential(theta_prior);
  
  // dirichlet prior for lambda
  lambda ~ dirichlet(alpha);
  
  //loop over all the games
  for(r in 1:N){
    
    lambda_i = lambda[team_i[r]];
    lambda_j = lambda[team_j[r]];
    
    outcome_probs[r][1] = lambda_i / (lambda_i + lambda_j * theta);
    outcome_probs[r][2] = lambda_j / (lambda_i * theta + lambda_j);
    outcome_probs[r][3] = (theta ^ 2 - 1) * lambda_i * lambda_j/((lambda_i + lambda_j * theta) * (lambda_i * theta + lambda_j));
    
    result[r] ~ categorical(outcome_probs[r]);
  }
}