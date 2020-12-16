data {
  int<lower=1> k; // number of mixture components
  int<lower=1> n; // number of data points
  vector[n] x; 
  int y[n]; // observations
  int N;
}
parameters { 
simplex[k] w ; // mixing proportions
real beta_0[k]; 
real beta_1[k]; 
}
model {
  real ps[k];
  //Priors
  beta_0 ~ normal(0, 100);
  beta_1 ~ normal(0, 100);
  w ~ dirichlet(rep_vector(1.0/k, k));
  
  //Likelihood
  for (i in 1:n) {
    for (j in 1:k)
      ps[j] = log(w[j]) + binomial_logit_lpmf(y[i]| N, beta_0[j]+beta_1[j]*x[i]); 
    target += log_sum_exp(ps);
  }
}
generated quantities {   //posterior predictive draws
  int y_rep[n];
  int<lower=1,upper=k> comp[n];  
  for(i in 1:n){
     comp[i] = categorical_rng(w); //gera um rótulo para a observação i
     y_rep[i] = binomial_rng(N, 1.0/(1.0+exp(-(beta_0[comp[i]]+beta_1[comp[i]]*x[i]))));
   }
}






