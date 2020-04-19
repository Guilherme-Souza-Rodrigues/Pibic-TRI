data {
  int<lower=1> J;                // número de alunos
  int<lower=1> K;                // número de questões
  int<lower=1> N;                // number of observations
  int<lower=1,upper=J> jj[N];    // student for observation n
  int<lower=1,upper=K> kk[N];    // question for observation n
  int<lower=0,upper=1> y[N];     // correctness of observation n
  int<lower=1> D;                // number of latent dimensions
}

transformed data {
  int<lower=1> L;
  L= D*(K-D)+D*(D+1)/2;  // number of non-zero loadings
}

parameters {    
  matrix[J,D] theta;         //person parameter matrix
  vector<lower=0>[L] alpha_l;   //first column discrimination matrix
  vector[K] c;
  vector[K] b;
}

transformed parameters{
  matrix[D,K] a; // connstrain the upper traingular elements to zero 
  for(i in 1:K){
    for(j in (i+1):D){
      a[j,i] = 0;
    }
  } 
{
  int index;
  index=0;
  for (j in 1:D) {
    for (i in j:K) {
      index = index + 1;
      a[j,i] = alpha_l[index];
    } 
  }
}
}

model {
  to_vector(theta) ~ normal(0,1); 
  alpha_l ~ normal(0,1);
  b ~ normal(0,1);
  c ~ beta(5,17);
{
  vector[N] nu;  //vector of predictor terms
  for (i in 1:N) 
    nu[i] = c[kk[i]]+((1-c[kk[i]])*inv_logit(theta[jj[i]]*col(a,kk[i])+ b[kk[i]])); 
  y ~ bernoulli(nu); 
}
}





