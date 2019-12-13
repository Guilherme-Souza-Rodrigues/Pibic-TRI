data {
  int<lower=1> N_alunos;              // number of students
  int<lower=1> N_itens;              // number of questions
  int<lower=1> N;                   // number of observations
  int<lower=1,upper=N_alunos> aluno[N];   // student for observation n
  int<lower=1,upper=N_itens> item[N];   // question for observation n
  int<lower=0,upper=1> y[N];     // correctness for observation n
}


parameters{
  vector[N_itens] b;
  vector<lower=0,upper=1>[N_itens] c;
  vector[N_alunos] theta;
  vector<lower=0>[N_itens] a;
}

model{
  a~normal(0,1);
  b~normal(0,1);
  c~beta(5,17);
  theta~normal(0,1);
  y~bernoulli(c[item]+((1-c[item]).*inv_logit((a[item] .*theta[aluno])-b[item])));
}
