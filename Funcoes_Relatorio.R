# Criacao da variavel Tema no banco de dados
nomes.q <- c("...medida_probabilidade.......", "...propriedade_probabilidade.......",
             "...probabilidade_total.......", "...teorema_Bayes.......", "...variaveis_aleatorias.......",
             "...distribuicao_binomial.......", "...distribuicao_binomial.......", "...distribuicao_geometrica.......",
             "...distribuicao_hipergeometrica.......", "...distribuicao_poisson.......", 
             "...aproximacao_poisson_binomial.......", "...funcao_densidade.......",
             "...distribuicao_acumulada.......", "...momentos.......", "...distribuicao_exponencial.......",
             "...distribuicao_normal.......", "...distribuicao_normal...", "...aproximacao_normal_binomial.......",
             "...distribuicao_condicional.......", "...covariancia_correlacao.......", "...distribuicao_media.......",
             "...distribuicao_proporcao.......", "...maxima_verossimilhanca.......",
             "...IC_media_normal.......", "...IC_media_t.......", "...IC_proporção.......", "...IC_proporcao.......",
             "...TH_media.......", "...tamanho_media.......", "...TH_proporcao.......", 
             "...pvalor_media.......", "...tamanho_prop.......", "...pvalor_proporcao.......", 
             "...valor_esperado_e_variancia.......")
temas <- c("medida_probabilidade", "propriedade_probabilidade",
           "probabilidade_total", "teorema_Bayes", "variaveis_aleatorias",
           "distribuicao_binomial", "distribuicao_binomial", "distribuicao_geometrica",
           "distribuicao_hipergeometrica", "distribuicao_poisson", 
           "aproximacao_poisson_binomial", "funcao_densidade",
           "distribuicao_acumulada", "momentos", "distribuicao_exponencial",
           "distribuicao_normal", "distribuicao_normal", "aproximacao_normal_binomial",
           "distribuicao_condicional", "covariancia_correlacao", "distribuicao_media",
           "distribuicao_proporcao", "maxima_verossimilhanca",
           "IC_media_normal", "IC_media_t", "IC_proporção", "IC_proporção",
           "TH_media", "tamanho_media", "TH_proporcao", 
           "pvalor_media", "tamanho_prop", "pvalor_proporcao", 
           "valor_esperado_e_variancia")
for (i in 1:length(temas)){
  dados.original$Tema <- gsub(nomes.q[i], temas[i], dados.original$Nome.questao)
  dados.original$Tema <- gsub(nomes.q[i], temas[i], dados.original$Tema)
}
for (i in 1:length(temas)){
  dados.original$Tema <- gsub(nomes.q[i], temas[i], dados.original$Tema)
}


# Alguns parâmetros 
nchains <- 4

niter <- 2500

n.questoes.prova <- 10

n.mcmc <- 1000

thin.mcmc <- 10

n.turmas <- length(unique(dados.original$Turma))

n.provas <- length(unique(dados.original$Numero.prova))

turmas <- c("AA","AB","BA","BB","CA","CB","CC","DA","DB","EA")
n.sim <- n.mcmc/thin.mcmc

# Criando o mapa de questões
mapa <- function(){for(prova in 1:n.provas) {
  mapa.questoes[,,prova] <- dplyr::filter(dados.original, Numero.prova==prova) %>%
    dplyr::select(Turma, Questao, Nome.questao) %>% 
    unique() %>% 
    dcast(Turma ~ Questao, value.var="Nome.questao") %>%  
    #dplyr::filter(., complete.cases(.)) %>%
    as.matrix() %>% 
    .[, -1]
}
  return(mapa.questoes)
}

#Criando dataframe com as respostas dicotômicas de cada aluno:
dico <- function(){for(prova in 1:n.provas) {
  data <- dados.original[order(dados.original$Matricula), ] %>%
    dplyr::filter(Numero.prova==prova) %>%
    droplevels()
  matricula <- data$Matricula
  acerto <- as.numeric(data$Acertou)
  questao <- data$Nome.questao
  turma <- data$Turma
  frame <- data.frame(matricula,acerto,questao,turma) %>% 
    dplyr::filter(., complete.cases(.))
  respostas.dico[[prova]] <- reshape(frame, v.names = "acerto", idvar = "matricula", timevar="questao", 
                           direction="wide")
  colnames(respostas.dico[[prova]]) <- gsub("^acerto.*?.","",colnames(respostas.dico[[prova]]))
}
  return(respostas.dico)}

# Cálculo dos estimadores dos parâmetros dos itens e do estimador do parâmetro de habilidade de
#cada aluno via Monte Carlo Cadeia de Markov:
mcmc<- function() {
  model <- stan_model(model_code = 
                        
                        "data {
                      int<lower=1> N_alunos;              // number of students
                      int<lower=1> N_itens;              // number of questions
                      int<lower=1> N;                   // number of observations
                      int<lower=1,upper=N_alunos> aluno[N];   // student for observation n
                      int<lower=1,upper=N_itens> item[N];   // question for observation n
                      int<lower=0,upper=1> y[N];     // correctness for observation n
                      
}

transformed data{
int mu;
int sigma;
mu=0;
sigma=1;

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
"

  )
  
banco_respostas <- dados.original%>%
  dplyr::select(Matricula,Nome.questao,Acertou,Numero.prova)%>%
  mutate(Nome.questao=as.character(Nome.questao),
         Matricula=as.numeric(Matricula))
##############
#  P1
##############
dados_p1 <- banco_respostas%>%
  filter(Numero.prova==1)%>%
  na.omit(Acertou)%>%
  mutate(Matricula.mod=as.numeric(as.factor(Matricula)),
         Nome.questao.mod=as.numeric(as.factor(Nome.questao)))%>%
  arrange(Matricula.mod,Nome.questao.mod)

Matriculas_equivalencia_p1 <- dados_p1%>%
  dplyr::select(Matricula,Matricula.mod)%>%
  distinct()%>%
  arrange(Matricula.mod)

Questoes_equivalencia_p1 <- dados_p1%>%
  dplyr::select(Nome.questao,Nome.questao.mod)%>%
  distinct()%>%
  arrange(Nome.questao.mod)

fit.p1 <- sampling(object=model,data=list(N_alunos=length(unique(dados_p1$Matricula.mod)),
                                          N_itens=length(unique(dados_p1$Nome.questao.mod)),
                                          N=nrow(dados_p1),
                                          aluno=dados_p1$Matricula.mod,
                                          item=dados_p1$Nome.questao.mod, 
                                          y=dados_p1$Acertou),iter=niter,chains=nchains,cores=4)

params.p1 <- extract(fit.p1)
itens.p1 <- array(c(params.p1$a,params.p1$b,params.p1$c),
                    dim = c(nchains*(niter/2) , nrow(Questoes_equivalencia_p1), 3),
                    dimnames = list(paste("simulação",1:(nchains*(niter/2))),
                                    Questoes_equivalencia_p1$Nome.questao,
                                    c('a','b','c')))

thetas.mcmc.p1 <- params.p1$theta
colnames(thetas.mcmc.p1) <- Matriculas_equivalencia_p1$Matricula
rm(Questoes_equivalencia_p1,Matriculas_equivalencia_p1,dados_p1,fit.p1)
##############
#  P2
##############
dados_p2 <- banco_respostas%>%
  filter(Numero.prova==2)%>%
  na.omit(Acertou)%>%
  mutate(Matricula.mod=as.numeric(as.factor(Matricula)),
         Nome.questao.mod=as.numeric(as.factor(Nome.questao)))%>%
  arrange(Matricula.mod,Nome.questao.mod)

Matriculas_equivalencia_p2 <- dados_p2%>%
  dplyr::select(Matricula,Matricula.mod)%>%
  distinct()%>%
  arrange(Matricula.mod)
  
Questoes_equivalencia_p2 <- dados_p2%>%
  dplyr::select(Nome.questao,Nome.questao.mod)%>%
  distinct()%>%
  arrange(Nome.questao.mod)
  
fit.p2 <- sampling(object=model,data=list(N_alunos=length(unique(dados_p2$Matricula.mod)),
                                          N_itens=length(unique(dados_p2$Nome.questao.mod)),
                                          N=nrow(dados_p2),
                                          aluno=dados_p2$Matricula.mod,
                                          item=dados_p2$Nome.questao.mod, 
                                          y=dados_p2$Acertou),iter=niter,chains=nchains,cores=4)
  
params.p2 <- extract(fit.p2)

itens.p2 <- array(c(params.p2$a,params.p2$b,params.p2$c),
                    dim = c(nchains*(niter/2) , nrow(Questoes_equivalencia_p2), 3),
                    dimnames = list(paste("simulação",1:(nchains*(niter/2))),
                                    Questoes_equivalencia_p2$Nome.questao,
                                    c('a','b','c')))
  
thetas.mcmc.p2 <- params.p2$theta
colnames(thetas.mcmc.p2) <- Matriculas_equivalencia_p2$Matricula
rm(Questoes_equivalencia_p2,Matriculas_equivalencia_p2,dados_p2,fit.p2)
##############
#  P3
##############
dados_p3 <- banco_respostas%>%
  filter(Numero.prova==3)%>%
  na.omit(Acertou)%>%
  mutate(Matricula.mod=as.numeric(as.factor(Matricula)),
         Nome.questao.mod=as.numeric(as.factor(Nome.questao)))%>%
  arrange(Matricula.mod,Nome.questao.mod)
  
Matriculas_equivalencia_p3 <- dados_p3%>%
  dplyr::select(Matricula,Matricula.mod)%>%
  distinct()%>%
  arrange(Matricula.mod)
  
Questoes_equivalencia_p3 <- dados_p3%>%
  dplyr::select(Nome.questao,Nome.questao.mod)%>%
  distinct()%>%
  arrange(Nome.questao.mod)
  
fit.p3 <- sampling(object=model,data=list(N_alunos=length(unique(dados_p3$Matricula.mod)),
                                          N_itens=length(unique(dados_p3$Nome.questao.mod)),
                                          N=nrow(dados_p3),
                                          aluno=dados_p3$Matricula.mod,
                                          item=dados_p3$Nome.questao.mod, 
                                          y=dados_p3$Acertou),iter=niter,chains=nchains,cores=4)
  
params.p3 <- extract(fit.p3)

itens.p3 <- array(c(params.p3$a,params.p3$b,params.p3$c),
                    dim = c(nchains*(niter/2) , nrow(Questoes_equivalencia_p3), 3),
                    dimnames = list(paste("simulação",1:(nchains*(niter/2))),
                                    Questoes_equivalencia_p3$Nome.questao,
                                    c('a','b','c')))
  
thetas.mcmc.p3 <- params.p3$theta
colnames(thetas.mcmc.p3) <- Matriculas_equivalencia_p3$Matricula
rm(Questoes_equivalencia_p3,Matriculas_equivalencia_p3,dados_p3,fit.p3)
  
##############
#  P4-subs
##############
dados_p4 <- banco_respostas%>%
  filter(Numero.prova==4)%>%
  na.omit(Acertou)%>%
  mutate(Matricula.mod=as.numeric(as.factor(Matricula)),
         Nome.questao.mod=as.numeric(as.factor(Nome.questao)))%>%
  arrange(Matricula.mod,Nome.questao.mod)
  
Matriculas_equivalencia_p4 <- dados_p4%>%
  dplyr::select(Matricula,Matricula.mod)%>%
  distinct()%>%
  arrange(Matricula.mod)
  
Questoes_equivalencia_p4 <- dados_p4%>%
  dplyr::select(Nome.questao,Nome.questao.mod)%>%
  distinct()%>%
  arrange(Nome.questao.mod)
  
fit.p4 <- sampling(object=model,data=list(N_alunos=length(unique(dados_p4$Matricula.mod)),
                                          N_itens=length(unique(dados_p4$Nome.questao.mod)),
                                          N=nrow(dados_p4),
                                          aluno=dados_p4$Matricula.mod,
                                          item=dados_p4$Nome.questao.mod, 
                                          y=dados_p4$Acertou),iter=niter,chains=nchains,cores=4)

params.p4 <- extract(fit.p4)

itens.p4 <- array(c(params.p4$a,params.p4$b,params.p4$c),
                    dim = c(nchains*(niter/2) , nrow(Questoes_equivalencia_p4), 3),
                    dimnames = list(paste("simulação",1:(nchains*(niter/2))),
                                    Questoes_equivalencia_p4$Nome.questao,
                                    c('a','b','c')))
  
thetas.mcmc.p4 <- params.p4$theta
colnames(thetas.mcmc.p4) <- Matriculas_equivalencia_p4$Matricula
rm(Questoes_equivalencia_p4,Matriculas_equivalencia_p4,dados_p4,fit.p4)
#########################
#  Juntando tudo
#########################
mcmc.itens <- list(itens.p1,itens.p2,itens.p3,itens.p4)
mcmc.theta <- list(thetas.mcmc.p1,thetas.mcmc.p2,thetas.mcmc.p3,thetas.mcmc.p4)
#save(mcmc.itens, file = "Parametros_Itens.RData")
return(list(thetas=mcmc.theta,itens=mcmc.itens))
}

# Dataframe dos coeficientes de cada questao
data.i <- function(){for (prova in 1:n.provas){
  itens.p[[prova]] <- matrix(0, nrow = ncol(mcmc.itens[[prova]][,,1]), ncol = 3)
  for (parametro in 1:3){
    for (item in 1:ncol(mcmc.itens[[prova]][,,1])){
      itens.p[[prova]][item, parametro] <- mean(mcmc.itens[[prova]][,item,parametro])
      itens.p[[prova]] <- data.frame(itens.p[[prova]])
      rownames(itens.p[[prova]]) <- colnames(mcmc.itens[[prova]][,,1])
      colnames(itens.p[[prova]]) <- letters[1:3]
    }
  }
}
save(itens.p, file = "Parametros_Itens_Prova.RData")
return(itens.p)
}

# Simulação com as probabilidades de que um aluno mediano acerte a questão para cada questão 
#selecionada em cada prova
sim.aluno.medio <- function(){
  for(prova in 1:n.provas){
    Pm.probs[[prova]] <- array(c(as.numeric(mcmc.itens[[prova]][,,3]) +                   
                                   (1-as.numeric(mcmc.itens[[prova]][,,3]))*
                                   (1/(1+exp( as.numeric(mcmc.itens[[prova]][,,2]))))),
                               dim=c(nchains*(niter/2), ncol(respostas.dico[[prova]])-2), 
                               dimnames=list(paste("Simulacao", 1:(nchains*(niter/2))),
                                             colnames(mcmc.itens[[prova]])))
    } 
  return(Pm.probs)
}

# Probabilidade de acerto de cada questão feita por cada aluno
prob.acerto.questoes <- function(){for (prova in 1:n.provas){
  P.probs[[prova]] <- matrix(0, nrow = nrow(respostas.dico[[prova]]), ncol=ncol(respostas.dico[[prova]][, -c(1,2)]))
  for (aluno in 1:nrow(respostas.dico[[prova]])){
    
    P.probs[[prova]][aluno,] <- c(
      itens.p[[prova]][,3] + (1-itens.p[[prova]][,3])*
        (1/(1+exp( -(itens.p[[prova]][,1]*colMeans(mcmc.theta[[prova]])[aluno] - itens.p[[prova]][,2]))))
    )
    
    P.probs[[prova]] <- data.frame(P.probs[[prova]])
    rownames(P.probs[[prova]]) <- colnames(mcmc.theta[[prova]])
    colnames(P.probs[[prova]]) <- rownames(itens.p[[prova]])
    
    
  }
}
  return(P.probs)
}


# Dataframes de cada prova com a probabilidade de acerto por tema (questão que o aluno fez do tema) para cada aluno 
prob.tema1 <- function(){for (aluno in 1:ncol(mcmc.theta[[1]])){
  Prob.tema1 <- data.frame(Prob.tema1)
  
  turma <- dados.original %>% 
    dplyr::filter(Numero.prova==1, Matricula==colnames(mcmc.theta[[1]])[aluno]) %>% 
    dplyr::select(Turma) %>% unique()
  turma <- turma[1,1]
  
  questoes <- dados.original %>% 
    dplyr::filter(Numero.prova==1, Matricula==colnames(mcmc.theta[[1]])[aluno]) %>% 
    dplyr::select(Nome.questao, Acertou) %>% unique()
  
  questoes <- as.character(questoes[is.na(questoes$Acertou)==FALSE,1])
  
  p <- P.probs[[1]][aluno, questoes]
  
  a = p[order(names(p))]
  
  at <- which(!(seq(1,10) %in% as.numeric(substring(names(a), 1,2))))
  
  if(length(at)==0) {Prob.tema1[aluno, ] <- a} 
  else if(length(at)==1){Prob.tema1[aluno, ] <- insert(unlist(c(a)), at, NA)}
  else if(length(at)==2){Prob.tema1[aluno, ] <- insert(insert(unlist(c(a)), at[1], NA), at[2], NA)}
  
}

rownames(Prob.tema1) <- colnames(mcmc.theta[[1]])
colnames(Prob.tema1) <- substring(names(a), 4, nchar(names(a))-7)

return(Prob.tema1)
}


prob.tema2 <- function(){for (aluno in 1:ncol(mcmc.theta[[2]])){
  Prob.tema2 <- data.frame(Prob.tema2)
  
  turma <- dados.original %>% 
    dplyr::filter(Numero.prova==2, Matricula==colnames(mcmc.theta[[2]])[aluno]) %>% 
    dplyr::select(Turma) %>% unique()
  turma <- turma[1,1]
  
  questoes <- dados.original %>% 
    dplyr::filter(Numero.prova==2, Matricula==colnames(mcmc.theta[[2]])[aluno]) %>% 
    dplyr::select(Nome.questao, Acertou) %>% unique()
  
  questoes <- as.character(questoes[is.na(questoes$Acertou)==FALSE,1])
  
  p <- P.probs[[2]][aluno, questoes]
  
  a = p[order(names(p))]
  
  at <- which(!(seq(11,20) %in% as.numeric(substring(names(a), 1,2))))
  
  if(length(at)==0) {Prob.tema2[aluno, ] <- a} 
  else if(length(at)==1){Prob.tema2[aluno, ] <- insert(unlist(c(a)), at, NA)}
  else if(length(at)==2){Prob.tema2[aluno, ] <- insert(insert(unlist(c(a)), at[1], NA), at[2], NA)}
  
}

rownames(Prob.tema2) <- colnames(mcmc.theta[[2]])
colnames(Prob.tema2) <- substring(names(a), 4, nchar(names(a))-7)

return(Prob.tema2)
}


prob.tema3 <- function(){for (aluno in 1:ncol(mcmc.theta[[3]])){
  Prob.tema3 <- data.frame(Prob.tema3)
  
  turma <- dados.original %>% 
    dplyr::filter(Numero.prova==3, Matricula==colnames(mcmc.theta[[3]])[aluno]) %>% 
    dplyr::select(Turma) %>% unique()
  turma <- turma[1,1]
  
  questoes <- dados.original %>% 
    dplyr::filter(Numero.prova==3, Matricula==colnames(mcmc.theta[[3]])[aluno]) %>% 
    dplyr::select(Nome.questao, Acertou) %>% unique()
  
  questoes <- as.character(questoes[is.na(questoes$Acertou)==FALSE,1])
  
  p <- P.probs[[3]][aluno, questoes]
  
  a = p[order(names(p))]
  
  at <- which(!(seq(21,30) %in% as.numeric(substring(names(a), 1,2))))
  
  if(length(at)==0) {Prob.tema3[aluno, ] <- a} 
  else if(length(at)==1){Prob.tema3[aluno, ] <- insert(unlist(c(a)), at, NA)}
  else if(length(at)==2){Prob.tema3[aluno, ] <- insert(insert(unlist(c(a)), at[1], NA), at[2], NA)}
  
}

rownames(Prob.tema3) <- colnames(mcmc.theta[[3]])
colnames(Prob.tema3) <- substring(names(a), 4, nchar(names(a))-7)

return(Prob.tema3)
}


# Dataframe com os acertos de cada aluno em cada tema
acertou.tema1 <- function(){for (aluno in 1:ncol(mcmc.theta[[1]])){
  tx <- dados.original %>% 
    dplyr::filter(Numero.prova==1, Matricula==as.numeric(colnames(mcmc.theta[[1]])[aluno])) %>% 
    dplyr::select(Acertou, Matricula, Tema, Nome.questao)
  tix <- tx[,1]
  names(tix) <- tx[,4]
  tix <- tix[order(names(tix))]
  at <- which(!(seq(1,10) %in% as.numeric(substring(names(tix), 1,2))))
  
  if(length(at)==0) {Acerto.tema1[aluno,] <- tix} 
  else if(length(at)==1){Acerto.tema1[aluno, ] <- insert(tix, at, NA)}
  else if(length(at)==2){Acerto.tema1[aluno, ] <- insert(insert(tix, at[1], NA), at[2], NA)}
  
}
Acerto.tema1 <- data.frame(Acerto.tema1)
rownames(Acerto.tema1) <- colnames(mcmc.theta[[1]])
colnames(Acerto.tema1) <- substring(names(tix), 4, nchar(names(tix))-7)

return(Acerto.tema1)
}


acertou.tema2 <- function(){for (aluno in 1:ncol(mcmc.theta[[2]])){
  tx <- dados.original %>% 
    dplyr::filter(Numero.prova==2, Matricula==as.numeric(colnames(mcmc.theta[[2]])[aluno])) %>% 
    dplyr::select(Acertou, Matricula, Tema, Nome.questao)
  tix <- tx[,1]
  names(tix) <- tx[,4]
  tix <- tix[order(names(tix))]
  at <- which(!(seq(11,20) %in% as.numeric(substring(names(tix), 1,2))))
  
  if(length(at)==0) {Acerto.tema2[aluno,] <- tix} 
  else if(length(at)==1){Acerto.tema2[aluno, ] <- insert(tix, at, NA)}
  else if(length(at)==2){Acerto.tema2[aluno, ] <- insert(insert(tix, at[1], NA), at[2], NA)}
  
}
Acerto.tema2 <- data.frame(Acerto.tema2)
rownames(Acerto.tema2) <- colnames(mcmc.theta[[2]])
colnames(Acerto.tema2) <- substring(names(tix), 4, nchar(names(tix))-7)

return(Acerto.tema2)
}


acertou.tema3 <- function(){for (aluno in 1:ncol(mcmc.theta[[3]])){
  tx <- dados.original %>% 
    dplyr::filter(Numero.prova==3, Matricula==as.numeric(colnames(mcmc.theta[[3]])[aluno])) %>% 
    dplyr::select(Acertou, Matricula, Tema, Nome.questao)
  tix <- tx[,1]
  names(tix) <- tx[,4]
  tix <- tix[order(names(tix))]
  at <- which(!(seq(21,30) %in% as.numeric(substring(names(tix), 1,2))))
  
  if(length(at)==0) {Acerto.tema3[aluno,] <- tix} 
  else if(length(at)==1){Acerto.tema3[aluno, ] <- insert(tix, at, NA)}
  else if(length(at)==2){Acerto.tema3[aluno, ] <- insert(insert(tix, at[1], NA), at[2], NA)}
  
}
Acerto.tema3 <- data.frame(Acerto.tema3)
rownames(Acerto.tema3) <- colnames(mcmc.theta[[3]])
colnames(Acerto.tema3) <- substring(names(tix), 4, nchar(names(tix))-7)

return(Acerto.tema3)
}


# Cálculo das componentes do desvio
c.d <- function(){
  for (aluno in 1:nrow(Acerto.tema)){
  for (tema in 1:30){
    if(is.na(Acerto.tema[aluno,tema])){cd[aluno,tema] <- NA}
    else if(Acerto.tema[aluno,tema]==1){cd[aluno,tema] <- -2*abs(log(Prob.acerto.tema[aluno,tema]))^.5}  
    else if(Acerto.tema[aluno,tema]==0){cd[aluno,tema] <- -2*abs(log(1-Prob.acerto.tema[aluno,tema]))^.5}
  }
}
cd <- data.frame(cd)
rownames(cd) <- rownames(Acerto.tema)
colnames(cd) <- colnames(Acerto.tema)

return(cd)
}


# Correlações entre os temas
corr.tema <- function(){for (tema in 1:30){
  for (tema2 in 1:30){
    cor.tema[tema,tema2] <- 
      cor(cd[complete.cases(cd[,c(tema,tema2)]),tema], cd[complete.cases(cd[,c(tema,tema2)]),tema2])
  }
}
cor.tema <- data.frame(cor.tema)
rownames(cor.tema) <- colnames(Acerto.tema)
colnames(cor.tema) <- colnames(Acerto.tema)

return(cor.tema)
}

# Array final para o balanceamento das provas, com o número de simulações, turmas, questões,
#provas e vetor com a probabilidade de acerto e o resultado da binomial se acertou ou não:

balanceamento <- function(){for(questao in 1:n.questoes.prova) {
  for(turma in 1:n.turmas) {
    for(prova in 1:n.provas) {
      nome.questao <- mapa.questoes[turma, questao, prova]
      if(nome.questao %in% dimnames(respostas.dico[[prova]])[[2]]) {
        dados.balanceamento[, turma, questao,prova, 1] <- as.numeric(Pm.probs[[prova]][, nome.questao])
      }
    }
  }
}
return(dados.balanceamento)
}

# Array final 
dados.fim <- function(){for(prova in 1:n.provas) {
  dados.finais[[prova]] <- array(dim=c(ncol(mcmc.theta[[prova]]), n.turmas, 
                                       n.questoes.prova, 3),
                                 dimnames = list(colnames(mcmc.theta[[prova]]),
                                                 dimnames(mapa.questoes)[[1]],
                                                 dimnames(mapa.questoes)[[2]],
                                                 c("Prob.acerto", "Acerto", "Residuo")))
  for(turma in 1:n.turmas) {
    for(questao in 1:n.questoes.prova) {
      nome.questao <- mapa.questoes[turma, questao, prova]
      if(nome.questao %in% dimnames(respostas.dico[[prova]])[[2]]) {
        (dados.finais[[prova]][, turma, questao, 1] <-
           as.numeric(P.probs[[prova]][,nome.questao]))}
    }
    dados.finais[[prova]][,,,2] <-
      rbinom(ncol(mcmc.theta[[prova]]), 1, c(dados.finais[[prova]][,,,1]))
    dados.finais[[prova]][,,,3] <- 
      abs(dados.finais[[prova]][,,,2] - dados.finais[[prova]][,,,1])
  }
}
  return(dados.finais)
}


# Probabilidade de um aluno mediano passar por prova
prob.aluno.mediano.passar <- function(){for (prova in 1:n.provas){
  for (turma in 1:n.turmas){
    P.am.passar[turma,prova] <- sum(soma.acerto[,turma,prova]>=5)/(nchains*(niter/2))
  }
}
  return(P.am.passar)
} 


# Notas dos alunos por prova
nota.prova <- function(){for (prova in 1:n.provas) {
  notaaluno[[prova]] <- aggregate(dados.original$Nota_prova[dados.original$Numero.prova==prova],list(dados.original$Matricula[dados.original$Numero.prova==prova]),mean) 
  colnames(notaaluno[[prova]]) <- c("Matricula", "Nota")
}
  return(notaaluno)
}

# Notas estimadas por TRI de cada aluno por prova
notas.estim <- function(){for (prova in 1:n.provas){
  theta2 <- mean(notaaluno[[prova]][,2]) + sd(notaaluno[[prova]][,2])*mcmc.theta[[prova]][100,]
  
  thetaajust <- theta2-min(theta2)
  
  notas.estimadas[[prova]] <- array(dim = c(length(respostas.dico[[prova]]$matricula),2), dimnames = list(1:length(respostas.dico[[prova]]$matricula),c("Matricula","Nota Estimada")))
  
  notas.estimadas[[prova]][,1] <- respostas.dico[[prova]]$matricula
  notas.estimadas[[prova]][,2] <- c(thetaajust*10/max(thetaajust))
}
  return(notas.estimadas)
}

# Dataframes dos alunos que não passariam e passariam por TRI, em cada prova
n.passou.hat <- function(){for (prova in 1:n.provas) {
  N.passou.hat[[prova]] <- array(dim = c(length(notas.estimadas[[prova]][notas.estimadas[[prova]]>=0 & notas.estimadas[[prova]]<5]),1), dimnames = list(as.numeric(notas.estimadas[[prova]][,1][notas.estimadas[[prova]][,2]>=0 & notas.estimadas[[prova]][,2]<5]),"Nota Estimada"))
  N.passou.hat[[prova]][,1] <- notas.estimadas[[prova]][notas.estimadas[[prova]]>=0 & notas.estimadas[[prova]]<5]
}
  return(N.passou.hat)
}

passou.hat <- function(){
  for (prova in 1:n.provas) {  
  Passou.hat[[prova]] <- array(dim = c(length(notas.estimadas[[prova]][,1][notas.estimadas[[prova]][,2]>=5 ]),1), dimnames = list(as.numeric(notas.estimadas[[prova]][,1][notas.estimadas[[prova]][,2]>=5]),"Nota Estimada"))
  Passou.hat[[prova]][,1] <- notas.estimadas[[prova]][,2][notas.estimadas[[prova]][,2]>=5]
}
  return(Passou.hat)
}


# Dataframes dos alunos que não passaram e passaram, em cada prova 

n.passou <- function(){for (prova in 1:n.provas) {
  N.passou[[prova]] <- array(dim = c(length(notaaluno[[prova]][notaaluno[[prova]]>=0 & notaaluno[[prova]]<5]),1), dimnames = list(as.numeric(notaaluno[[prova]][,1][notaaluno[[prova]][,2]>=0 & notaaluno[[prova]][,2]<5]),"Nota"))
  
  N.passou[[prova]][,1] <- notaaluno[[prova]][notaaluno[[prova]]>=0 & notaaluno[[prova]]<5]
}
 return(N.passou)
}

passou <- function(){for (prova in 1:n.provas) {
  Passou[[prova]] <- array(dim = c(length(notaaluno[[prova]][,1][notaaluno[[prova]][,2]>=5 ]),1), dimnames = list(as.numeric(notaaluno[[prova]][,1][notaaluno[[prova]][,2]>=5]),"Nota"))
  
  Passou[[prova]][,1] <- notaaluno[[prova]][,2][notaaluno[[prova]][,2]>=5]
}
return(Passou)
}


# Dataframe com as probabilidades das simulações com theta mediano passar em PE em cada turma
Sim.passar <- function(){for (t in 1:n.turmas){
  matriz.notas <- soma.acerto[,t,]
  descartada.sim <- pior.prova <- max.col(-matriz.notas, ties.method="last") 
  
  condicao <- (pior.prova %in% 1:2) & 
    ((matriz.notas[, 4] - matriz.notas[, 3])*4 > (matriz.notas[, 4] - matriz.notas[, pior.prova])*3)
  descartada[condicao] <- 3  
  
  matriz.notas <- cbind(matriz.notas, descartada=descartada)
  
  Nota_final <- numeric(nrow(nota.prova))
  
  for(aluno in 1:nrow(nota.prova)) {
    if (matriz.notas[aluno, 5] %in% 1:2) pesos <- c(3,4,3)/10
    else pesos <- c(3,3,4)/10
    Nota_final[aluno] <- matriz.notas[aluno, -c(matriz.notas[aluno, 5], 5)] %*% pesos
  }
  for (simulacao in 1:(nchains*(niter/2))){
    sim.passar[simulacao,t] <- 
      (sum(soma.acerto[simulacao,t,])-
         min(soma.acerto[simulacao,t,]))/(n.provas-1)
  }
}
  return(sim.passar)
}

# Probabilidade de um aluno mediano passar em PE por turma
p.sim.passar <- function(){for (turma in 1:n.turmas){
  P.sim.passar[,turma] <- sum(sim.passar[,turma]>=5)/(nchains*(niter/2))
}
return(P.sim.passar)
}


### GRÁFICOS ###
g_notas_curso <- function(){ggplot(dados.original1, aes(factor(Curso, level = medcurso[,1]), Nota_prova)) + 
    geom_jitter(position=position_jitter(0.2), alpha=.4, aes(color=Grupo)) +
    stat_summary(aes(y = Nota_prova, group=1), fun.y=mean, colour="black", geom="line",group=1) +
    ylab("Notas") + xlab("Curso") + coord_flip() + theme_classic()}

g_media_provas <- function(){ggplot(medpturma, aes(Prova, Media, color=Turma, group=Turma)) + 
    geom_point() + geom_line() + ylim(0,10) + ylab("Média") +
    scale_color_manual(values=c(rep("grey",10),'black')) +
    theme_classic() + theme(legend.position = "none")
}

g_radar1 <- function(){radarchart(data[[1]],  axistype=0, pcol=c(rep("azure4",10), 'black'), 
                                  plwd=1, plty=1, cglcol="beige", cglty=1, 
                                  caxislabels=seq(0,55,5), axislabcol="grey", cglwd=0.3, 
                                  vlcex=0.8)
}
g_radar2 <- function(){radarchart(data[[2]],  axistype=0, pcol=c(rep("grey",10), 'black'), 
                                  plwd=1, plty=1, cglcol="beige", cglty=1, 
                                  caxislabels=seq(0,55,5), axislabcol="grey", cglwd=0.3, 
                                  vlcex=0.8)
}
g_radar3 <- function(){radarchart(data[[3]],  axistype=0, pcol=c(rep("grey",10), 'black'), 
                                  plwd=1, plty=1, cglcol="beige", cglty=1, 
                                  caxislabels=seq(0,55,5), axislabcol="grey", cglwd=0.3, 
                                  vlcex=0.8)
}

g_fluxo_sankey <- function(){
  require(networkD3)
  sankeyNetwork(Links = links, Nodes = nodes,
                                Source = "IDsource", Target = "IDtarget",
                                Value = "value", NodeID = "name2", 
                                LinkGroup = 'energy_type', colourScale = JS(
                                  sprintf(
                                    'd3.scaleOrdinal() .domain(%s)
                                    .range(%s)',
                                    jsonlite::toJSON(color_scale$domain),
                                    jsonlite::toJSON(color_scale$range)
                                  )
                                ), fontSize = 15, iteration=0)}

g_confusao_subs <- function(){tabela1 %>% 
  ggplot(aes(tabela1$MencaosemP4, tabela1$MencaoposP4)) + xlab("Menção sem P4") + ylab("Menção pós P4") +
  geom_tile(aes(fill=Quantidade)) +
  scale_fill_gradient(low="white", high="red") +
  scale_x_continuous(breaks=0:4, expand=c(0, 0), labels=c("II","MI","MM","MS","SS")) +
  scale_y_continuous(breaks=0:4, expand=c(0, 0), labels=c("II","MI","MM","MS","SS")) +
  geom_rect(aes(xmin=-.5, xmax=4.5, ymin=-.5, ymax=4.5), 
            color="red", alpha=0, size=1)}

g_prob_mediano_passar_prova <- function(){ggplot(data.g, aes(Turma, Probabilidade, color=Prova, group=Prova)) + 
  geom_point() + geom_line() + ggtitle("Probabilidade de um aluno mediano passar por prova e por turma") +
  ylim(0,1) + theme_light()}

g_prob_mediano_passar <- function(){ggplot(P.sim.passar, aes(Turma, Probabilidade, group=Linha)) + geom_point() + geom_line() + ggtitle("Probabilidade de um aluno mediano passar em PE por turma") + ylim(0,1) +
  theme_light()}

g_correlograma <- function(){ggcorr(adjm)}

g_rede_associacao <- function(){
  par(bg="white", mar=c(0,0,0,0))
  plot(network, 
       vertex.size=9,
       vertex.color=my_color, 
       vertex.label.cex=0.7,
       vertex.label.color="black",
       vertex.frame.color="black"
  )
  
  #text(0.1,1,"Rede de associação dos temas",col="black", cex=1.5)
  legend(x=.8, y=.9, 
         legend=paste("Prova", levels(as.factor(cluster$cluster))), 
         col = c("green", "blue", "red") , 
         bty = "n", pch=20 , pt.cex = 2, cex = 1,
         text.col="black" , horiz = F)
}

g_confusao_tri <- function(){tabela %>% 
  ggplot(aes(Classico, TRI)) +
  geom_tile(aes(fill=Quantidade)) +
  scale_fill_gradient(low="white", high="red") +
  scale_x_continuous(breaks=0:4, expand=c(0, 0), labels=c("II","MI","MM","MS","SS")) +
  scale_y_continuous(breaks=0:4, expand=c(0, 0), labels=c("II","MI","MM","MS","SS")) +
  geom_rect(aes(xmin=-.5, xmax=4.5, ymin=-.5, ymax=4.5), 
            color="red", alpha=0, size=1)}

g_Pm.probs.means <- function(){
  ggplot(Pm.probs.means,aes(x=Prob,y=reorder(tema,Prob,mean)))+geom_line()+
    geom_point(aes(color=Prob),show.legend = FALSE)+
    scale_x_continuous(limits = c(0,1),breaks = seq(0,1,0.2))+
    facet_grid(prova ~ ., scales="free_y", space="free_y")+
    scale_colour_gradient(limits=c(0,1),low="red",high="green")+
    xlab(aes(label=" "))+
    ylab(aes(label=" "))+
    theme_light()
}

P.acertar.probit <- function(a,b,c,habilidade) {
  c+(1-c)*pnorm(a*habilidade-b)
}

P.acertar.logit <- function(a,b,c,habilidade){
  c+(1-c)*(1/(1+exp( -(a*habilidade-b) )))
  
}


cci_p1 <- function() {
  ggplot(subset(cci,prova==1),aes(x=habilidade,y=prob,color=questao))+geom_line()+
    facet_wrap(.~tema,ncol = 2)+
    scale_y_continuous(limits = c(0,1))+
    scale_x_continuous(limits = c(-4,4))+
    xlab(aes(label="habilidade"))+
    ylab(aes(label="Probabilidade de acerto"))+
    theme_light()
  

}

cci_p2 <- function() {
  ggplot(subset(cci,prova==2),aes(x=habilidade,y=prob,color=questao))+geom_line()+
    facet_wrap(.~tema,ncol = 2)+
    scale_y_continuous(limits = c(0,1))+
    scale_x_continuous(limits = c(-4,4))+
    xlab(aes(label="habilidade"))+
    ylab(aes(label="Probabilidade de acerto"))+
    theme_light()
  
}
cci_p3 <- function() {
  ggplot(subset(cci,prova==3),aes(x=habilidade,y=prob,color=questao))+geom_line()+
    facet_wrap(.~tema,ncol = 2)+
    scale_y_continuous(limits = c(0,1))+
    scale_x_continuous(limits = c(-4,4))+
    xlab(aes(label="habilidade"))+
    ylab(aes(label="Probabilidade de acerto"))+
    theme_light()
  
}


