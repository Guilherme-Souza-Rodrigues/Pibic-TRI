# Lendo os dados das provas e carregando os pacotes:
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "dplyr", "tidyr", "reshape2", "irtoys", "ltm", "mirt",
               "bairt", "ggplot2", "R.utils", "igraph", "factoextra", "threejs", "GGally",
               "pander", "rstan", "fmsb", "tibble", "stringr", "antaresViz", "networkD3", "knitr",
               "kableExtra", "corrr", "antaresViz", "RColorBrewer")

P.acertar.probit <- function(a,b,c,habilidade) {
  c+(1-c)*pnorm(a*habilidade-b)
}

fii_cord <- function(a, c, prob) {
  a^(2)*((1-prob)/prob)*((prob-c)/(1-c))^(2)
}


P.acertar.logit <- function(a,b,c,habilidade){
  c+(1-c)*(1/(1+exp(-(a*habilidade-b))))
}

# Criando o mapa de questões
mapa <- function() {
  mapa.questoes <- array(dim=c(n.turmas, n.questoes.prova, n.provas), 
                         dimnames=list(unique(dados.original$Turma)[order(unique(dados.original$Turma))], 
                                       paste("Questao", 1:n.questoes.prova), 
                                       paste("Prova", 1:n.provas)))
  for(prova in 1:n.provas) {
    mapa.questoes[,,prova] <- dplyr::filter(dados.original, Numero.prova==prova) %>%
      dplyr::select(Turma, Questao, Nome.questao) %>% 
      unique() %>% 
      dcast(Turma ~ Questao, value.var="Nome.questao") %>%  
      as.matrix() %>% 
      .[, -1]
  }
  return(mapa.questoes)
}

# Criando dataframe com as respostas dicotômicas de cada aluno:
dico <- function(){
  respostas.dico <- vector(n.provas, mode="list")
  for(prova in 1:n.provas) {
    data <- dados.original[order(dados.original$Matricula), ] %>%
      dplyr::filter(Numero.prova==prova) %>%
      droplevels()
    matricula <- data$Matricula
    acerto <- as.numeric(data$Acertou)
    questao <- data$Nome.questao
    turma <- data$Turma
    frame <- data.frame(matricula, acerto, questao, turma) %>% 
      dplyr::filter(., complete.cases(.))
    respostas.dico[[prova]] <- reshape(frame, v.names="acerto", idvar="matricula", timevar="questao", 
                                       direction="wide")
    colnames(respostas.dico[[prova]]) <- gsub("^acerto.*?.", "", colnames(respostas.dico[[prova]]))
  }
  return(respostas.dico)
}


# Cálculo dos estimadores dos parâmetros dos itens e do estimador do parâmetro de habilidade de
# cada aluno via Monte Carlo Cadeia de Markov:
mcmc <- function() {
  model <- stan_model(model_code = 
                        "data {
                      int<lower=1> N_alunos;              // number of students
                      int<lower=1> N_itens;              // number of questions
                      int<lower=1> N;                   // number of observations
                      int<lower=1, upper=N_alunos> aluno[N];   // student for observation n
                      int<lower=1, upper=N_itens> item[N];   // question for observation n
                      int<lower=0, upper=1> y[N];     // correctness for observation n
}

parameters{
vector[N_itens] b;
vector<lower=0, upper=1> [N_itens] c;
vector[N_alunos] theta;
vector<lower=0> [N_itens] a;
}

model{
a ~ normal(0,1);
b ~ normal(0,1);
c ~ beta(5,17);
theta ~ normal(0,1);
y ~ bernoulli(c[item]+((1-c[item]).*inv_logit((a[item].*theta[aluno])-b[item])));
}"
)
  
  banco_respostas <- dados.original %>%
    dplyr::select(Matricula, Nome.questao, Acertou, Numero.prova) %>%
    mutate(Nome.questao=as.character(Nome.questao),
           Matricula=as.numeric(Matricula))
  
  mcmc.itens <- vector(n.provas, mode="list")
  mcmc.theta <- vector(n.provas, mode="list")
  
  for(prova in 1:n.provas) {
    dados_aux <- banco_respostas %>%
      filter(Numero.prova==prova) %>%
      na.omit(Acertou) %>%
      mutate(Matricula.mod=as.numeric(as.factor(Matricula)),
             Nome.questao.mod=as.numeric(as.factor(Nome.questao))) %>%
      arrange(Matricula.mod, Nome.questao.mod)
    
    Matriculas_equivalencia_aux <- dados_aux %>%
      dplyr::select(Matricula, Matricula.mod) %>%
      distinct() %>%
      arrange(Matricula.mod)
    
    Questoes_equivalencia_aux <- dados_aux %>%
      dplyr::select(Nome.questao, Nome.questao.mod) %>%
      distinct() %>%
      arrange(Nome.questao.mod)
    
    fit.aux <- sampling(object=model,
                        data=list(N_alunos=length(unique(dados_aux$Matricula.mod)),
                                  N_itens=length(unique(dados_aux$Nome.questao.mod)),
                                  N=nrow(dados_aux),
                                  aluno=dados_aux$Matricula.mod,
                                  item=dados_aux$Nome.questao.mod, 
                                  y=dados_aux$Acertou),
                        iter=n.mcmc, cores=4, control=list(max_treedepth=15))
    
    params.aux <- rstan::extract(fit.aux)
    itens.aux <- array(c(params.aux$a, params.aux$b, params.aux$c),
                       dim=c(n.mcmc*4/2, nrow(Questoes_equivalencia_aux), 3),
                       dimnames=list(paste("simulação", 1:(n.mcmc*4/2)),
                                       Questoes_equivalencia_aux$Nome.questao,
                                       c('a', 'b', 'c')))
    mcmc.itens[[prova]] <- itens.aux
    
    thetas.mcmc.aux <- params.aux$theta
    colnames(thetas.mcmc.aux) <- Matriculas_equivalencia_aux$Matricula
    mcmc.theta[[prova]] <- thetas.mcmc.aux
    
    rm(Questoes_equivalencia_aux, Matriculas_equivalencia_aux, dados_aux, fit.aux)
  }
  
  names(mcmc.itens) <- paste0("itens.p", 1:n.provas)
  names(mcmc.theta) <- paste0("thetas.mcmc.p", 1:n.provas)
  return(list(thetas=mcmc.theta, itens=mcmc.itens))
}



# Dataframe dos coeficientes estimados de cada questao
data.estimados <- function(){
  itens.p <- vector(n.provas, mode="list")
  for (prova in 1:n.provas) {
    n.questoes.sorteadas <- ncol(mcmc.itens[[prova]][,,1])
    itens.p[[prova]] <- matrix(0, nrow=n.questoes.sorteadas, ncol=3)
    for (parametro in 1:3){
      for (item in 1:n.questoes.sorteadas){
        itens.p[[prova]][item, parametro] <- mean(mcmc.itens[[prova]][, item, parametro])
      }
    }
    itens.p[[prova]] <- data.frame(itens.p[[prova]])
    rownames(itens.p[[prova]]) <- colnames(mcmc.itens[[prova]][,,1])
    colnames(itens.p[[prova]]) <- letters[1:3]
  }
  names(itens.p) <- paste("Prova", 1:4)
  save(itens.p, file="Parametros_Itens_Prova.RData")
  return(itens.p)
}

# Função para arredondar para a terceira casa
round3 <- function(x) {
  round(x, 3)
}


# Simulação com as probabilidades de que um aluno mediano acerte a questão para cada questão 
#selecionada em cada prova
sim.aluno.medio <- function(){
  for(prova in 1:n.provas){
    Pm.probs[[prova]] <- array(c(P.acertar.logit(a=0,
                                                 b=as.numeric(mcmc.itens[[prova]][,,2]),
                                                 c=as.numeric(mcmc.itens[[prova]][,,3]),
                                                 habilidade=0)),
                               dim=c(n.mcmc*4/2, ncol(respostas.dico[[prova]])-2), 
                               dimnames=list(paste("Simulacao", 1:(n.mcmc*4/2)),
                                             colnames(mcmc.itens[[prova]])))
  } 
  return(Pm.probs)
}



# Probabilidade de acerto de cada questão feita por cada aluno
prob.acerto.questoes <- function(){
  for (prova in 1:n.provas){
    P.probs[[prova]] <- matrix(0, nrow = nrow(respostas.dico[[prova]]), ncol=ncol(respostas.dico[[prova]][, -c(1,2)]))
    for (aluno in 1:nrow(respostas.dico[[prova]])){
      
      P.probs[[prova]][aluno,] <- c(P.acertar.logit(a=itens.p[[prova]][,1],
                                                    b=itens.p[[prova]][,2],
                                                    c=itens.p[[prova]][,3],
                                                    habilidade=colMeans(mcmc.theta[[prova]])[aluno]))
      
      P.probs[[prova]] <- data.frame(P.probs[[prova]])
      rownames(P.probs[[prova]]) <- colnames(mcmc.theta[[prova]])
      colnames(P.probs[[prova]]) <- rownames(itens.p[[prova]])
    }
  }
  return(P.probs)
}




# Dataframes de cada prova com a probabilidade de acerto por tema (questão que o aluno fez do tema) para cada aluno 
prob.tema <- function(numero.prova){
  Prob.tema <- matrix(0, nrow=ncol(mcmc.theta[[numero.prova]]), ncol=n.questoes.prova)
  for (aluno in 1:ncol(mcmc.theta[[numero.prova]])){
    Prob.tema <- data.frame(Prob.tema)
    
    turma <- dados.original %>% 
      dplyr::filter(Numero.prova==numero.prova, 
                    Matricula==colnames(mcmc.theta[[numero.prova]])[aluno]) %>% 
      dplyr::select(Turma) %>% unique()
    turma <- turma[1,1]
    
    questoes <- dados.original %>% 
      dplyr::filter(Numero.prova==numero.prova, 
                    Matricula==colnames(mcmc.theta[[numero.prova]])[aluno]) %>% 
      dplyr::select(Nome.questao, Acertou) %>% unique()
    
    questoes <- as.character(questoes[is.na(questoes$Acertou)==FALSE,1])
    
    p <- P.probs[[numero.prova]][aluno, questoes]
    
    a = p[order(names(p))]
    
    at <- which(!(((numero.prova-1)*10 + 1:10) %in% as.numeric(substring(names(a), 1,2))))
    
    if(length(at)==0) {Prob.tema[aluno, ] <- a} 
    else if(length(at)==1){Prob.tema[aluno, ] <- insert(unlist(c(a)), at, NA)}
    else if(length(at)==2){Prob.tema[aluno, ] <- insert(insert(unlist(c(a)), at[1], NA), at[2], NA)}
  }
  rownames(Prob.tema) <- colnames(mcmc.theta[[numero.prova]])
  colnames(Prob.tema) <- substring(names(a), 4, nchar(names(a))-7)
  return(Prob.tema)
}


# Dataframe com os acertos de cada aluno em cada tema
acertou.tema <- function(numero.prova){
  Acerto.tema <- matrix(0, nrow=ncol(mcmc.theta[[numero.prova]]), ncol=n.questoes.prova)
  for (aluno in 1:ncol(mcmc.theta[[numero.prova]])){
    tx <- dados.original %>% 
      dplyr::filter(Numero.prova==numero.prova, 
                    Matricula==as.numeric(colnames(mcmc.theta[[numero.prova]])[aluno])) %>% 
      dplyr::select(Acertou, Matricula, Tema, Nome.questao)
    tix <- tx[,1]
    names(tix) <- tx[,4]
    tix <- tix[order(names(tix))]
    at <- which(!(((numero.prova-1)*10 + 1:10) %in% as.numeric(substring(names(tix), 1,2))))
    
    if(length(at)==0) {Acerto.tema[aluno,] <- tix} 
    else if(length(at)==1){Acerto.tema[aluno, ] <- insert(tix, at, NA)}
    else if(length(at)==2){Acerto.tema[aluno, ] <- insert(insert(tix, at[1], NA), at[2], NA)}
    
  }
  Acerto.tema <- data.frame(Acerto.tema)
  rownames(Acerto.tema) <- colnames(mcmc.theta[[numero.prova]])
  colnames(Acerto.tema) <- substring(names(tix), 4, nchar(names(tix))-7)
  
  return(Acerto.tema)
}


# Cálculo das componentes do desvio
c.d <- function(){
  for (aluno in 1:nrow(Acerto.tema)){
    for (tema in 1:30){
      if(is.na(Acerto.tema[aluno,tema])){cd[aluno,tema] <- NA}
      else if(Acerto.tema[aluno,tema]==1){cd[aluno,tema] <- -2*log(Prob.acerto.tema[aluno,tema])}
      else if(Acerto.tema[aluno,tema]==0){cd[aluno,tema] <- -2*log(1-Prob.acerto.tema[aluno,tema])}
    }
  }
  # cd <- abs(Acerto.tema - Prob.acerto.tema)
  cd <- data.frame(cd)
  rownames(cd) <- rownames(Acerto.tema)
  colnames(cd) <- colnames(Acerto.tema)
  return(cd)
}


# Array final para o balanceamento das provas, com o número de simulações, turmas, questões,
#provas e vetor com a probabilidade de acerto e o resultado da binomial se acertou ou não:
balanceamento <- function(){
  dados.balanceamento <- array(dim=c(n.mcmc*4/2, n.turmas, n.questoes.prova, n.provas, 2),
                               dimnames = list(dimnames(mcmc.itens[[1]])[[1]],
                                               dimnames(mapa.questoes)[[1]],
                                               dimnames(mapa.questoes)[[2]],
                                               dimnames(mapa.questoes)[[3]],
                                               c("prob.acerto", "sim.acerto")))
  for(questao in 1:n.questoes.prova) {
    for(turma in 1:n.turmas) {
      for(prova in 1:n.provas) {
        nome.questao <- mapa.questoes[turma, questao, prova]
        if(nome.questao %in% dimnames(respostas.dico[[prova]])[[2]]) {
          dados.balanceamento[, turma, questao,prova, 1] <- as.numeric(Pm.probs[[prova]][, nome.questao])
          dados.balanceamento[, turma, questao, prova, 2] <- 
            rbinom(n.mcmc*4/2, 1, dados.balanceamento[, turma, questao, prova, 1])
        }
      }
    }
  }
  
  # Trocando os NA's das quesões anuladas por pontos nas simulações
  dados.balanceamento[, , , , 2][is.na(dados.balanceamento[, , , , 2])] <- 1
  return(dados.balanceamento)
}

# # Array final 
# dados.fim <- function() {
#   for(prova in 1:n.provas) {
#     dados.finais[[prova]] <- array(dim=c(ncol(mcmc.theta[[prova]]), n.turmas, 
#                                          n.questoes.prova, 3),
#                                    dimnames = list(colnames(mcmc.theta[[prova]]),
#                                                    dimnames(mapa.questoes)[[1]],
#                                                    dimnames(mapa.questoes)[[2]],
#                                                    c("Prob.acerto", "Acerto", "Residuo")))
#     for(turma in 1:n.turmas) {
#       for(questao in 1:n.questoes.prova) {
#         nome.questao <- mapa.questoes[turma, questao, prova]
#         if(nome.questao %in% dimnames(respostas.dico[[prova]])[[2]]) {
#           (dados.finais[[prova]][, turma, questao, 1] <-
#              as.numeric(P.probs[[prova]][,nome.questao]))}
#       }
#       dados.finais[[prova]][,,,2] <-
#         rbinom(ncol(mcmc.theta[[prova]]), 1, c(dados.finais[[prova]][,,,1]))
#       dados.finais[[prova]][,,,3] <- 
#         abs(dados.finais[[prova]][,,,2] - dados.finais[[prova]][,,,1])
#     }
#   }
#   return(dados.finais)
# }


# Probabilidade de um aluno mediano passar por prova
nota.esperada.mediano <- function(){
  NE.mediano <- array(dim = c(n.turmas, n.provas+1), 
                       dimnames=list(dimnames(mapa.questoes)[[1]], 
                                     c(dimnames(mapa.questoes)[[3]],"Turma")))
  for (prova in 1:n.provas) {
    for (turma in 1:n.turmas){
      # P.am.passar[turma,prova] <- mean(soma.acerto[,turma,prova]>=5)
      NE.mediano[turma,prova] <- mean(soma.acerto[, turma,prova])
    }
  }
  return(NE.mediano)
} 


# Notas dos alunos por prova
nota.prova <- function() {
  for (prova in 1:n.provas) {
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
n.passou.hat <- function(){
  for (prova in 1:n.provas) {
    N.passou.hat[[prova]] <- array(dim = c(length(notas.estimadas[[prova]][notas.estimadas[[prova]]>=0 & notas.estimadas[[prova]]<5]),1), dimnames = list(as.numeric(notas.estimadas[[prova]][,1][notas.estimadas[[prova]][,2]>=0 & notas.estimadas[[prova]][,2]<5]),"Nota Estimada"))
    N.passou.hat[[prova]][,1] <- notas.estimadas[[prova]][notas.estimadas[[prova]]>=0 & notas.estimadas[[prova]]<5]
  }
  return(N.passou.hat)
}

passou.hat <- function(){
  for (prova in 1:n.provas) {  
    Passou.hat[[prova]] <- array(dim = c(length(notas.estimadas[[prova]][,1][notas.estimadas[[prova]][,2]>=5 ]),1), 
                                 dimnames = list(as.numeric(notas.estimadas[[prova]][,1][notas.estimadas[[prova]][,2]>=5]),
                                                 "Nota Estimada"))
    Passou.hat[[prova]][,1] <- notas.estimadas[[prova]][,2][notas.estimadas[[prova]][,2]>=5]
  }
  return(Passou.hat)
}


# Dataframes dos alunos que não passaram e passaram, em cada prova 

n.passou <- function(){
  for (prova in 1:n.provas) {
    N.passou[[prova]] <- array(dim = c(length(notaaluno[[prova]][notaaluno[[prova]]>=0 & notaaluno[[prova]]<5]),1), dimnames = list(as.numeric(notaaluno[[prova]][,1][notaaluno[[prova]][,2]>=0 & notaaluno[[prova]][,2]<5]),"Nota"))
    
    N.passou[[prova]][,1] <- notaaluno[[prova]][notaaluno[[prova]]>=0 & notaaluno[[prova]]<5]
  }
  return(N.passou)
}

passou <- function(){
  for (prova in 1:n.provas) {
    Passou[[prova]] <- array(dim = c(length(notaaluno[[prova]][,1][notaaluno[[prova]][,2]>=5 ]),1), dimnames = list(as.numeric(notaaluno[[prova]][,1][notaaluno[[prova]][,2]>=5]),"Nota"))
    
    Passou[[prova]][,1] <- notaaluno[[prova]][,2][notaaluno[[prova]][,2]>=5]
  }
  return(Passou)
}


# Dataframe com as probabilidades das simulações com theta mediano passar em PE em cada turma
Sim.passar <- function(){
  matriz.notas.sim <- NULL
  descartada.sim <- NULL
  pior.prova.sim <- NULL
  condicao.sim <- NULL
  Nota_final.sim <- NULL
  sim.passar <- array(dim = c(n.mcmc*4/2,n.turmas), dimnames = list(paste("Simulacao", 1:(n.mcmc*4/2)), dimnames(mapa.questoes)[[1]] ))
  
  for (t in 1:n.turmas){
    matriz.notas.sim <- soma.acerto[,t,]
    descartada.sim <- pior.prova.sim <- max.col(-matriz.notas.sim, ties.method="last") 
    
    # condicao.sim <- (pior.prova.sim %in% 1:2) & 
    #   ((matriz.notas.sim[, 4] - matriz.notas.sim[, 3])*4 > (matriz.notas.sim[, 4] - matriz.notas.sim[, pior.prova.sim])*3)
    # descartada.sim[condicao.sim] <- 3  
    
    matriz.notas.sim <- cbind(matriz.notas.sim, descartada=descartada.sim)
    
    Nota_final.sim <- numeric(nrow(nota.prova))
    
    for(aluno in 1:nrow(nota.prova)) {
      if (matriz.notas.sim[aluno, 5] %in% 1:2) pesos <- c(3,4,3)/10
      else pesos <- c(3,3,4)/10
      Nota_final.sim[aluno] <- matriz.notas.sim[aluno, -c(matriz.notas.sim[aluno, 5], 5)] %*% pesos
    }
    for (simulacao in 1:(n.mcmc*4/2)){
      sim.passar[simulacao,t] <- 
        (sum(soma.acerto[simulacao,t,])-
           min(soma.acerto[simulacao,t,]))/(n.provas-1)
    }
  }
  return(sim.passar)
}


# Probabilidade de um aluno mediano passar em PE por turma
Prob.mediano.passar <- function() {
  nota.final.sim <- matrix(NA, dim(soma.acerto)[1], n.turmas)
  rownames(nota.final.sim) <- dimnames(soma.acerto)[[1]]
  colnames(nota.final.sim) <- dimnames(soma.acerto)[[2]]
  for(simulacao in 1:dim(soma.acerto)[1]) {
    pior.prova <- apply(soma.acerto[simulacao, ,], 1, which.min) 
    for(turma in 1:n.turmas) {
      # O cálculo abaixo não corresponde exatamente ao que é feito na disciplia.
      nota.final.sim[simulacao, turma] <- soma.acerto[simulacao, turma, -pior.prova[turma]] %*% c(3, 3, 4)/10
    }
  }
  prob.mediano.passar <- colMeans(nota.final.sim >= 5)
  return(prob.mediano.passar)
}


# Probabilidade de um aluno mediano passar em PE por turma (antigo!)
p.sim.passar <- function(){
  for (turma in 1:n.turmas){
    P.sim.passar[,turma] <- sum(sim.passar[,turma]>=5)/(n.mcmc*4/2)
  }
  return(P.sim.passar)
}



################
### GRÁFICOS ###
################

g_notas_curso <- function(nome="Notas_cursos.png"){
  tmp <- dados.original1 %>%
    left_join(notas.finais) %>%
    dplyr::select(Matricula, Curso, Nota_final, Grupo) %>%
    unique() 
  
  tmp <- tmp %>%
    group_by(Curso) %>%
    summarize(media=mean(Nota_final)) %>%
    left_join(tmp, .) %>%
    arrange(media) %>%
    filter(complete.cases(.))
  
  tmp2 <- tmp %>%
    dplyr::select(Curso, media)  %>%
    unique()
  
    tmp %>%
    mutate(Curso=factor(Curso, levels=tmp2[,1])) %>%
    ggplot(aes(Curso, Nota_final, group=Curso)) + 
    geom_hline(yintercept=c(0, 3, 5, 7, 9, 10), col=gray(.9), size=.3, linetype=1) +
    geom_jitter(position=position_jitter(.2, 0), alpha=.4, aes(color=Grupo), size=3) +
    stat_summary(fun.y=mean, colour="black", geom="line", size=1, alpha=.8, group=1) +
    scale_y_continuous(limits=c(0, 10), 
                       breaks=c(0, 3, 5, 7, 9, 10),
                       expand=c(.05, .05)) +
    ylab("Notas") + 
    xlab("Curso") + 
    coord_flip() +
    theme(axis.text.y=element_text(size=10, color="black"),
          axis.text.x=element_text(size=10, color="black"),
          legend.title=element_text(size=10),
          legend.text=element_text(size=10),
          legend.margin=margin(l=20, unit='pt')
    )
    ggsave(filename="Notas_cursos.png", device = NULL, width=14, height=7)
}


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100, alpha=.8)[1:n]
}


g_media_provas <- function() {
  medpturma %>%
    filter(Turma != "Media Geral") %>%
    ggplot(aes(as.numeric(Prova), Media, color=Turma, group=Turma)) + 
    # geom_point() + 
    geom_vline(xintercept=1:4, col=gray(.9), size=.3, linetype=1) +
    geom_hline(yintercept=c(0, 3, 5, 7, 9, 10), col=gray(.9), size=.3, linetype=1) +
    geom_line(size=1, lineend="round", linejoin = "round") + 
    # scale_y_continuous(limits=c(0, 10), breaks=seq(0, 10, 2)) +
    scale_y_continuous(limits=c(0, 10), 
                       breaks=c(0, 3, 5, 7, 9, 10), 
                       expand=c(0, 0)) +
    scale_x_continuous(limits=c(1, 4), expand=c(0, 0)) +
    ylab("Média") +
    xlab("Prova") +
    scale_color_manual(values=c(gg_color_hue(n.turmas),'black'))
  ggsave(filename="Media_provas.png", device = NULL, width=8, height=6)
}


g_radar <- function(numero.prova) {
  png(filename = paste0("g_radar_", numero.prova, ".png"),
      bg = "transparent", width=3200, height=2400)
  radarchart(data[[numero.prova]][-nrow(data[[numero.prova]]), ], 
             axistype=0, 
             pcol=c(gg_color_hue(n.turmas), 'black'), 
             plwd=10, 
             plty=1, 
             pty=32, 
             centerzero=T,
             cglcol=gray(.9), 
             cglty=1, 
             caxislabels=seq(0,55,5), 
             axislabcol="black", 
             cglwd=4, 
             # na.itp=F,
             vlcex=6)
  dev.off()
}


g_confusao_subs <- function() {
  cores <- rep("white", nrow(tabela1))
  cores[tabela1$Quantidade>0] <- "black"
  tabela1 %>% 
    ggplot(aes(MencaosemP4, MencaoposP4)) + 
    xlab("Menção antes da Prova substitutiva") + 
    ylab("Menção após a Prova substitutiva") +
    geom_tile(aes(fill=Quantidade)) +
    geom_text(aes(label=Quantidade), colour=cores) +
    scale_fill_gradient(low="white", high=gg_color_hue(2)[1]) +
    scale_x_continuous(breaks=1:5, expand=c(0, 0), labels=c("II","MI","MM","MS","SS")) +
    scale_y_continuous(breaks=1:5, expand=c(0, 0), labels=c("II","MI","MM","MS","SS"))
  ggsave(filename="confusao_subs.pdf", device=NULL, width=7.5, height=7.5)
}


g_porp_p4 <- function() {
  notas.finais %>%
    filter(complete.cases(.)) %>%
    mutate(faixa=cut(`Prova 1`*.3 + `Prova 2`*.3 + `Prova 3`*.4, 
                     seq(0, 10.1, by=1), right=F, include.lowest=T)) %>%
    group_by(faixa) %>%
    summarise(sim=sum(`Prova 4`>0), nao=sum(`Prova 4`==0)) %>%
    melt(value.name = "numero", variable.name="Fizeram") %>% 
    ggplot(aes(faixa, y=numero, fill=Fizeram)) + 
    geom_col() +
    ylab("Número de alunos") + 
    xlab("Faixa de nota") +
    geom_vline(xintercept=c(0, 3, 5, 7, 9, 10)+.5, col=gray(.9), size=.3, linetype=1) +
    scale_y_continuous(expand=expand_scale(mult=c(0, .1))) +
    theme(panel.border=element_blank())
    ggsave(filename="porp_p4.pdf", device = NULL, width=7.5, height=7.5)
}


g_nota_esperada_mediano <- function(){
  ggplot(data.g, aes(Turma, Nota.esperada, color=Prova, group=Prova)) + 
    theme(panel.grid.major=element_line(colour=gray(.95))) +
    geom_line(size=1.2) + 
    scale_y_continuous(limits=c(0, 10), breaks=c(0, 3, 5, 7, 9, 10), expand=c(0, 0)) +
    ylab("Nota esperda")
  ggsave(filename="nota_esperada.pdf", device = NULL, width=7.5, height=7.5)
}

g_prob_mediano_passar <- function(){
  ggplot(P.mediano.passar, aes(Turma, Probabilidade, group=Linha)) + 
    theme(panel.grid.major=element_line(colour=gray(.95))) +
    geom_line(size=1.2) +
    scale_y_continuous(limits=0:1, breaks=seq(0, 1, by=.2), expand=c(0, 0))
}

g_correlograma <- function(){
  ggcorr(data = NULL,
         cor_matrix=cor.tema, 
         # nbreaks=7,
         limits=c(0, 1),
         layout.exp = 8, 
         hjust = 1) +
    theme(panel.border=element_blank())
  ggsave(filename="correlograma.pdf", device=NULL, width=11, height=6)
}


my_color <- c(rep("lightgreen", 10),
              rep("lightblue", 10),
              rep(gg_color_hue(2)[1], 10))


g_rede_associacao <- function(min.cor=0, semente=123) {
  if(!is.null(semente)) set.seed(semente)
  # adjm <- 1*(cor.tema > min.cor)
  adjm <- cor.tema
  adjm[adjm < min.cor] <- 0
  adjm <- round(50*adjm)
  network <- graph_from_adjacency_matrix(adjm, 
                                         weighted=T, diag=F, mode="plus")

  png(filename="rede.png",
      bg="transparent", width=3000, height=2400)
  par(bg="white", mar=c(0,0,0,0))
  plot(network, 
       vertex.size=9,
       vertex.color=my_color, 
       vertex.label.cex=4,
       vertex.label.color="black",
       vertex.frame.color=my_color, 
       edge.width=(E(network)$weight*1),
       edge.color=gray(.95),
       edge.curved=0.4
  )
  legend("topleft", 
         legend=paste("Prova", levels(as.factor(cluster$cluster))), 
         col=unique(my_color), 
         bty="n", pch=19, pt.cex=6, cex=4,
         text.col="black", horiz=F)
  dev.off()
}
  

g_confusao_tri <- function(){
  cores <- rep("white", nrow(tabela))
  cores[tabela$Quantidade>0] <- "black"
  tabela %>% 
    ggplot(aes(Classico, TRI)) +
    geom_tile(aes(fill=Quantidade)) +
    geom_text(aes(label=Quantidade), colour=cores) +
    scale_fill_gradient(low="white", high=gg_color_hue(2)[1]) +
    xlab("Abordagem clássica") + 
    ylab("Abordagem via TRI") +
    scale_x_continuous(breaks=0:4, expand=c(0, 0), labels=c("II","MI","MM","MS","SS")) +
    scale_y_continuous(breaks=0:4, expand=c(0, 0), labels=c("II","MI","MM","MS","SS"))
  ggsave(filename="confusao_TRI.pdf", device=NULL, width=7.5, height=7.5)
}



g_Pm.probs.means <- function() {
  Pm.probs.means$tema <- as.factor(Pm.probs.means$tema) 
  Pm.probs.means$tema <- factor(Pm.probs.means$tema, 
                                levels=rev(ordem.temas$tema))
  Pm.probs.means %>% 
  ggplot(aes(x=Prob, y=tema)) +
    geom_line() +
    geom_point(aes(color=Prob), show.legend=FALSE) +
    scale_x_continuous(limits=c(0,1), breaks=seq(0,1,0.2), expand=c(0,0)) +
    facet_grid(prova ~ ., scales="free_y", space="free_y") +
    scale_colour_gradient(limits=c(0,1), low="red", high="green") +
    xlab(aes(label="Probabilidade")) +
    ylab(aes(label="")) +
    theme(panel.grid.major=element_line(colour=gray(.85)),
          axis.title.y=element_text(margin=unit(rep(.5, 4), "cm"), size=10),
          axis.title.x=element_text(margin=unit(rep(.5, 4), "cm"), size=10),
          axis.text.y=element_text(size=10),
          axis.text.x=element_text(size=10),
          strip.text.x=element_text(size=10),
          strip.text.y=element_text(size=10)
    )
}





cci.plot <- function(numero.prova) {
  cci$tema <- as.factor(cci$tema) 
  cci$tema <- factor(cci$tema, levels=ordem.temas$tema)
  cci %>% 
    subset(prova==numero.prova) %>% 
    ggplot(aes(x=habilidade, y=prob, color=questao))+
    geom_line(aes(linetype=Revisar)) +
    facet_wrap(.~tema, ncol=2) +
    scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2))+
    scale_x_continuous(limits=c(-3,3), expand=c(0, 0), breaks=seq(-3,3,1))+
    scale_color_discrete(name="Questão")+
    xlab(aes(label="habilidade"))+
    ylab(aes(label="Probabilidade de acerto")) +
    theme(panel.grid.major=element_line(colour=gray(.85)),
          axis.title.y=element_text(margin=unit(rep(.5, 4), "cm"), size=10),
          axis.title.x=element_text(margin=unit(rep(.5, 4), "cm"), size=10),
          axis.text.y=element_text(size=8),
          axis.text.x=element_text(size=8),
          legend.title=element_text(size=9),
          legend.text=element_text(size=9),
          strip.text.x=element_text(size=10),
          strip.text.y=element_text(size=10)
    )
}



fii_plot <- function(numero.prova) {
  ggplot(subset(cci, prova==numero.prova),
         aes(x=habilidade,y=fii,color=questao))+
    geom_line()+
    facet_wrap(.~tema,ncol = 2)+
    scale_x_continuous(limits = c(-4,4))+
    xlab(aes(label="habilidade"))+
    ylab(aes(label="informação do item")) +
    theme(panel.grid.major=element_line(colour=gray(.85)),
          axis.title.y=element_text(margin=unit(rep(.5, 4), "cm"), size=10),
          axis.title.x=element_text(margin=unit(rep(.5, 4), "cm"), size=10),
          axis.text.y=element_text(size=10),
          axis.text.x=element_text(size=10),
          strip.text.x=element_text(size=10),
          strip.text.y=element_text(size=10)
    )
}


teste.inf <- function(){
  ggplot(subset(inf.teste.df),
         aes(x=habilidade,y=teste,color=Turma))+
    geom_line()+
    facet_wrap(.~prova,ncol = 3)+
    scale_x_continuous(limits = c(-3,3)) +
    xlab(aes(label="Habilidade"))+
    ylab(aes(label="Informação do teste")) +
    theme(panel.grid.major=element_line(colour=gray(.85)))
    ggsave(filename="teste.inf.pdf", device=NULL, width=15, height=7)
}

tabela.resumo <- function() {
  aux1 <- dados.original1 %>%
    left_join(notas.finais) %>%
    group_by(Turma) %>%
    summarize(Alunos=length(unique(Matricula)),
              Nota_media=round(mean(Nota_prova), 1),
              Aprov=round(mean(Nota_final>=5)*100, 1),
              SS=round(mean(Mencao=="SS", na.rm=T)*100, 1))
  
  aux2 <- dados.original1 %>%
    left_join(notas.finais) %>%
    summarize(Alunos=length(unique(Matricula)),
              Nota_media=mean(Nota_final),
              Aprov=round(mean(Nota_final>=5)*100, 1),
              SS=round(mean(Mencao=="SS", na.rm=T)*100, 1))
  
  tabela_resumo <- bind_rows(aux1, aux2) 
  tabela_resumo$Turma <- fct_explicit_na(tabela_resumo$Turma, "Total")
  return(tabela_resumo)
}
