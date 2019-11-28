#Lendo os dados das provas e carregando os pacotes:
if(length(ls()) > 0) rm(list = ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "dplyr", "tidyr", "reshape2", "irtoys", "ltm", "mirt", "bairt","ggplot2", "R.utils", "igraph", "factoextra", "threejs", "GGally", "pander")

dados.original <- read.csv2(file.choose())

# Correção do nome dos Cursos
dados.original$Curso <- gsub("Ã¢","â",dados.original$Curso)
dados.original$Curso <- gsub("Ã§Ã£","çã",dados.original$Curso)
dados.original$Curso <- gsub("Ã","í",dados.original$Curso)
dados.original$Curso <- gsub("Engenharia Quí.....","Engenharia Química",dados.original$Curso)
dados.original$Curso <- gsub("Educação Fí.....","Educação Física",dados.original$Curso)
dados.original$Curso <- gsub("íª","ê",dados.original$Curso)
dados.original$Curso <- gsub("í´","ô",dados.original$Curso)
dados.original$Curso <- gsub("í©","é",dados.original$Curso)

dados.original$Tema <- gsub("...medida_probabilidade.......", "medida_probabilidade", dados.original$Nome.questao)
dados.original$Tema <- gsub("...propriedade_probabilidade.......", "propriedade_probabilidade", dados.original$Tema)
dados.original$Tema <- gsub("...probabilidade_total.......", "probabilidade_total", dados.original$Tema)
dados.original$Tema <- gsub("...teorema_Bayes.......", "teorema_Bayes", dados.original$Tema)
dados.original$Tema <- gsub("...variaveis_aleatorias.......", "variaveis_aleatorias", dados.original$Tema)
dados.original$Tema <- gsub("...distribuicao_binomial.......", "distribuicao_binomial", dados.original$Tema)
dados.original$Tema <- gsub("...distribuicao_geometrica.......", "distribuicao_geometrica", dados.original$Tema)
dados.original$Tema <- gsub("...distribuicao_hipergeometrica.......", "distribuicao_hipergeometrica", dados.original$Tema)
dados.original$Tema <- gsub("...distribuicao_poisson.......", "distribuicao_poisson", dados.original$Tema)
dados.original$Tema <- gsub("...aproximacao_poisson_binomial.......", "aproximacao_poisson_binomial", dados.original$Tema)
dados.original$Tema <- gsub("...funcao_densidade.......", "funcao_densidade", dados.original$Tema)
dados.original$Tema <- gsub("...distribuicao_acumulada.......", "distribuicao_acumulada", dados.original$Tema)
dados.original$Tema <- gsub("...momentos.......", "momentos", dados.original$Tema)
dados.original$Tema <- gsub("...distribuicao_exponencial.......", "distribuicao_exponencial", dados.original$Tema)
dados.original$Tema <- gsub("...distribuicao_normal.......", "distribuicao_normal", dados.original$Tema)
dados.original$Tema <- gsub("...distribuicao_normal...", "distribuicao_normal", dados.original$Tema)
dados.original$Tema <- gsub("...aproximacao_normal_binomial.......", "aproximacao_normal_binomial", dados.original$Tema)
dados.original$Tema <- gsub("...distribuicao_condicional.......", "distribuicao_condicional", dados.original$Tema)
dados.original$Tema <- gsub("...covariancia_correlacao.......", "covariancia_correlacao", dados.original$Tema)
dados.original$Tema <- gsub("...distribuicao_media.......", "distribuicao_media", dados.original$Tema)
dados.original$Tema <- gsub("...distribuicao_proporcao.......", "distribuicao_proporcao", dados.original$Tema)
dados.original$Tema <- gsub("...maxima_verossimilhanca.......", "maxima_verossimilhanca", dados.original$Tema)
dados.original$Tema <- gsub("...IC_media_normal.......", "IC_media_normal", dados.original$Tema)
dados.original$Tema <- gsub("...IC_media_t.......", "IC_media_t", dados.original$Tema)
dados.original$Tema <- gsub("...IC_proporção.......", "IC_proporção", dados.original$Tema)
dados.original$Tema <- gsub("...IC_proporcao.......", "IC_proporção", dados.original$Tema)
dados.original$Tema <- gsub("...TH_media.......", "TH_media", dados.original$Tema)
dados.original$Tema <- gsub("...tamanho_media.......", "tamanho_media", dados.original$Tema)
dados.original$Tema <- gsub("...TH_proporcao.......", "TH_proporcao", dados.original$Tema)
dados.original$Tema <- gsub("...pvalor_media.......", "pvalor_media", dados.original$Tema)
dados.original$Tema <- gsub("...tamanho_prop.......", "tamanho_prop", dados.original$Tema)
dados.original$Tema <- gsub("...pvalor_proporcao.......", "pvalor_proporcao", dados.original$Tema)
dados.original$Tema <- gsub("...valor_esperado_e_variancia.......", "valor_esperado_e_variancia", dados.original$Tema)


n.questoes.prova <- 10
n.mcmc <- 1000
thin.mcmc <- 10
n.turmas <- length(unique(dados.original$Turma))
n.provas <- length(unique(dados.original$Numero.prova))
turmas <- c("AA","AB","BA","BB","CA","CB","CC","DA","DB","EA")

#Criando o mapa de questões
mapa.questoes <- array(dim=c(n.turmas, n.questoes.prova, n.provas), 
                       dimnames=list(unique(dados.original$Turma)[order(unique(dados.original$Turma))], paste("Questao", 1:n.questoes.prova), paste("Prova", 1:n.provas)))

for(prova in 1:n.provas) {
  mapa.questoes[,,prova] <- dplyr::filter(dados.original, Numero.prova==prova) %>%
    dplyr::select(Turma, Questao, Nome.questao) %>% 
    unique() %>% 
    dcast(Turma ~ Questao, value.var="Nome.questao") %>%  
    #dplyr::filter(., complete.cases(.)) %>%
    as.matrix() %>% 
    .[, -1]
}


#Criando dataframe com as respostas dicotômicas de cada aluno:
dico <- vector(n.provas, mode="list")

for(prova in 1:n.provas) {
  data <- dados.original[order(dados.original$Matricula), ] %>%
    dplyr::filter(Numero.prova==prova) %>%
    droplevels()
  matricula <- data$Matricula
  acerto <- as.numeric(data$Acertou)
  questao <- data$Nome.questao
  turma <- data$Turma
  frame <- data.frame(matricula,acerto,questao,turma) %>% 
    dplyr::filter(., complete.cases(.))
  dico[[prova]] <- reshape(frame, v.names = "acerto", idvar = "matricula", timevar="questao", 
                           direction="wide")
  colnames(dico[[prova]]) <- gsub("^acerto.*?.","",colnames(dico[[prova]]))
}

#Cálculo dos estimadores dos parâmetros dos itens e do estimador do parâmetro de habilidade de
#cada aluno via Monte Carlo Cadeia de Markov:
set.seed(120173344)

mcmc.itens <- vector(n.provas, mode="list")
mcmc.theta <- vector(n.provas, mode="list")
mod <- vector(n.provas, mode="list")

for(prova in 1:n.provas) {
  mod[[prova]] <- mcmc.3pnob(dico[[prova]][,-c(1,2)], c.prior=select.c.prior(5),iter=n.mcmc,burning=0, thin=thin.mcmc)
  
  mcmc.theta[[prova]] <- array(dim=c(n.mcmc/thin.mcmc, length(dico[[prova]]$matricula)))
  
  mcmc.theta[[prova]] <- mod[[prova]]$mcmcobj$theta
  dimnames(mcmc.theta[[prova]]) <- list(paste("Simulacao", 1:(n.mcmc/thin.mcmc)),
                                        dico[[prova]]$matricula)
  
  mcmc.itens[[prova]] <- array(dim=c(n.mcmc/thin.mcmc, ncol(dico[[prova]])-2, 3), 
                               dimnames=list(paste("Simulacao", 1:(n.mcmc/thin.mcmc)),
                                             colnames(dico[[prova]][, -c(1,2)]),
                                             c("a", "b", "c")))
  mcmc.itens[[prova]][,, 1] <- mod[[prova]]$mcmcobj$a
  mcmc.itens[[prova]][,, 2] <- mod[[prova]]$mcmcobj$b
  mcmc.itens[[prova]][,, 3] <- mod[[prova]]$mcmcobj$c
}  

save(mcmc.itens, file = "Parametros_Itens.RData")

itens.p <- vector(n.provas, mode="list")

for (prova in 1:n.provas){
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

list_of_datasets <- list("Prova 1" = itens.p[[1]], "Prova 2" = itens.p[[2]],
                         "Prova 3" = itens.p[[3]], "Prova 4" = itens.p[[4]])


# Simulação com as probabilidade de que um aluno mediano acerte a questão para cada questão 
#selecionada em cada prova
Pm.probs <- vector(n.provas, mode="list")
names(Pm.probs) <- paste0("Prova", 1:n.provas)

for (prova in 1:n.provas){
  Pm.probs[[prova]] <- array(c(as.numeric(mcmc.itens[[prova]][,,3]) +                   
                                 (1-as.numeric(mcmc.itens[[prova]][,,3]))*
                                 pnorm(-as.numeric(mcmc.itens[[prova]][,,2]))),dim=c(n.mcmc/thin.mcmc, ncol(dico[[prova]])-2), 
                             dimnames=list(paste("Simulacao", 1:(n.mcmc/thin.mcmc)),
                                           colnames(dico[[prova]][, -c(1,2)])))
}


# Probabilidade de acerto de cada questão feita por cada aluno
P.probs <- vector(n.provas, mode="list")
names(P.probs) <- paste0("Prova", 1:n.provas)

for (prova in 1:n.provas){
  P.probs[[prova]] <- matrix(0, nrow = nrow(dico[[prova]]), ncol=ncol(dico[[prova]][, -c(1,2)]))
  for (aluno in 1:nrow(dico[[prova]])){
    
    P.probs[[prova]][aluno,] <- c(
      itens.p[[prova]][,3] + (1-itens.p[[prova]][,3])*
        pnorm(itens.p[[prova]][,1]*colMeans(mcmc.theta[[prova]])[aluno] - itens.p[[prova]][,2])
    )
    
    P.probs[[prova]] <- data.frame(P.probs[[prova]])
    rownames(P.probs[[prova]]) <- colnames(mcmc.theta[[prova]])
    colnames(P.probs[[prova]]) <- colnames(dico[[prova]][, -c(1,2)])
    
    
  }
}


# Dataframes de cada prova com a probabilidade de acerto por tema (questão que o aluno fez do tema) para cada aluno 

Prob.tema1 <- matrix(0, nrow=ncol(mcmc.theta[[1]]), ncol=n.questoes.prova)
for (aluno in 1:ncol(mcmc.theta[[1]])){
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

Prob.tema2 <- matrix(0, nrow=ncol(mcmc.theta[[2]]), ncol=n.questoes.prova)
for (aluno in 1:ncol(mcmc.theta[[2]])){
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


Prob.tema3 <- matrix(0, nrow=ncol(mcmc.theta[[3]]), ncol=n.questoes.prova)
for (aluno in 1:ncol(mcmc.theta[[3]])){
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


Prob.tema1$Matricula <- colnames(mcmc.theta[[1]])
Prob.tema2$Matricula <- colnames(mcmc.theta[[2]])
Prob.tema3$Matricula <- colnames(mcmc.theta[[3]])

Prob.acerto.tema <- full_join(Prob.tema1, Prob.tema2, by="Matricula")
Prob.acerto.tema <- full_join(Prob.acerto.tema, Prob.tema3, by="Matricula")
rownames(Prob.acerto.tema) <- Prob.acerto.tema[,11]
Prob.acerto.tema <- Prob.acerto.tema[,-11]


Acertou.tema1 <- matrix(0, nrow=ncol(mcmc.theta[[1]]), ncol=n.questoes.prova)
for (aluno in 1:ncol(mcmc.theta[[1]])){
  tx <- dados.original %>% 
    dplyr::filter(Numero.prova==1, Matricula==as.numeric(colnames(mcmc.theta[[1]])[aluno])) %>% 
    dplyr::select(Acertou, Matricula, Tema, Nome.questao)
  tix <- tx[,1]
  names(tix) <- tx[,4]
  tix <- tix[order(names(tix))]
  at <- which(!(seq(1,10) %in% as.numeric(substring(names(tix), 1,2))))
  
  if(length(at)==0) {Acertou.tema1[aluno,] <- tix} 
  else if(length(at)==1){Acertou.tema1[aluno, ] <- insert(tix, at, NA)}
  else if(length(at)==2){Acertou.tema1[aluno, ] <- insert(insert(tix, at, NA), at[2], NA)}
  
}
Acertou.tema1 <- data.frame(Acertou.tema1)
rownames(Acertou.tema1) <- colnames(mcmc.theta[[1]])
colnames(Acertou.tema1) <- substring(names(tix), 4, nchar(names(tix))-7)

Acertou.tema2 <- matrix(0, nrow=ncol(mcmc.theta[[2]]), ncol=n.questoes.prova)
for (aluno in 1:ncol(mcmc.theta[[2]])){
  tx <- dados.original %>% 
    dplyr::filter(Numero.prova==2, Matricula==as.numeric(colnames(mcmc.theta[[2]])[aluno])) %>% 
    dplyr::select(Acertou, Matricula, Tema, Nome.questao)
  tix <- tx[,1]
  names(tix) <- tx[,4]
  tix <- tix[order(names(tix))]
  at <- which(!(seq(11,20) %in% as.numeric(substring(names(tix), 1,2))))
  
  if(length(at)==0) {Acertou.tema2[aluno,] <- tix} 
  else if(length(at)==1){Acertou.tema2[aluno, ] <- insert(tix, at, NA)}
  else if(length(at)==2){Acertou.tema2[aluno, ] <- insert(insert(tix, at, NA), at[2], NA)}
  
}
Acertou.tema2 <- data.frame(Acertou.tema2)
rownames(Acertou.tema2) <- colnames(mcmc.theta[[2]])
colnames(Acertou.tema2) <- substring(names(tix), 4, nchar(names(tix))-7)


Acertou.tema3 <- matrix(0, nrow=ncol(mcmc.theta[[3]]), ncol=n.questoes.prova)
for (aluno in 1:ncol(mcmc.theta[[3]])){
  tx <- dados.original %>% 
    dplyr::filter(Numero.prova==3, Matricula==as.numeric(colnames(mcmc.theta[[3]])[aluno])) %>% 
    dplyr::select(Acertou, Matricula, Tema, Nome.questao)
  tix <- tx[,1]
  names(tix) <- tx[,4]
  tix <- tix[order(names(tix))]
  at <- which(!(seq(21,30) %in% as.numeric(substring(names(tix), 1,2))))
  
  if(length(at)==0) {Acertou.tema3[aluno,] <- tix} 
  else if(length(at)==1){Acertou.tema3[aluno, ] <- insert(tix, at, NA)}
  else if(length(at)==2){Acertou.tema3[aluno, ] <- insert(insert(tix, at, NA), at[2], NA)}
  
}
Acertou.tema3 <- data.frame(Acertou.tema3)
rownames(Acertou.tema3) <- colnames(mcmc.theta[[3]])
colnames(Acertou.tema3) <- substring(names(tix), 4, nchar(names(tix))-7)


Acertou.tema1$Matricula <- colnames(mcmc.theta[[1]])
Acertou.tema2$Matricula <- colnames(mcmc.theta[[2]])
Acertou.tema3$Matricula <- colnames(mcmc.theta[[3]])

Acerto.tema <- full_join(Acertou.tema1, Acertou.tema2, by="Matricula")
Acerto.tema <- full_join(Acerto.tema, Acertou.tema3, by="Matricula")
rownames(Acerto.tema) <- Acerto.tema[,11]
Acerto.tema <- Acerto.tema[,-11]

Acerto.tema.0 <- Acerto.tema
Acerto.tema.0[is.na(Acerto.tema.0)] <- 0

Prob.acerto.tema.0 <- Prob.acerto.tema
Prob.acerto.tema.0[is.na(Prob.acerto.tema.0)] <- 0

#H.hat <- var(2*(Acerto.tema*log(Acerto.tema/Prob.acerto.tema) + (1-Acerto.tema)*log(1/(1-Prob.acerto.tema))), na.rm = T)


cd <- matrix(0, nrow=nrow(Acerto.tema), ncol=30)
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

cd.0 <- cd[complete.cases(cd),]


cor.tema <- matrix(0, nrow=30, ncol=30)
for (tema in 1:30){
  for (tema2 in 1:30){
    cor.tema[tema,tema2] <- 
      cor(cd[complete.cases(cd[,c(tema,tema2)]),tema], cd[complete.cases(cd[,c(tema,tema2)]),tema2])
  }
}
cor.tema <- data.frame(cor.tema)
rownames(cor.tema) <- colnames(Acerto.tema)
colnames(cor.tema) <- colnames(Acerto.tema)


cluster <- kmeans(t(cor.tema), 3) 
# distribuicao_geometrica (1 -> 2),  funcao_densidade (2 -> 3), valor_esperado_e_variancia (2 -> 3), distribuicao_exponencial (2 -> 1), distribuicao_normal (2 -> 3), distribuicao_condicional (2 -> 1)


#Array final para o balanceamento das provas, com o número de simulações, turmas, questões,
#provas e vetor com a probabilidade de acerto e o resultado da binomial se acertou ou não:

n.sim <- n.mcmc/thin.mcmc

dados.balanceamento <- array(dim=c(n.sim, n.turmas, n.questoes.prova, n.provas, 2),
                             dimnames = list(dimnames(mcmc.itens[[prova]])[[1]],
                                             dimnames(mapa.questoes)[[1]],
                                             dimnames(mapa.questoes)[[2]],
                                             dimnames(mapa.questoes)[[3]],
                                             c("prob.acerto", "sim.acerto")))

for(questao in 1:n.questoes.prova) {
  for(turma in 1:n.turmas) {
    for(prova in 1:n.provas) {
      nome.questao <- mapa.questoes[turma, questao, prova]
      if(nome.questao %in% dimnames(dico[[prova]])[[2]]) {
        dados.balanceamento[, turma, questao,prova, 1] <- as.numeric(Pm.probs[[prova]][, nome.questao])
      }
    }
  }
}


dados.balanceamento[,,,,2] <- rbinom(n.sim*n.questoes.prova*n.turmas*n.provas, 1, c(dados.balanceamento[,,,,1]))



for (prova in 1:n.provas){
  P.probs[[prova]] <- matrix(0, nrow = nrow(dico[[prova]]), ncol=ncol(dico[[prova]][, -c(1,2)]))
  for (aluno in 1:nrow(dico[[prova]])){
    
    P.probs[[prova]][aluno,] <- c(
      itens.p[[prova]][,3] + (1-itens.p[[prova]][,3])*
        pnorm(itens.p[[prova]][,1]*colMeans(mcmc.theta[[prova]])[aluno] - itens.p[[prova]][,2])
    )
    
    P.probs[[prova]] <- data.frame(P.probs[[prova]])
    rownames(P.probs[[prova]]) <- colnames(mcmc.theta[[prova]])
    colnames(P.probs[[prova]]) <- colnames(dico[[prova]][, -c(1,2)])
    
    
  }
}



dados.finais <- vector(n.provas, mode="list")

for(prova in 1:n.provas) {
  dados.finais[[prova]] <- array(dim=c(ncol(mcmc.theta[[prova]]), n.turmas, 
                                       n.questoes.prova, 3),
                                 dimnames = list(colnames(mcmc.theta[[prova]]),
                                                 dimnames(mapa.questoes)[[1]],
                                                 dimnames(mapa.questoes)[[2]],
                                                 c("Prob.acerto", "Acerto", "Residuo")))
  for(turma in 1:n.turmas) {
    for(questao in 1:n.questoes.prova) {
      nome.questao <- mapa.questoes[turma, questao, prova]
      if(nome.questao %in% dimnames(dico[[prova]])[[2]]) {
        (dados.finais[[prova]][, turma, questao, 1] <-
           as.numeric(P.probs[[prova]][,nome.questao]))}
    }
    dados.finais[[prova]][,,,2] <-
      rbinom(ncol(mcmc.theta[[prova]]), 1, c(dados.finais[[prova]][,,,1]))
    dados.finais[[prova]][,,,3] <- 
      abs(dados.finais[[prova]][,,,2] - dados.finais[[prova]][,,,1])
  }
}


# Probabilidade de um aluno mediano passar por prova
P.am.passar <- array(dim = c(n.turmas, n.provas+1), dimnames = list(dimnames(mapa.questoes)[[1]], c(dimnames(mapa.questoes)[[3]],"Turma")))

for (prova in 1:n.provas){
  for (turma in 1:n.turmas){
    P.am.passar[turma,prova] <- sum(apply(dados.balanceamento[,,,,2], c(1,2,4), sum, na.rm = T)[,turma,prova]>=5)/n.sim
  }
}

P.am.passar[,5] <- dimnames(mapa.questoes)[[1]]



# Medidas decritivas

turma <- list(dados.original$Turma)
medturma <- aggregate(dados.original[,"Nota_prova"],turma,mean)
medturmaprova <- aggregate(dados.original[,"Nota_prova"],list(dados.original$Turma,dados.original$Numero.prova),mean)
colnames(medturmaprova) <- c("Turma", "Prova", "Media")
medcurso <- aggregate(dados.original[,"Nota_prova"],list(dados.original$Curso),mean)
medprova <- aggregate(dados.original[,"Nota_prova"],list(dados.original$Numero.prova),mean)
medano <- aggregate(dados.original[,"Nota_prova"],list(dados.original$Ano),mean)
mediaaluno <- aggregate(dados.original[,"Nota_prova"],list(dados.original$Matricula),mean)
mediaaluno.prova <- aggregate(dados.original[,"Nota_prova"],list(dados.original$Matricula,dados.original$Numero.prova),mean)


# notas dos alunos por prova
notaaluno <- vector(n.provas, mode="list")

for (prova in 1:n.provas) {
  notaaluno[[prova]] <- aggregate(dados.original$Nota_prova[dados.original$Numero.prova==prova],list(dados.original$Matricula[dados.original$Numero.prova==prova]),mean) 
  colnames(notaaluno[[prova]]) <- c("Matricula", "Nota")
}


# notas estimadas por TRI de cada aluno por prova
notas.estimadas <- vector(n.provas, mode="list")


for (prova in 1:n.provas){
  theta2 <- mean(notaaluno[[prova]][,2]) + sd(notaaluno[[prova]][,2])*mcmc.theta[[prova]][100,]
  
  thetaajust <- theta2-min(theta2)
  
  notas.estimadas[[prova]] <- array(dim = c(length(dico[[prova]]$matricula),2), dimnames = list(1:length(dico[[prova]]$matricula),c("Matricula","Nota Estimada")))
  
  notas.estimadas[[prova]][,1] <- dico[[prova]]$matricula
  notas.estimadas[[prova]][,2] <- c(thetaajust*10/max(thetaajust))
}

# dataframes dos alunos que não passariam e passariam por TRI, em cada prova 
N.passou.hat <- vector(n.provas, mode="list")
Passou.hat <- vector(n.provas, mode="list")

for (prova in 1:n.provas) {
  N.passou.hat[[prova]] <- array(dim = c(length(notas.estimadas[[prova]][notas.estimadas[[prova]]>=0 & notas.estimadas[[prova]]<5]),1), dimnames = list(as.numeric(notas.estimadas[[prova]][,1][notas.estimadas[[prova]][,2]>=0 & notas.estimadas[[prova]][,2]<5]),"Nota Estimada"))
  
  N.passou.hat[[prova]][,1] <- notas.estimadas[[prova]][notas.estimadas[[prova]]>=0 & notas.estimadas[[prova]]<5]
  
  Passou.hat[[prova]] <- array(dim = c(length(notas.estimadas[[prova]][,1][notas.estimadas[[prova]][,2]>=5 ]),1), dimnames = list(as.numeric(notas.estimadas[[prova]][,1][notas.estimadas[[prova]][,2]>=5]),"Nota Estimada"))
  
  Passou.hat[[prova]][,1] <- notas.estimadas[[prova]][,2][notas.estimadas[[prova]][,2]>=5]
}


# dataframes dos alunos que não passaram e passaram, em cada prova 
N.passou <- vector(n.provas, mode="list")
Passou <- vector(n.provas, mode="list")

for (prova in 1:n.provas) {
  N.passou[[prova]] <- array(dim = c(length(notaaluno[[prova]][notaaluno[[prova]]>=0 & notaaluno[[prova]]<5]),1), dimnames = list(as.numeric(notaaluno[[prova]][,1][notaaluno[[prova]][,2]>=0 & notaaluno[[prova]][,2]<5]),"Nota"))
  
  N.passou[[prova]][,1] <- notaaluno[[prova]][notaaluno[[prova]]>=0 & notaaluno[[prova]]<5]
  
  Passou[[prova]] <- array(dim = c(length(notaaluno[[prova]][,1][notaaluno[[prova]][,2]>=5 ]),1), dimnames = list(as.numeric(notaaluno[[prova]][,1][notaaluno[[prova]][,2]>=5]),"Nota"))
  
  Passou[[prova]][,1] <- notaaluno[[prova]][,2][notaaluno[[prova]][,2]>=5]
}


#dataframe com as probabilidades das simulações com theta mediano passar em PE em cada turma

sim.passar <- array(dim = c(n.sim,n.turmas), dimnames = list(paste("Simulacao", 1:n.sim),dimnames(mapa.questoes)[[1]]))

for (simulacao in 1:n.sim){
  for (turma in 1:n.turmas){
    sim.passar[simulacao,turma] <- (sum(apply(dados.balanceamento[,,,,2], c(1,2,4), sum, na.rm = T)[simulacao,turma,])-min(apply(dados.balanceamento[,,,,2], c(1,2,4), sum, na.rm = T)[simulacao,turma,]))/(n.provas-1)
  }
}



# Probabilidade de um aluno mediano passar em PE por turma
P.sim.passar <- array(dim = c(1,n.turmas), dimnames = list("Probabilidade",dimnames(mapa.questoes)[[1]]))

for (turma in 1:n.turmas){
  P.sim.passar[,turma] <- sum(sim.passar[,turma]>=5)/n.sim
}

# Construção de matriz das notas finais
nota.prova <- matrix(0, ncol = n.provas+1, nrow = length(unique(dados.original$Matricula)))
nota.prova[,1] <- unique(dados.original$Matricula)
nota.prova[,2] <- ifelse(unique(dados.original$Matricula) %in% notaaluno[[1]][,1], notaaluno[[1]][,2], 0)
nota.prova[,3] <- ifelse(unique(dados.original$Matricula) %in% notaaluno[[2]][,1], notaaluno[[2]][,2], 0)
nota.prova[,4] <- ifelse(unique(dados.original$Matricula) %in% notaaluno[[3]][,1], notaaluno[[3]][,2], 0)
nota.prova[,5] <- ifelse(unique(dados.original$Matricula) %in% notaaluno[[4]][,1], notaaluno[[4]][,2], 0)
nota.prova <- data.frame(nota.prova)
colnames(nota.prova) <- c("Matricula", "Prova 1", "Prova 2", "Prova 3", "Prova 4")

nota.prova1 <- matrix(0, ncol = n.provas, nrow = length(notaaluno[[1]][,1]))
nota.prova1[,1] <- notaaluno[[1]][,2]
nota.prova1[,2] <- ifelse(notaaluno[[1]][,1] %in% notaaluno[[2]][,1], notaaluno[[2]][,2], -1)
nota.prova1[,3] <- ifelse(notaaluno[[1]][,1] %in% notaaluno[[3]][,1], notaaluno[[3]][,2], -1)
nota.prova1[,4] <- ifelse(notaaluno[[1]][,1] %in% notaaluno[[4]][,1], notaaluno[[4]][,2], -1)
nota.prova1 <- data.frame(nota.prova1)
colnames(nota.prova1) <- c("Prova 1", "Prova 2", "Prova 3", "Prova 4")


media.prova <- matrix(0, ncol = n.provas, nrow = length(nota.prova[,1]))
media.prova[,1] <- nota.prova[,1]
media.prova[,2] <- nota.prova[,2]
media.prova[,3] <- .5*nota.prova[,2] + .5*nota.prova[,3]
media.prova[,4] <- .3*nota.prova[,2] + .3*nota.prova[,3] + .4*nota.prova[,4]

media.prova <- data.frame(media.prova)
colnames(media.prova) <- c("Matricula", "Prova1", "Prova2", "Prova3")

lim.mencao <- c(3, 4.8, 6.8, 8.8)

mencao.m.prova <- matrix("SR", ncol = 4, nrow = length(media.prova$Matricula))
for (prova in 2:4){
  for (aluno in 1:length(media.prova$Matricula)){
    if(media.prova[aluno,prova]>=0 & media.prova[aluno,prova]<lim.mencao[1]){mencao.m.prova[aluno,prova] <- "II"}
    else if(media.prova[aluno,prova]>=lim.mencao[1] & media.prova[aluno,prova]<lim.mencao[2]){mencao.m.prova[aluno,prova] <- "MI"}
    else if(media.prova[aluno,prova]>=lim.mencao[2] & media.prova[aluno,prova]<lim.mencao[3]){mencao.m.prova[aluno,prova] <- "MM"}
    else if(media.prova[aluno,prova]>=lim.mencao[3] & media.prova[aluno,prova]<lim.mencao[4]){mencao.m.prova[aluno,prova] <- "MS"}
    else if(media.prova[aluno,prova]>=lim.mencao[4]){mencao.m.prova[aluno,prova] <- "SS"}
  }
}

mencao.m.prova[,1] <- unique(dados.original$Matricula)

mencao.m.prova <- data.frame(mencao.m.prova)
colnames(mencao.m.prova) <- c("Matricula", "Prova1", "Prova2", "Prova3")

matriz.notas <- cbind(coalesce(nota.prova$`Prova 1`, 0),
                      coalesce(nota.prova$`Prova 2`, 0),
                      coalesce(nota.prova$`Prova 3`, 0),
                      coalesce(nota.prova$`Prova 4`, 0))
descartada <- pior.prova <- max.col(-matriz.notas, ties.method="last") 
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

notas.finais <- nota.prova %>%
  mutate(Nota_final=Nota_final) %>%
  arrange(Matricula) %>%
  mutate(Mencao=cut(Nota_final, righ=F, c(0.1,lim.mencao,10.1), 
                    labels=c("II", "MI", "MM", "MS", "SS")))


#estimadas por TRI  
# Construção de matriz das notas finais estimadas
nota.prova.estimada <- matrix(0, ncol = n.provas+1, nrow = length(unique(dados.original$Matricula)))
nota.prova.estimada[,1] <- unique(dados.original$Matricula)
nota.prova.estimada[,2] <- ifelse(unique(dados.original$Matricula) %in% notas.estimadas[[1]][,1], notas.estimadas[[1]][,2], 0)
nota.prova.estimada[,3] <- ifelse(unique(dados.original$Matricula) %in% notas.estimadas[[2]][,1], notas.estimadas[[2]][,2], 0)
nota.prova.estimada[,4] <- ifelse(unique(dados.original$Matricula) %in% notas.estimadas[[3]][,1], notas.estimadas[[3]][,2], 0)
nota.prova.estimada[,5] <- ifelse(unique(dados.original$Matricula) %in% notas.estimadas[[4]][,1], notas.estimadas[[4]][,2], 0)
nota.prova.estimada <- data.frame(nota.prova.estimada)
colnames(nota.prova.estimada) <- c("Matricula", "Prova 1", "Prova 2", "Prova 3", "Prova 4")


matriz.notas.estimadas <- cbind(coalesce(nota.prova.estimada$`Prova 1`, 0),
                                coalesce(nota.prova.estimada$`Prova 2`, 0),
                                coalesce(nota.prova.estimada$`Prova 3`, 0),
                                coalesce(nota.prova.estimada$`Prova 4`, 0))
descartada <- pior.prova <- max.col(-matriz.notas.estimadas, ties.method="last") 
condicao <- (pior.prova %in% 1:2) & 
  ((matriz.notas[, 4] - matriz.notas.estimadas[, 3])*4 > (matriz.notas.estimadas[, 4] - matriz.notas.estimadas[, pior.prova])*3)
descartada[condicao] <- 3  
matriz.notas.estimadas <- cbind(matriz.notas.estimadas, descartada=descartada)
Nota_final <- numeric(nrow(nota.prova))
for(aluno in 1:nrow(nota.prova.estimada)) {
  if (matriz.notas.estimadas[aluno, 5] %in% 1:2) pesos <- c(3,4,3)/10
  else pesos <- c(3,3,4)/10
  Nota_final[aluno] <- matriz.notas.estimadas[aluno, -c(matriz.notas.estimadas[aluno, 5], 5)] %*% pesos
}

notas.finais.estimadas <- nota.prova.estimada %>%
  mutate(Nota_final=Nota_final) %>%
  arrange(Matricula) %>%
  mutate(Mencao=cut(Nota_final, righ=F, c(0.1,lim.mencao,10.1), 
                    labels=c("II", "MI", "MM", "MS", "SS")))


# Nova variavel para agrupar os cursos
dados.original$Grupo[str_detect(dados.original$Curso,"Engenharia ")] <- 'Engenharia'
dados.original$Grupo[str_detect(dados.original$Curso,"Comp")] <- 'Computação'
dados.original$Grupo[str_detect(dados.original$Curso,"Educ")] <- 'Ed. Física'
dados.original$Grupo[str_detect(dados.original$Curso,"Econ")] <- 'Econômicas'
dados.original$Grupo[str_detect(dados.original$Curso,"Outros")] <- 'Outros'
dados.original$Grupo[is.na(dados.original$Curso)] <- 'NA'

# Comparação das mencoes finais dos alunos que fizeram subs antes e depois desta
nota.3 <- notas.finais[notas.finais$Matricula %in% notaaluno[[3]][,1][notaaluno[[3]][,1] %in% notaaluno[[4]][,1]],4]
nota.subs <- notas.finais[notas.finais$Matricula %in% notaaluno[[3]][,1][notaaluno[[3]][,1] %in% notaaluno[[4]][,1]],5]

mencao.3 <- mencao.m.prova[mencao.m.prova$Matricula %in% notaaluno[[3]][,1][notaaluno[[3]][,1] %in% notaaluno[[4]][,1]],4]
mencao.subs <- notas.finais[notas.finais$Matricula %in% notaaluno[[3]][,1][notaaluno[[3]][,1] %in% notaaluno[[4]][,1]],7]