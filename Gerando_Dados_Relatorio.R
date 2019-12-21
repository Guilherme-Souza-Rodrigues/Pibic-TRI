#Lendo os dados das provas e carregando os pacotes:
if(length(ls()) > 0) rm(list = ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "dplyr", "tidyr", "reshape2", "irtoys", "ltm", "mirt", "bairt","ggplot2", "R.utils", "igraph", "factoextra", "threejs", "GGally", "pander","rstan")

dados.original <- read.csv2(choose.files(multi = FALSE, caption = "Escolha o arquivo com o Banco de Respostas"))

# Correção do nome dos Cursos
erros <- c("Ã¢", "Ã§Ã£", "Ã", "Engenharia Quí.....", "Educação Fí.....", 
           "íª", "í´", "í©")
correcoes <- c("â", "çã", "í", "Engenharia Química", "Educação Física",
               "ê", "ô", "é")
for (i in 1:length(erros)){
  dados.original$Curso <- gsub(erros[i], correcoes[i], dados.original$Curso)
}

# Variável Nota_prova
np <- dados.original[,c("Matricula", "Acertou", "Numero.prova")]
for (aluno in 1:length(dados.original$Matricula)){
  if(np$Numero.prova==1){
    s <- np[np$Matricula==dados.original$Matricula[aluno], ]
    dados.original$Nota_prova[aluno] <- 
      sum(s[s$Numero.prova==1, "Acertou"]==TRUE)
  }
  else if(np$Numero.prova==2){
    s <- np[np$Matricula==dados.original$Matricula[aluno], ]
    dados.original$Nota_prova[aluno] <- 
      sum(s[s$Numero.prova==2, "Acertou"]==TRUE)
  }
  else if(np$Numero.prova==3){
    s <- np[np$Matricula==dados.original$Matricula[aluno], ]
    dados.original$Nota_prova[aluno] <- 
      sum(s[s$Numero.prova==3, "Acertou"]==TRUE)
  }
  else if(np$Numero.prova==4){
    s <- np[np$Matricula==dados.original$Matricula[aluno], ]
    dados.original$Nota_prova[aluno] <- 
      sum(s[s$Numero.prova==4, "Acertou"]==TRUE)
  }
    }

# Carregando o arquivo com as funções 
source(choose.files(multi = FALSE, caption = "Escolha o arquivo com as Funcoes suplementares"))

# Criando o mapa de questões
mapa.questoes <- array(dim=c(n.turmas, n.questoes.prova, n.provas), 
                       dimnames=list(unique(dados.original$Turma)[order(unique(dados.original$Turma))], paste("Questao", 1:n.questoes.prova), paste("Prova", 1:n.provas)))
mapa.questoes <- mapa()

# Criando dataframe com as respostas dicotômicas de cada aluno:
respostas.dico <- vector(n.provas, mode="list")
respostas.dico <- dico()

# Cálculo dos estimadores dos parâmetros dos itens e do estimador do parâmetro de habilidade de
#cada aluno via Monte Carlo Cadeia de Markov:
mcmc.itens <- vector(n.provas, mode="list")
mcmc.theta <- vector(n.provas, mode="list")
mod <- vector(n.provas, mode="list")

mcmc.itens <- mcmci()
mcmc.theta <- mcmct()

# Dataframes dos coeficientes de cada questao
itens.p <- vector(n.provas, mode="list")
itens.p <- data.i()

list_of_datasets <- list("Prova 1" = itens.p[[1]], "Prova 2" = itens.p[[2]],
                         "Prova 3" = itens.p[[3]], "Prova 4" = itens.p[[4]])

# Simulação com as probabilidades de que um aluno mediano acerte a questão para cada questão 
#selecionada em cada prova
Pm.probs <- vector(n.provas, mode="list")
names(Pm.probs) <- paste0("Prova", 1:n.provas)
Pm.probs <- sim.aluno.medio()

# Probabilidade de acerto de cada questão feita por cada aluno
P.probs <- vector(n.provas, mode="list")
names(P.probs) <- paste0("Prova", 1:n.provas)
P.probs <- prob.acerto.questoes()

# Dataframes de cada prova com a probabilidade de acerto por tema (questão que o aluno fez do tema) para cada aluno 
Prob.tema1 <- matrix(0, nrow=ncol(mcmc.theta[[1]]), ncol=n.questoes.prova)
Prob.tema1 <- prob.tema1()

Prob.tema2 <- matrix(0, nrow=ncol(mcmc.theta[[2]]), ncol=n.questoes.prova)
Prob.tema2 <- prob.tema2()

Prob.tema3 <- matrix(0, nrow=ncol(mcmc.theta[[3]]), ncol=n.questoes.prova)
Prob.tema3 <- prob.tema3()

Prob.tema1$Matricula <- colnames(mcmc.theta[[1]])
Prob.tema2$Matricula <- colnames(mcmc.theta[[2]])
Prob.tema3$Matricula <- colnames(mcmc.theta[[3]])

Prob.acerto.tema <- full_join(Prob.tema1, Prob.tema2, by="Matricula")
Prob.acerto.tema <- full_join(Prob.acerto.tema, Prob.tema3, by="Matricula")
rownames(Prob.acerto.tema) <- Prob.acerto.tema[,11]
Prob.acerto.tema <- Prob.acerto.tema[,-11]

# Dataframe com os acertos de cada aluno em cada tema
Acerto.tema1 <- matrix(0, nrow=ncol(mcmc.theta[[1]]), ncol=n.questoes.prova)
Acerto.tema1 <- acertou.tema1()

Acerto.tema2 <- matrix(0, nrow=ncol(mcmc.theta[[2]]), ncol=n.questoes.prova)
Acerto.tema2 <- acertou.tema2()

Acerto.tema3 <- matrix(0, nrow=ncol(mcmc.theta[[3]]), ncol=n.questoes.prova)
Acerto.tema3 <- acertou.tema3()

Acerto.tema1$Matricula <- colnames(mcmc.theta[[1]])
Acerto.tema2$Matricula <- colnames(mcmc.theta[[2]])
Acerto.tema3$Matricula <- colnames(mcmc.theta[[3]])

Acerto.tema <- full_join(Acerto.tema1, Acerto.tema2, by="Matricula")
Acerto.tema <- full_join(Acerto.tema, Acerto.tema3, by="Matricula")
rownames(Acerto.tema) <- Acerto.tema[,11]
Acerto.tema <- Acerto.tema[,-11]

# Cálculo das componentes do desvio
cd <- matrix(0, nrow=nrow(Acerto.tema), ncol=30)
cd <- c.d()

# Correlações entre os temas
cor.tema <- matrix(0, nrow=30, ncol=30)
cor.tema <- corr.tema()

# Clusterização k-means
cluster <- kmeans(t(cor.tema), 3) 

# Array final para o balanceamento das provas, com o número de simulações, turmas, questões,
#provas e vetor com a probabilidade de acerto e o resultado da binomial se acertou ou não:
dados.balanceamento <- array(dim=c(nchains*(niter/2), n.turmas, n.questoes.prova, n.provas, 2),
                             dimnames = list(dimnames(mcmc.itens[[1]])[[1]],
                                             dimnames(mapa.questoes)[[1]],
                                             dimnames(mapa.questoes)[[2]],
                                             dimnames(mapa.questoes)[[3]],
                                             c("prob.acerto", "sim.acerto")))
dados.balanceamento <- balanceamento()
dados.balanceamento[,,,,2] <- rbinom(nchains*(niter/2)*n.questoes.prova*n.turmas*n.provas, 1, c(dados.balanceamento[,,,,1]))


# Array final 
dados.finais <- vector(n.provas, mode="list")
dados.finais <- dados.fim()

# Probabilidade de um aluno mediano passar por prova
P.am.passar <- array(dim = c(n.turmas, n.provas+1), dimnames = list(dimnames(mapa.questoes)[[1]], c(dimnames(mapa.questoes)[[3]],"Turma")))
P.am.passar <- prob.aluno.mediano.passar()
P.am.passar[,5] <- dimnames(mapa.questoes)[[1]]

# Notas dos alunos por prova
notaaluno <- vector(n.provas, mode="list")
notaaluno <- nota.prova()

# Notas estimadas por TRI de cada aluno por prova
notas.estimadas <- vector(n.provas, mode="list")
notas.estimadas <- notas.estim()

# Dataframes dos alunos que passariam por TRI, em cada prova 
Passou.hat <- vector(n.provas, mode="list")
Passou.hat <- passou.hat()

# Dataframes dos alunos que não passariam por TRI, em cada prova 
N.passou.hat <- vector(n.provas, mode="list")
N.passou.hat <- n.passou.hat

# Dataframes dos alunos que passaram, em cada prova
Passou <- vector(n.provas, mode="list")
Passou <- passou()

# Dataframes dos alunos que não passaram, em cada prova 
N.passou <- vector(n.provas, mode="list")
N.passou <- n.passou()

# Dataframe com as probabilidades das simulações com theta mediano passar em PE em cada turma
sim.passar <- array(dim = c(nchains*(niter/2),n.turmas), dimnames = list(paste("Simulacao", 1:nchains*(niter/2)),dimnames(mapa.questoes)[[1]]))
sim.passar <- Sim.passar()

# Probabilidade de um aluno mediano passar em PE por turma
P.sim.passar <- array(dim = c(1,n.turmas), dimnames = list("Probabilidade",dimnames(mapa.questoes)[[1]]))
P.sim.passar <- p.sim.passar()

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

# Para os gráficos

dados.original1 <- dados.original

dados.original1$Curso <- factor(dados.original1$Curso, 
                                levels = c(unique(dados.original$Curso[str_detect(dados.original$Curso,"Outros")])[-2],
                                                                  unique(dados.original$Curso[str_detect(dados.original$Curso,"Comp")])[-1],                                unique(dados.original$Curso[str_detect(dados.original$Curso,"Econ")])[-2],     
                                                                  unique(dados.original$Curso[str_detect(dados.original$Curso,"Educ")])[-2],
                                                                  unique(dados.original$Curso[str_detect(dados.original$Curso,"Engenharia ")])[-9])[-15])
#
medpturma <- matrix(c(rep(c('AA', 'AB', 'BA', 'BB', 'CA', 'CB', 'CC', 'DA', 'DB', 'EA'),4),
                      rep("Media Geral", 4), rep(c(1,2,3,4), each=10), 1,2,3,4, rep(0, 44)),
                    nrow=44, ncol=3)
medpturma[1:40, 3] <- medturmaprova$Media
medpturma[41:44, 3] <- medprova$x
medpturma <- data.frame(medpturma)
colnames(medpturma) <- c("Turma", "Prova", "Media")
medpturma$Media <- as.numeric(as.character(medpturma$Media))

#
d.tema <- dplyr::filter(dados.original, Numero.prova==1) %>% 
  dplyr::select(Matricula, Acertou, Tema, Turma) 

data <- data.frame(tapply(d.tema$Acertou, list(d.tema$Turma, d.tema$Tema), sum))

tema <- colnames(data)
turma <- rownames(data)
datan <- matrix(0, ncol=ncol(data), nrow=nrow(data))
for (i in 1:n.turmas){
  for (r in 1:10)
    datan[i,r] <- length(d.tema$Tema[d.tema$Turma==turma[i]][d.tema$Tema[d.tema$Turma==turma[i]]==tema[r]])
  
}

data <- data/datan

data1 <- rbind(data[which.max(rowMeans(data, na.rm=T)),], data[which.min(rowMeans(data, na.rm=T)[-c(1,2)]),], rowMeans(data, na.rm=T))
rownames(data1)[3] <- 'Média geral'

data <- rbind(rep(1,10), rep(0,10) , data)

data1 <- rbind(rep(1,3), rep(0,3) , data1)

#
prob.v <- c(nrow(mencao.m.prova[mencao.m.prova$Prova2=="MI",][mencao.m.prova[mencao.m.prova$Prova2=="MI",][,1] %in% mencao.m.prova[mencao.m.prova$Prova1=="SS",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova2=="MM",][mencao.m.prova[mencao.m.prova$Prova2=="MM",][,1] %in% mencao.m.prova[mencao.m.prova$Prova1=="SS",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova2=="MS",][mencao.m.prova[mencao.m.prova$Prova2=="MS",][,1] %in% mencao.m.prova[mencao.m.prova$Prova1=="SS",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova2=="SS",][mencao.m.prova[mencao.m.prova$Prova2=="SS",][,1] %in% mencao.m.prova[mencao.m.prova$Prova1=="SS",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova2=="MI",][mencao.m.prova[mencao.m.prova$Prova2=="MI",][,1] %in% mencao.m.prova[mencao.m.prova$Prova1=="MS",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova2=="MM",][mencao.m.prova[mencao.m.prova$Prova2=="MM",][,1] %in% mencao.m.prova[mencao.m.prova$Prova1=="MS",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova2=="MS",][mencao.m.prova[mencao.m.prova$Prova2=="MS",][,1] %in% mencao.m.prova[mencao.m.prova$Prova1=="MS",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova2=="SS",][mencao.m.prova[mencao.m.prova$Prova2=="SS",][,1] %in% mencao.m.prova[mencao.m.prova$Prova1=="MS",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova2=="II",][mencao.m.prova[mencao.m.prova$Prova2=="II",][,1] %in% mencao.m.prova[mencao.m.prova$Prova1=="MM",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova2=="MI",][mencao.m.prova[mencao.m.prova$Prova2=="MI",][,1] %in% mencao.m.prova[mencao.m.prova$Prova1=="MM",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova2=="MM",][mencao.m.prova[mencao.m.prova$Prova2=="MM",][,1] %in% mencao.m.prova[mencao.m.prova$Prova1=="MM",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova2=="MS",][mencao.m.prova[mencao.m.prova$Prova2=="MS",][,1] %in% mencao.m.prova[mencao.m.prova$Prova1=="MM",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova2=="II",][mencao.m.prova[mencao.m.prova$Prova2=="II",][,1] %in% mencao.m.prova[mencao.m.prova$Prova1=="MI",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova2=="MI",][mencao.m.prova[mencao.m.prova$Prova2=="MI",][,1] %in% mencao.m.prova[mencao.m.prova$Prova1=="MI",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova2=="MM",][mencao.m.prova[mencao.m.prova$Prova2=="MM",][,1] %in% mencao.m.prova[mencao.m.prova$Prova1=="MI",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova2=="MS",][mencao.m.prova[mencao.m.prova$Prova2=="MS",][,1] %in% mencao.m.prova[mencao.m.prova$Prova1=="MI",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova2=="II",][mencao.m.prova[mencao.m.prova$Prova2=="II",][,1] %in% mencao.m.prova[mencao.m.prova$Prova1=="II",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova2=="MI",][mencao.m.prova[mencao.m.prova$Prova2=="MI",][,1] %in% mencao.m.prova[mencao.m.prova$Prova1=="II",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova2=="MM",][mencao.m.prova[mencao.m.prova$Prova2=="MM",][,1] %in% mencao.m.prova[mencao.m.prova$Prova1=="II",][,1],]),
            
            nrow(mencao.m.prova[mencao.m.prova$Prova3=="SS",][mencao.m.prova[mencao.m.prova$Prova3=="SS",][,1] %in% mencao.m.prova[mencao.m.prova$Prova2=="SS",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova3=="MS",][mencao.m.prova[mencao.m.prova$Prova3=="MS",][,1] %in% mencao.m.prova[mencao.m.prova$Prova2=="SS",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova3=="MM",][mencao.m.prova[mencao.m.prova$Prova3=="MM",][,1] %in% mencao.m.prova[mencao.m.prova$Prova2=="SS",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova3=="MI",][mencao.m.prova[mencao.m.prova$Prova3=="MI",][,1] %in% mencao.m.prova[mencao.m.prova$Prova2=="SS",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova3=="SS",][mencao.m.prova[mencao.m.prova$Prova3=="SS",][,1] %in% mencao.m.prova[mencao.m.prova$Prova2=="MS",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova3=="MS",][mencao.m.prova[mencao.m.prova$Prova3=="MS",][,1] %in% mencao.m.prova[mencao.m.prova$Prova2=="MS",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova3=="MM",][mencao.m.prova[mencao.m.prova$Prova3=="MM",][,1] %in% mencao.m.prova[mencao.m.prova$Prova2=="MS",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova3=="MI",][mencao.m.prova[mencao.m.prova$Prova3=="MI",][,1] %in% mencao.m.prova[mencao.m.prova$Prova2=="MS",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova3=="MS",][mencao.m.prova[mencao.m.prova$Prova3=="MS",][,1] %in% mencao.m.prova[mencao.m.prova$Prova2=="MM",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova3=="MM",][mencao.m.prova[mencao.m.prova$Prova3=="MM",][,1] %in% mencao.m.prova[mencao.m.prova$Prova2=="MM",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova3=="MI",][mencao.m.prova[mencao.m.prova$Prova3=="MI",][,1] %in% mencao.m.prova[mencao.m.prova$Prova2=="MM",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova3=="II",][mencao.m.prova[mencao.m.prova$Prova3=="II",][,1] %in% mencao.m.prova[mencao.m.prova$Prova2=="MM",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova3=="MS",][mencao.m.prova[mencao.m.prova$Prova3=="MS",][,1] %in% mencao.m.prova[mencao.m.prova$Prova2=="MI",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova3=="MM",][mencao.m.prova[mencao.m.prova$Prova3=="MM",][,1] %in% mencao.m.prova[mencao.m.prova$Prova2=="MI",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova3=="MI",][mencao.m.prova[mencao.m.prova$Prova3=="MI",][,1] %in% mencao.m.prova[mencao.m.prova$Prova2=="MI",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova3=="II",][mencao.m.prova[mencao.m.prova$Prova3=="II",][,1] %in% mencao.m.prova[mencao.m.prova$Prova2=="MI",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova3=="MM",][mencao.m.prova[mencao.m.prova$Prova3=="MM",][,1] %in% mencao.m.prova[mencao.m.prova$Prova2=="II",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova3=="MI",][mencao.m.prova[mencao.m.prova$Prova3=="MI",][,1] %in% mencao.m.prova[mencao.m.prova$Prova2=="II",][,1],]),
            nrow(mencao.m.prova[mencao.m.prova$Prova3=="II",][mencao.m.prova[mencao.m.prova$Prova3=="II",][,1] %in% mencao.m.prova[mencao.m.prova$Prova2=="II",][,1],]),
            
            nrow(notas.finais[notas.finais$Mencao=="SS",][notas.finais[notas.finais$Mencao=="SS",][,1] %in% mencao.m.prova[mencao.m.prova$Prova3=="SS",][,1],]),
            nrow(notas.finais[notas.finais$Mencao=="MS",][notas.finais[notas.finais$Mencao=="MS",][,1] %in% mencao.m.prova[mencao.m.prova$Prova3=="SS",][,1],]),
            nrow(notas.finais[notas.finais$Mencao=="MM",][notas.finais[notas.finais$Mencao=="MM",][,1] %in% mencao.m.prova[mencao.m.prova$Prova3=="SS",][,1],]),
            nrow(notas.finais[notas.finais$Mencao=="MI",][notas.finais[notas.finais$Mencao=="MI",][,1] %in% mencao.m.prova[mencao.m.prova$Prova3=="SS",][,1],]),
            nrow(notas.finais[notas.finais$Mencao=="SS",][notas.finais[notas.finais$Mencao=="SS",][,1] %in% mencao.m.prova[mencao.m.prova$Prova3=="MS",][,1],]),
            nrow(notas.finais[notas.finais$Mencao=="MS",][notas.finais[notas.finais$Mencao=="MS",][,1] %in% mencao.m.prova[mencao.m.prova$Prova3=="MS",][,1],]),
            nrow(notas.finais[notas.finais$Mencao=="MM",][notas.finais[notas.finais$Mencao=="MM",][,1] %in% mencao.m.prova[mencao.m.prova$Prova3=="MS",][,1],]),
            nrow(notas.finais[notas.finais$Mencao=="MI",][notas.finais[notas.finais$Mencao=="MI",][,1] %in% mencao.m.prova[mencao.m.prova$Prova3=="MS",][,1],]),
            nrow(notas.finais[notas.finais$Mencao=="MS",][notas.finais[notas.finais$Mencao=="MS",][,1] %in% mencao.m.prova[mencao.m.prova$Prova3=="MM",][,1],]),
            nrow(notas.finais[notas.finais$Mencao=="MM",][notas.finais[notas.finais$Mencao=="MM",][,1] %in% mencao.m.prova[mencao.m.prova$Prova3=="MM",][,1],]),
            nrow(notas.finais[notas.finais$Mencao=="MI",][notas.finais[notas.finais$Mencao=="MI",][,1] %in% mencao.m.prova[mencao.m.prova$Prova3=="MM",][,1],]),
            nrow(notas.finais[notas.finais$Mencao=="II",][notas.finais[notas.finais$Mencao=="II",][,1] %in% mencao.m.prova[mencao.m.prova$Prova3=="MM",][,1],]),
            nrow(notas.finais[notas.finais$Mencao=="MS",][notas.finais[notas.finais$Mencao=="MS",][,1] %in% mencao.m.prova[mencao.m.prova$Prova3=="MI",][,1],]),
            nrow(notas.finais[notas.finais$Mencao=="MM",][notas.finais[notas.finais$Mencao=="MM",][,1] %in% mencao.m.prova[mencao.m.prova$Prova3=="MI",][,1],]),
            nrow(notas.finais[notas.finais$Mencao=="MI",][notas.finais[notas.finais$Mencao=="MI",][,1] %in% mencao.m.prova[mencao.m.prova$Prova3=="MI",][,1],]),
            nrow(notas.finais[notas.finais$Mencao=="II",][notas.finais[notas.finais$Mencao=="II",][,1] %in% mencao.m.prova[mencao.m.prova$Prova3=="MI",][,1],]),
            nrow(notas.finais[notas.finais$Mencao=="MM",][notas.finais[notas.finais$Mencao=="MM",][,1] %in% mencao.m.prova[mencao.m.prova$Prova3=="II",][,1],]),
            nrow(notas.finais[notas.finais$Mencao=="MI",][notas.finais[notas.finais$Mencao=="MI",][,1] %in% mencao.m.prova[mencao.m.prova$Prova3=="II",][,1],]),
            nrow(notas.finais[notas.finais$Mencao=="II",][notas.finais[notas.finais$Mencao=="II",][,1] %in% mencao.m.prova[mencao.m.prova$Prova3=="II",][,1],])
)


library(networkD3)

links <- data.frame(
  source=c("SS1", "SS1", "SS1", "SS1", 
           "MS1", "MS1", "MS1", "MS1", 
           "MM1", "MM1", "MM1", "MM1",
           "MI1", "MI1", "MI1", "MI1", 
           "II1", "II1", "II1",        "SS2", "SS2", "SS2", "SS2",
           "MS2", "MS2", "MS2", "MS2",
           "MM2", "MM2", "MM2", "MM2", 
           "MI2", "MI2", "MI2", "MI2",
           "II2", "II2", "II2",         
           "SS3", "SS3", "SS3", "SS3",
           "MS3", "MS3", "MS3", "MS3",
           "MM3", "MM3", "MM3", "MM3", 
           "MI3", "MI3", "MI3", "MI3",
           "II3", "II3", "II3"), 
  
  target=c("MI2", "MM2", "MS2", "SS2",
           "MI2", "MM2", "MS2", "SS2",
           "II2", "MI2", "MM2", "MS2",
           "II2", "MI2", "MM2", "MS2",
           "II2", "MI2", "MM2",        "SS3", "MS3", "MM3", "MI3",               
           "SS3", "MS3", "MM3", "MI3",                                                              "MS3", "MM3", "MI3", "II3",                                                              "MS3", "MM3", "MI3", "II3",                                                              "MM3", "MI3", "II3",
           "SS4", "MS4", "MM4", "MI4",               
           "SS4", "MS4", "MM4", "MI4",                                                              "MS4", "MM4", "MI4", "II4",                                                              "MS4", "MM4", "MI4", "II4",                                                              "MM4", "MI4", "II4"), 
  
  value=c(prob.v))

links <- links %>% filter(value>0)

nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique(),
  
  name2=substring(c(as.character(links$source), 
                    as.character(links$target)) %>% unique(),1,nchar(c(as.character(links$source), 
                                                                       as.character(links$target)) %>% unique())-1)
)

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

links$energy_type <- as.character(links$source)

color_scale <- data.frame(
  range = c(rep(c("darkgreen", "green", "lightgreen", "red", "darkred"),4)),
  domain = c("SS1", "MS1", "MM1", "MI1", "II1", "SS2", "MS2", "MM2", "MI2", "II2",
             "SS3", "MS3", "MM3", "MI3", "II3", "SS4", "MS4", "MM4", "MI4", "II4"),
  nodes = nodes,
  stringsAsFactors = FALSE
)

#
tabela1 <- as.data.frame(table(mencao.3, mencao.subs))
colnames(tabela1) <- c("MencaosemP4", "MencaoposP4", "Quantidade")
class(tabela1$MencaosemP4) <- "numeric"
class(tabela1$MencaoposP4) <- "numeric"
class(tabela1$Quantidade) <- "numeric"
tabela1[, 1:2] <- tabela1[, 1:2] - 1

#
data.g <- data.frame(Probabilidade=c(as.numeric(P.am.passar[,1]),as.numeric(P.am.passar[,2]),
                                     as.numeric(P.am.passar[,3]),as.numeric(P.am.passar[,4])),
                     Turma=c(rep(dimnames(mapa.questoes)[[1]],4)), Prova=rep(c(dimnames(mapa.questoes)[[3]]),each=10))

#
P.sim.passar <- t(data.frame(P.sim.passar))
P.sim.passar <- data.frame(P.sim.passar,Turma=c(dimnames(mapa.questoes)[[1]]), Linha=c(rep(1,10)))

#
adjm <- as.matrix(cor.tema)
adjm[abs(adjm)<0.3] <- 0

network <- graph_from_adjacency_matrix(adjm, weighted=T, diag=F, mode = "undirected")

my_color <- c(rep("green", 10),
              rep("blue", 10),
              rep("red", 10))

#
tabela <- as.data.frame(table(notas.finais$Mencao, notas.finais.estimadas$Mencao))
colnames(tabela) <- c("Classico", "TRI", "Quantidade")
class(tabela$Classico) <- "numeric"
class(tabela$TRI) <- "numeric"
class(tabela$Quantidade) <- "numeric"
tabela[, 1:2] <- tabela[, 1:2] - 1
