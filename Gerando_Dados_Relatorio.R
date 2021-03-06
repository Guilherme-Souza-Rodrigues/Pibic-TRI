### Definindo os parâmetros da análise
if(length(ls()) > 0) rm(list = ls())
set.seed(123)
n.mcmc <- 5000
# lim.mencao <- c(3, 4.8, 6.8, 8.8)
lim.mencao <- c(3, 5, 7, 9)

# Carregando o arquivo com as funções 
source(choose.files(multi=FALSE, 
                    caption="Escolha o arquivo com as Funcoes suplementares"),
       encoding="UTF-8")

#### Organizando os dados
# Lendo os dados com as respostas dos alunos
dados.original <- read.csv2(choose.files(multi=FALSE, 
                                         caption="Escolha o arquivo com o Banco de Respostas"),
                            fileEncoding="UTF-8")

# Criacao da variavel Tema no banco de dados
dados.original <- dados.original %>% 
  mutate(Tema=str_extract(Nome.questao, "[:alpha:][^\\d]*[:alpha:]"))

# Variável Acertou como 0 e 1
class(dados.original$Acertou) <- "numeric"

# Variável Nota_prova
dados.original <- dados.original %>%
  group_by(Matricula, Numero.prova) %>%
  summarise(Nota_prova=sum(Acertou)) %>% 
  left_join(x=dados.original, y=.)

# Lista de alunos que concluiramo curso
# Isso é, fizeram ou a prova 3, ou a psub ou foram aprovados.
concluiram.curso <- dados.original %>%
  group_by(Matricula, Numero.prova) %>%
  summarize(nota=sum(Acertou)) %>%
  dcast(Matricula ~ Numero.prova, value.var="nota") %>%
  rename(P1=`1`, P2=`2`, P3=`3`, P4=`4`) %>%
  left_join(dados.original, .) %>%
  filter(!is.na(P3) | !is.na(P4) | (P1 + P2 >= 17)) %>%
  dplyr::select(Matricula) %>%
  unique()

# Removendo os alunos quenão concluiram o curso
dados.original <- dados.original %>%
  filter(Matricula %in% concluiram.curso$Matricula)

# 
n.questoes.prova <- max(dados.original$Questao)
n.turmas <- length(unique(dados.original$Turma))
n.provas <- length(unique(dados.original$Numero.prova))
turmas <- levels(dados.original$Turma)

# Criando o mapa de questões
mapa.questoes <- mapa()

# Alterando a nota devido a questões anuladas
anulacoes <- is.na(mapa.questoes)
class(anulacoes) <- "numeric"
pontos <- data.frame(Turma=rep(turmas,4), Prova=rep(1:4, each=10))
pontos$devidos <- c(rowMeans(anulacoes[,,1])*10, rowMeans(anulacoes[,,2])*10,
                    rowMeans(anulacoes[,,3])*10, rowMeans(anulacoes[,,4])*10)

for (prova in 1:n.provas){
  for (turma in 1:n.turmas){
    dados.original[dados.original$Numero.prova==prova & dados.original$Turma==turmas[turma],]$Nota_prova <-  
      dados.original[dados.original$Numero.prova==prova & dados.original$Turma==turmas[turma],]$Nota_prova + 
      sum(anulacoes[turmas[turma],,prova])
  }
}

# Criando dataframe com as respostas dicotômicas de cada aluno:
respostas.dico <- dico()



# Cálculo dos estimadores dos parâmetros dos itens e do estimador do parâmetro de habilidade de
#cada aluno via Monte Carlo Cadeia de Markov:
mcmc <- mcmc()
mcmc.itens <- mcmc[["itens"]]
mcmc.theta <- mcmc[["thetas"]]


# Dataframes dos coeficientes estimados de cada questao
itens.p <- data.estimados()

# Tabela dos parametrôs de cada questão
itens <- rbind(cbind(itens.p[[1]], prova=1),
               cbind(itens.p[[2]], prova=2),
               cbind(itens.p[[3]], prova=3)) %>%
  rownames_to_column("aux") %>%
  mutate(tema=str_extract(aux, "[:alpha:][^\\d]*[:alpha:]"),
         n.tema=str_sub(aux, 1, 2),
         questao=str_sub(aux, str_length(aux)-5, str_length(aux)-4)) %>%
  dplyr::select(n.tema, tema, questao, prova, a, b, c) %>%
  mutate_at(c("a", "b", "c"), round3)

aux <- dados.original %>%
  filter(Numero.prova!=4) %>%
  dplyr::select(Matricula, Nome.questao, Acertou) %>%
  mutate(tema=str_extract(Nome.questao, "[:alpha:][^\\d]*[:alpha:]"),
         questao=str_sub(Nome.questao, str_length(Nome.questao)-5, str_length(Nome.questao)-4)) %>%
  dplyr::select(Matricula, tema, questao, Acertou) 

aux1 <- aux %>%
  group_by(tema, questao) %>%
  count()

aux2 <- aux %>%
  group_by(tema, questao, Acertou) %>%
  count() %>%
  ungroup() %>%
  filter(Acertou==1) %>%
  rename(acerto=n) %>%
  dplyr::select(tema, questao, acerto)

itens.parcial <- aux1 %>%
  left_join(aux2, by=c("tema", "questao"))
rm(aux1, aux2)


# dataframes para o gráfico de cuva caracteristica do item, informação do item e do teste.
cci <- NULL  
for (i in 1:101) {
  cci <- rbind(cci, itens) 
}
n.itens <- nrow(itens)
cci <- cci %>%
  arrange(n.tema, questao) %>%
  cbind(habilidade=rep(seq(-3, 3, length=101), n.itens)) %>%
  mutate(prob=P.acertar.logit(a, b, c, habilidade),
         fii=fii_cord(a, c, prob))

cci <- cci %>%
  filter((habilidade==-3 & prob>.4) |
           (habilidade==3 & prob<.8)) %>%
  dplyr::select(tema, questao) %>%
  mutate(Revisar="Sim") %>%
  left_join(cci, .) %>%
  replace_na(list(Revisar="Não")) %>%
  mutate(Revisar=factor(Revisar))

questoes.turma <- dados.original %>%
  dplyr::select(Turma, Nome.questao) %>%
  distinct() %>%
  mutate(tema=str_extract(Nome.questao, "[:alpha:][^\\d]*[:alpha:]"),
         questao=str_sub(Nome.questao,str_length(Nome.questao)-5, str_length(Nome.questao)-4)) %>%
  dplyr::select(tema,questao,Turma)

inf.teste.df <- cci %>%
  left_join(questoes.turma, by=c("tema", "questao")) %>%
  group_by(prova, habilidade, Turma) %>%
  summarise(teste=sum(fii)) %>%
  ungroup() %>%
  mutate(prova=paste0("Prova ", prova))
 

# Simulação com as probabilidades de que um aluno mediano acerte a questão para cada questão 
# selecionada em cada prova
Pm.probs <- vector(n.provas, mode="list")
names(Pm.probs) <- paste0("Prova", 1:n.provas)
Pm.probs <- sim.aluno.medio()


Pm.probs.means <- unlist(lapply(Pm.probs, colMeans)) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  rename(Prob=".", aux='rowname') %>%
  mutate(prova=paste0("Prova ",str_sub(aux,6,6)),
         tema=str_sub(aux,11,str_length(aux)-7),
         questao=str_sub(aux,str_length(aux)-5,str_length(aux)-4)) %>%
  dplyr::select(tema,prova,questao,Prob) %>%
  filter(prova!="Prova 4")

# juntando a probabilidade do aluno mediano acertar, numero de alunos que fizeram,
# e número de acertos a questao na tabela de itens
itens <- itens %>%
  dplyr::select(-prova) %>%
  left_join(Pm.probs.means, by=c("tema","questao")) %>%
  left_join(itens.parcial, by=c("tema", "questao")) %>%
  mutate("% acerto"=(acerto/n)*100) %>%
  dplyr::select(-acerto)

itens <- cci %>%
  filter(habilidade %in% c(-3, 3)) %>%
  dplyr::select(-prova, -a, -b, -c, -fii, -Revisar) %>%
  dcast(...~ habilidade, value.var="prob") %>%
  rename(prob_menor=`-3`, prob_maior=`3`) %>%
  mutate(prob_menor=round(prob_menor, 3),
         prob_maior=round(prob_maior, 3)) %>%
  left_join(itens, ., by=c("n.tema", "tema", "questao")) 

ordem.temas <- cci %>% 
  dplyr::select(tema) %>%
  unique()

#separando a tabela de itens por prova
itens_print <- vector(3, mode="list")
for (numero.prova in 1:3) {
  itens_print[[numero.prova]] <- itens %>%
  filter(prova==paste("Prova", numero.prova)) %>%
  dplyr::select(-prova)
}


# Probabilidade estimada (pontual) de acerto de cada questão feita por cada aluno
P.probs <- vector(n.provas, mode="list")
names(P.probs) <- paste0("Prova", 1:n.provas)
P.probs <- prob.acerto.questoes()


# Dataframes de cada prova com a probabilidade de acerto por tema (questão que o aluno fez do tema) para cada aluno 
Prob.tema <- vector(3, mode="list")
for (prova in 1:3) {
  Prob.tema[[prova]] <- prob.tema(prova)
  Prob.tema[[prova]]$Matricula <- colnames(mcmc.theta[[prova]])
}

Prob.acerto.tema <- full_join(Prob.tema[[1]], Prob.tema[[2]], by="Matricula")
Prob.acerto.tema <- full_join(Prob.acerto.tema, Prob.tema[[3]], by="Matricula")
rownames(Prob.acerto.tema) <- Prob.acerto.tema[,11]
Prob.acerto.tema <- Prob.acerto.tema[,-11]

# Dataframe com os acertos de cada aluno em cada tema
Acerto.tema <- vector(3, mode="list")
for (prova in 1:3) {
  Acerto.tema[[prova]] <- acertou.tema(prova)
  Acerto.tema[[prova]]$Matricula <- colnames(mcmc.theta[[prova]])
}

Acerto.tema.aux <- full_join(Acerto.tema[[1]], Acerto.tema[[2]], by="Matricula")
Acerto.tema <- full_join(Acerto.tema.aux, Acerto.tema[[3]], by="Matricula")
rownames(Acerto.tema) <- Acerto.tema[,11]
Acerto.tema <- Acerto.tema[,-11]

# Cálculo das componentes do desvio
cd <- matrix(0, nrow=nrow(Acerto.tema), ncol=30)
cd <- c.d()

# Correlações entre os temas
cor.tema <- as_matrix(correlate(cd))
diag(cor.tema) <- 0

# # Clusterização k-means
cor.tema <- pmax(cor.tema, 0)
cluster <- kmeans(t(cor.tema), 3)


# Array final para o balanceamento das provas, com o número de simulações, turmas, questões,
#provas e vetor com a probabilidade de acerto e o resultado da binomial se acertou ou não:
dados.balanceamento <- balanceamento()

# # Array final 
# dados.finais <- vector(n.provas, mode="list")
# dados.finais <- dados.fim()


# Nota esperada de um aluno mediano por prova
soma.acerto <- apply(dados.balanceamento[,,,,2], c(1,2,4), sum)
NE.mediano <- nota.esperada.mediano()
NE.mediano[,5] <- dimnames(mapa.questoes)[[1]]



data.g <- data.frame(Nota.esperada=c(as.numeric(NE.mediano[,1]), as.numeric(NE.mediano[,2]),
                                     as.numeric(NE.mediano[,3]), as.numeric(NE.mediano[,4])),
                     Turma=c(rep(dimnames(mapa.questoes)[[1]],4)), Prova=rep(c(dimnames(mapa.questoes)[[3]]),each=10))

prob.mediano.passar <- Prob.mediano.passar()


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
N.passou.hat <- n.passou.hat()

# Dataframes dos alunos que passaram, em cada prova
Passou <- vector(n.provas, mode="list")
Passou <- passou()

# Dataframes dos alunos que não passaram, em cada prova 
N.passou <- vector(n.provas, mode="list")
N.passou <- n.passou()

# Construção de matriz das notas finais
nota.prova <- matrix(0, ncol = n.provas+1, nrow = length(unique(dados.original$Matricula)))
nota.prova[,1] <- unique(dados.original$Matricula)
nota.prova <- data.frame(nota.prova)
colnames(nota.prova) <- c("Matricula", "Prova 1", "Prova 2", "Prova 3", "Prova 4")
nota.prova <- nota.prova %>% arrange(Matricula)
for (aluno in 1:nrow(nota.prova)){
  np <- unique(dados.original[dados.original$Matricula==nota.prova[aluno,1], 
                              c("Matricula", "Nota_prova", "Numero.prova")])
  nota.prova[aluno,2] <- ifelse(length(np[np$Numero.prova==1, "Nota_prova"])>0, 
                                np[np$Numero.prova==1, "Nota_prova"], 0)
  nota.prova[aluno,3] <- ifelse(length(np[np$Numero.prova==1, "Nota_prova"])>0, 
                                np[np$Numero.prova==2, "Nota_prova"], 0)
  nota.prova[aluno,4] <- ifelse(length(np[np$Numero.prova==1, "Nota_prova"])>0, 
                                np[np$Numero.prova==3, "Nota_prova"], 0)
  nota.prova[aluno,5] <- ifelse(length(np[np$Numero.prova==1, "Nota_prova"])>0, 
                                np[np$Numero.prova==4, "Nota_prova"], 0)
}
nota.prova[is.na(nota.prova)] <- 0

media.prova <- matrix(0, ncol = n.provas, nrow = length(nota.prova[,1]))
media.prova[,1] <- nota.prova[,1]
media.prova[,2] <- nota.prova[,2]
media.prova[,3] <- .5*nota.prova[,2] + .5*nota.prova[,3]
media.prova[,4] <- .3*nota.prova[,2] + .3*nota.prova[,3] + .4*nota.prova[,4]
media.prova <- data.frame(media.prova)
colnames(media.prova) <- c("Matricula", "Prova1", "Prova2", "Prova3")

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

mencao.m.prova[,1] <- media.prova[,1]
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

# Dataframe com as probabilidades das simulações com theta mediano passar em PE em cada turma
sim.passar <- Sim.passar()

# Probabilidade de um aluno mediano passar em PE por turma
P.sim.passar <- array(dim=c(1,n.turmas), 
                      dimnames=list("Probabilidade", dimnames(mapa.questoes)[[1]]))
P.sim.passar <- p.sim.passar()

#
P.sim.passar <- t(data.frame(P.sim.passar))
P.sim.passar <- data.frame(P.sim.passar,Turma=c(dimnames(mapa.questoes)[[1]]), Linha=c(rep(1,10)))


#estimadas por TRI  
# Construção de matriz das notas finais estimadas

np1 <- as.data.frame(notas.estimadas[[1]])
np2 <- as.data.frame(notas.estimadas[[2]])
np3 <- as.data.frame(notas.estimadas[[3]])
np4 <- as.data.frame(notas.estimadas[[4]])

nota.prova.estimada <- matrix(0, ncol = n.provas+1, nrow = length(unique(dados.original$Matricula)))
nota.prova.estimada[,1] <- unique(dados.original$Matricula)
nota.prova.estimada <- data.frame(nota.prova.estimada)
colnames(nota.prova.estimada) <- c("Matricula", "Prova 1", "Prova 2", "Prova 3", "Prova 4")
nota.prova.estimada <- nota.prova.estimada %>% arrange(Matricula)
for (aluno in 1:nrow(nota.prova.estimada)){
  nota.prova.estimada[aluno, 2] <- ifelse(length(np1[np1$Matricula==nota.prova.estimada[aluno,1],
                                                    "Nota Estimada"])>0, 
                                         np1[np1$Matricula==nota.prova.estimada[aluno,1],
                                             "Nota Estimada"], 0)
  nota.prova.estimada[aluno, 3] <- ifelse(length(np2[np2$Matricula==nota.prova.estimada[aluno,1],
                                                    "Nota Estimada"])>0, 
                                         np2[np2$Matricula==nota.prova.estimada[aluno,1],
                                             "Nota Estimada"], 0)
  nota.prova.estimada[aluno, 4] <- ifelse(length(np3[np3$Matricula==nota.prova.estimada[aluno,1],
                                                    "Nota Estimada"])>0, 
                                         np3[np3$Matricula==nota.prova.estimada[aluno,1],
                                             "Nota Estimada"], 0)
  nota.prova.estimada[aluno, 5] <- ifelse(length(np4[np4$Matricula==nota.prova.estimada[aluno,1],
                                                    "Nota Estimada"])>0, 
                                         np4[np4$Matricula==nota.prova.estimada[aluno,1],
                                             "Nota Estimada"], 0)
}


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


# Ordenação das notas para atribuição de menções finais via TRI
notas.ord <- notas.finais[order(notas.finais$Nota_final),]
notas.ord.tri <- notas.finais.estimadas[order(notas.finais.estimadas$Nota_final),-7]

len.men <- c(length(notas.ord$Mencao[notas.ord$Mencao=="II"]), 
             length(na.exclude(notas.ord$Mencao[notas.ord$Mencao=="MI"])),
             length(na.exclude(notas.ord$Mencao[notas.ord$Mencao=="MM"])),
             length(na.exclude(notas.ord$Mencao[notas.ord$Mencao=="MS"])),
             length(na.exclude(notas.ord$Mencao[notas.ord$Mencao=="SS"])))
notas.ord.tri$Mencao <- c(rep("II", len.men[1]),
                          rep("MI", len.men[2]),
                          rep("MM", len.men[3]),
                          rep("MS", len.men[4]),
                          rep("SS", len.men[5]))

notas.ord <- notas.ord %>% arrange(Matricula)
notas.ord.tri <- notas.ord.tri %>% arrange(Matricula)

# Nova variavel para agrupar os cursos
dados.original$Grupo[str_detect(dados.original$Curso,"Comp")] <- 'Computação'
dados.original$Grupo[str_detect(dados.original$Curso,"Engenharia ")] <- 'Engenharia'
dados.original$Grupo[str_detect(dados.original$Curso,"Educ")] <- 'Ed. Física'
dados.original$Grupo[str_detect(dados.original$Curso,"Econ")] <- 'Econômicas'
dados.original$Grupo[str_detect(dados.original$Curso,"Cont")] <- 'Contábeis'
dados.original$Grupo[str_detect(dados.original$Curso,"Outros")] <- 'Outros'
dados.original$Grupo[is.na(dados.original$Curso)] <- 'NA'




# Comparação das mencoes finais dos alunos que fizeram subs antes e depois desta
nota.3 <- notas.finais[notas.finais$Matricula %in% notaaluno[[3]][,1][notaaluno[[3]][,1] %in% notaaluno[[4]][,1]],4]
nota.subs <- notas.finais[notas.finais$Matricula %in% notaaluno[[3]][,1][notaaluno[[3]][,1] %in% notaaluno[[4]][,1]],5]

mencao.3 <- mencao.m.prova[mencao.m.prova$Matricula %in% notaaluno[[3]][,1][notaaluno[[3]][,1] %in% notaaluno[[4]][,1]],4]
mencao.subs <- notas.finais[notas.finais$Matricula %in% notaaluno[[3]][,1][notaaluno[[3]][,1] %in% notaaluno[[4]][,1]],7]


#
indices.sub <- notas.finais$'Prova 4' > 0
tabela1 <- as.data.frame(table(mencao.m.prova[indices.sub,4], notas.finais[indices.sub,7]))
colnames(tabela1) <- c("MencaosemP4", "MencaoposP4", "Quantidade")
class(tabela1$MencaosemP4) <- "numeric"
class(tabela1$MencaoposP4) <- "numeric"
class(tabela1$Quantidade) <- "numeric"


# Textos
texto.subs <- paste0(tabela1[6,3], " alunos aumentaram a menção de II para MI, ",
                    tabela1[12,3], ", de MI para MM, ",
                    tabela1[17,3], ", de MI para MS e ",
                    tabela1[18,3], ", de MM para MS")

# Medidas decritivas
turma <- list(dados.original$Turma)
medturma <- aggregate(dados.original[,"Nota_prova"],turma,mean)
medturmaprova <- aggregate(dados.original[,"Nota_prova"],list(dados.original$Turma,dados.original$Numero.prova),mean)
colnames(medturmaprova) <- c("Turma", "Prova", "Media")
medcurso <- aggregate(dados.original[,"Nota_prova"],list(dados.original$Curso),mean)
medcurso <- medcurso %>% arrange(x)
medprova <- aggregate(dados.original[,"Nota_prova"],list(dados.original$Numero.prova),mean)
medano <- aggregate(dados.original[,"Nota_prova"],list(dados.original$Ano),mean)
mediaaluno <- aggregate(dados.original[,"Nota_prova"],list(dados.original$Matricula),mean)
mediaaluno.prova <- aggregate(dados.original[,"Nota_prova"],list(dados.original$Matricula,dados.original$Numero.prova),mean)


# Para os gráficos
dados.original1 <- dados.original
dados.original1$Curso <- factor(dados.original1$Curso, 
                                levels = c(as.character(unique(dados.original$Curso[str_detect(dados.original$Curso,"Outros")])[-2]),
                                          as.character(unique(dados.original$Curso[str_detect(dados.original$Curso,"Comp")])[-1]),   
                                          as.character(unique(dados.original$Curso[str_detect(dados.original$Curso,"Ciências")])),
                                          as.character(unique(dados.original$Curso[str_detect(dados.original$Curso,"Engenharia ")]))))
    
dados.original1 <- dados.original1[complete.cases(dados.original1[,"Grupo"]),]
    
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
d.tema <- vector(n.provas, mode="list")
data <- vector(n.provas, mode="list")
tema <- vector(n.provas, mode="list")
turma <- vector(n.provas, mode="list")
datan <- vector(n.provas, mode="list")
data1 <- vector(n.provas, mode="list")

for (prova in 1:n.provas){
  d.tema[[prova]] <- dplyr::filter(dados.original, Numero.prova==prova) %>% 
    dplyr::select(Matricula, Acertou, Tema, Turma) 
  
  data[[prova]] <- data.frame(tapply(d.tema[[prova]]$Acertou, list(d.tema[[prova]]$Turma, d.tema[[prova]]$Tema), sum))
  
  tema[[prova]] <- colnames(data[[prova]])
  turma[[prova]] <- rownames(data[[prova]])
  datan[[prova]] <- matrix(0, ncol=ncol(data[[prova]]), nrow=nrow(data[[prova]]))
  
  for (i in 1:n.turmas){
    for (r in 1:10)
      datan[[prova]][i,r] <- length(d.tema[[prova]]$Tema[d.tema[[prova]]$Turma==turma[[prova]][i]][d.tema[[prova]]$Tema[d.tema[[prova]]$Turma==turma[[prova]][i]]==tema[[prova]][r]])
  }
  
  data[[prova]] <- data[[prova]]/datan[[prova]]
  data[[prova]] <- rbind(data[[prova]], colMeans(data[[prova]], na.rm=T))
  rownames(data[[prova]])[11] <- 'Média geral'
  data[[prova]] <- rbind(rep(1,10), rep(0,10) , data[[prova]])
}

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


links <- data.frame(
  source=c("SS1", "SS1", "SS1", "SS1", 
           "MS1", "MS1", "MS1", "MS1", 
           "MM1", "MM1", "MM1", "MM1",
           "MI1", "MI1", "MI1", "MI1", 
           "II1", "II1", "II1",        
           "SS2", "SS2", "SS2", "SS2",
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
           "II2", "MI2", "MM2",        
           "SS3", "MS3", "MM3", "MI3",               
           "SS3", "MS3", "MM3", "MI3",                                                              "MS3", "MM3", "MI3", "II3",                                                              "MS3", "MM3", "MI3", "II3",                                                              "MM3", "MI3", "II3",
           "SS4", "MS4", "MM4", "MI4",               
           "SS4", "MS4", "MM4", "MI4",                                                              "MS4", "MM4", "MI4", "II4",                                                              "MS4", "MM4", "MI4", "II4",                                                              "MM4", "MI4", "II4"), 
  value=c(prob.v))

links <- links %>% 
  filter(value>0)

nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique(),
  name2=substring(c(as.character(links$source), 
                    as.character(links$target)) %>% unique(),
                  1, nchar(c(as.character(links$source), as.character(links$target)) %>% 
                             unique())-1))

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1
links$energy_type <- as.character(links$source)




color_scale <- data.frame(
  range = c(rep(c("darkcyan", "darkcyan", "darkcyan", "gray", "gray"), 4)),
  domain = c("SS1", "MS1", "MM1", "MI1", "II1", "SS2", "MS2", "MM2", "MI2", "II2",
            "SS3", "MS3", "MM3", "MI3", "II3", "SS4", "MS4", "MM4", "MI4", "II4"),
  nodes = nodes,
  stringsAsFactors = FALSE
)




#
tabela <- as.data.frame(table(notas.ord$Mencao, notas.ord.tri$Mencao))
colnames(tabela) <- c("Classico", "TRI", "Quantidade")
class(tabela$Classico) <- "numeric"
class(tabela$TRI) <- "numeric"
class(tabela$Quantidade) <- "numeric"
tabela[, 1:2] <- tabela[, 1:2] - 1




sankey <- sankeyNetwork(Links = links, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name2", 
              LinkGroup='energy_type', colourScale = JS(
                sprintf(
                  'd3.scaleOrdinal() .domain(%s)
                                    .range(%s)',
                  jsonlite::toJSON(color_scale$domain),
                  jsonlite::toJSON(color_scale$range)
                )
              ), 
              fontSize=50, 
              iteration=0)
savePlotAsPng(sankey, file="sankey.png",
              width=2800, height=1000)
    
tabela_resumo <- tabela.resumo()

    
# Salvando os dados 
save.image(file = "DadosPE.RData")
