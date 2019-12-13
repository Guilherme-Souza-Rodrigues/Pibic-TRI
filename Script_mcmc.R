library(readr)
library(tidyverse)
library(rstan)
setwd("D:/PIBIC_GUILHERME")
model <- stan_model("TRI.stan")
###################################################
dados<- read.csv("D:/PIBIC_GUILHERME/Banco_respostas.csv",
                            sep=";", stringsAsFactors=FALSE)
banco_respostas <- dados%>%
  dplyr::select(Matricula,Nome.questao,Acertou,Numero.prova)
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
                                          y=dados_p1$Acertou),iter=4000,chains=4,cores=4)
a_p1 <-cbind(Questoes_equivalencia_p1,a=colMeans(extract(fit.p1)$a),prova=1)
b_p1 <-cbind(Questoes_equivalencia_p1,b=colMeans(extract(fit.p1)$b),prova=1)
c_p1 <-cbind(Questoes_equivalencia_p1,c=colMeans(extract(fit.p1)$c),prova=1)
theta_p1 <-cbind(Matriculas_equivalencia_p1,theta1=colMeans(extract(fit.p1)$theta))
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
                                          y=dados_p2$Acertou),iter=4000,chains=4,cores=4)
a_p2 <-cbind(Questoes_equivalencia_p2,a=colMeans(extract(fit.p2)$a),prova=2)
b_p2 <-cbind(Questoes_equivalencia_p2,b=colMeans(extract(fit.p2)$b),prova=2)
c_p2 <-cbind(Questoes_equivalencia_p2,c=colMeans(extract(fit.p2)$c),prova=2)
theta_p2 <-cbind(Matriculas_equivalencia_p2,theta2=colMeans(extract(fit.p2)$theta))
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
                                          y=dados_p3$Acertou),iter=4000,chains=4,cores=4)
a_p3 <-cbind(Questoes_equivalencia_p3,a=colMeans(extract(fit.p3)$a),prova=3)
b_p3 <-cbind(Questoes_equivalencia_p3,b=colMeans(extract(fit.p3)$b),prova=3)
c_p3 <-cbind(Questoes_equivalencia_p3,c=colMeans(extract(fit.p3)$c),prova=3)
theta_p3 <-cbind(Matriculas_equivalencia_p3,theta3=colMeans(extract(fit.p3)$theta))
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
                                          y=dados_p4$Acertou),iter=4000,chains=4,cores=4)
a_p4 <-cbind(Questoes_equivalencia_p4,a=colMeans(extract(fit.p4)$a),prova=4)
b_p4 <-cbind(Questoes_equivalencia_p4,b=colMeans(extract(fit.p4)$b),prova=4)
c_p4 <-cbind(Questoes_equivalencia_p4,c=colMeans(extract(fit.p4)$c),prova=4)
theta_p4 <-cbind(Matriculas_equivalencia_p4,theta4=colMeans(extract(fit.p4)$theta))
rm(Questoes_equivalencia_p4,Matriculas_equivalencia_p4,dados_p4,fit.p4)
#########################
#  Juntando tudo
#########################
a <- rbind(a_p1,a_p2,a_p3,a_p4)
b <- rbind(b_p1,b_p2,b_p3,b_p4)
c <- rbind(c_p1,c_p2,c_p3,c_p4)

theta <- left_join(theta_p1,theta_p2,"Matricula")%>%
  left_join(theta_p3,"Matricula")%>%
  left_join(theta_p4,"Matricula")%>%
  dplyr::select(Matricula,theta1,theta2,theta3,theta4)
rm(a_p1,a_p2,a_p3,a_p4,
   b_p1,b_p2,b_p3,b_p4,
   c_p1,c_p2,c_p3,c_p4,
   theta_p1,theta_p2,theta_p3,theta_p4)
alunos <- theta

itens <- cbind(a[,c("Nome.questao","prova","a")],b=b[,"b"],c=c[,"c"])%>%
  dplyr::select(Nome.questao,a,b,c,prova)%>%
  mutate(Nome.questao=str_remove(Nome.questao,".Rnw"))%>%
  mutate(questao=str_sub(Nome.questao,str_length(Nome.questao)-1),
         tema=str_sub(Nome.questao,4,str_length(Nome.questao)-3))%>%
  dplyr::select(-Nome.questao)

itens <- itens[,c("tema","questao","a","b","c","prova")]%>%
  arrange(prova)

itens_tabela <- itens

itens.p <- list(subset(itens,prova==1,select = -prova),
            subset(itens,prova==2,select = -prova),
            subset(itens,prova==3,select = -prova),
            subset(itens,prova==4,select = -prova))





