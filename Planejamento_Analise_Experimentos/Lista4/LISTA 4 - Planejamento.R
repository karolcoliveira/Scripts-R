# Lista 4 das disciplina Analise e Planejamento de Experimentos

#Exercício1
#(Passo 1) Dados do tratamento e variável-resposta
Rações <- c ("A","A","A","A","A","B","B","B","B","B","C","C","C","C","C","D","D","D","D","D","E","E","E","E","E")
Peso <- c (870, 1050, 850, 800, 1040, 1310, 1520, 1490, 1300, 1440, 1250, 1010, 1010, 1210, 1030, 900, 1130, 1050, 1075, 880, 990, 1250, 1000, 1265, 1050)

#(Passo 2) Análise de Variância
library(ExpDes.pt)

Raç <- dic(Rações, Peso, quali = TRUE, mcomp = "tukey", hvar='bartlett', sigT = 0.05, sigF = 0.05)

#(Passo 3) Análise dos Resíduos
plotres(Raç)

#Exercício2
#(Passo 1) Dados do tratamento e variável-resposta

Tratamento <- c ("C","C","C","C","C","C","1","1","1","1","1","1","2","2","2","2","2","2")
Ensaio <- c ("1","2","3","4","5","6","1","2","3","4","5","6","1","2","3","4","5","6")
Resposta <- c (1147, 1273, 1216, 1046, 1108, 1265, 1169, 1323, 1276, 1240, 1432, 1562, 1009, 1260, 1143, 1099, 1385, 1164)

#(Passo 2) Análise de Variância com Tukey
RespFarm <- dbc(Tratamento, Ensaio, Resposta, quali = TRUE, mcomp = "tukey", hvar="oneillmathews", sigT = 0.05, sigF = 0.05)

# Análise de Variância com Duncan
RespFarm <- dbc(Tratamento, Ensaio, Resposta, quali = TRUE, mcomp = "duncan", hvar="oneillmathews", sigT = 0.05, sigF = 0.05)

#(Passo 3) Análise dos Resíduos
plotres(RespFarm)

#Exercício3
#(Passo 1) Carregar dados do tratamento e variável-resposta
setwd ("/Users/Karol/Documents/FACULDADE/SÉTIMO PERÍODO/PLANEJAMENTO E ANÁLISE DE EXPERIMENTO")
dados <-read.table(file.choose(), header = TRUE, dec = ".")

#(Passo 2) Análise de Variância com Duncan
Tempodemontagem <-dql(dados$Método, dados$Ordem, dados$Operador, dados$Tempo, quali = TRUE, mcomp = "duncan", sigT = 0.05, sigF = 0.05)

#(Passo 3) Análise dos Resíduos
plotres(Tempodemontagem)

#Exercício4
#(Passo 1) Carregar dados do tratamento e variável-resposta
dados <-read.table(file.choose(), header = TRUE, dec = ".")

#(Passo 2) Análise de Variância com Tukey
library(ExpDes.pt)
atividade<-fat2.dbc(dados$Linhagem, dados$Bloco, dados$Atividade, quali = c(TRUE, TRUE), mcomp = "tukey", fac.names = c("Linhagem", "Bloco"), sigT = 0.05, sigF = 0.05)

#(Passo 3) Análise dos Resíduos 
plotres(atividade)