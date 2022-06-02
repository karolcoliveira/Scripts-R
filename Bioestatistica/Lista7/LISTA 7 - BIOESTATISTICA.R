#Lista 7 - Correlacao e Regressao da disciplina EAB052 - Bioestatistica

#2) Foi feito um estudo sobre o peso final (Y) de peixes tratados com doses extrasde racao (X)

#2b) Faca um diagrama de dispersao para os dados.
peso<-c(495,560,590,620,615)
racao<-c(0,5,10,15,20)
plot(racao,pch=19)
plot(racao,peso,pch=19)

#2c) Estime o coeficiente de correlacao linear de Pearson (ρ) e interprete-o.
cor.test(peso,racao)

#2d) Estime a equacao de regressao que melhor se ajusta aos dados.
reta<-lm(peso~racao)
summary(reta)

coef<-summary(reta)$coefficients[,1]
anova(reta)
range(racao)

#2e) Plote a equacao estimada no grafico de dispersao.
x<-0:20
y<-coef[1] + coef[2]*x
plot(x,y,'l',xlab='racao',ylab='peso')
points(racao,peso,pch=15)
legend("topright", bty='n', legend = expression(paste('Y = 516 + 6X', R^2,'=0.86')))

#3) Procurou-se realizar um estudo com o objetivo de saber o efeito na producao 
# de massa muscular de um grupo de pessoas alimentadas com diferentes teores de
# proteina. Foram obtidos os seguintes dados:

#3b) Faca um diagrama de dispersao para os dados.
proteina<-c(10,12,14,16,18,20,22)
massa<-c(11.8,10.2,12.1,13.2,12.1,15.4,15.6)
plot(proteina,pch=19)
plot(proteina,massa, pch=19)

#3c) Estime o coeficiente de correlacao linear de Pearson (ρ) e interprete-o.
cor.test(proteina,massa)

#3d) Estime a equacao de regressao que melhor se ajusta aos dados.
reta<-lm(massa~proteina)
summary(reta)

coef<-summary(reta)$coefficients[,1]
anova(reta)
range(proteina)

#3e) Plote a equacao estimada no grafico de dispersao.
x<-0:25
y<-coef[1] + coef[2]*x
plot(x,y,'l',xlab='proteina',ylab='massa')
points(proteina,massa,pch=15)
legend("topright", bty='n', legend = expression(paste('Y = 6.6857 + 0.3893X', R^2,'=0.7251')))