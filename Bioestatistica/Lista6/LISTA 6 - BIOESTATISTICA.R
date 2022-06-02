#Lista 6 - Comparação de variâncias, médias e proporções

#3) Num torneio de voleibol, a recuperacao energetica dos jogadores apos uma
#partida e de suma importancia. Um grupo de nutricionistas desenvolveu 2
#cardapios com o objetivo de melhorar a recuperacao energetica (%) e
#diagnosticar qual dieta foi eficiente nesta recuperacao. 
#Foram observados 5 atletas para cada dieta, obtendo-se os seguintes resultados:

# Decida se as dietas sao equivalentes ou se existe uma melhor, ao nivel de 5% de significancia.

## Calculo da variancia das dietas a e b

dietaa<-c(45,51,50,62,43)
dietab<-c(45,35,43,59,48)
var(dietaa)
var(dietab)
var(na.omit(dietab))

# TESTE F PARA AS VARIANCIAS
### H0: varA = varB (varA/varB=1)
### H1: varA dif varB (varA/varB dif 1)
#Significancia: 0.05
#conf.level = 0.95
var.test(dietab,dietaa,conf.level=0.95, alternative='two.sided')
# Como valor-p > alfa => Aceitar H0

#TESTE T PARA COMPARACAO DE MEDIAS
#H0: mA = mB (mA-mB=0)
#H1: mA > mB (mA-mB>0)
#Significancia: 0.05
#conf.level = 0.95
t.test(dietaa,dietab,conf.level=0.95, alternative='two.sided', paired=FALSE, var.equal=TRUE)
# Como valor-p > alfa => Aceitar H0

#4) O departamento de marketing de uma grande indústria de alimentos está fazendo um estudo para mudar 
# a embalagem de seu macarrão instantâneo. Duas embalagens (A e B) foram avaliados, quanto à aceitação, 
# por 10 consumidores desse produto. A escala utilizada foi: 
# 1 (péssimo), 2 (ruim), 3 (regular), 4 (bom) e 5 (ótimo). Os dados amostrais seguem.

#4c) Com o α escolhido, infira sobre a igualdade das variâncias populacionais

embalagema<-c(3,3,4,4,4,4,4,5,5,5)
embalagemb<-c(3,3,3,3,2,2,2,1,1,1)
var(embalagema)
var(embalagemb)
var(na.omit(embalagemb))

# TESTE F PARA AS VARIANCIAS
#H0: varA = varB (varA/varB=1)
#H1: varA dif varB (varA/varB dif 1)
#SignificAncia: 0.1
#conf.level = 0.90
var.test(embalagemb,embalagema,conf.level=0.90, alternative='two.sided')
# Como valor-p > alfa => Aceitar H0

#De posse da decisão da letra (c), compare as notas médias das embalagens, 
# também adotando o mesmo nível de significância.

# TESTE T PARA MEDIAS
#H0: mA = mB (mA-mB=0)
#H1: mA > mB (mA-mB>0)
#Significancia: 0.1
#conf.level = 0.90
t.test(embalagema,embalagemb,conf.level=0.90, alternative='two.sided', paired=FALSE, var.equal=TRUE)
# Como valor-p < alfa => Rejeitar H0


#5) Dois irmãos gostam da mesma marca de carro. Então, decidiram comprar
# dois exemplares idênticos, ambos zero quilômetros. Um deles viaja muito e sempre
# anota o consumo do carro na estrada. Por sua vez, o outro nunca viaja, mas anota o
# consumo do carro dentro da cidade. Os dadosa seguir estão em km/L de gasolina.

# Compare o consumo na estrada e na cidade, verificando se os consumos médios são iguais.

#CALCULO DA VARIANCIA

cidade<-c(7.7,7.4,9.3,8.5,10,8.9,10,9.2,8.8,8.9)
estrada<-c(12.3,10.2,10.9,11.9,10.9,11.5,11.7,9.6,10.7,11)
var(cidade)
var(estrada)
var(na.omit(estrada))

# TESTE F PARA AS VARIANCIAS
#H0: varA = varB (varA/varB=1)
#H1: varA dif varB (varA/varB dif 1)
#Significancia: 0.05
#conf.level = 0.95
var.test(cidade,estrada,conf.level=0.95, alternative='two.sided')
# Como valor-p > alfa => Aceitar H0

# TESTE T PARA MEDIAS
#H0: mA = mB (mA-mB=0)
#H1: mA > mB (mA-mB>0)
#Significancia: 0.05
#conf.level = 0.95
t.test(cidade,estrada,conf.level=0.95, alternative='two.sided',  paired=FALSE, var.equal=TRUE)
# Como valor-p < alfa => Rejeitar H0

#6) Em um estudo sobre leptospirose em gambás no Município de São Paulo (Bertola, 2006), 12 gambás
# apresentaram teste diagnóstico positivo em um total de 116 gambás testados localizados na Zona Sul. Na
# Zona Leste, foram observados 9 gambás positivos em um total de 24 gambás testados. Verifique se há
# diferença significativa entre as proporções de animais positivos nas duas regiões.

#Estimacao pontual
(xA<-12)
(xB<-9)
(nA<-116)
(nB<-24)
(pA<-xA/nA)
(pB<-xB/nB)

#TESTE QUI-QUADRADO
#H0: pA=pB (pA-pB=0)
#H1: pA dif pB (pA-pB dif 0)
#Significancia: 0.5
#conf.level=0.95
prop.test(c(xB,xA),c(nB,nA), conf.level = 0.95, correct = FALSE)
# Como houve erro na aproximação do teste qui-quadrado, fazer o teste z para as proporções

#TESTE Z
zc<-(pB-pA) / sqrt(pA*(1-pA)/nA + pB*(1-pB)/nB)

2*pnorm(zc,0,1,lower.tail = F)
#Como p-valor é menor que alpha (0.05), rejeita-se H0 com 5% de significância. 
# As proporções de gambás positivados são distintas.

#7) Em um estudo sobre leptospirose em capivaras (Shimabukuro, 2006), foram observados 27 animais
# positivos na calha do Rio Tietê na região metropolitana de São Paulo, em um total de 45 animais testados.
# Como grupo-controle, foram observadas capivaras de outras regiões do Estado de São Paulo. Nesse
# grupo, de um total de 34 animais testados, 12 foram positivos. Verifique se há diferença significativa
# entre as proporções de animais positivos nos dois grupos.

#Estimacaoo pontual
(xA<-27)
(xB<-12)
(nA<-45)
(nB<-34)
(pA<-xA/nA)
(pB<-xB/nB)

#TESTE QUI-QUADRADO
#H0: pA=pB (pA-pB=0)
#H1: pA dif pB (pA-pB dif 0)
#Significancia: 0.5
#conf.level = 0.95
prop.test(c(xA,xB),c(nA,nB), conf.level = 0.95, correct = FALSE)
# Como p-valor é menor que alpha (0.05), rejeita-se H0 com 5% de significância.
# As proporções de capivaras positivadas são distintas

#TESTE Z
#H0: pA=pB (pA-pB=0)
#H1: pA dif pB (pA-pB dif 0)
#Significancia: 0.5
#conf.level = 0.95
zc<-(pA-pB) / sqrt(pA*(1-pA)/nA + pB*(1-pB)/nB)
2*pnorm(zc,0,1, lower.tail = F)
prop.test(c(xA,xB),c(nA,nB), conf.level = 0.95, correct = FALSE)
# Como p-valor é menor que alpha (0.05), rejeita-se H0 com 5% de significância.
# As proporções de capivaras positivadas são distintas