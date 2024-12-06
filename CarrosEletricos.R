
###############################################################################
###############################################################################
###############################################################################

# Projeto Feedback 01
# Curso: Big Analytics com R e Microsoft Azure Machine Learning 3
#Formação Cientista de Dados 3.0
#Aluna: Fabiana Martins da Silva
#Contato: ms.fabiana@yahoo.com.br

#Objetivo: Prever o consumo de energia de carros elétricos com base
#em diversos fatores de utilização e características dos veículos

###############################################################################
###############################################################################
###############################################################################

#Bibliotecas necessárias para o script
library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)
library(corrplot)
library(caret)

#Carregamento do dataset em excel
dados = read_excel("FEV-data-Excel.xlsx")
View(dados)

#Informações sobre o dataset
str(dados) # 53 linhas e 25 colunas

#Melhorando os nomes das colunas, retirando espaços
colnames(dados)
colnames(dados)<-c("CarFullName", "Make", "Model", "MinimalPricePLN", "EnginePower_KM", 
                   "MaximumTorque_Nm", "TypeBrakes", "DriveType", "BatteryVCapacity_kWh", 
                   "RangeWLTP_km", "Wheelbase_cm", "Length_cm", "Width_cm", "Height_cm", 
                   "MinimalEmptyWeight_kg", "PermissableGrossWeight_kg", "MaximumLoadCapacity_kg",
                   "NumberSeats", "NumberDoors", "TireSize_in", "MaximumSpeed_Kph", "BootCapacityVDA_l", 
                   "Acceleration0_100kph_s", "MaximumDCChargingPower_kW", "meanEnergyConsumption_kWh_100_km")
colnames(dados)

View(dados)
sum(is.na(dados)) #o dataset possui valores NA

#Vendo as linhas que possuem nulos. Ao todo 11 linhas
View(dados%>% filter(!complete.cases(dados)))

#Vendo as colunas que possuem nulos
#TypeBrakes 1
#PermissableGrossWeight_kg 8
#MaximumLoadCapacity_kg 8
#BootCapacityVDA_l 1
#Acceleration0_100kph_s 3
#meanEnergyConsumption_kWh_100_km  9
sapply(dados, function(y) sum(is.na(y)))

#A variável target (meanEnergyConsumption_kWh_100_km) está nula em 9 linhas.
#Sem esse dado histórico, as linhas não servirão para fazer o treinamento e portanto
#vamos deletar estas 9 linhas 
dados<-dados%>%filter(!is.na(dados$meanEnergyConsumption_kWh_100_km))

#Agora vamos ver como ficou a situação dos NAs novamente
sum(is.na(dados)) #o dataset ainda possui valores NA

#Vendo as linhas que possuem nulos. Ao todo 2 linhas
View(dados%>% filter(!complete.cases(dados)))

#Vendo as colunas que possuem nulos
#TypeBrakes 1
#BootCapacityVDA_l 1
#Acceleration0_100kph_s 2
sapply(dados, function(y) sum(is.na(y)))


################################################################################
#Vamos verificar os dois casos de carros que tem campos NA
#Um é da Mercedes e outro é da Nissan
#O da Nissan achei um catálogo na internet que dá a informação de 14 segundos
#(https://www.nissan-cdn.net/content/dam/Nissan/pt/brochures/E-Catalago_e-NV200_PT.pdf)
#Assim sendo preencherei o campo
dados$Acceleration0_100kph_s[is.na(dados$Acceleration0_100kph_s)&dados$Make=="Nissan"]=14

#A mercedes possue dois representantes com características bem diferentes
#Mas não achei catálogo do modelo que tem campos NA então ele será deletado
dados = na.omit(dados)

#Informações finais de shape sobre o dataset que iremos trabalhar
str(dados)#43 linhas e 25 colunas


######Variáveis categóricas######
colunas = sapply(dados, typeof)
cats = colnames(dados[colunas=="character"])
cats

#A coluna 01 parece ser uma combinação da coluna 02 (fabricante)
#com a coluna 03 (Modelo)
#O código abaixo testa esta hipótese, que é verdadeira
combina <- dados%>%
           select(Make, Model)%>%
           mutate(paste(Make, Model, sep = " "))
identical(dados$CarFullName, combina$`paste(Make, Model, sep = " ")`)

#A variável Make - fabricante###################################################
unique(dados$Make)

#As marcas tem diferentes quantidades de carros na lista
#Audi, Kia, Porche e Volksvagen são as que tem mais representantes
table(dados$Make)

#Pelos gráfigos abaixo consigo ver que a Audi e a Porche tem mais concentração de 
#pontos com valores altos de média de consumo enquanto que Volks e Kia são mais econômicos
ggplot(dados)+
  geom_point(mapping = aes(x = Make, y = meanEnergyConsumption_kWh_100_km, colour= Make), size = 4)

#Vamos analisar então a media de consumo para cada grupo
dados%>%
  select(Make, meanEnergyConsumption_kWh_100_km)%>%
  group_by(Make)%>%
  summarise(mean = mean(meanEnergyConsumption_kWh_100_km))%>%
  arrange(desc(mean))
  
#A variável TypeBrakes#########################################################
unique(dados$TypeBrakes)#possui apenas 2 categorias

table(dados$TypeBrakes)

#O tipo "disc(front+rear) tem bem mais representantes (36) e não parece ser vantagem pois tem um grande range de 
#consumo
#O tipo disc(front)+drum(rear) tem apenas 7 representantes na lista porém todos com baixo consumo... 
ggplot(dados)+
  geom_point(mapping = aes(x = TypeBrakes, y = meanEnergyConsumption_kWh_100_km, colour= TypeBrakes), size = 4)

dados%>%
  select(TypeBrakes, meanEnergyConsumption_kWh_100_km)%>%
  group_by(TypeBrakes)%>%
  summarise(mean = mean(meanEnergyConsumption_kWh_100_km))%>%
  arrange(desc(mean))

#Apenas uma observação: Os 4 carros da Volks do dataset sao esse tipo de freio
dados%>%
  filter(TypeBrakes == "disc (front) + drum (rear)")


#A variável Drive type##########################################################
unique(dados$DriveType)#possui 3 categorias

table(dados$DriveType)

#O tipo 2WD tem uma concentração de seus pontos com baixo consumo enquanto o oposto
#ocorre com o 4WD
ggplot(dados)+
  geom_point(mapping = aes(x = DriveType, y = meanEnergyConsumption_kWh_100_km, colour= DriveType), size = 4)

dados%>%
  select(DriveType, meanEnergyConsumption_kWh_100_km)%>%
  group_by(DriveType)%>%
  summarise(mean = mean(meanEnergyConsumption_kWh_100_km))%>%
  arrange(desc(mean))

######Variáveis numericas######
numericas = colnames(dados[colunas=="double"])
numericas

df_num = as.data.frame(dados[numericas])
View(df_num)

#Plotando os histogramas das variáveis numéricas e da target
list <-lapply(1:ncol(df_num),function(col) ggplot(df_num, aes(x = df_num[[col]]))+
                                           geom_histogram()+
                                           labs(x = colnames(df_num[col])))

plot_grid(plotlist = list)


#Plotando os scatterplots das variáveis numéricas em relação a target
n = ncol(df_num)
n = n - 1
list <-lapply(1:n,function(col) ggplot(df_num, aes(x = df_num[[col]], y=df_num[["meanEnergyConsumption_kWh_100_km"]]))+
                geom_point()+
                labs(x = colnames(df_num[col]))+
                ylab("mean"))


plot_grid(plotlist = list)


#Plotando a correlação entre as variáveis numericas (inclui a target tb)
#Existe colinearidade entre as variáveis
correl = cor(df_num)
View(correl)
corrplot(correl, method = 'number', number.cex = 0.5, tl.cex = 0.5)
#para que o gráfico fique visivel temos que aumentar bastante a área de plotagem do R Studio

############################################################################
#Das variáveis categóricas, descartarei CarFullName, Model e TypeBrakes e utilizaremos 
#as demais como preditoras. 
dados$CarFullName<-NULL
dados$Model<-NULL
dados$TypeBrakes<-NULL
View(dados)

dados$Make <- as.numeric(as.factor(dados$Make))
dados$DriveType <- as.numeric(as.factor(dados$DriveType))
str(dados)

#Não vamos usar as variáveis cuja correlação com a target seja menor que abs(0.20)
which(abs(correl["meanEnergyConsumption_kWh_100_km", ]) < 0.2)
ignore<-unlist(names(which(abs(correl["meanEnergyConsumption_kWh_100_km", ]) < 0.2)))

dados[ignore]<-NULL
str(dados)#Ainda restaram 19 preditoras

#Separando dados de treino e teste
set.seed(23)
idx_treino <- createDataPartition(y= dados$meanEnergyConsumption_kWh_100_km, p=0.70, list=FALSE,)
idx_treino
treino_completo <- dados[idx_treino,]
X_treino <- treino_completo[,1:18]
y_treino <- treino_completo[, 19]
teste_completo<-dados[-idx_treino,]
X_teste <- teste_completo[,1:18]
y_teste <- teste_completo[, 19]

dim(X_treino)
dim(y_treino)
dim(X_teste)
dim(y_teste)


#Vamos processar as variáveis numericas para uma mesma escala
processamento <- preProcess(treino_completo, method=c("range"))
treino_completo_processado<- predict(processamento, treino_completo)
X_treino_processado <- treino_completo_processado[,1:18]
y_treino_processado <- treino_completo_processado[, 19]
View(treino_processado)
str(treino_processado)

teste_completo_processado = predict(processamento, teste_completo)
X_teste_processado <- teste_completo_processado[,1:18]
y_teste_processado <- teste_completo_processado[, 19]


#Abaixo algumas combinações de regressão linear
#Como há muita colinearidade entre as variáveis numéricas, optei por ter o segundo e terceiro modelo onde restringi
#a apenas variáveis preditoras que selecionei.
set.seed(23)
ctrl <- trainControl(method = "repeatedcv", number= 10, repeats = 20, verboseIter = FALSE)
modelo1 <- train(meanEnergyConsumption_kWh_100_km ~ ., data = treino_completo_processado, method = "lm", trControl = ctrl)         

modelo2 <- train(meanEnergyConsumption_kWh_100_km ~ Make+ DriveType + MaximumSpeed_Kph + MaximumLoadCapacity_kg, data = treino_completo_processado, method = "lm", trControl = ctrl)         

modelo3 <- train(meanEnergyConsumption_kWh_100_km ~ DriveType + MaximumSpeed_Kph + MaximumLoadCapacity_kg, data = treino_completo_processado, method = "lm", trControl = ctrl)


previsao1_treino <- predict (modelo1, newdata = X_treino_processado)
R2(previsao1_treino, y_treino_processado)
previsao1 <- predict(modelo1, newdata = X_teste_processado)
R2(previsao1, y_teste_processado)
varImp(modelo1)

previsao2_treino <- predict(modelo2, newdata = X_treino_processado)
R2(previsao2_treino, y_treino_processado)
previsao2 <- predict(modelo2, newdata = X_teste_processado)
R2(previsao2, y_teste_processado)

previsao3_treino <- predict(modelo3, newdata = X_treino_processado)
R2(previsao3_treino, y_treino_processado)
previsao3 <- predict(modelo3, newdata = X_teste_processado)
R2(previsao3, y_teste_processado)

#Dentre os três primeiros modelos lineares:
#O primeiro modelo linear apresentou melhor desempenho.
#Abaixo tentativa de usar Lasso. Ainda não teve o desempenho melhor que o modelo1
set.seed(23)
lambda <- c(0.1, 0.01, 0.001, 0.0001, 0.00001, 0.000001, 0.0000001, 0.00000001, 0.000000001, 0)
modelo4 <- train(x = X_treino_processado, y = treino_completo_processado$meanEnergyConsumption_kWh_100_km, method = "glmnet", trControl = ctrl, tuneGrid = expand.grid(alpha = 1, lambda = lambda))
modelo4$bestTune

previsao4_treino <- predict(modelo4, newdata = X_treino_processado)
R2(previsao4_treino, y_treino_processado)
previsao4 <- predict(modelo4, newdata = X_teste_processado)
R2(previsao4, y_teste_processado)


# Teste KNN pra regressao
set.seed(23)
#modelo5 <- knnreg(X_treino_processado, treino_completo_processado$meanEnergyConsumption_kWh_100_km, k = 3)
grid <- expand.grid(.k=seq(1,15,by=1))
modelo5 <- train(x = X_treino_processado, y = treino_completo_processado$meanEnergyConsumption_kWh_100_km, method = "knn", trControl = ctrl, tuneGrid = grid)
print(modelo5)
modelo5$bestTune
previsao5 <- predict(modelo5, newdata = X_teste_processado)
R2(previsao5,  y_teste_processado)

#Conclusão: Nos testes feitos o modelo 1 apresentou a melhor previsão entretanto é bom pontuar 
#que tanto o dataset de treino qt o de teste é muito pequeno e seria interessante testar com 
#datasets maiores
