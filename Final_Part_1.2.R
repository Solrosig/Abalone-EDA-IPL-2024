
### Parte 1 - Análise exploratória geral da base de dados

#Packages
install.packages("corrplot")
library(corrplot)
#install.packages("dplyr")
#library(dplyr)
library(moments) #for skewness
library(car) # for Q-Q plots
library (readr)
library(DescTools)
library(ggplot2)
library(ggcorrplot)


#1.1 A manipulação dos dados (preparação da base de dados para o tratamento estatístico)  

abalone_raw2<-read.csv("abalone_raw.csv")
str(abalone_raw2) #4177 amostras, 9 variaveis


#A análise à existência de observações omissas;
sum(is.na(abalone_raw2)) # 0 valores omissos
any(is.na(abalone_raw2)) # 0 valores omissos

# Redução da base de dados para 500 amostras
abalone<- head(abalone_raw2, 500)
str(abalone)

#Criar a nova variavel "Age" (quantitativa)
abalone$Age <- abalone$Rings+1.5

#Criar a nova variavel “AgeGroup” (ordinal)
abalone$AgeGroup <- cut(abalone$Age, breaks=c(0,5,10,31), labels=c("Young", "Adult", "Old"))
abalone$AgeGroup

#Converter as variáveis em fatores
#variavel “AgeGroup”
table(abalone$AgeGroup)
abalone$AgeGroup<-factor(abalone$AgeGroup)
abalone$AgeGroup
unique(abalone$AgeGroup) #verificar as categorias - "Young" "Adult" "Old"
#variavel “Sex”
table(abalone$Sex)
abalone$Sex <- factor(abalone$Sex, levels = c("F", "I", "M"), labels = c("Female", "Infant", "Male"))
abalone$Sex


#Inspeção de dados antes de guardar
str(abalone)
any(is.na(abalone))
dim(abalone)
sapply(abalone, class)
#Guardar a preparada base de dados
save(abalone, file="abalonef.Rdata");


#1.2 Escolha das 4 variaveis para analise:
#Variavel Nominal - Sex: Female, Male, Infant
#Variavel Ordinal - AgeGroup: "Old", "Adult", "Young"
#Variavel Quantitativa - Length: num, continuous (mm)/ Longest shell measurement
#Variavel Quantitativa - Whole.weight: num, continuous ((grams)/ Whole abalone wheight

colSums(is.na(abalone[, c("Sex", "AgeGroup", "Length", "Whole.weight")]))  # 0 missing values
abalone<-abalone[, c("Sex", "AgeGroup", "Length", "Whole.weight")]
abalone


#1.3 Análise geral 
#Summary de variaveis escolhidas
summary(abalone)

# Length: Min.: 0.0750, 1st Qu.: 0.4387, Median: 0.5300, Mean: 0.5068, 3rd Qu.: 0.5950, Max.: 0.7450 
# Whole.weight: Min.: 0.0020, 1st Qu.: 0.3910, Median: 0.7768, Mean: 0.7852, 3rd Qu.: 1.0744, Max.: 2.5500


# Descriptive Statistics of Length Variavel
length_stats <- data.frame(
  Mean = mean(abalone$Length), #media
  Mean_trimmed = mean(abalone$Length, trim=0.05), #média aparada a 5%
  Median = median(abalone$Length), #mediana
  Variance = var(abalone$Length),
  StdDev = sd(abalone$Length),
  MAD = mad(abalone$Length), #default central value as median
  MAD_mean = mad(abalone$Length, center = mean(abalone$Length)), #central value as mean
  IQR = IQR(abalone$Length),
  Skewness = skewness(abalone$Length),
  Min = min(abalone$Length),
  Max = max(abalone$Length),
  Outliers = length(boxplot.stats(abalone$Length)$out)
)
print(length_stats)

#  Mean    Median   Variance    StdDev     IQR   Skewness  Min    Max    Outliers 
# 0.50679   0.53 0.01563171 0.1250268 0.15625 -0.7429347 0.075  0.745       10

#Analise de extremos (mínimo e máximo), quartis, amplitude inter-quartis da variavel Length
#min
min(abalone$Length) #0.075 mm
which.min(abalone$Length) #amostra #237
abalone$Length[237]
abalone[237,]
#max
max(abalone$Length) #0.745 mm
which.max(abalone$Length) # amostra #359
abalone$Length[359]
abalone[359,]
#quartis e amplitude inter-quartis
range(abalone$Length) #min 0.075 max 0.745
diff(range(abalone$Length)) #0.67 mm
quantile(abalone$Length)
quantile(abalone$Length, c(0.05,0.95)) #5%=0.265, 95%=0.675
IQR(abalone$Length) #0.15625
summary(abalone$Length)

# Diagrama de extremos e quartis. Boxplot.
boxplot(abalone$Length,
        main="Boxplot do comprimento",
        xlab="Comprimento de abalones (mm)", ylab="",
        horizontal = TRUE,
        sub="Amostra de 500 individuos",
        col="skyblue")

boxplot.stats(abalone$Length)

#Coeficiente de assimetria - skewness de comprimento (Length)
skewness(abalone$Length) #-0.7429347
#Histograma de comprimento
hist(abalone$Length, main="Histograma do comprimento ", sub="Amostra de 500 individuos",
     xlab="Comprimento de abalones(mm)", ylab="Frequência", col="skyblue",breaks=6)


# Medidas descritivas da variavel de Peso total - Whole Weight
whole_weight_stats <- data.frame(
  Mean = mean(abalone$Whole.weight),
  Mean_trimmed = mean(abalone$Whole.weight, trim=0.05),
  Median = median(abalone$Whole.weight),
  Variance = var(abalone$Whole.weight),
  StdDev = sd(abalone$Whole.weight),
  MAD = mad(abalone$Whole.weight),
  MAD_mean = mad(abalone$Whole.weight, center = mean(abalone$Whole.weight)),
  IQR = IQR(abalone$Whole.weight),
  Skewness = skewness(abalone$Whole.weight),
  Min = min(abalone$Whole.weight),
  Max = max(abalone$Whole.weight),
  Outliers = length(boxplot.stats(abalone$Whole.weight)$out)
)
print(whole_weight_stats)
#     Mean Median   Variance    StdDev     IQR   Skewness  Min    Max    Outliers 
#   0.785165 0.77675 0.2309401 0.4805623 0.683375 0.525905 0.002 2.55        5

#Analise de extremos (mínimo e máximo), quartis, amplitude inter-quartis da variavel Whole wait
min(abalone$Whole.weight) #0.002 gr
which.min(abalone$Whole.weight) #amostra #237
abalone$Whole.weight[237]
abalone[237,]


max(abalone$Whole.weight) #2.55 gr
which.max(abalone$Whole.weight) # amostra #166
abalone$Whole.weight[166]
abalone[166,]


range(abalone$Whole.weight) #min 0.002 max 2.55
diff(range(abalone$Whole.weight)) #2.548 gr
quantile(abalone$Whole.weight)
#0%      25%      50%      75%     100% 
#0.002 0.3910 0.77675 1.074375 2.550000 
quantile(abalone$Whole.weight, c(0.05,0.95)) #5%=0.265, 95%=0.675
IQR(abalone$Whole.weight) #0.683375
summary(abalone$Whole.weight)
#Coeficiente de assimetria - skewness do peso total - Whole.Weight
skewness(abalone$Whole.weight) #0.525905

# Diagrama de extremos e quartis. Boxplot.
boxplot(abalone$Whole.weight,
        main="Boxplot do peso total",
        xlab="Peso total de abalones (gr)", ylab="",
        horizontal = TRUE,
        sub="Amostra de 500 individuos",
        col="green")

boxplot.stats(abalone$Whole.weight)

#Visualisação de histograma de peso total
hist(abalone$Whole.weight, main="Histograma do peso total", sub="Amostra de 500 individuos",
     xlab="Peso total de abalones (gr)", ylab="Frequência", col="green",breaks=6)


#Variavel Sexo de abalone
sex_table <- table(abalone$Sex)
sex_table
#Female Infant   Male 
#199     94    207
sex_proportions <- round(prop.table(table(abalone$Sex))*100,2)
sex_proportions
#  Female Infant   Male 
#   39.8   18.8   41.4

#Representação gráfica do género
par(mfrow=c(1,2)) 
barplot_sex<-barplot(sex_table,
                     xlab="Sexo", 
                     ylab="Número de observações",             
        col=c("pink","lightgreen","blue"),
        ylim = c(0, max(sex_table) + 10))
text(x = barplot_sex, 
     y = sex_table + 6, 
     labels = sex_table)

pie(sex_proportions, col=c("pink","lightgreen","blue"))
par(mfrow=c(1,1))
mtext("Representação gráfica do género",side=3,line=1)


#Variavel Age Group de abalone
agegroup_table<-table(abalone$AgeGroup)
agegroup_table
agegroup_proportions<-round(prop.table(table(abalone$AgeGroup))*100,2)
agegroup_proportions

#Representação gráfica de grupos de idade
par(mfrow=c(1,2))
barplot_agegroup<- barplot(agegroup_table, 
        #main="Grupos da idade de moluscos abalone",
        xlab="Grupo de idade", 
        ylab="Número de observações", 
        col=rainbow(3),
        ylim=c(0,400))
text(c(0.70,1.9,3.1,4.3), 
     agegroup_table+10, 
     labels = agegroup_table)

pie(agegroup_proportions,col=rainbow(3))
par(mfrow=c(1,1))
mtext("Representação gráfica da idade de moluscos abalone",side=3,line=1)


#1.4. Intervalos de confiança par variavel nominal 
# proportions of individuals by category in Sex
sex_table <- table(abalone$Sex) #tabela de frequência
#n=500
sex_table$Male<-abalone$Sex=="Male"

sex_proportions <- round(prop.table(table(abalone$Sex))*100,2)
sex_proportions
#  Female Infant   Male 
#   39.8   18.8   41.4

#um intervalo, com 95% de confiança, 
#para a proporção de abalones do sexo:
#a) Masculino
abalone$SexM <- abalone$Sex == "M"
prop.test(rev(table(abalone$SexM),
          500,
          conf.level= 0.95, 
          correct = TRUE)
          
#um intervalo, com 95% de confiança, 
#para a proporção de abalones do sexo:
#b) Feminino
abalone$SexF <- abalone$Sex == "F"
prop.test(rev(table(abalone$SexF),
              500,
              conf.level= 0.95, 
              correct = TRUE)

#um intervalo, com 95% de confiança, 
#para a proporção de abalones do sexo:
#b) Infant
abalone$SexI <- abalone$Sex == "I"
prop.test(rev(table(abalone$SexI),
              500,
              conf.level= 0.95, 
              correct = TRUE)



#1.5. Testes de normalidade
# Shapiro-Wilk Test para comprimento de abalone
# H0: Comprimento tem distribuição normal
# H1: Comprimento não distribuição normal
shapiro.test(abalone$Length)
#p-value = 8.657e-11
#p-value = 8.657e-11 < alfa = 0.05 -> Rejeitar H0

shapiro.test(abalone$Whole.weight)
#p-value = 1.123e-08
#p-value = 1.123e-08 < alfa = 0.05 -> Rejeitar H0

# Q-Q Plots
qqPlot(abalone$Length, 
       main = "Q-Q Plot do comprimento",
       xlab="Quantis da distribuição normal",
       ylab = "Quantis do comprimento")
#potenciais outliers: 237, 239

qqPlot(abalone$Whole.weight, 
       main = "Q-Q Plot do peso total",
       xlab="Quantis da distribuição normal",
       ylab = "Quantis do peso total")
#potencias outliers: 166, 359



#1.6. Comparação dos valores de variaveis quantitativos (Comprimento e Peso total) 
#com valores de categorias de valor ordinal

#medidas de tendência central, de dispersão, de assimetria e boxplots de variaveis quantitativos
summary(abalone$Length)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0750  0.4387  0.5300  0.5068  0.5950  0.7450 
IQR(abalone$Length)
max(abalone$Length)-min(abalone$Length)
sd(abalone$Length)
skewness(abalone$Length)

boxplot(abalone$Length,
        main = "Comprimento de abalones",
        sub="Amostra de 500 individuos",
        xlab = "Comprimento (mm)",
        col="lightblue",
        horizontal = TRUE)

boxplot.stats(abalone$Length)

summary(abalone$Whole.weight)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.002  0.3910  0.7768  0.7852  1.0744  2.5500

#A média, o desvio padrão, e variabilidade do comprimento dos abalones
# consoante o seu grupo da idade.


tapply(abalone$Length, abalone$AgeGroup, mean)
#Young     Adult       Old 
#0.1200 0.3554911 0.5549479
tapply(abalone$Length, abalone$AgeGroup, sd)
#Young      Adult        Old 
#0.03763863 0.09442324 0.08478969 
tapply(abalone$Length, abalone$AgeGroup, var)
#Young       Adult         Old 
#0.001416667 0.008915748 0.007189292 

#A média e o desvio padrão do peso total dos abalones
# consoante o seu grupo da idade.

tapply(abalone$Whole.weight, abalone$AgeGroup, sd)
#Young     Adult       Old 
#0.00825 0.2158911 0.4241614

tapply(abalone$Whole.weight, abalone$AgeGroup, mean)
#Young     Adult       Old 
#0.0111250 0.2739063 0.9423451

tapply(abalone$Whole.weight, abalone$AgeGroup, var)
#Young        Adult          Old 
#0.0000680625 0.0466089664 0.1799129257 

#calculação dos coeficientes de variabilidade CV
#Comprimento (Length) por Age Group

LF<- abalone$Length[abalone$AgeGroup == "Young"]
sd(LF)/mean(LF) # 0.3136553
LM<-abalone$Length[abalone$AgeGroup == "Adult"]
sd(LM)/mean(LM) # 0.2656135
LI<-abalone$Length[abalone$AgeGroup == "Old"]
sd(LI)/mean(LI) # 0.1527886

#O maior coeficiente de variabilidade no comprimento em relação à média indica
#que os comprimentos dos abalones jovens são mais inconsistentes em comparação com outras faixas etárias
##Os abalones velhos têm a menor variabilidade em comprimento, 
#sugerindo que os seus comprimentos são mais consistentes. 

#Peso total (Whole.weight) por Age Group

WF<-abalone$Whole.weight[abalone$AgeGroup == "Young"]
sd(WF)/mean(WF) #0.741573
WM<-abalone$Whole.weight[abalone$AgeGroup == "Adult"]
sd(WM)/mean(WM) #0.7881934
WI<-abalone$Whole.weight[abalone$AgeGroup == "Old"]
sd(WI)/mean(WI) #0.4501127
#Os abalones adultos apresentam a maior variabilidade no peso total em relação à média, 
#indicando uma maior inconsistência nos seus pesos.
#Os abalones velhos têm a menor variabilidade em Whole.weight 
#significando que os seus pesos são relativamente uniformes. 


# Boxplot de cumprimento por grupos de idade (variavel ordinal)
boxplot_comprimento_idade<-ggplot(abalone, aes(x = AgeGroup, y = Length)) + 
  geom_boxplot(fill = "skyblue") +
  labs(title = "Comparação do Comprimento de abalones por Grupos de idade", x = "Grupos de idade", y = "Comprimento (mm)")
boxplot_comprimento_idade

# Boxplot de peso total por grupos de idade (variavel ordinal)
boxplot_peso_idade<-ggplot(abalone, aes(x = AgeGroup, y = Whole.weight)) + 
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Comparação do Peso total de abalones por Grupos de idade", x = "Grupos de idade", y = "Peso total (gr)")
boxplot_peso_idade



#1.7. Medidas de associação e teste de independência para variaveis qualitativas: 
#nominal (Sex)  and ordinal (AgeGroup)

#tabela de contingência entre variaveis qualitativas
table_sex_agegroup <- table(abalone$Sex, abalone$AgeGroup)
prop.table(table_sex_agegroup)
round(prop.table(table_sex_agegroup)*100,2)

#representação gráfica de 
barplot(table_sex_agegroup, 
        main="Grupos de idade por género", 
        xlab="Idade", 
        ylab="Número de abalones",
        col = c("pink","lightgreen", "blue"),
        legend.text = TRUE,
        beside = TRUE)

#Medidas de associação entre as variáveis

ContCoef(table_sex_agegroup);
# Coeficiente de contigência de Pearson 0.5256545

CramerV(table_sex_agegroup); 
# Coeficiente de Cramer 0.436928

Phi(table_sex_agegroup)
# Coeficiente Phi 0.6179096

#Teste de independência, com um nível de significância 5%
# alfa = 0.05
# H0: variáveis Sex e AgeGroup são independentes
# H1: variáveis Sexo e AgeGroup não são independentes
fisher.test(table_sex_agegroup)
#p-value = 2.2e-16 < alfa = 0.05 -> rejeitar H0

#chisquare test para verificar valores esperados
chisq_test <- chisq.test(table_sex_agegroup)
expected_counts <- chisq_test$expected
expected_counts # há baixos valores 0.752 < 1, e 2 valores < 5 -> recomendado utilizar fisher test
chisq_test 
#X-squared = 190.91, df = 4, p-value < 2.2e-16



#1.8. Análise de correlação entre todas as variáveis quantitativas

#Relação entre variaveis quantitativas escolhidas
plot(abalone$Length, 
     abalone$Whole.weight,
     main = "Comprimento vs Peso total dos abalones",
     xlab = "Comprimento (em mm)",
     ylab = "Peso total (em gr)")

#Determinação do valor do coeficiente de correlação
# H0: Comprimento tem distribuição normal
# H1: Comprimento não tem distribuição normal
shapiro.test(abalone$Length)
# valor p = 8.657e-11 < alfa = 0.05 -> rejeitar H0
# H0: Peso total tem distribuição normal
# H1: Peso total não tem distribuição normal
shapiro.test(abalone$Whole.weight)
# valor p = 1.123e-08 < alfa = 0.05 -> rejeitar H0
# O coeficiente de Pearson não pode ser utilizado. 
#É preciso determinar o coeficiente de Spearman (ou Kendall)

#Teste, com 5% de significância,
#se a correlação entre estas duas variáveis é nula
# H0: Correlação nula (rho = 0)
# H1: Correlação não nula (rho != 0)
cor.test(abalone$Length,
         abalone$Whole.weight, 
         method = "spearman")
# p-value < 2.2e-16 < alfa -> rejeitar H0,

# O coeficiente de correlação Spearman
cor(abalone$Length,
    abalone$Whole.weight, 
    method = "spearman")
# Correlação de Spearman = 0.9783138

#O coeficiente de correlação é positivo 0.978 e muito forte 


par(mfrow=c(1,3))
plot(abalone$Length[abalone$Sex=="Female"], 
     abalone$Whole.weight[abalone$Sex=="Female"],
     main = "Genero feminino",
     xlab = "Comprimento (em mm)",
     ylab = "Peso total (em gr)")

plot(abalone$Length[abalone$Sex=="Male"], 
     abalone$Whole.weight[abalone$Sex=="Male"],
     main = "Genero masculino",
     xlab = "Comprimento (em mm)",
     ylab = "Peso total (em gr)")

plot(abalone$Length[abalone$Sex=="Infant"], 
     abalone$Whole.weight[abalone$Sex=="Infant"],
     main = "Genero infantil",
     xlab = "Comprimento (em mm)",
     ylab = "Peso total (em gr)")


#A matriz de correlação para visualizar coeficientes de correlação entre as variáveis quantitativas
str(abalone_raw2)

quant_vars <- abalone_raw2[, c("Length", "Diameter", "Height", "Whole.weight", "Shucked.weight", "Viscera.weight", "Shell.weight")]
quant_vars_pairs<-pairs(abalone_raw2[, c("Length", "Diameter", "Height", "Whole.weight", "Shucked.weight", "Viscera.weight", "Shell.weight")])

cor_matrix <-round(cor(quant_vars,
    method="spearman"),4)


corrplot(cor_matrix, 
         method = "color",
         type = "lower",
         addCoef.col = TRUE,
         diag = FALSE,
         order = "hclust",
         number.cex = 0.7)

ggcorrplot(cor_matrix, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)


