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

#Remover variavel age
abalone<-abalone[,-10]

#Converter as variáveis em fatores
#variavel “AgeGroup”
table(abalone$AgeGroup)
abalone$AgeGroup<-factor(abalone$AgeGroup)
abalone$AgeGroup
unique(abalone$AgeGroup) #verificar as categorias - "Young" "Adult" "Old"
#variavel “Sex”
table(abalone$Sex)
abalone$Sex <- factor(abalone$Sex)
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


#1.3 a apresentação de tabelas, gráficos e medidas adequadas ao resumo da informação das 4 variáveis escolhidas para análise 
#(uma nominal, uma ordinal e duas quantitativas);


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

#     Mean Median   Variance    StdDev     IQR   Skewness  Min    Max    Outliers 
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
#Histograma 
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


#1.4. a apresentação de intervalos com 95% de confiança para a percentagem de indivíduos em cada
#categoria da variável nominal;

abalone$SexF <- abalone$Sex == "F"
abalone$SexI <- abalone$Sex == "I"
abalone$SexM <- abalone$Sex == "M"
table(abalone$Sex)

#Sexo Feminino
prop.test(rev(table(abalone$SexF)), 
          conf.level= 0.95, 
          correct = TRUE)
#IC bilateral: p pertence ]0.355, 0.442[ com 95% de confiança para indivíduos do sexo feminino

#Infante
prop.test(rev(table(abalone$SexI)), 
          conf.level= 0.95, 
          correct = TRUE)
#IC bilateral: p pertence ]0.155, 0.226[ com 95% de confiança para indivíduos infantes

#Sexo Masculino
prop.test(rev(table(abalone$SexM)), 
          conf.level= 0.95, 
          correct = TRUE)
#IC bilateral: p pertence ]0.371, 0.459[ com 95% de confiança para indivíduos do sexo masculino


#1.5. a análise se alguma das duas variáveis quantitativas escolhidas pode ser caracterizada por uma
#distribuição normal;

#Testes de normalidade
# Shapiro-Wilk Test para comprimento de abalone
# H0: Comprimento tem distribuição normal
# H1: Comprimento não tem distribuição normal

shapiro.test(abalone$Length)

#p-value = 8.657e-11 < alfa = 0.05 -> Rejeitar H0, logo há evidência estatística
#de que o comprimento de abalone não tem distribuição normal

shapiro.test(abalone$Whole.weight)

#p-value = 1.123e-08 < alfa = 0.05 -> Rejeitar H0, logo há evidência estatística
#de que o peso de abalone não tem distribuição normal

#Desta forma podemos concluir que nenhuma das variáveis quantitativas escolhidas
#pode ser caracterizada por uma distribuição normal

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



#1.6. a comparação dos valores observados nas duas variáveis quantitativas nas diferentes categorias
#da variável ordinal, incluindo uma comparação da sua variabilidade em cada categoria, bem
#como do seu valor médio; (Comprimento e peso total)


#Variabilidade - amplitude, amplitude inter-quartis, MAD, variância, desvio padrão, CV, outliers

#Comprimento dos Abalones - por AgeGroup

#Valor médio - média
tapply(abalone$Length, abalone$AgeGroup, mean)
#Young      Adult       Old     
#0.1200000  0.3554911  0.5549479  

#Desvio padrão
tapply(abalone$Length, abalone$AgeGroup, sd)
#Young      Adult        Old 
#0.03763863 0.09442324 0.08478969 

#Variância
tapply(abalone$Length, abalone$AgeGroup, var)
#Young       Adult         Old 
#0.001416667 0.008915748 0.007189292 

#Amplitude
tapply(abalone$Length, abalone$AgeGroup, range)
#   Young        Adult         Old
#0.075 0.165   0.16 0.61   0.280 0.745

#Amplitude Inter-Quartil
tapply(abalone$Length, abalone$AgeGroup, IQR)
#Young     Adult     Old   
#0.03750  0.13750  0.11125 

#Desvio médio absoluto
tapply(abalone$Length, abalone$AgeGroup, mad)
#Young        Adult       Old     
#0.0407715  0.1000755  0.0815430  

#Coeficiente de variação
tapply(abalone$Length, abalone$AgeGroup, function(x) {
  media <- mean(x)
  desvio_padrao <- sd(x)
  cv <- (desvio_padrao/media)
  return(cv)
})
#Young       Adult       Old  
#0.3136553 0.2656135 0.1527886



#Peso dos Abalones - por AgeGroup

#Por AgeGroup

#Valor médio - média
tapply(abalone$Whole.weight, abalone$AgeGroup, mean)
#Young        Adult        Old
#0.0111250  0.2739063  0.9423451 

#Desvio padrão
tapply(abalone$Whole.weight, abalone$AgeGroup, sd)
#Young      Adult        Old 
#0.0082500  0.2158911 0.4241614 

#Variância
tapply(abalone$Whole.weight, abalone$AgeGroup, var)
#Young            Adult         Old 
#0.0000680625  0.0466089664 0.1799129257

#Amplitude
tapply(abalone$Whole.weight, abalone$AgeGroup, range)
#   Young          Adult          Old
#0.0020 0.0215   0.021 1.153   0.124 2.550

#Amplitude Inter-Quartil
tapply(abalone$Whole.weight, abalone$AgeGroup, IQR)
#Young      Adult     Old   
#0.008625  0.252125  0.561125 

#Desvio médio absoluto
tapply(abalone$Whole.weight, abalone$AgeGroup, mad)
#Young        Adult       Old     
#0.0081543  0.1686457  0.4162400  

#Coeficiente de variação
tapply(abalone$Whole.weight, abalone$AgeGroup, function(x) {
  media <- mean(x)
  desvio_padrao <- sd(x)
  cv <- (desvio_padrao/media)
  return(cv)
})
#Young       Adult       Old  
#0.7415730  0.7881934  0.4501127


#As duas variáveis, "Length" e "Whole.Weight", aumentam de forma consistente em 
#média à medida que os grupos etários avançam, mostrando que os abalones crescem
#com a idade.
#A variação (como desvio padrão, amplitude, IQR e MAD) é maior para o peso do 
#que para o comprimento, o que significa que o peso apresenta diferenças mais 
#marcantes entre os indivíduos.
#O coeficiente de variação mostra que, no comprimento, a variação relativa é 
#mais evidente nos jovens, enquanto, no peso, essa variação é maior nos grupos 
#Young e Adult.
#Assim, "Whole.Weight" apresenta maior dispersão e diferenças entre os 
#indivíduos do que a variável "Lenght", especialmente nos abalones mais velhos.


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

#1.7
t_sex_age <- table(abalone$AgeGroup, abalone$Sex)
library(DescTools);ContCoef(t_sex_age);
# 0.525 existe alguma associacao entre as variaveis 
# alfa = 0.05
#H0: variáveis AgeGroup e Sex são independentes
#H1: variáveis AgeGroup e Sex não são independentes
fisher.test(t_sex_age)
# p-value < 2.2e-16 < 0.05 alfa, logo rejeitamos HO, existe evidencia estatistica que as variaveis nao sao independentes

#1.8
#Como testado anteriormente as variaveis quantitativas nao seguem a normalidade, logo nao podemos utilizar o metodo pearson
names(abalone)
cor(abalone[,c(2,5)], method = "spearman")
# correlacao = 0.9783138, ha uma relacao monotona quando uma variavel aumenta a outra tambem aumenta


# ANÁLISE FATORIAL
# Correlação entre variáveis
abalone_af <- abalone[,2:9]
cor(abalone_af)
library(corrplot); 
corrplot(cor(abalone_af), 
         type = "lower", 
         addCoef.col = TRUE,
         diag = FALSE)

pairs(abalone_af, col=abalone$AgeGroup)
pairs(abalone_af, col=abalone$Sex)

# Índice KMO
library(psych); 
KMO(abalone_af)
# Overall MSA =  0.87 -> boa adequação à aplicação da análise fatorial
# Todas as variáveis MSA >= 0.78 (boa)

# Número de componentes utilizadas, com referência à variância explicada
pca <- princomp(abalone_af, cor=TRUE)
summary(pca)
# Gráfico scree plot
library(factoextra); 
fviz_eig(pca, addlabels=TRUE)
# vamos utilizar duas componentes, ficamos com 93.3% da informação (87.8+5.5)
# pesos 
pca$loadings
pca$scores
fviz_pca_ind(pca, 
             col.ind = "cos2", 
             gradient.cols = c("blue", "yellow", "red"), 
             legend.title = "Representação")
fviz_pca_var(pca, 
             col.var = "contrib", 
             gradient.cols = c("blue", "yellow", "red"), 
             repel = TRUE, 
             legend.title = "Contribuição")
fviz_pca_biplot(pca)
biplot(pca)

# Distinguir sexo e ageGroup

# Sexo
boxplot(pca$scores[,1] ~ abalone$Sex)
# 1 componente permite discriminar parcialmente o sex I dos restantes
boxplot(pca$scores[,2] ~ abalone$Sex)
boxplot(pca$scores[,3] ~ abalone$Sex)
boxplot(pca$scores[,4] ~ abalone$Sex)
boxplot(pca$scores[,5] ~ abalone$Sex)
boxplot(pca$scores[,6] ~ abalone$Sex)
boxplot(pca$scores[,7] ~ abalone$Sex)
boxplot(pca$scores[,8] ~ abalone$Sex)
pairs(pca$scores,
      col = abalone$Sex)
fviz_pca_biplot(pca, 
                fill.ind = abalone$Sex, 
                pointshape = 21,
                pointsize = 2,
                addEllipses = TRUE, 
                legend.title = "Sex")

# ageGroup
boxplot(pca$scores[,1] ~ abalone$AgeGroup)
# permite discrimanar Young e parcialmente Old e Adult
boxplot(pca$scores[,2] ~ abalone$AgeGroup)
boxplot(pca$scores[,3] ~ abalone$AgeGroup)
# permite discriminar young dos restantes
boxplot(pca$scores[,4] ~ abalone$AgeGroup)
boxplot(pca$scores[,5] ~ abalone$AgeGroup)
boxplot(pca$scores[,6] ~ abalone$AgeGroup)
boxplot(pca$scores[,7] ~ abalone$AgeGroup)
boxplot(pca$scores[,8] ~ abalone$AgeGroup)
pairs(pca$scores,
      col = abalone$AgeGroup)
fviz_pca_biplot(pca, 
                fill.ind = abalone$AgeGroup, 
                axes = c(1,3),
                pointshape = 21, 
                pointsize = 2,
                addEllipses = TRUE, 
                legend.title = "AgeGroup")



#3. Análise de Clusters

#Variáveis quantitativas -> Length, Diameter, Height, Whole.Weight, Shucked.Weight, Viscera.Weight, Shell.Weight, Rings, Age

#• a aplicação de um método hierárquico (apresentando o respetivo dendrograma);


#Standardizar variáveis quantitativas

abalone_s <- as.data.frame(scale(abalone[,2:10]))
abalone_s

#Experimentação para descobrir o melhor dendrograma

distance_matrix <- dist(abalone_s, "euclidean")
distance_matrix2 <- dist(abalone_s, "manhattan")
distance_matrix3 <- dist(abalone_s, "minkowski")

#Definição e representação do número de clusters por análise ao gráfico

hc = hclust(distance_matrix, method="ward.D2")
plot(hc, hang=-1);

rect.hclust(hc, 2, border="red")
rect.hclust(hc, 3, border="blue")

hc2 = hclust(distance_matrix2, method="ward.D2") #segunda melhor combinação
plot(hc2, hang=-1);

rect.hclust(hc2, 2, border="red")
rect.hclust(hc2, 3, border="blue")
rect.hclust(hc2, 4, border="green")

hc3 = hclust(distance_matrix3, method="ward.D2")
plot(hc3, hang=-1);

rect.hclust(hc3, 2, border="red")
rect.hclust(hc3, 3, border="blue")

library(cluster);

cluster <- cutree(hc, 2)
cluster2 <- cutree(hc2, 2)
cluster3 <- cutree(hc3, 2)
cluster_3 <- cutree(hc, 3)
cluster2_3 <- cutree(hc2, 3)
cluster3_3 <- cutree(hc3, 3)

library(factoextra)

fviz_silhouette(silhouette(cluster, distance_matrix)) #0.47 com 2 clusters
fviz_silhouette(silhouette(cluster2, distance_matrix2)) #0.47 com 2 clusters
fviz_silhouette(silhouette(cluster3, distance_matrix3)) #0.47 com 2 clusters

fviz_silhouette(silhouette(cluster_3, distance_matrix)) #0.36 com 2 clusters
fviz_silhouette(silhouette(cluster2_3, distance_matrix2)) #0.4 com 2 clusters
fviz_silhouette(silhouette(cluster3_3, distance_matrix3)) #0.36 com 2 clusters

#Nesta análise, 2 clusters obtém melhores valores do que 3

library(fpc);

calinhara(abalone_s, cluster) #2 clusters: 598.3716
calinhara(abalone_s, cluster2) #2 clusters: 466.5097
calinhara(abalone_s, cluster3) #2 clusters: 598.3716


calinhara(abalone_s, cluster_3) #3 clusters: 625.5605
calinhara(abalone_s, cluster2_3) #3 clusters: 580.502
calinhara(abalone_s, cluster3_3) #3 clusters: 625.5605

#Nesta análise, 3 clusters obtém melhores valores do que 2



#• a aplicação de um método não hierárquico (k-means);

library(factoextra)

fviz_nbclust(abalone_s, kmeans, method = "wss") #4 clusters
fviz_nbclust(abalone_s, kmeans, method = "silhouette") #2 clusters


#Aplicação do k-means a 2 clusters:

km2 <- kmeans(abalone_s, 2);
print(km2)

#Within cluster sum of squares by cluster:
#  [1] 1181.9708  666.3329
#(between_SS / total_SS =  58.8 %)


km2$totss #dispersão total 4491
km2$withinss #dispersão intra cluster
#Cluster1: 1181.9708  
#Cluster2: 666.3329

km2$tot.withinss #dispersão intra total: 1848.304
km2$betweenss #dispersão inter cluster: 2642.696

km2$size
#cluster 1: 277  individuos
#cluster 2: 223 individuos


#Aplicação do k-means a 4 clusters:

km4 <- kmeans(abalone_s, 4);
print(km4)
help("kmeans")

#Within cluster sum of squares by cluster:
#  [1] 272.4379 153.1390 364.5787 140.9963
#(between_SS / total_SS =  79.3 %)


km4$totss #dispersão total 4491
km4$withinss #dispersão intra cluster
#Cluster1: 272.4379   
#Cluster2: 153.1390 
#Cluster3: 364.5787 
#Cluster4: 140.9963

km4$tot.withinss #dispersão intra total: 931.1519
km4$betweenss #dispersão inter cluster: 3559.848

km4$size
#cluster 1: 73 indivíduos
#cluster 2: 140 indivíduos
#Cluster 3: 190 indivíduos
#Cluster 4: 97 indivíduos


#• a comparação dos grupos obtidos com os grupos definidos pelas variáveis qualitativas utilizadas na primeira questão.

library(aricode);
#2 clusters
table(abalone$Sex, km2$cluster);
ARI(abalone$Sex, km2$cluster) #0.1735119

table(abalone$AgeGroup, km2$cluster);
ARI(abalone$AgeGroup, km2$cluster) #0.3568938

#4 clusters
table(abalone$Sex, km4$cluster);
ARI(abalone$Sex, km4$cluster) #0.1588959

table(abalone$AgeGroup, km4$cluster);
ARI(abalone$AgeGroup, km4$cluster) #0.2028441





# Algoritmo de classificação Naive Bayes
# Classificar variáveis qualitativas
set.seed(123456789) 
s <- sample(1:nrow(abalone))
Treino <- s[1:floor(0.75*nrow(abalone))]
abalone_treino <- abalone[Treino,]
abalone_teste <- abalone[-Treino,]

# Estimação do modelo com a amostra de treino
library(e1071)
# sexo
NBC_sex <- naiveBayes(abalone_treino[,2:9], abalone_treino$Sex)
# ageGroup
NBC_ageGroup <- naiveBayes(abalone_treino[,2:9], abalone_treino$AgeGroup)

# Avaliação da fiabilidade do modelo com a amostra de teste
# sexo
NBC_sex.prob <- predict(NBC_sex, abalone_teste, type="raw")
NBC_sex.class <- predict(NBC_sex, abalone_teste)
library(caret)
confusionMatrix(NBC_sex.class, abalone_teste$Sex)
confusionMatrix(NBC_sex.class, abalone_teste$Sex, mode="prec_recall")
# ageGroup
NBC_ageGroup.prob <- predict(NBC_ageGroup, abalone_teste, type="raw")
NBC_ageGroup.class <- predict(NBC_ageGroup, abalone_teste)
library(caret)
confusionMatrix(NBC_ageGroup.class, abalone_teste$AgeGroup)
confusionMatrix(NBC_ageGroup.class, abalone_teste$AgeGroup, mode="prec_recall")

